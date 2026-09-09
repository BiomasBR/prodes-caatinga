"""
Precipitacao acumulada quinzenal MERGE/CPTEC-INPE - 2022
por poligono do shapefile Tiles_BDC_Caatinga_Clusters_v2.zip

Saidas:
  1) precipitacao_MERGE_quinzenal_2022_long.csv
  2) precipitacao_MERGE_quinzenal_2022_wide.csv
  3) precipitacao_MERGE_quinzenal_2022.xlsx

Definicao das quinzenas:
  - Q1: dias 01 a 15
  - Q2: dias 16 ao ultimo dia do mes

Fonte oficial MERGE:
https://ftp.cptec.inpe.br/modelos/tempo/MERGE/GPM/DAILY/

Estrutura dos arquivos diarios:
YYYY/MM/MERGE_CPTEC_YYYYMMDD.grib2

O MERGE diario contem, entre outras informacoes:
  PREC = precipitacao acumulada em 24 h [kg/m2], numericamente equivalente a mm de agua.

O calculo espacial usa exactextract e a operacao:
  mean(coverage_weight=area_spherical_m2)
isto e, a media da lamina acumulada ponderada pela area da parte de cada celula
que intercepta o poligono, corrigindo tambem a variacao de area das celulas em
coordenadas geograficas.

Para Jupyter, instale os pacotes (uma unica vez):
  %pip install geopandas rasterio exactextract requests pandas numpy openpyxl tqdm

Se o Rasterio do seu ambiente nao tiver suporte ao driver GRIB, em ambiente
Conda/Anaconda use preferencialmente:
  conda install -c conda-forge rasterio gdal eccodes geopandas
  pip install exactextract requests openpyxl tqdm

Depois, execute no notebook:
  %run precipitacao_MERGE_quinzenal_2022_por_poligono.py
"""

from __future__ import annotations

import calendar
import sys
import time
from datetime import date, timedelta
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import rasterio
from rasterio.windows import Window, from_bounds
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from tqdm.auto import tqdm

try:
    from exactextract import exact_extract
except ImportError as exc:
    raise ImportError(
        "O pacote 'exactextract' nao esta instalado. No Jupyter execute: "
        "%pip install exactextract"
    ) from exc


# ============================================================================
# 1. CONFIGURACAO
# ============================================================================

ANO = 2022

# ZIP enviado pelo usuario. Se estiver em outra pasta, informe o caminho completo.
SHAPEFILE_ZIP = Path("Tiles_BDC_Caatinga_Clusters_v2.zip")

# Campo identificador unico dos 113 poligonos do arquivo enviado.
ID_FIELD = "Tile_str"

BASE_URL = "https://ftp.cptec.inpe.br/modelos/tempo/MERGE/GPM/DAILY"

# Diretorios locais
PASTA_DADOS = Path("MERGE_2022_daily")
PASTA_TEMP = Path("MERGE_2022_quinzenas_temp")
PASTA_SAIDA = Path("resultados_MERGE_2022")

# Se True, os 366 GRIB2 ficam em disco para permitir reexecucao sem novo download.
# O conjunto de 2022 ocupa aproximadamente algumas centenas de MB.
MANTER_ARQUIVOS_DIARIOS = True

# Se True, qualquer dia ausente interrompe o processamento para evitar acumulados
# quinzenais subestimados.
INTERROMPER_SE_DIA_FALTANTE = True

# Numero de tentativas adicionais em caso de falha de conexao.
MAX_RETRIES = 5
TIMEOUT = 120

# Margem ao redor do conjunto de poligonos, em graus, usada para recortar o MERGE
# antes da estatistica zonal. 0.2 graus = aproximadamente duas celulas do MERGE.
MARGEM_GRAUS = 0.2


# ============================================================================
# 2. FUNCOES AUXILIARES
# ============================================================================


def criar_sessao_http() -> requests.Session:
    """Cria sessao HTTP com retries automaticos."""
    retry = Retry(
        total=MAX_RETRIES,
        connect=MAX_RETRIES,
        read=MAX_RETRIES,
        status=MAX_RETRIES,
        backoff_factor=1.5,
        status_forcelist=(429, 500, 502, 503, 504),
        allowed_methods=frozenset(["GET", "HEAD"]),
        raise_on_status=False,
    )
    adapter = HTTPAdapter(max_retries=retry)
    sessao = requests.Session()
    sessao.mount("https://", adapter)
    sessao.mount("http://", adapter)
    sessao.headers.update(
        {"User-Agent": "Mozilla/5.0 Python MERGE-CPTEC-INPE zonal precipitation"}
    )
    return sessao


def url_merge_diario(data: date) -> str:
    """Monta a URL oficial do MERGE diario para uma data."""
    return (
        f"{BASE_URL}/{data:%Y}/{data:%m}/"
        f"MERGE_CPTEC_{data:%Y%m%d}.grib2"
    )


def caminho_merge_diario(data: date) -> Path:
    """Caminho local de um GRIB2 diario."""
    pasta = PASTA_DADOS / f"{data:%m}"
    pasta.mkdir(parents=True, exist_ok=True)
    return pasta / f"MERGE_CPTEC_{data:%Y%m%d}.grib2"


def baixar_arquivo(sessao: requests.Session, data: date) -> Path:
    """Baixa um arquivo diario. Reaproveita arquivo ja existente."""
    destino = caminho_merge_diario(data)

    # Arquivos MERGE diarios costumam ter centenas de kB. Evita reutilizar arquivo
    # vazio/corrompido de uma tentativa anterior.
    if destino.exists() and destino.stat().st_size > 10_000:
        return destino

    url = url_merge_diario(data)
    temporario = destino.with_suffix(destino.suffix + ".part")

    try:
        with sessao.get(url, stream=True, timeout=TIMEOUT) as resposta:
            if resposta.status_code != 200:
                raise RuntimeError(
                    f"Falha ao baixar {data:%Y-%m-%d}: HTTP {resposta.status_code}\n{url}"
                )

            with open(temporario, "wb") as f:
                for bloco in resposta.iter_content(chunk_size=1024 * 1024):
                    if bloco:
                        f.write(bloco)

        if temporario.stat().st_size <= 10_000:
            raise RuntimeError(
                f"Arquivo muito pequeno e possivelmente invalido: {temporario}"
            )

        temporario.replace(destino)
        return destino

    except Exception:
        if temporario.exists():
            temporario.unlink()
        raise


def verificar_driver_grib() -> None:
    """Confirma que o GDAL/Rasterio atual consegue ler GRIB2."""
    with rasterio.Env() as env:
        drivers = env.drivers()
    if "GRIB" not in drivers:
        raise RuntimeError(
            "Seu Rasterio/GDAL nao possui o driver GRIB. Em Anaconda/Miniconda, "
            "instale: conda install -c conda-forge rasterio gdal eccodes geopandas"
        )


def localizar_banda_precipitacao(src: rasterio.io.DatasetReader) -> int:
    """
    Localiza a banda de precipitacao no GRIB2.

    O CTL oficial do MERGE lista PREC antes de NEST. Ainda assim, a funcao tenta
    identificar a banda pelos metadados do GDAL e usa a banda 1 apenas como fallback.
    """
    candidatos = []

    for banda in src.indexes:
        partes = []
        if src.descriptions and len(src.descriptions) >= banda:
            partes.append(str(src.descriptions[banda - 1] or ""))
        tags = src.tags(banda)
        partes.extend([f"{k}={v}" for k, v in tags.items()])
        texto = " ".join(partes).upper()

        # Nomes que normalmente aparecem em metadados GRIB/GDAL.
        if any(chave in texto for chave in ["PREC", "PRECIP", "APCP"]):
            if "NEST" not in texto and "NUMBER OF STATIONS" not in texto:
                candidatos.append(banda)

    if candidatos:
        return candidatos[0]

    # Estrutura oficial MERGE: banda/variavel PREC seguida de NEST.
    if src.count >= 1:
        print(
            "AVISO: nao foi possivel identificar PREC pelos metadados do GRIB; "
            "sera utilizada a banda 1, conforme a estrutura oficial do MERGE."
        )
        return 1

    raise RuntimeError("O GRIB2 nao possui bandas legiveis.")


def preparar_poligonos(caminho_zip: Path):
    """Le o ZIP, valida ID e calcula area de cada poligono."""
    if not caminho_zip.exists():
        raise FileNotFoundError(
            f"Shapefile ZIP nao encontrado: {caminho_zip.resolve()}\n"
            "Coloque o ZIP na mesma pasta do notebook/script ou altere SHAPEFILE_ZIP."
        )

    gdf = gpd.read_file(f"zip://{caminho_zip.resolve()}")

    if ID_FIELD not in gdf.columns:
        raise KeyError(
            f"Campo '{ID_FIELD}' nao encontrado. Campos existentes: {list(gdf.columns)}"
        )

    if gdf[ID_FIELD].isna().any():
        raise ValueError(f"Existem valores nulos no campo {ID_FIELD}.")

    if gdf[ID_FIELD].duplicated().any():
        duplicados = gdf.loc[gdf[ID_FIELD].duplicated(), ID_FIELD].tolist()
        raise ValueError(
            f"O campo {ID_FIELD} deve ser unico. IDs duplicados: {duplicados[:10]}"
        )

    if gdf.crs is None:
        raise ValueError("O shapefile nao possui CRS definido.")

    # Remove geometrias vazias/nulas e tenta corrigir geometrias invalidas.
    gdf = gdf[gdf.geometry.notna() & ~gdf.geometry.is_empty].copy()
    if (~gdf.geometry.is_valid).any():
        gdf["geometry"] = gdf.geometry.make_valid()

    # Mantem ID como texto.
    gdf[ID_FIELD] = gdf[ID_FIELD].astype(str)

    # Area em projecao equivalente global caso a entrada nao seja projetada.
    if gdf.crs.is_projected:
        area_km2 = gdf.geometry.area / 1_000_000.0
    else:
        area_km2 = gdf.to_crs("EPSG:6933").geometry.area / 1_000_000.0

    tabela_area = pd.DataFrame(
        {ID_FIELD: gdf[ID_FIELD].values, "area_poligono_km2": area_km2.values}
    )

    return gdf, tabela_area


def datas_quinzena(ano: int, mes: int, quinzena: int) -> list[date]:
    """Retorna todas as datas de uma quinzena."""
    if quinzena == 1:
        inicio = date(ano, mes, 1)
        fim = date(ano, mes, 15)
    elif quinzena == 2:
        ultimo = calendar.monthrange(ano, mes)[1]
        inicio = date(ano, mes, 16)
        fim = date(ano, mes, ultimo)
    else:
        raise ValueError("quinzena deve ser 1 ou 2")

    n = (fim - inicio).days + 1
    return [inicio + timedelta(days=i) for i in range(n)]


def janela_estudo(src, gdf_raster_crs: gpd.GeoDataFrame) -> Window:
    """Calcula uma janela raster que cobre todos os poligonos + margem."""
    minx, miny, maxx, maxy = gdf_raster_crs.total_bounds

    # Para o MERGE (grade geografica 0.1 graus), aplica margem em graus.
    if src.crs and src.crs.is_geographic:
        minx -= MARGEM_GRAUS
        miny -= MARGEM_GRAUS
        maxx += MARGEM_GRAUS
        maxy += MARGEM_GRAUS

    w = from_bounds(minx, miny, maxx, maxy, transform=src.transform)
    w = w.round_offsets().round_lengths()

    raster_total = Window(0, 0, src.width, src.height)
    try:
        return w.intersection(raster_total)
    except Exception as exc:
        raise RuntimeError(
            "Os poligonos nao interceptam a grade do MERGE. Verifique o CRS."
        ) from exc


def processar_quinzena(
    sessao: requests.Session,
    gdf_original: gpd.GeoDataFrame,
    tabela_area: pd.DataFrame,
    ano: int,
    mes: int,
    quinzena: int,
) -> pd.DataFrame:
    """Baixa, soma e extrai a precipitacao acumulada de uma quinzena."""

    datas = datas_quinzena(ano, mes, quinzena)
    arquivos = []
    falhas = []

    print(
        f"\n{ano}-{mes:02d} Q{quinzena}: "
        f"{datas[0]:%d/%m/%Y} a {datas[-1]:%d/%m/%Y}"
    )

    for d in tqdm(datas, desc="Download/leitura", leave=False):
        try:
            arquivos.append((d, baixar_arquivo(sessao, d)))
        except Exception as exc:
            falhas.append((d, str(exc)))
            if INTERROMPER_SE_DIA_FALTANTE:
                raise RuntimeError(
                    f"Nao foi possivel obter o MERGE de {d:%Y-%m-%d}. "
                    "O processamento foi interrompido para nao subestimar a chuva."
                ) from exc

    if not arquivos:
        raise RuntimeError("Nenhum arquivo diario foi obtido para a quinzena.")

    # Abre o primeiro arquivo para definir grade, CRS e janela espacial.
    with rasterio.open(arquivos[0][1]) as src0:
        crs_merge = src0.crs or rasterio.crs.CRS.from_epsg(4326)
        banda_prec = localizar_banda_precipitacao(src0)
        gdf_merge = gdf_original.to_crs(crs_merge)
        win = janela_estudo(src0, gdf_merge)
        transform_win = src0.window_transform(win)
        altura = int(win.height)
        largura = int(win.width)

    acumulado = np.zeros((altura, largura), dtype=np.float64)
    n_validos = np.zeros((altura, largura), dtype=np.uint16)

    for d, arquivo in tqdm(arquivos, desc="Acumulando dias", leave=False):
        with rasterio.open(arquivo) as src:
            # Confere compatibilidade basica da grade.
            if src.width <= 0 or src.height <= 0:
                raise RuntimeError(f"Raster invalido: {arquivo}")

            banda = localizar_banda_precipitacao(src)
            arr = src.read(banda, window=win, masked=True).astype("float64")
            arr = arr.filled(np.nan)

            # Precipitacao negativa nao e fisicamente valida; tambem remove undef.
            arr[arr < 0] = np.nan

            valido = np.isfinite(arr)
            acumulado[valido] += arr[valido]
            n_validos[valido] += 1

    dias_processados = len(arquivos)

    # Uma celula so entra no acumulado se tiver dado valido em TODOS os dias
    # processados da quinzena. Isso impede soma parcial silenciosa.
    acumulado[n_validos < dias_processados] = np.nan

    # GeoTIFF temporario da quinzena, ja recortado para a area dos poligonos.
    PASTA_TEMP.mkdir(parents=True, exist_ok=True)
    tif_quinzena = PASTA_TEMP / f"MERGE_{ano}_{mes:02d}_Q{quinzena}_acum_mm.tif"

    nodata = -9999.0
    gravar = np.where(np.isfinite(acumulado), acumulado, nodata).astype("float32")

    profile = {
        "driver": "GTiff",
        "height": altura,
        "width": largura,
        "count": 1,
        "dtype": "float32",
        "crs": crs_merge,
        "transform": transform_win,
        "nodata": nodata,
        "compress": "deflate",
        "predictor": 3,
    }

    with rasterio.open(tif_quinzena, "w", **profile) as dst:
        dst.write(gravar, 1)
        dst.set_band_description(1, "precipitacao_acumulada_quinzenal_mm")

    # Estatistica zonal exata. Como o MERGE esta em lon/lat, usa area esferica
    # em m2 para ponderar corretamente as celulas em diferentes latitudes.
    stats = exact_extract(
        str(tif_quinzena),
        gdf_merge,
        [
            "mean(coverage_weight=area_spherical_m2)",
            "count(coverage_weight=area_spherical_km2)",
        ],
        include_cols=[ID_FIELD],
        output="pandas",
    )

    # Os nomes retornados normalmente sao 'mean' e 'count'. O codigo abaixo
    # tambem funciona caso a versao do exactextract use nomes mais longos.
    cols_mean = [c for c in stats.columns if "mean" in str(c).lower()]
    cols_count = [c for c in stats.columns if "count" in str(c).lower()]
    if not cols_mean or not cols_count:
        raise RuntimeError(
            f"Colunas inesperadas retornadas por exactextract: {list(stats.columns)}"
        )

    stats = stats.rename(
        columns={
            cols_mean[0]: "precip_acum_mm",
            cols_count[0]: "area_valida_km2",
        }
    )

    stats[ID_FIELD] = stats[ID_FIELD].astype(str)
    resultado = tabela_area.merge(stats, on=ID_FIELD, how="left")

    resultado["ano"] = ano
    resultado["mes"] = mes
    resultado["quinzena"] = quinzena
    resultado["data_inicio"] = datas[0].isoformat()
    resultado["data_fim"] = datas[-1].isoformat()
    resultado["dias_esperados"] = len(datas)
    resultado["dias_processados"] = dias_processados
    resultado["cobertura_pct"] = (
        100.0 * resultado["area_valida_km2"] / resultado["area_poligono_km2"]
    ).clip(lower=0, upper=100)

    # Ordem das colunas da tabela longa.
    resultado = resultado[
        [
            ID_FIELD,
            "ano",
            "mes",
            "quinzena",
            "data_inicio",
            "data_fim",
            "dias_esperados",
            "dias_processados",
            "precip_acum_mm",
            "area_poligono_km2",
            "area_valida_km2",
            "cobertura_pct",
        ]
    ]

    # Arredondamentos apenas para apresentacao.
    resultado["precip_acum_mm"] = resultado["precip_acum_mm"].round(2)
    resultado["area_poligono_km2"] = resultado["area_poligono_km2"].round(3)
    resultado["area_valida_km2"] = resultado["area_valida_km2"].round(3)
    resultado["cobertura_pct"] = resultado["cobertura_pct"].round(2)

    # Se o usuario preferir economizar espaco em disco, remove os GRIB2 ja usados.
    if not MANTER_ARQUIVOS_DIARIOS:
        for _, arquivo in arquivos:
            try:
                arquivo.unlink()
            except FileNotFoundError:
                pass

    # O GeoTIFF quinzenal e intermediario; o resultado solicitado e tabular.
    try:
        tif_quinzena.unlink()
    except FileNotFoundError:
        pass

    return resultado


def formatar_excel(caminho: Path) -> None:
    """Aplica formatacao simples ao Excel ja criado."""
    from openpyxl import load_workbook

    wb = load_workbook(caminho)
    for ws in wb.worksheets:
        ws.freeze_panes = "A2"
        ws.auto_filter.ref = ws.dimensions
        for coluna in ws.columns:
            largura = 0
            letra = coluna[0].column_letter
            for celula in coluna[:1000]:
                if celula.value is not None:
                    largura = max(largura, len(str(celula.value)))
            ws.column_dimensions[letra].width = min(max(largura + 2, 10), 28)
    wb.save(caminho)


# ============================================================================
# 3. PROCESSAMENTO PRINCIPAL
# ============================================================================


def main():
    inicio_total = time.time()

    print("=" * 78)
    print("MERGE/CPTEC-INPE - PRECIPITACAO ACUMULADA QUINZENAL - 2022")
    print("=" * 78)

    verificar_driver_grib()

    PASTA_DADOS.mkdir(parents=True, exist_ok=True)
    PASTA_TEMP.mkdir(parents=True, exist_ok=True)
    PASTA_SAIDA.mkdir(parents=True, exist_ok=True)

    gdf, tabela_area = preparar_poligonos(SHAPEFILE_ZIP)

    print(f"Poligonos: {len(gdf)}")
    print(f"Campo ID: {ID_FIELD}")
    print(f"CRS de entrada: {gdf.crs}")
    print(f"Periodo: {ANO} - 24 quinzenas")
    print(f"Fonte: {BASE_URL}/")

    sessao = criar_sessao_http()
    resultados = []

    for mes in range(1, 13):
        for quinzena in (1, 2):
            df_q = processar_quinzena(
                sessao=sessao,
                gdf_original=gdf,
                tabela_area=tabela_area,
                ano=ANO,
                mes=mes,
                quinzena=quinzena,
            )
            resultados.append(df_q)

    longo = pd.concat(resultados, ignore_index=True)
    longo = longo.sort_values([ID_FIELD, "ano", "mes", "quinzena"]).reset_index(drop=True)

    # Formato largo: uma linha por poligono e 24 colunas de precipitacao.
    tmp = longo.copy()
    tmp["periodo"] = tmp.apply(
        lambda r: f"P_{int(r['ano'])}_{int(r['mes']):02d}_Q{int(r['quinzena'])}_mm",
        axis=1,
    )
    largo = tmp.pivot(index=ID_FIELD, columns="periodo", values="precip_acum_mm").reset_index()
    largo.columns.name = None

    # Acrescenta area do poligono no formato largo.
    largo = tabela_area.merge(largo, on=ID_FIELD, how="left")
    largo["area_poligono_km2"] = largo["area_poligono_km2"].round(3)

    # Ordena as 24 colunas cronologicamente.
    cols_periodos = [
        f"P_{ANO}_{mes:02d}_Q{q}_mm"
        for mes in range(1, 13)
        for q in (1, 2)
    ]
    largo = largo[[ID_FIELD, "area_poligono_km2"] + cols_periodos]

    # Metadados para a planilha Excel.
    metadados = pd.DataFrame(
        {
            "item": [
                "produto",
                "instituicao",
                "ano",
                "resolucao_espacial",
                "frequencia_origem",
                "variavel",
                "unidade_saida",
                "definicao_Q1",
                "definicao_Q2",
                "estatistica_espacial",
                "fonte",
                "observacao",
            ],
            "valor": [
                "MERGE - Produto de precipitacao",
                "CPTEC/INPE",
                str(ANO),
                "0.1 grau (~10 km)",
                "diaria",
                "PREC - precipitacao acumulada em 24 h",
                "mm por quinzena",
                "dias 01 a 15",
                "dias 16 ao ultimo dia do mes",
                "media da lamina ponderada pela area de intersecao das celulas",
                f"{BASE_URL}/",
                "kg/m2 de agua e numericamente equivalente a mm de precipitacao",
            ],
        }
    )

    csv_long = PASTA_SAIDA / "precipitacao_MERGE_quinzenal_2022_long.csv"
    csv_wide = PASTA_SAIDA / "precipitacao_MERGE_quinzenal_2022_wide.csv"
    xlsx = PASTA_SAIDA / "precipitacao_MERGE_quinzenal_2022.xlsx"

    longo.to_csv(csv_long, index=False, encoding="utf-8-sig")
    largo.to_csv(csv_wide, index=False, encoding="utf-8-sig")

    with pd.ExcelWriter(xlsx, engine="openpyxl") as writer:
        longo.to_excel(writer, sheet_name="quinzenal_long", index=False)
        largo.to_excel(writer, sheet_name="quinzenal_wide", index=False)
        metadados.to_excel(writer, sheet_name="metadados", index=False)

    formatar_excel(xlsx)

    duracao = (time.time() - inicio_total) / 60.0

    print("\n" + "=" * 78)
    print("PROCESSAMENTO CONCLUIDO")
    print("=" * 78)
    print(f"Linhas na tabela longa: {len(longo)}")
    print(f"Esperado: {len(gdf)} poligonos x 24 quinzenas = {len(gdf) * 24}")
    print(f"\nCSV longo : {csv_long.resolve()}")
    print(f"CSV largo : {csv_wide.resolve()}")
    print(f"Excel     : {xlsx.resolve()}")
    print(f"Tempo total: {duracao:.1f} min")

    print("\nPrimeiras linhas:")
    print(longo.head(10).to_string(index=False))

    return longo, largo


if __name__ == "__main__":
    LONGO, LARGO = main()
