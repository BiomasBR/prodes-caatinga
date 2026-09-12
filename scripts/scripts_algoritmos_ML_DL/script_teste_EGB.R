# ==================================================================================

# CLASSIFICAÇÃO EGB_ST - TESTES 2026.2
# EXTREME GRADIENT BOOSTING - EGB_ST
# RESPONSÁVEL: JEANNE
# DATA: 15/08/2026

# ==================================================================================

# LIMPAR AMBIENTE

# ==================================================================================

rm(list = ls()) 

# ==================================================================================

# CONFIGURAÇÕES PARA OTIMIZAR USO DA GPU E CPU

# ==================================================================================

# devtools::install_github("e-sensing/sits@dev", force = TRUE) # Instalar versão dev do sits
# Sys.setenv(SITS_GPU_PIPELINE = "stream") -> NÃO EXISTE MAIS NO SITS
# Sys.setenv(SITS_FORCE_CPU = "TRUE") # Desliga a GPU, portanto, usamos o FALSE
Sys.getenv("SITS_FORCE_CPU") # Deve estar vazio para usar GPU
torch::cuda_is_available() # O cuda deve estar disponível = TRUE

# ==================================================================================

# PACOTES

# ==================================================================================

# Para instalar pacote sits

# devtools::install_github("e-sensing/sits", dependencies = TRUE)
# install.packages("sits", dependencies = TRUE)

library(sits)
library(tidyverse)
library(sf)
library(terra)
library(raster)
library(luz)
library(torch)

# ==================================================================================

# CONFIGURAÇÕES

# ==================================================================================

# O modelo será treinado com todas as amostras da RM

tiles_treino <- c("037011", "037012", "037013",
                  "038012", "038013", "039012",
                  "039013", "039014", "040012",
                  "040013", "040014", "041012",
                  "041013", "041014", "041015")

# E a classificação será feita apenas nesses tiles:

tile_classificacao <- c("037011", "037012","038012")

# Datas inicial e final para as classificações

start_date <- "2024-07-27"
end_date   <- "2025-12-19"

# Criar novas pastas onde os arquivos serão armazenados

dir_rds   <- "arquivos_rds"
dir_model <- "modelos"
dir_out   <- "classificacao_EGB_ST"

dir.create(dir_rds, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_model, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# ==================================================================================

# CUBO DE TREINO (MÚLTIPLOS TILES)

# ==================================================================================

# Criar cubo de dados com tiles e datas definidas anteriormente

cubo_treino <- sits_cube(
  source     = "BDC",
  collection = "SENTINEL-2-16D",
  tiles      = tiles_treino,
  start_date = start_date,
  end_date   = end_date
)

## Remoção das bandas B01 e B09

cubo_treino <- sits_select(
  cubo_treino,
  bands = c("B02","B03","B04","B05",
            "B06","B07","B08","B11",
            "B12","B8A","CLOUD")
)

sits_bands(cubo_treino)
sits_timeline(cubo_treino)

saveRDS(cubo_treino, file.path(dir_rds, "cubo_treino_teste_EGB_ST.rds"))
cubo_treino <- readRDS(file.path(dir_rds, "cubo_treino_teste_EGB_ST.rds"))

# ==================================================================================

## Essa função vai ajudar a deixar a marcação de tempo de processamento mais amigável.
### Formatando saída da contagem de tempo dos processamentos:

formatar_tempo <- function(segundos) {
  horas <- floor(segundos / 3600)
  minutos <- floor((segundos %% 3600) / 60)
  segundos <- round(segundos %% 60)
  
  sprintf("%02dh %02dm %02ds", horas, minutos, segundos)
}

# ==================================================================================
# SALVAR TEMPOS DE PROCESSAMENTO
# ==================================================================================

registrar_tempo <- function(
    etapa,
    tempo,
    arquivo = file.path(dir_out, "tempos_processamento.csv")
)  {
  
  linha <- data.frame(
    etapa = etapa,
    tempo_segundos = as.numeric(tempo["elapsed"]),
    tempo_horas = as.numeric(tempo["elapsed"]) / 3600,
    tempo_formatado = formatar_tempo(tempo["elapsed"])
  )
  
  write.table(
    linha,
    file = arquivo,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(arquivo),
    append = file.exists(arquivo)
  )
}

# ==================================================================================

# AMOSTRAS (USANDO CUBO DE TREINO)

# ==================================================================================

## Observação: os diretórios devem ser verificados para leitura dos arquivos.

tempo_sits_get_data <- system.time({
  amostras <- sits_get_data(
    cube        = cubo_treino,
    samples     = "amostras_adicionais_finais_RM01.shp",
    label_attr  = "label",
    bands       = sits_bands(cubo_treino),
    multicores  = 38,
    memsize     = 110, 
    progress    = TRUE
  )
})

formatar_tempo(tempo_sits_get_data["elapsed"])

registrar_tempo("Cubo de amostras", tempo_sits_get_data)

saveRDS(amostras, file.path(dir_rds, "amostras_cubo_teste_EGB_ST.rds"))

# Recarregar em nova sessão

amostras <- readRDS(file.path(dir_rds,"amostras_cubo_teste_EGB_ST.rds"))

summary(amostras)
sits_bands(amostras)

# ==================================================================================

# TREINAMENTO DO MODELO E VALIDAÇÃO K-FOLD

# ==================================================================================

set.seed(220)

tempo_treino <- system.time({
  modelo_egb <- sits_train(
    samples = amostras,
    ml_method = sits_xgboost()
  )
})

formatar_tempo(tempo_treino["elapsed"])

registrar_tempo("Treinamento", tempo_treino)

saveRDS(modelo_egb, file.path(dir_model, "modelo_EGB_ST.rds"))
modelo_egb <- readRDS(file.path(dir_model, "modelo_EGB_ST.rds"))

plot(modelo_egb)

egb_validate <- sits_kfold_validate(
  samples = amostras,
  folds = 5,
  ml_method = sits_xgboost(),
  multicores = 5
)

egb_validate

plot(egb_validate, type = "confusion_matrix")

# ==================================================================================

# GERAR CUBO 

# ==================================================================================

cubo_classificacao <- sits_cube(
  source     = "BDC",
  collection = "SENTINEL-2-16D",
  tiles      = tile_classificacao,
  start_date = start_date,
  end_date   = end_date
)

cubo_classificacao <- sits_select(
  cubo_classificacao,
  bands = c("B02","B03","B04","B05",
            "B06","B07","B08","B11",
            "B12","B8A","CLOUD")
)

# ==================================================================================

# LOOP DE CLASSIFICAÇÃO POR TILE A TILE 

# ==================================================================================

for (tile in tile_classificacao) {
  
  cat("\n=============================================================\n")
  cat("PROCESSANDO TILE:", tile, "\n")
  cat("=============================================================\n\n")
  
  # Definir porcentagens de cada classe e extrair valores para suavização
  
  cubo_tile <- sits_select(cubo_classificacao, tiles = tile)
  
tempo_classificacao <- system.time({
  class_probs <- sits_classify(
    data       = cubo_tile,
    ml_model   = modelo_egb,
    output_dir = dir_out,
    multicores = 1, # Aqui é melhor deixar 1 multicores
    memsize    = 96, # O máximo é 144 na GPU
    gpu_memory = 8,
    progress   = TRUE,
    version    = "EGB_ST"
  )
})

# Mostra o tempo de processamento

formatar_tempo(tempo_classificacao["elapsed"])

registrar_tempo(
  paste("Classificação - Tile", tile),
  tempo_classificacao
)

# ==================================================================================

# VARIÂNCIA

# ==================================================================================

# Calcular valores de variância para cada classe 

tempo_variance <- system.time({
  variance <- sits_variance(
    cube           = class_probs,
    window_size    = 5,
    neigh_fraction = 0.5,
    output_dir     = dir_out,
    multicores     = 30,
    memsize        = 86,
    version        = "EGB_ST"
  )
})

formatar_tempo(tempo_variance["elapsed"])

registrar_tempo(
  paste("Variância - Tile", tile),
  tempo_variance
)

# ==================================================================================

# HIPERPARÂMETROS DE SUAVIZAÇÃO

# ==================================================================================

# Definir porcentagens de cada classe e extrair valores para suavização

sumv_df <- as.data.frame(summary(variance))

cat("\n--- VARIÂNCIA (percentis) –", "TILE", tile, "\n")
print(sumv_df)

tempo_smooth <- system.time({
    smooth_values <- c(
      aflor_rocha = sumv_df["80%", "aflor_rocha"],
      agua        = sumv_df["85%", "agua"],
      graminea    = sumv_df["85%", "graminea"],
      queimada    = sumv_df["85%", "queimada"],
      supressao   = sumv_df["80%", "supressao"],
      veg_natural = sumv_df["85%", "veg_natural"]
    )
  })

formatar_tempo(tempo_smooth["elapsed"])

registrar_tempo(
  paste("Valores Smooth - Tile", tile),
  tempo_smooth
)

# ==================================================================================

# SUAVIZAÇÃO E CLASSIFICAÇÃO TEMÁTICA FINAL

# ==================================================================================

tempo_smooth_map <- system.time({
    cube_smooth <- sits_smooth(
      cube           = class_probs,
      smoothness     = smooth_values,
      window_size    = 5,
      neigh_fraction = 0.5,
      progress       = TRUE,
      output_dir     = dir_out,
      multicores     = 30,
      memsize        = 86,
      version        = "EGB_ST"
    )
    
    # Mapa Classificado
    
    sits_label_classification(
      cube       = cube_smooth,
      output_dir = dir_out,
      multicores = 30,
      memsize    = 82,
      version    = "EGB_ST"
    )
  })

formatar_tempo(tempo_smooth_map["elapsed"])

registrar_tempo(
  paste("Suavização + Mapa - Tile", tile),
  tempo_smooth_map
)

# ==================================================================================

# INCERTEZA

# ==================================================================================

tempo_uncertainty <- system.time({
  uncertainty <- sits_uncertainty(
    cube       = class_probs,
    type       = "margin",
    output_dir = dir_out,
    multicores = 30,
    memsize    = 86,
    version    = "EGB_ST"
  )
})

formatar_tempo(tempo_uncertainty["elapsed"])

registrar_tempo(
  paste("Incerteza - Tile", tile),
  tempo_uncertainty
)

}

# ==================================================================================

# RELATÓRIO FINAL DO TEMPO DE PROCESSAMENTO

# ==================================================================================

tempos_df <- tibble(
  etapa = c(
    "Cubo de amostras",
    "Treinamento",
    "Classificação",
    "Variância",
    "Valores Smooth",
    "Suavização + Mapa",
    "Incerteza"
  ),
  
  tempo_horas = c(
    tempo_sits_get_data["elapsed"] / 3600,
    tempo_treino["elapsed"] / 3600,
    tempo_classificacao["elapsed"] / 3600,
    tempo_variance["elapsed"] / 3600,
    tempo_smooth["elapsed"] / 3600,
    tempo_smooth_map["elapsed"] / 3600,
    tempo_uncertainty["elapsed"] / 3600
  ),
  
  tempo_formatado = c(
    formatar_tempo(tempo_sits_get_data["elapsed"]),
    formatar_tempo(tempo_treino["elapsed"]),
    formatar_tempo(tempo_classificacao["elapsed"]),
    formatar_tempo(tempo_variance["elapsed"]),
    formatar_tempo(tempo_smooth["elapsed"]),
    formatar_tempo(tempo_smooth_map["elapsed"]),
    formatar_tempo(tempo_uncertainty["elapsed"])
  )
)

tempos_df