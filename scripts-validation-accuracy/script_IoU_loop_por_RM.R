# ============================================================
# AVALIAÇÃO IoU - SITS VS PRODES
# PRODES CAATINGA 2024
# AVALIAÇÃO POR RM (LOOP)
# COM ÁREAS DE FALSO POSITIVO E FALSO NEGATIVO
# E SALVANDO ARQUIVOS ESPACIAIS COM PROTEÇÃO
# ============================================================

# ============================================================
# LIMPAR AMBIENTE
# ============================================================

rm(list = ls())

# ============================================================
# CARREGAR PACOTES
# ============================================================

library(sf)
library(tidyverse)
library(writexl)

# ============================================================
# DESLIGAR MOTOR S2
# ============================================================

sf::sf_use_s2(FALSE)

# ============================================================
# LISTA DE RMs A PROCESSAR
# ============================================================

RMs <- c("RM1", "RM2", "RM3", "RM4", "RM5", "RM6", "RM7")

# ============================================================
# PASTA DOS SHAPEFILES DE ENTRADA
# ============================================================

pasta_shp <- "arquivos_shp_corrigidos_por_RM"

# ============================================================
# PASTA DE SAÍDA DOS ARQUIVOS ESPACIAIS
# ============================================================

pasta_saida <- "saidas_gpkg_RM"

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# ============================================================
# FUNÇÃO AUXILIAR PARA PREPARAR GEOMETRIA PARA SALVAR
# ============================================================

preparar_para_salvar_rm <- function(geom, rm_id, tipo, area_ha, crs_saida = 31984) {
  
  if (is.null(geom)) {
    return(NULL)
  }
  
  sf_obj <- st_sf(
    RM = rm_id,
    tipo = tipo,
    area_ha = round(area_ha, 2),
    geometry = st_sfc(geom, crs = crs_saida)
  )
  
  sf_obj <- sf_obj[!st_is_empty(sf_obj), ]
  
  if (nrow(sf_obj) == 0) {
    return(NULL)
  }
  
  tipos_geom <- unique(as.character(st_geometry_type(sf_obj)))
  
  if (any(tipos_geom %in% c("GEOMETRYCOLLECTION", "GEOMETRY"))) {
    sf_obj <- st_collection_extract(sf_obj, "POLYGON")
  }
  
  sf_obj <- sf_obj[!st_is_empty(sf_obj), ]
  
  if (nrow(sf_obj) == 0) {
    return(NULL)
  }
  
  sf_obj <- tryCatch(
    st_cast(sf_obj, "MULTIPOLYGON", warn = FALSE),
    error = function(e) sf_obj
  )
  
  sf_obj <- st_make_valid(sf_obj)
  sf_obj <- sf_obj[!st_is_empty(sf_obj), ]
  
  if (nrow(sf_obj) == 0) {
    return(NULL)
  }
  
  return(sf_obj)
}

# ============================================================
# TABELA PARA ARMAZENAR RESULTADOS
# ============================================================

resultados <- data.frame()

# ============================================================
# LOOP PRINCIPAL
# ============================================================

for (rm_id in RMs) {
  
  cat("\n==============================\n")
  cat("Processando:", rm_id, "\n")
  cat("==============================\n")
  
  # ----------------------------------------------------------
  # Caminhos dos arquivos
  # ----------------------------------------------------------
  
  sits_file <- file.path(
    pasta_shp,
    paste0("supressao_", rm_id, "_2024_dissolve_total.shp")
  )
  
  prodes_file <- file.path(
    pasta_shp,
    paste0("desmatamento_", rm_id, "_2024_dissolve_total.shp")
  )
  
  # ----------------------------------------------------------
  # Verificar se arquivos existem
  # ----------------------------------------------------------
  
  if (!file.exists(sits_file)) {
    cat("Arquivo SITS não encontrado para", rm_id, "\n")
    next
  }
  
  if (!file.exists(prodes_file)) {
    cat("Arquivo PRODES não encontrado para", rm_id, "\n")
    next
  }
  
  # ----------------------------------------------------------
  # Ler arquivos
  # ----------------------------------------------------------
  
  sits <- st_read(sits_file, quiet = TRUE)
  prodes <- st_read(prodes_file, quiet = TRUE)
  
  # ----------------------------------------------------------
  # Garantir mesmo CRS
  # ----------------------------------------------------------
  
  if (st_crs(sits) != st_crs(prodes)) {
    prodes <- st_transform(prodes, st_crs(sits))
  }
  
  # ----------------------------------------------------------
  # Projetar para CRS métrico
  # UTM 24S - SIRGAS 2000
  # EPSG: 31984
  # ----------------------------------------------------------
  
  sits_proj <- st_transform(sits, 31984)
  prodes_proj <- st_transform(prodes, 31984)
  
  # ----------------------------------------------------------
  # Corrigir geometrias
  # ----------------------------------------------------------
  
  sits_proj <- st_make_valid(sits_proj)
  prodes_proj <- st_make_valid(prodes_proj)
  
  # ----------------------------------------------------------
  # Consolidar cada base em uma única geometria
  # ----------------------------------------------------------
  
  geom_sits <- tryCatch(
    st_union(st_geometry(sits_proj)),
    error = function(e) NULL
  )
  
  geom_prodes <- tryCatch(
    st_union(st_geometry(prodes_proj)),
    error = function(e) NULL
  )
  
  # ----------------------------------------------------------
  # Áreas de cada base
  # ----------------------------------------------------------
  
  area_sits <- if (!is.null(geom_sits) && !all(st_is_empty(geom_sits))) {
    as.numeric(st_area(geom_sits)) / 10000
  } else {
    0
  }
  
  area_prodes <- if (!is.null(geom_prodes) && !all(st_is_empty(geom_prodes))) {
    as.numeric(st_area(geom_prodes)) / 10000
  } else {
    0
  }
  
  # ----------------------------------------------------------
  # Interseção
  # ----------------------------------------------------------
  
  intersec_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
    tryCatch(st_intersection(geom_sits, geom_prodes), error = function(e) NULL)
  } else {
    NULL
  }
  
  area_intersec <- if (!is.null(intersec_geom) && !all(st_is_empty(intersec_geom))) {
    as.numeric(st_area(intersec_geom)) / 10000
  } else {
    0
  }
  
  # ----------------------------------------------------------
  # União
  # ----------------------------------------------------------
  
  uniao_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
    tryCatch(st_union(geom_sits, geom_prodes), error = function(e) NULL)
  } else if (!is.null(geom_sits)) {
    geom_sits
  } else if (!is.null(geom_prodes)) {
    geom_prodes
  } else {
    NULL
  }
  
  area_uniao <- if (!is.null(uniao_geom) && !all(st_is_empty(uniao_geom))) {
    as.numeric(st_area(uniao_geom)) / 10000
  } else {
    0
  }
  
  # ----------------------------------------------------------
  # Falso positivo
  # ----------------------------------------------------------
  
  falso_pos_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
    tryCatch(st_difference(geom_sits, geom_prodes), error = function(e) NULL)
  } else if (!is.null(geom_sits)) {
    geom_sits
  } else {
    NULL
  }
  
  area_falso_positivo <- if (!is.null(falso_pos_geom) && !all(st_is_empty(falso_pos_geom))) {
    as.numeric(st_area(falso_pos_geom)) / 10000
  } else {
    0
  }
  
  # ----------------------------------------------------------
  # Falso negativo
  # ----------------------------------------------------------
  
  falso_neg_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
    tryCatch(st_difference(geom_prodes, geom_sits), error = function(e) NULL)
  } else if (!is.null(geom_prodes)) {
    geom_prodes
  } else {
    NULL
  }
  
  area_falso_negativo <- if (!is.null(falso_neg_geom) && !all(st_is_empty(falso_neg_geom))) {
    as.numeric(st_area(falso_neg_geom)) / 10000
  } else {
    0
  }
  
  # ----------------------------------------------------------
  # Métricas
  # ----------------------------------------------------------
  
  IoU <- if (area_uniao > 0) area_intersec / area_uniao else NA_real_
  
  Precision <- if (area_sits > 0) area_intersec / area_sits else NA_real_
  
  Recall <- if (area_prodes > 0) area_intersec / area_prodes else NA_real_
  
  F1 <- if (!is.na(Precision) && !is.na(Recall) && (Precision + Recall) > 0) {
    2 * (Precision * Recall) / (Precision + Recall)
  } else {
    NA_real_
  }
  
  # ----------------------------------------------------------
  # Guardar resultado
  # ----------------------------------------------------------
  
  resultado <- data.frame(
    RM = rm_id,
    Area_SITS_ha = round(area_sits, 2),
    Area_PRODES_ha = round(area_prodes, 2),
    Area_Intersec_ha = round(area_intersec, 2),
    Area_Uniao_ha = round(area_uniao, 2),
    Area_Falso_Positivo_ha = round(area_falso_positivo, 2),
    Area_Falso_Negativo_ha = round(area_falso_negativo, 2),
    IoU = round(IoU, 4),
    Precision = round(Precision, 4),
    Recall = round(Recall, 4),
    F1 = round(F1, 4)
  )
  
  resultados <- bind_rows(resultados, resultado)
  
  # ----------------------------------------------------------
  # Criar pasta da RM
  # ----------------------------------------------------------
  
  pasta_rm <- file.path(pasta_saida, rm_id)
  
  if (!dir.exists(pasta_rm)) {
    dir.create(pasta_rm, recursive = TRUE)
  }
  
  # ----------------------------------------------------------
  # Preparar e salvar interseção
  # ----------------------------------------------------------
  
  intersec_sf <- preparar_para_salvar_rm(
    geom = intersec_geom,
    rm_id = rm_id,
    tipo = "intersecao",
    area_ha = area_intersec
  )
  
  if (!is.null(intersec_sf)) {
    st_write(
      intersec_sf,
      dsn = file.path(pasta_rm, paste0("intersecao_", rm_id, ".gpkg")),
      layer = "intersecao",
      delete_layer = TRUE,
      quiet = TRUE
    )
  }
  
  # ----------------------------------------------------------
  # Preparar e salvar falso positivo
  # ----------------------------------------------------------
  
  falso_pos_sf <- preparar_para_salvar_rm(
    geom = falso_pos_geom,
    rm_id = rm_id,
    tipo = "falso_positivo",
    area_ha = area_falso_positivo
  )
  
  if (!is.null(falso_pos_sf)) {
    st_write(
      falso_pos_sf,
      dsn = file.path(pasta_rm, paste0("falso_positivo_", rm_id, ".gpkg")),
      layer = "falso_positivo",
      delete_layer = TRUE,
      quiet = TRUE
    )
  }
  
  # ----------------------------------------------------------
  # Preparar e salvar falso negativo
  # ----------------------------------------------------------
  
  falso_neg_sf <- preparar_para_salvar_rm(
    geom = falso_neg_geom,
    rm_id = rm_id,
    tipo = "falso_negativo",
    area_ha = area_falso_negativo
  )
  
  if (!is.null(falso_neg_sf)) {
    st_write(
      falso_neg_sf,
      dsn = file.path(pasta_rm, paste0("falso_negativo_", rm_id, ".gpkg")),
      layer = "falso_negativo",
      delete_layer = TRUE,
      quiet = TRUE
    )
  }
  
  cat(rm_id, "processada com sucesso! ✅\n")
  print(resultado)
}

# ============================================================
# RESULTADO FINAL
# ============================================================

print(resultados)

# ============================================================
# SALVAR EM XLSX
# ============================================================

write_xlsx(resultados, "resultados_IoU_RMs.xlsx")
