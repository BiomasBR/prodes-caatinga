# ============================================================
# AVALIAÇÃO IoU - SITS VS PRODES
# PRODES CAATINGA 2024
# AVALIAÇÃO PARA ÁREA ANALISADA (SEM ACURÁCIA GLOBAL)
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
# CAMINHO DOS ARQUIVOS
# ============================================================

pasta_shp <- "arquivos_shp_bioma_caatinga_dissolvido"

sits_file <- file.path(pasta_shp, "supressao_sits_2024_bioma_dissolvido.shp")
prodes_file <- file.path(pasta_shp, "desmatamento_prodes_2024_bioma_dissolvido.shp")

# ============================================================
# VERIFICAR EXISTÊNCIA
# ============================================================

if (!file.exists(sits_file)) stop("Arquivo SITS não encontrado")
if (!file.exists(prodes_file)) stop("Arquivo PRODES não encontrado")

# ============================================================
# LER SHAPEFILES
# ============================================================

cat("Lendo arquivos...\n")

sits <- st_read(sits_file, quiet = TRUE)
prodes <- st_read(prodes_file, quiet = TRUE)

# ============================================================
# GARANTIR MESMO CRS
# ============================================================

if (st_crs(sits) != st_crs(prodes)) {
  prodes <- st_transform(prodes, st_crs(sits))
}

# ============================================================
# PROJETAR PARA CRS MÉTRICO
# ============================================================

sits_proj <- st_transform(sits, 31984)
prodes_proj <- st_transform(prodes, 31984)

# ============================================================
# CORRIGIR GEOMETRIAS
# ============================================================

sits_proj <- st_make_valid(sits_proj)
prodes_proj <- st_make_valid(prodes_proj)

# ============================================================
# CONSOLIDAR GEOMETRIAS
# ============================================================

cat("Unindo geometrias...\n")

geom_sits <- tryCatch(
  st_union(st_geometry(sits_proj)),
  error = function(e) NULL
)

geom_prodes <- tryCatch(
  st_union(st_geometry(prodes_proj)),
  error = function(e) NULL
)

# ============================================================
# ÁREAS
# ============================================================

area_sits <- if (!is.null(geom_sits) && !all(st_is_empty(geom_sits))) {
  as.numeric(st_area(geom_sits)) / 10000
} else 0

area_prodes <- if (!is.null(geom_prodes) && !all(st_is_empty(geom_prodes))) {
  as.numeric(st_area(geom_prodes)) / 10000
} else 0

# ============================================================
# INTERSEÇÃO
# ============================================================

intersec_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
  tryCatch(st_intersection(geom_sits, geom_prodes), error = function(e) NULL)
} else NULL

area_intersec <- if (!is.null(intersec_geom) && !all(st_is_empty(intersec_geom))) {
  as.numeric(st_area(intersec_geom)) / 10000
} else 0

# ============================================================
# UNIÃO
# ============================================================

uniao_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
  tryCatch(st_union(geom_sits, geom_prodes), error = function(e) NULL)
} else NULL


area_uniao <- if (!is.null(uniao_geom) && !all(st_is_empty(uniao_geom))) {
  as.numeric(st_area(uniao_geom)) / 10000
} else 0

# ============================================================
# FALSO POSITIVO
# ============================================================

falso_pos_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
  tryCatch(st_difference(geom_sits, geom_prodes), error = function(e) NULL)
} else NULL

area_falso_positivo <- if (!is.null(falso_pos_geom) && !all(st_is_empty(falso_pos_geom))) {
  as.numeric(st_area(falso_pos_geom)) / 10000
} else 0

# ============================================================
# FALSO NEGATIVO
# ============================================================

falso_neg_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
  tryCatch(st_difference(geom_prodes, geom_sits), error = function(e) NULL)
} else NULL

area_falso_negativo <- if (!is.null(falso_neg_geom) && !all(st_is_empty(falso_neg_geom))) {
  as.numeric(st_area(falso_neg_geom)) / 10000
} else 0

# ============================================================
# MÉTRICAS
# ============================================================

IoU <- if (area_uniao > 0) area_intersec / area_uniao else NA_real_

Precision <- if (area_sits > 0) area_intersec / area_sits else NA_real_

Recall <- if (area_prodes > 0) area_intersec / area_prodes else NA_real_

F1 <- if (!is.na(Precision) && !is.na(Recall) && (Precision + Recall) > 0) {
  2 * (Precision * Recall) / (Precision + Recall)
} else NA_real_

# ============================================================
# RESULTADO FINAL
# ============================================================

resultado <- data.frame(
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

print(resultado)

# ============================================================
# SALVAR RESULTADO
# ============================================================

write_xlsx(resultado, "resultado_bioma_caatinga.xlsx")
