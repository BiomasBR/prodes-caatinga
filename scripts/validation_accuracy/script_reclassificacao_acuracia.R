# Script para Reclassificação do Bioma Caatinga ----------------------------------------------------------------------------------------------------------------
# Reclassificação para análise de acurácia de mapas classificados  -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data: 10/04/2026 --------------------------------------------------------

# 1. Carregar o cubo classificado final (mapa temático).
# 2. Construir um cubo SITS das classes de não supressão.
# 3. Reclassificar o mapa final.

# Classes finais: abiótico, água, queimada, supressão, vegetação natural + Mascara PRODES 2024
# (máscara em áreas a serem excluídas).

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Configuração do Ambiente --------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes necessários para a análise

library(torch)
library(luz)
library(sits)
library(tibble)
library(terra)
library(dplyr)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Carregamento do Cubo Classificado Final -----------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Tiles RM5

tiles_rm5 <- c("034017")

input_dir <- "map_classificado"

class_cube <- sits_cube(
  source      = "BDC",
  collection  = "SENTINEL-2-16D",
  data_dir    = input_dir, # Pasta do diretório onde se encontra o mapa classificado
  parse_info  = c("satellite", "sensor", "tile", "start_date",
                  "end_date", "band", "version"),
  bands       = "class",
  version     = "v1", 
  tiles       = tiles_rm5,
  start_date  = "2024-07-27", # Verificar data da imagem .tif
  end_date    = "2025-10-16",
  labels      = c(
    "1" = "abiotico",
    "2" = "agua",
    "3" = "queimada",
    "4" = "supressao",
    "5" = "veg_natural"))

sits_colors_set(tibble(
  name  = c("abiotico","queimada","supressao","veg_natural","agua"),
  color = c("#1A1A1A","#D60C00","#FAE9A0","#A6D96A","#A1DDEF")))

plot(class_cube)

view(class_cube)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Construção do Cubo da classe não supressao PRODES 2023 ---------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Definir diretório da máscara

out_dir <- "map_classificado" 

# Criar cubo da máscara

mask_cube <- sits_cube(
  source      = "BDC",
  collection  = "SENTINEL-2-16D",
  data_dir    = out_dir,
  parse_info  = c("satellite","sensor","tile","start_date",
                  "end_date","band","version"),
  bands       = "class",
  version     = "v1",
  tiles       = tiles_rm5,
  start_date  = "2024-07-27",
  end_date    = "2025-10-16",
  labels      = c(
    "4" = "supressao",
    "1" = "nao_supressao",
    "2" = "nao_supressao",
    "3" = "nao_supressao",
    "5" = "nao_supressao"))

sits_colors_set(tibble(
  name  = c("supressao","nao_supressao"),
  color = c("#FAE9A0","gray40")))

view(mask_cube)

plot(mask_cube, legend_text_size = 0.7)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Reclassificação Tile a Tile -----------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Criar nova pasta onde serão armazenados os mapas reclassificados

temp_dir <- "map_reclassificado_acuracia"
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

reclass_result <- sits_reclassify(
    cube = class_cube,
    mask = mask_cube,
    rules = list(
      "supressao" = cube == "supressao" & mask == "supressao", # Pixel valor 4
      "nao_supressao" = cube %in% c("abiotico", "agua", "queimada", "veg_natural") # Outros pixels do cubo do mapa classificado
    ),
    output_dir = temp_dir,
    version     = "v1",
    multicores  = 16,
    memsize     = 58
  )

plot(reclass_result, legend_text_size = 0.7)

view(reclass_result)


