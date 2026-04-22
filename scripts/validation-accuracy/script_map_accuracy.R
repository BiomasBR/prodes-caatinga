
# Avaliação Acurácia de Mapas Classificados ---------------------------------------------------------------------------------------------------------------
# PRODES 2025 - RM5 - Tile 034017 -------------------------------------------------------------------------------------------------------------------------
# Data: 05/04/2026 ----------------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes necessários para a análise

rm(list = ls())
gc()

library(sits)
library(tibble)

# Criação do cubo -----------------------------------------------------------------------------------------------------------------------------------------

# Define the classes of the probability cube

labels <- c("4" = "supressao",
            "6" = "nao_supressao")

# Directory where the data is stored 

data_dir <- "imagens_arquivos_rds"

# Create a probability data cube from a file 

cube_2025_class <- sits_cube(
  source      = "BDC",
  collection  = "SENTINEL-2-16D",
  data_dir    = data_dir,
  bands       = "class",
  labels      = labels,
  version     = "v1"
)

sits_colors_set(tibble(
  name  = c("supressao","nao_supressao"),
  color = c("#662700","#009999")))

plot(cube_2025_class)
view(cube_2025_class)

## Salvar gráfico em formato .png

png("mapa_classificacao_2025.png", width = 2000, height = 1600, res = 200)

plot(cube_2025_class)

dev.off()

## Salvar gráfico em formato .tiff

library(terra)

# escolher um tile e uma data do cube classificado
r <- sits_as_terra(
  cube = cube_2025_class,
  tile = cube_2025_class$tile[1],
  date = cube_2025_class$start_date[1]
)

writeRaster(
  r,
  filename = "/home/jovyan/avaliacao_acuracia_mapas_classificados/acuracia_duas_classes/classificacao_2025.tif",
  filetype = "GTiff",
  overwrite = TRUE
)

# Amostragem ----------------------------------------------------------------------------------------------------------------------------------------------

sampling_design <- sits_sampling_design(
  cube = cube_2025_class,
  expected_ua = c(
    "supressao" = 0.60,
    "nao_supressao" = 0.75
  ),
  alloc_options = c(60, 50),
  std_err = 0.02,
  rare_class_prop = 0.1
)

# show sampling design

sampling_design

# Generate stratified samples -----------------------------------------------------------------------------------------------------------------------------

## Salvar e ler arquivo de amostras

saveRDS(sampling_design, "sampling_design_2classes.rds")
sampling_design <- readRDS("sampling_design_2classes.rds")
view(sampling_design)

samples_sf <- sits_stratified_sampling(
  cube = cube_2025_class,
  sampling_design = sampling_design,
  alloc = "equal",
  multicores = 4
)

view(samples_sf)
plot(samples_sf)
class(samples_sf)

# Salvar arquivo sf ---------------------------------------------------------------------------------------------------------------------------------------

# save sf object as SHP file

tempdir_r <- "sample_estratificada"
dir.create(tempdir_r, showWarnings = FALSE)

sf::st_write(samples_sf, 
             file.path(tempdir_r, "samples_2classes.shp"), 
             append = FALSE
)

# Raster das amostras -------------------------------------------------------------------------------------------------------------------------------------

library(terra)

raster_cube <- rast(cube_2025_class$file_info[[1]]$path) #carrega o tif do cubo
samples_vect <- vect(samples_sf) #transformas as amostras estratificadas em vetor
samples_vect <- project(samples_vect, raster_cube) #coloca na mesma projeção

vals <- extract(raster_cube, samples_vect) #extrai os valores das amostras extratificadas do raster
head(vals) #ver os nomes 

unique(values(raster_cube)) # Aqui é confirmado que o raster tem NA (onde está a máscara)

sum(is.na(vals$lyr.1)) #confere se há valores NAs nas amostras estratificadas extraídas

plot(raster_cube)
points(crds(samples_vect), col = "red", pch = 16, cex = 0.5)

# Avaliação de acurácia ------------------------------------------------------------------------------------------------------------------------------------

# Get ground truth points

valid_sample <- sf::read_sf("samples_2classes_verified_034017.shp")

# Calculate accuracy according to Olofsson's method

area_acc <- sits_accuracy(cube_2025_class, 
                          validation = valid_sample,
                          multicores = 4)

# Print the area estimated accuracy 

area_acc

# Matriz de confusão

area_acc$error_matrix
