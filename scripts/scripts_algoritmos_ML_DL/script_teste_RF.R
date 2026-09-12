# ==================================================================================

# CLASSIFICAÇÃO RANDOM FOREST - TESTES 2026.2
# TESTE RANDOM FOREST SEM MÁSCARA DE EXCLUSÃO - RF_SM
# RESPONSÁVEL: JEANNE FRANCO
# DATA: 24/07/2026

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

# Para instalar pacote sits da versão estável do CRAN

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

# CONFIGURAÇÕES DE DIRETÓRIOS E PARÂMETROS

# ==================================================================================

# O modelo será treinado com todas as amostras da RM

tiles_treino <- c("037011", "037012", "037013",
                  "038012", "038013", "039012",
                  "039013", "039014", "040012",
                  "040013", "040014", "041012",
                  "041013", "041014", "041015")

# E a classificação será feita apenas nesses tiles

tile_classificacao <- c("037011", "037012","038012")

start_date <- "2024-07-27"
end_date   <- "2025-12-19"

dir_rds   <- "arquivos_rds"
dir_model <- "modelos"
dir_out   <- "classificacao_RF_SM"

dir.create(dir_rds, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_model, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# ==================================================================================

# CUBO DE TREINO 

# ==================================================================================

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

saveRDS(cubo_treino, file.path(dir_rds, "cubo_treino_teste_RF_SM.rds"))
cubo_treino <- readRDS(file.path(dir_rds, "cubo_treino_teste_RF_SM.rds"))

# ==================================================================================

## Essa função vai ajudar a deixar a marcação de tempo de processamento mais amigável
### Formtando saída da contagem de tempo de processamento

formatar_tempo <- function(segundos) {
  horas <- floor(segundos / 3600)
  minutos <- floor((segundos %% 3600) / 60)
  segundos <- round(segundos %% 60)
  
  sprintf("%02dh %02dm %02ds", horas, minutos, segundos)
}

# ==================================================================================

# 2. EXTRAÇÃO E SALVAMENTO DE AMOSTRAS (COLOQUE O GPKG NA PASTA ANTES)

# ==================================================================================

## Observações: os diretórios devem ser verificados para leitura dos arquivos.

amostras_sf <- sf::read_sf("amostras_adicionais_finais_RM01.shp") # Definir diretório das amostras
nrow(amostras_sf)
unique(amostras_sf$label)

tempo_sits_get_data <- system.time({
amostras_cubo <- sits_get_data(
  cube       = cubo_treino,
  samples    = amostras_sf,
  label_attr = "label",
  multicores = 20,
  progress   = TRUE
)
})

formatar_tempo(tempo_sits_get_data["elapsed"])

saveRDS(amostras_cubo, file.path(dir_rds, "amostras_processadas_teste_RF_SM.rds"))

# Explorar informações das amostras

sits_bands(amostras_cubo)
sits_labels(amostras_cubo)
summary(amostras_cubo)
amostras_cubo[1,]$time_series[[1]]

# Recarregar amostras do cubo em nova sessão

amostras_cubo <- readRDS(file.path(dir_rds, "amostras_processadas_teste_RF_SM.rds"))
summary(amostras_cubo)

# ==================================================================================================

# VISUALIZAR PADRÕES DE SÉRIES TEMPORAIS

# ==================================================================================================

padroes_tempo_amostras <- sits_patterns(amostras_cubo)

# Gráfico

plot(padroes_tempo_amostras)

# ==================================================================================================

# DEFINIR CORES DAS CLASSES

# ==================================================================================================

sits_colors_set(tibble(
  name  = c("aflor_rocha","queimada","supressao", "veg_natural", "agua"),
  color = c("#1A1A1A", "#D60C00", "#FAE9A0","#A6D96A", "#A1DDEF")))

# ==================================================================================================

# TREINAMENTO DO MODELO E VALIDAÇÃO K-FOLD 

# ==================================================================================================

# Treinar modelo

set.seed(520)

tempo_treino <- system.time({
rf_model <- sits_train(
  samples   = amostras_cubo,
  ml_method = sits_rfor())
})

formatar_tempo(tempo_treino["elapsed"])

# Verificar variáveis mais importantes e salvar modelo

plot(rf_model)

# Salvar modelo Random Forest

saveRDS(rf_model, file.path(dir_model, "rf_model_teste_RF_SM.rds"))

# Salvar e ler o modelo Random Forest

rf_model <- readRDS(file.path(dir_model, "rf_model_teste_RF_SM.rds"))
sits_labels(rf_model)

# Validação cruzada

set.seed(520)

rfor_valid <- sits_kfold_validate(
  samples    = amostras_cubo,
  folds      = 5,
  ml_method  = sits_rfor(),
  multicores = 5)

rfor_valid

plot(rfor_valid, type = "confusion_matrix")

# ==================================================================================

# CUBO DE CLASSIFICAÇÃO 

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
  
  # Selecionar um tile específico do cubo para o loop tile a tile
  
  cubo_tile <- sits_select(cubo_classificacao, tiles = tile)
  
  # --------------------------------------------------------------
  # 1) CLASSIFICAÇÃO DEFAULT (probabilidades)
  # --------------------------------------------------------------
  
  tempo_classificacao <- system.time({
  class_probs <- sits_classify(
    data       = cubo_tile,
    ml_model   = rf_model,
    multicores = 16,   
    memsize    = 58,
    gpu_memory = 5,
    progress   = TRUE,
    output_dir = dir_out,
    version    = "RF_SM"
  )
  })
  
  formatar_tempo(tempo_classificacao["elapsed"])
  
  # --------------------------------------------------------------
  # 2) SUAVIZAÇÃO COM HIPERPARÂMETROS
  # --------------------------------------------------------------
  
  # --------------------------------------------------------------------------------------------------------
  # Cálculos das variâncias para usar o sits_smooth
  # --------------------------------------------------------------------------------------------------------
  
  tempo_variance <- system.time({
  variance <- sits_variance(
    cube           = class_probs,
    window_size    = 5,
    neigh_fraction = 0.50,
    multicores     = 16,   
    memsize        = 58,
    gpu_memory     = 5,
    output_dir     = dir_out,
    version        = "RF_SM"
  )
  })
  
  formatar_tempo(tempo_variance["elapsed"])
  
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
  
  cat("\n--- Smoothness escolhidos ---\n")
  print(smooth_values)
  
  formatar_tempo(tempo_smooth["elapsed"])

  
  # ---------------------------------------------------------------------------------
  # Suavização espacial
  # ---------------------------------------------------------------------------------
  
  tempo_smooth_map <- system.time({
  smooth_class <- sits_smooth(
    cube           = class_probs,
    window_size    = 5,
    neigh_fraction = 0.50,
    smoothness     = smooth_values,
    multicores     = 16,   
    memsize        = 58,
    gpu_memory     = 5,
    output_dir     = dir_out,
    version        = "RF_SM"
  )
  })
  
  formatar_tempo(tempo_smooth_map["elapsed"])
  
  # --------------------------------------------------------------
  # 3) CLASSIFICAÇÃO FINAL (MAPA DE CLASSES)
  # --------------------------------------------------------------

  map_class <- sits_label_classification(
    cube       = smooth_class,
    version    = "RF_SM",
    output_dir = dir_out
  )
  
  # -------------------------------------------------------------------
  # 4) MAPA DE INCERTEZA
  # -------------------------------------------------------------------
  
  tempo_uncertainty <- system.time({
  uncertainty <- sits_uncertainty(
    cube        = class_probs,
    type        = "margin",
    version     = "RF_SM",
    output_dir  = dir_out,
    multicores  = 16,   
    memsize     = 58,
    gpu_memory  = 5,
    progress    = TRUE
  )
  })
  
  formatar_tempo(tempo_uncertainty["elapsed"])
  
  cat("\n>>> TILE", tile, "FINALIZADO COM SUCESSO! <<<\n\n")
}

cat("\n\n=============================================================\n")
cat("PROCESSAMENTO DEFAULT DE TODOS OS TILES CONCLUÍDO!\n")
cat("Arquivos salvos em:", dir_out, "\n")
cat("=================================================================\n")

# ==================================================================================

# 7. RELATÓRIO FINAL DO TEMPO DE PROCESSAMENTO

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
