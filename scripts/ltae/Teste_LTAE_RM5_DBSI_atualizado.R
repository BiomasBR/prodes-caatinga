# ==================================================================================

# PRODES 2025 - CLASSIFICAÇÃO LTAE - BANDAS = DBSI

# TREINO: múltiplos tiles | CLASSIFICAÇÃO DOS TILES: "034016", "034017", "033018" 

# ==================================================================================

rm(list = ls())

# ==================================================================================

# PACOTES

# ==================================================================================

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

# O modelo será treinado com todas as amostras da RM5 (Região de Mapeamento 5, com 8 tiles)
tiles_treino <- c("033016", "033018", "034016",
                  "034017", "034018", "035015",
                  "035016", "035017")

# E a classificação será feita apenas nesses tiles
tile_classificacao <- c("034016", "034017", "033018")

start_date <- "2024-07-27"
end_date   <- "2025-10-16"

dir_rds   <- "Prodes2025/Arquivos_rds"
dir_model <- "Prodes2025/modelos"
dir_indices<-"Prodes2025/Resultado/RM5/Índices"
dir_out   <- "Prodes2025/Resultado/RM5/Classificacao_LTAE"


dir.create(dir_rds, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_model, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# ==================================================================================

# 1. CUBO DE TREINO (MÚLTIPLOS TILES)

# ==================================================================================

cubo_treino <- sits_cube(
  source     = "BDC",
  collection = "SENTINEL-2-16D",
  tiles      = tiles_treino,
  start_date = start_date,
  end_date   = end_date
)

cubo_treino <- sits_apply(
  cubo_treino,
  DBSI = ((B11 - B03) / (B11 + B03)) - ((B08 - B04) / (B08 + B04)),
  output_dir = dir_indices, #modifique o local conforme o seu computador
  progress = TRUE,
  multicores = 30
  
)

cubo_treino <- sits_select(
  cubo_treino,
  bands = c("B02","B03","B04","B05","B06","B07","B08",
            "B11","B12","B8A", "DBSI", "CLOUD")
)



sits_bands(cubo_treino)
sits_timeline(cubo_treino)


saveRDS(cubo_treino, file.path(dir_rds, "cubo_treino_rm5_dbsi_ltae_2025.rds"))
cubo_treino <- readRDS(file.path(dir_rds, "cubo_treino_rm5_dbsi_ltae_2025.rds"))

# ==================================================================================

# 2. AMOSTRAS (USANDO CUBO DE TREINO)

# ==================================================================================

# amostras <- sits_get_data(
#   cube        = cubo_treino,
#   samples     = "Prodes2025/Amostras/RM5/amostras_adicionais_finais_RM05.shp",
#   label_attr  = "label",
#   bands       = sits_bands(cubo_treino),
#   multicores  = 38,# o máximo  é 44
#   memsize     = 110, # o máximo é 144
#   progress    = TRUE
# )
# 
# saveRDS(amostras, file.path(dir_rds, "amostras_rm5_dbsi.rds"))
# Recarregar em nova sessão
amostras <- readRDS(
  "Prodes2025/Arquivos_rds/amostras_rm5_dbsi.rds")

sits_bands(amostras)
summary(amostras)

# ==================================================================================

# 3. TUNING LTAE

# ==================================================================================

## Essa função vai ajudar a deixar a marcação de tempo de processamento mais amigável
### Formtando saída da contagem de tempo de processamento
formatar_tempo <- function(segundos) {
  horas <- floor(segundos / 3600)
  minutos <- floor((segundos %% 3600) / 60)
  segundos <- round(segundos %% 60)
  
  sprintf("%02dh %02dm %02ds", horas, minutos, segundos)
}



tempo_tuning <- system.time({ #para contabilizar o tempo de processamento
  tuned_rm5 <- sits_tuning(
    samples = amostras,
    ml_method = sits_lighttae(),
    params = sits_tuning_hparams(
      optimizer = torch::optim_adamw,
      opt_hparams = list(
        lr = loguniform(10^-2, 10^-4),
        weight_decay = loguniform(10^-2, 10^-8)
      )
    ),
    trials = 40,
    gpu_memory = 30,
    multicores = 30,
    progress = FALSE
  )
})


formatar_tempo(tempo_tuning["elapsed"])
#[1] "01h 10m 23s"


saveRDS(tuned_rm5, "Prodes2025/Arquivos_rds/tuned_ltae_rm5_dbsi.rds")
tuned_rm5 <- readRDS("Prodes2025/Arquivos_rds/tuned_ltae_rm5_dbsi.rds")


# Obtain accuracy, kappa, lr, and weight decay for the 5 best results
# Hyperparameters are organized as a list
hparams_5 <- tuned_rm5[1:5,]$opt_hparams

# Extract learning rate and weight decay from the list
lr_5 <- purrr::map_dbl(hparams_5, function(h) h$lr)
wd_5 <- purrr::map_dbl(hparams_5, function(h) h$weight_decay)

# Create a tibble to display the results
best_5 <- tibble::tibble(
  accuracy = tuned_rm5[1:5,]$accuracy,
  kappa = tuned_rm5[1:5,]$kappa,
  lr    = lr_5,
  weight_decay = wd_5)

# Print the best five combination of hyperparameters
best_5

# accuracy kappa       lr weight_decay
# <dbl> <dbl>    <dbl>        <dbl>
# 1    0.906 0.879 0.000236  0.0000171  
# 2    0.903 0.874 0.000777  0.000000653
# 3    0.899 0.870 0.000223  0.000965   
# 4    0.894 0.863 0.00172   0.000000931
# 5    0.892 0.861 0.000641  0.00000558 
# 
best_params <- tuned_rm5$opt_hparams[[1]]

print(best_params)
$lr
[1] 0.0002357948

$weight_decay
[1] 1.714641e-05



# ==================================================================================

# 4. TREINAMENTO FINAL

# ==================================================================================

set.seed(42)
tempo_treino <- system.time({
  modelo_ltae <- sits_train(
    samples = amostras,
    ml_method = sits_lighttae(
      optimizer = torch::optim_adamw,
      opt_hparams = list(
        lr = best_params$lr, #aqui foram escolhidos os melhores parâmetros
        weight_decay = best_params$weight_decay #aqui foram escolhidos os melhores parâmetros 
      ), 
      epochs = 100 ### o default do sits é 150, usar 100 e ver onde estabilizou
    )
  )
})


plot(modelo_ltae)
#mostra o tempo de processamento
formatar_tempo(tempo_treino["elapsed"])
#[1] "00h 02m 32s


saveRDS(modelo_ltae , file.path(dir_model, "modelo_ltae_rm5_dbsi.rds"))
modelo_ltae2<-readRDS("Prodes2025/modelos/modelo_ltae_rm5_dbsi.rds")

plot(modelo_ltae)


# ==================================================================================

# 5. CUBO DE CLASSIFICAÇÃO - PARA OS TILES "034016", "034017", "033018

# ==================================================================================

cubo_classificacao <- sits_cube(
  source     = "BDC",
  collection = "SENTINEL-2-16D",
  tiles      = tile_classificacao,
  start_date = start_date,
  end_date   = end_date
)

cubo_classificacao <- sits_apply(
  cubo_classificacao,
  DBSI = ((B11 - B03) / (B11 + B03)) - ((B08 - B04) / (B08 + B04)),
  output_dir = dir_indices,
  progress = TRUE,
  multicores = 30
)


cubo_classificacao <- sits_select(
  cubo_classificacao,
  bands = c("B02","B03","B04","B05","B06","B07","B08",
            "B11","B12","B8A","DBSI","CLOUD")
)

# ==================================================================================

# 6. CLASSIFICAÇÃO

# ==================================================================================

tempo_classificacao <- system.time({
  class_probs <- sits_classify(
    data       = cubo_classificacao,
    ml_model   = modelo_ltae,
    output_dir = dir_out,
    multicores = 1, ## aqui é melhor deixar 1 multicores
    memsize    = 96, #o máximo é 144
    gpu_memory = 16, #o máximo é 47 mas só rodou com 16
    progress   = TRUE,
    version    = "dbsi"
  )
})

# mostra o tempo de processamento
formatar_tempo(tempo_classificacao["elapsed"])



# ==================================================================================

# 7. VARIÂNCIA

# ==================================================================================

tempo_variance <- system.time({
  variance <- sits_variance(
    cube           = class_probs,
    window_size    = 5,
    neigh_fraction = 0.5,
    output_dir     = dir_out,
    multicores     = 30,
    memsize        = 86,
    version        = "dbsi"
  )
})

formatar_tempo(tempo_variance["elapsed"])


# ==================================================================================

# 8. HIPERPARÂMETROS DE SUAVIZAÇÃO

# ==================================================================================


tempo_smooth <- system.time({
  smooth_values_tiles <- purrr::map(tile_classificacao, function(tile_id) {
    
    cat("\n=============================\n")
    cat("TILE:", tile_id, "\n")
    cat("=============================\n")
    
    sumv_df <- as.data.frame(
      summary(
        variance %>% dplyr::filter(tile == tile_id)
      )
    )
    
    smooth_values <- c(
      aflor_rocha = sumv_df["80%", "aflor_rocha"],
      agua        = sumv_df["85%", "agua"],
      queimada    = sumv_df["85%", "queimada"],
      supressao   = sumv_df["80%", "supressao"],
      veg_natural = sumv_df["85%", "veg_natural"]
    )
    
    print(smooth_values)
    return(smooth_values)
  })
})

names(smooth_values_tiles) <- tile_classificacao
smooth_values_tiles


# ==================================================================================
# 9. SUAVIZAÇÃO (POR TILE) + MAPA FINAL
# ==================================================================================

tempo_smooth_map <- system.time({
  
  mapa_tiles <- purrr::imap(smooth_values_tiles, function(smooth_vals, tile_id) {
    
    cat("\n=============================\n")
    cat("PROCESSANDO TILE:", tile_id, "\n")
    cat("=============================\n")
    
    cube_tile <- class_probs %>%
      dplyr::filter(tile == tile_id)
    
    # SMOOTH
    cube_smooth <- sits_smooth(
      cube           = cube_tile,
      smoothness     = smooth_vals,
      window_size    = 5,
      neigh_fraction = 0.5,
      progress       = TRUE,
      output_dir     = dir_out,
      multicores     = 30,
      memsize        = 86,
      version        = "dbsi"
    )
    
    # MAPA FINAL
    sits_label_classification(
      cube       = cube_smooth,
      output_dir = dir_out,
      multicores = 30,
      memsize    = 82,
      version    = "dbsi"
    )
  })
})

formatar_tempo(tempo_smooth_map["elapsed"])



# ==================================================================================

# 10. INCERTEZA

# ==================================================================================

tempo_uncertainty <- system.time({
  uncertainty <- sits_uncertainty(
    cube       = class_probs,
    type       = "margin",
    output_dir = dir_out,
    multicores = 30,
    memsize    = 86,
    version    = "dbsi"
  )
})

formatar_tempo(tempo_uncertainty["elapsed"])


###Tempo de processamento
tempos_df <- tibble(
  etapa = c(
    "Tuning",
    "Treinamento",
    "Classificação",
    "Variância",
    "Valores Smooth",
    "Suavização + Mapa",
    "Incerteza"
  ),
  
  tempo_horas = c(
    tempo_tuning["elapsed"] / 3600,
    tempo_treino["elapsed"] / 3600,
    tempo_classificacao["elapsed"] / 3600,
    tempo_variance["elapsed"] / 3600,
    tempo_smooth["elapsed"] / 3600,
    tempo_smooth_map["elapsed"] / 3600,
    tempo_uncertainty["elapsed"] / 3600
  ),
  
  tempo_formatado = c(
    formatar_tempo(tempo_tuning["elapsed"]),
    formatar_tempo(tempo_treino["elapsed"]),
    formatar_tempo(tempo_classificacao["elapsed"]),
    formatar_tempo(tempo_variance["elapsed"]),
    formatar_tempo(tempo_smooth["elapsed"]),
    formatar_tempo(tempo_smooth_map["elapsed"]),
    formatar_tempo(tempo_uncertainty["elapsed"])
  )
)

tempos_df


