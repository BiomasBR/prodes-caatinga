# ==================================================================================================
# Avaliação de desempenho por TILE
# PRODES x SITS
# Pacote segmetric
# Autoria: Jeanne Franco
# Data: 15/07/2026
# ==================================================================================================

library(segmetric)
library(sf)
library(lwgeom)
library(dplyr)
library(openxlsx)

# Desabilitar s2
sf::sf_use_s2(FALSE)

# Pasta dos shapefiles
pasta <- "arquivos_shp_corrigidos_por_tile"

# Métricas
metrics <- c("F_measure",
             "IoU",
             "precision",
             "recall",
             "Dice")

# Regiões
RMs <- paste0("RM", 1:7)

# Tabela de resultados
resultado_final <- data.frame()

# ======================================================================================
# LOOP DAS RMS
# ======================================================================================

for(rm in RMs){
  
  cat("\n=================================================\n")
  cat("Processando", rm, "\n")
  cat("=================================================\n")
  
  arquivo_ref <- file.path(
    pasta,
    paste0("desmatamento_prodes_2024_", rm, "_tiles_dissolve.shp")
  )
  
  arquivo_seg <- file.path(
    pasta,
    paste0("supressao_sits_2024_", rm, "_tiles_dissolve.shp")
  )
  
  # Ler arquivos
  
  ref <- st_read(arquivo_ref, quiet = TRUE) |>
    st_make_valid()
  
  seg <- st_read(arquivo_seg, quiet = TRUE) |>
    st_make_valid()
  
  # Manter mesmo CRS para ambos os arquivos
  
  seg <- st_transform(seg, st_crs(ref))
  
  # Padronizar campo do tile
  
  ref$tiles <- as.character(ref$tiles)
  seg$tile  <- as.character(seg$tile)
  
  # Lista de tiles existentes na referência
  
  lista_tiles <- sort(unique(ref$tiles))
  
  # ====================================================================================
  # LOOP DOS TILES
  # ====================================================================================
  
  for(tile_atual in lista_tiles){
    
    cat("Tile:", tile_atual, "\n")
    
    ref_tile <- ref |>
      filter(tiles == tile_atual)
    
    seg_tile <- seg |>
      filter(tile == tile_atual)
    
    # Ignorar se algum estiver vazio
    
    if(nrow(ref_tile) == 0 | nrow(seg_tile) == 0){
      
      cat("Sem correspondência.\n")
      
      next
      
    }
    
    # Calcular métricas
    
    tryCatch({
      
      objeto_seg <- sm_read(
        ref_sf = ref_tile,
        seg_sf = seg_tile
      )
      
      metricas <- sm_compute(
        objeto_seg,
        metrics
      )
      
      resultado_final <- rbind(
        resultado_final,
        
        data.frame(
          
          RM = rm,
          
          Tile = tile_atual,
          
          F_measure = metricas$F_measure,
          
          IoU = metricas$IoU,
          
          Precision = metricas$precision,
          
          Recall = metricas$recall,
          
          Dice = metricas$Dice
          
        )
        
      )
      
    },
    
    error = function(e){
      
      message(
        "Erro em ",
        rm,
        " - Tile ",
        tile_atual,
        ": ",
        e$message
      )
      
    })
    
  }
  
}

# ======================================================================================
# Resultado
# ======================================================================================

resultado_final

# Ordenar

resultado_final <- resultado_final |>
  arrange(RM, Tile)

# Visualizar

print(resultado_final)

# ==================================================================================================
# Salvar em Excel
# ==================================================================================================

write.xlsx(
  resultado_final,
  file = "metricas_acuracia_por_tile_prodes_2024.xlsx",
  overwrite = TRUE
)
