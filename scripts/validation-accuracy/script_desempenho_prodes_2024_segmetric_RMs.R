# ==================================================================================================
# Avaliação de Desempenho - PRODES x SITS
# Pacote segmetric
# Autoria: Jeanne Franco
# Data: 14/07/2026
# Processa automaticamente todas as regiões RM1-RM7
# ==================================================================================================

# Pacotes ------------------------------------------------------------------------------------------

library(segmetric)
library(sf)
library(lwgeom)
library(dplyr)
library(openxlsx)

# Desabilitar s2
sf::sf_use_s2(FALSE)

# Pasta onde estão os shapefiles
pasta <- "arquivos_shp_corrigidos_por_RM"

# Métricas
metrics <- c("F_measure", "IoU", "precision", "recall", "Dice")

# Regiões
rms <- paste0("RM", 1:7)

# Data frame para armazenar os resultados
resultado_final <- data.frame()

# ==================================================================================================
# LOOP
# ==================================================================================================

for(rm in rms){
  
  cat("\n=========================================\n")
  cat("Processando:", rm, "\n")
  cat("=========================================\n")
  
  arquivo_ref <- file.path(
    pasta,
    paste0("desmatamento_", rm, "_2024_dissolve_total.shp")
  )
  
  arquivo_seg <- file.path(
    pasta,
    paste0("supressao_", rm, "_2024_dissolve_total.shp")
  )
  
  # Verificar existência dos arquivos
  
  if(!file.exists(arquivo_ref)){
    warning("Arquivo não encontrado: ", arquivo_ref)
    next
  }
  
  if(!file.exists(arquivo_seg)){
    warning("Arquivo não encontrado: ", arquivo_seg)
    next
  }
  
  # ================================================================================================
  # Processamento
  # ================================================================================================
  
  tryCatch({
    
    # Ler arquivos
    
    data_ref <- st_read(
      arquivo_ref,
      quiet = TRUE
    ) |>
      st_make_valid()
    
    data_seg <- st_read(
      arquivo_seg,
      quiet = TRUE
    ) |>
      st_make_valid()
    
    # Mesmo CRS
    
    data_seg <- st_transform(
      data_seg,
      st_crs(data_ref)
    )
    
    # Criar objeto do segmetric
    
    objeto <- sm_read(
      ref_sf = data_ref,
      seg_sf = data_seg
    )
    
    # Calcular métricas
    
    metricas <- sm_compute(
      objeto,
      metrics
    )
    
    # Adicionar à tabela final
    
    resultado_final <- rbind(
      resultado_final,
      
      data.frame(
        
        RM = rm,
        
        F_measure = metricas$F_measure,
        
        IoU = metricas$IoU,
        
        Precision = metricas$precision,
        
        Recall = metricas$recall,
        
        Dice = metricas$Dice
        
      )
      
    )
    
  },
  
  error = function(e){
    
    message("Erro em ", rm, ": ", e$message)
    
  })
  
}

# ==================================================================================================
# Organizar tabela
# ==================================================================================================

resultado_final <- resultado_final |>
  arrange(RM)

print(resultado_final)

# ==================================================================================================
# Salvar em Excel
# ==================================================================================================

write.xlsx(
  resultado_final,
  file = "metricas_acuracia_por_RM_prodes_2024.xlsx",
  overwrite = TRUE
)
