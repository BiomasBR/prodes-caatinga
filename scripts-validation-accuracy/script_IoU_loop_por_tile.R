# ============================================================
# AVALIAÇÃO IoU - SITS VS PRODES
# PRODES CAATINGA 2024
# AVALIAÇÃO POR TILE PARA TODAS AS RMs
# COM ÁREAS DE FALSO POSITIVO E FALSO NEGATIVO
# E SALVANDO SHAPEFILES DE INTERSEÇÃO, FP E FN
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
# PASTA DOS SHAPEFILES
# ============================================================

pasta_shp <- "arquivos_shp_corrigidos_por_tile"

# ============================================================
# PASTA DE SAÍDA DOS SHAPEFILES
# ============================================================

pasta_saida <- "saidas_shp_tiles"

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# ============================================================
# FUNÇÃO AUXILIAR PARA PREPARAR GEOMETRIA PARA SHAPEFILE
# ============================================================

preparar_shp <- function(geom, rm_id, tile_id, tipo, area_ha, crs_saida = 31984) {
  
  if (is.null(geom)) {
    return(NULL)
  }
  
  # cria objeto sf
  shp_obj <- st_sf(
    RM = rm_id,
    tile = tile_id,
    tipo = tipo,
    area_ha = round(area_ha, 2),
    geometry = st_sfc(geom, crs = crs_saida)
  )
  
  # remover vazios
  shp_obj <- shp_obj[!st_is_empty(shp_obj), ]
  if (nrow(shp_obj) == 0) return(NULL)
  
  # extrair apenas polígonos de collections
  tipos <- unique(as.character(st_geometry_type(shp_obj)))
  
  if (any(tipos %in% c("GEOMETRYCOLLECTION", "GEOMETRY"))) {
    shp_obj <- st_collection_extract(shp_obj, "POLYGON")
  }
  
  # remover vazios novamente
  shp_obj <- shp_obj[!st_is_empty(shp_obj), ]
  if (nrow(shp_obj) == 0) return(NULL)
  
  # converter para multipolygon
  shp_obj <- tryCatch(
    st_cast(shp_obj, "MULTIPOLYGON", warn = FALSE),
    error = function(e) shp_obj
  )
  
  # validar
  shp_obj <- st_make_valid(shp_obj)
  
  # remover vazios finais
  shp_obj <- shp_obj[!st_is_empty(shp_obj), ]
  if (nrow(shp_obj) == 0) return(NULL)
  
  # garantir tipo poligonal aceito
  tipos_finais <- unique(as.character(st_geometry_type(shp_obj)))
  if (!all(tipos_finais %in% c("POLYGON", "MULTIPOLYGON"))) {
    return(NULL)
  }
  
  return(shp_obj)
}

# ============================================================
# LISTAR ARQUIVOS SITS E IDENTIFICAR AS RMs
# ============================================================

arquivos_sits <- list.files(
  path = pasta_shp,
  pattern = "^supressao_sits_2024_RM[0-9]+_tiles_dissolve\\.shp$",
  full.names = FALSE
)

rms <- stringr::str_extract(arquivos_sits, "RM[0-9]+")

print(rms)

# ============================================================
# TABELA FINAL DE RESULTADOS
# ============================================================

resultados_finais <- data.frame()

# ============================================================
# LOOP POR RM
# ============================================================

for (rm_id in rms) {
  
  cat("\n==============================\n")
  cat("Processando", rm_id, "\n")
  cat("==============================\n")
  
  # ----------------------------------------------------------
  # nomes dos arquivos da RM
  # ----------------------------------------------------------
  
  arquivo_sits <- file.path(
    pasta_shp,
    paste0("supressao_sits_2024_", rm_id, "_tiles_dissolve.shp")
  )
  
  arquivo_prodes <- file.path(
    pasta_shp,
    paste0("desmatamento_prodes_2024_", rm_id, "_tiles_dissolve.shp")
  )
  
  # ----------------------------------------------------------
  # verificar se os arquivos existem
  # ----------------------------------------------------------
  
  if (!file.exists(arquivo_sits)) {
    cat("Arquivo SITS não encontrado para", rm_id, "\n")
    next
  }
  
  if (!file.exists(arquivo_prodes)) {
    cat("Arquivo PRODES não encontrado para", rm_id, "\n")
    next
  }
  
  # ----------------------------------------------------------
  # ler shapefiles
  # ----------------------------------------------------------
  
  sits <- st_read(arquivo_sits, quiet = TRUE)
  prodes <- st_read(arquivo_prodes, quiet = TRUE)
  
  # ----------------------------------------------------------
  # padronizar nome da coluna tile
  # ----------------------------------------------------------
  
  if ("tiles" %in% names(prodes)) {
    prodes <- prodes %>% rename(tile = tiles)
  }
  
  if (!"tile" %in% names(sits)) {
    stop(paste("A coluna 'tile' não foi encontrada em", arquivo_sits))
  }
  
  if (!"tile" %in% names(prodes)) {
    stop(paste("A coluna 'tile' não foi encontrada em", arquivo_prodes))
  }
  
  sits$tile <- as.character(sits$tile)
  prodes$tile <- as.character(prodes$tile)
  
  # ----------------------------------------------------------
  # garantir mesmo CRS
  # ----------------------------------------------------------
  
  if (st_crs(sits) != st_crs(prodes)) {
    prodes <- st_transform(prodes, st_crs(sits))
  }
  
  # ----------------------------------------------------------
  # projetar para CRS métrico
  # ----------------------------------------------------------
  
  sits_proj <- st_transform(sits, 31984)
  prodes_proj <- st_transform(prodes, 31984)
  
  # ----------------------------------------------------------
  # corrigir geometrias
  # ----------------------------------------------------------
  
  sits_proj <- st_make_valid(sits_proj)
  prodes_proj <- st_make_valid(prodes_proj)
  
  # ----------------------------------------------------------
  # lista de tiles da RM
  # ----------------------------------------------------------
  
  tiles_lista <- sort(unique(c(sits_proj$tile, prodes_proj$tile)))
  
  resultados_rm <- data.frame()
  
  # ----------------------------------------------------------
  # criar pasta da RM
  # ----------------------------------------------------------
  
  pasta_rm <- file.path(pasta_saida, rm_id)
  
  if (!dir.exists(pasta_rm)) {
    dir.create(pasta_rm, recursive = TRUE)
  }
  
  # ==========================================================
  # LOOP POR TILE
  # ==========================================================
  
  for (tile_id in tiles_lista) {
    
    cat("  Processando tile:", tile_id, "\n")
    
    # filtrar tile
    sits_tile <- sits_proj %>% filter(.data$tile == tile_id)
    prodes_tile <- prodes_proj %>% filter(.data$tile == tile_id)
    
    # --------------------------------------------------------
    # unir geometrias dentro do tile
    # --------------------------------------------------------
    
    geom_sits <- if (nrow(sits_tile) > 0) {
      tryCatch(st_union(st_geometry(sits_tile)), error = function(e) NULL)
    } else {
      NULL
    }
    
    geom_prodes <- if (nrow(prodes_tile) > 0) {
      tryCatch(st_union(st_geometry(prodes_tile)), error = function(e) NULL)
    } else {
      NULL
    }
    
    # --------------------------------------------------------
    # áreas de cada base
    # --------------------------------------------------------
    
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
    
    # --------------------------------------------------------
    # interseção
    # --------------------------------------------------------
    
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
    
    # --------------------------------------------------------
    # união
    # --------------------------------------------------------
    
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
    
    # --------------------------------------------------------
    # falso positivo e falso negativo
    # --------------------------------------------------------
    
    falso_pos_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
      tryCatch(st_difference(geom_sits, geom_prodes), error = function(e) NULL)
    } else if (!is.null(geom_sits)) {
      geom_sits
    } else {
      NULL
    }
    
    falso_neg_geom <- if (!is.null(geom_sits) && !is.null(geom_prodes)) {
      tryCatch(st_difference(geom_prodes, geom_sits), error = function(e) NULL)
    } else if (!is.null(geom_prodes)) {
      geom_prodes
    } else {
      NULL
    }
    
    area_falso_positivo <- if (!is.null(falso_pos_geom) && !all(st_is_empty(falso_pos_geom))) {
      as.numeric(st_area(falso_pos_geom)) / 10000
    } else {
      0
    }
    
    area_falso_negativo <- if (!is.null(falso_neg_geom) && !all(st_is_empty(falso_neg_geom))) {
      as.numeric(st_area(falso_neg_geom)) / 10000
    } else {
      0
    }
    
    # --------------------------------------------------------
    # métricas
    # --------------------------------------------------------
    
    IoU <- if (area_uniao > 0) area_intersec / area_uniao else NA_real_
    Precision <- if (area_sits > 0) area_intersec / area_sits else NA_real_
    Recall <- if (area_prodes > 0) area_intersec / area_prodes else NA_real_
    
    F1 <- if (!is.na(Precision) && !is.na(Recall) && (Precision + Recall) > 0) {
      2 * (Precision * Recall) / (Precision + Recall)
    } else {
      NA_real_
    }
    
    # --------------------------------------------------------
    # resultado do tile
    # --------------------------------------------------------
    
    resultado <- data.frame(
      RM = rm_id,
      tile = tile_id,
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
    
    resultados_rm <- bind_rows(resultados_rm, resultado)
    
    # --------------------------------------------------------
    # preparar shapefiles
    # --------------------------------------------------------
    
    intersec_sf <- preparar_shp(
      geom = intersec_geom,
      rm_id = rm_id,
      tile_id = tile_id,
      tipo = "intersecao",
      area_ha = area_intersec
    )
    
    falso_pos_sf <- preparar_shp(
      geom = falso_pos_geom,
      rm_id = rm_id,
      tile_id = tile_id,
      tipo = "falso_positivo",
      area_ha = area_falso_positivo
    )
    
    falso_neg_sf <- preparar_shp(
      geom = falso_neg_geom,
      rm_id = rm_id,
      tile_id = tile_id,
      tipo = "falso_negativo",
      area_ha = area_falso_negativo
    )
    
    # --------------------------------------------------------
    # salvar shapefiles do tile
    # --------------------------------------------------------
    
    if (!is.null(intersec_sf)) {
      st_write(
        intersec_sf,
        dsn = file.path(pasta_rm, paste0("intersecao_", rm_id, "_", tile_id, ".shp")),
        delete_layer = TRUE,
        quiet = TRUE
      )
    }
    
    if (!is.null(falso_pos_sf)) {
      st_write(
        falso_pos_sf,
        dsn = file.path(pasta_rm, paste0("falso_positivo_", rm_id, "_", tile_id, ".shp")),
        delete_layer = TRUE,
        quiet = TRUE
      )
    }
    
    if (!is.null(falso_neg_sf)) {
      st_write(
        falso_neg_sf,
        dsn = file.path(pasta_rm, paste0("falso_negativo_", rm_id, "_", tile_id, ".shp")),
        delete_layer = TRUE,
        quiet = TRUE
      )
    }
    
    cat("  ", tile_id, "processado com sucesso! ✅\n")
  }
  
  resultados_finais <- bind_rows(resultados_finais, resultados_rm)
  
  cat("Finalizado:", rm_id, "\n")
}

# ============================================================
# RESULTADO FINAL
# ============================================================

print(resultados_finais)

# ============================================================
# SALVAR EM XLSX
# ============================================================

write_xlsx(resultados_finais, "resultados_IoU_tiles.xlsx")
