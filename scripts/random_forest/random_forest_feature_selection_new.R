#
# Random Forest training and variable importance analysis
#

#
# 1. Load required packages
#

library(tibble)
library(dplyr)
library(stringr)
library(sits)
library(sitsdata)
library(randomForestExplainer)

library(ggplot2)


#
# 2. Define temporary directory
#

tempdir_r <- "~/sitsbook/tempdir/R/cl_machinelearning"
dir.create(tempdir_r, showWarnings = FALSE)


#
# 3. Set seed for reproducibility
#

set.seed(290356)


#
# 4. Load trained Random Forest model
#

rfor_model <- readRDS("~/Downloads/rf_model2_rm5.rds")


#
# 5. Inspect model bands
#

band_names <- sits_bands(rfor_model)
print(band_names)


#
# 6. Plot default variable importance (sits)
#

plot(rfor_model)


#
# 7. Export model to randomForestExplainer format
#

rf_obj <- sits_model_export(rfor_model)


#
# 8. Compute variable importance metrics
#

imp <- measure_importance(rf_obj)


#
# 9. Rank variables by importance (mean minimal depth)
#

imp_ordered <- imp %>%
  arrange(mean_min_depth)


#
# 10. Display ranked importance table
#

print(imp_ordered)


#
# 11. Plot importance distribution for all variables
#

plot_min_depth_distribution(rf_obj, k = nrow(imp))


#
# 12. Select top 50 most important variables
#

top_variables <- imp_ordered %>%
  slice(1:50)


#
# 13. Display top variables
#

print(top_variables[, c("variable", "mean_min_depth")])


#
# 14. Define function to extract variable type
#

# Order band names by decreasing length to avoid prefix conflicts
band_names <- band_names[order(nchar(band_names), decreasing = TRUE)]

clean_variable_type <- function(x, band_names) {
  
  # Check if variable belongs to a known spectral band
  for (band in band_names) {
    if (str_detect(x, paste0("^", band))) {
      return(band)
    }
  }
  
  # Otherwise, assume it is an index and remove trailing numbers
  return(str_remove(x, "[0-9]+$"))
}


#
# 15. Compute frequency of variable types
#

variable_type_frequency <- top_variables %>%
  mutate(
    variable_type = sapply(variable, clean_variable_type, band_names = band_names)
  ) %>%
  count(variable_type, sort = TRUE)


#
# 16. Display frequency results
#

print(variable_type_frequency)


#
# 17. Vertical bar plot of variable type frequency
#

ggplot(
  variable_type_frequency,
  aes(x = reorder(variable_type, -n), y = n, fill = variable_type)
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 5) +
  labs(
    title = "Frequency of Spectral Variables Among Top 50 Most Important Features",
    x = "Spectral Variable",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black")
  ) +
  expand_limits(y = max(variable_type_frequency$n) + 1)
