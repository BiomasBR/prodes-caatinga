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


#
# 2. Define temporary directory
#
# Create a temporary directory for intermediate files, if it does not exist.
#

tempdir_r <- "~/sitsbook/tempdir/R/cl_machinelearning"
dir.create(tempdir_r, showWarnings = FALSE)


#
# 3. Set seed for reproducibility
#

set.seed(290356)


#
# 4. Train the Random Forest model
#
# The model is trained using the Mato Grosso MOD13Q1 sample set.
#

rfor_model <- sits_train(
  samples = samples_matogrosso_mod13q1,
  ml_method = sits_rfor(num_trees = 100)
)


#
# 5. Plot the default variable importance summary
#
# This plot shows the most important variables according to the default
# visualization provided by the sits package.
#

plot(rfor_model)


#
# 6. Export the fitted Random Forest model
#
# The model is exported to access detailed variable importance metrics
# using the randomForestExplainer package.
#

rf_obj <- sits_model_export(rfor_model)


#
# 7. Compute variable importance measures
#
# The resulting table contains several importance metrics, including
# mean minimal depth, where lower values indicate higher importance.
#

imp <- measure_importance(rf_obj)


#
# 8. Rank variables by importance
#
# Variables are ordered from the most important to the least important
# based on mean minimal depth.
#

imp_ordered <- imp %>%
  arrange(mean_min_depth)


#
# 9. Display the complete ranked importance table
#

print(imp_ordered)


#
# 10. Plot the importance distribution for all variables
#
# The argument k = nrow(imp) ensures that all variables are included
# in the graphical representation.
#

plot_min_depth_distribution(rf_obj, k = nrow(imp))


#
# 11. Select the top 20 most important variables
#
# This subset can be used to identify the most relevant spectral-temporal
# variables for classification.
#

top_variables <- imp_ordered %>%
  slice(1:20)


#
# 12. Display the selected top variables
#
# Only the variable name and mean minimal depth are shown for clarity.
#

print(top_variables[, c("variable", "mean_min_depth")])


#
# 13. Count the frequency of each variable type among the top variables
#
# Numerical suffixes are removed from variable names to group them by
# spectral index or band type (e.g., NDVI, EVI, NIR, MIR).
#

variable_type_frequency <- top_variables %>%
  mutate(variable_type = str_remove(variable, "[0-9]+")) %>%
  count(variable_type, sort = TRUE)


#
# 14. Display the frequency of variable types among the top 20 variables
#

print(variable_type_frequency)