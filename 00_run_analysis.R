# ===
# Run complete analysis
# ===

rm(list = ls(all.names = TRUE))

source(file.path("R", "00_setup.R"))
source(file.path("R", "02_descriptives.R"))
source(file.path("R", "03_binary_models.R"))
source(file.path("R", "04_multinomial_models.R"))
