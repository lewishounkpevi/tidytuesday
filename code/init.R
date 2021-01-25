
#### INIT ####

NEED <- c(
  "tidyverse" , "lubridate", "stringr", 
  "tictoc", "SmartEDA", "scales", "descr", "openxlsx", "data.table",
  "corrplot", "patchwork", "factoextra", "cluster", 
  "cclust", "vegan", "FactoMineR", "dendextend", "tidytuesdayR",
  "rKenyaCensus"
  
) # tictoc pour quantifier le temps d'exécution

inst <- NEED %in% installed.packages()

if (length(NEED[!inst]) > 0) install.packages(NEED[!inst])

# CHARGEMENT DES PACKAGES

invisible(lapply(NEED, library, character.only = TRUE))

options(bitmapType='cairo')