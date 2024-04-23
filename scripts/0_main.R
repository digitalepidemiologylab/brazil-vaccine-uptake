# Script information ------------
#' Aim: Run different scripts
#' Author: Laura Espinosa
#' Date created: 24 August 2022
#' Date updated: 05 February 2023


# Packages ----------------------
## install/load "pacman" to help installing and loading other packages
message("Installing and loading packages")

while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

## load packages
p_load(
  # Script annotations
  tidyverse, vroom, purrr, tidyr,
  # caret, 
  
  # Script descriptive analysis     
  # data.table, ggpubr, 
  ggspatial, 
  graphics, stats, janitor, readr, stringr, tibble, utils,
  readxl, tools, padr, lubridate, gtsummary, DataExplorer,
  plotly, caTools, tm, tidytext, textcat, sf,
  #    
  rnaturalearth  
  #utils, zoo
  )

# Annotations of tweets and full dataset ----------
source('scripts/1b_annotations_all_tweets.R')

# Descriptive analysis -----------
source('scripts/1_descriptive_analysis.R')

# Datasets for machine learning models --------------
source('scripts/2_Files_preparation_ML.R')

# Check packages used in each script -------
packages_annotations <- NCmisc::list.functions.in.file("scripts/1b_annotations_all_tweets.R")
packages_descriptive <- NCmisc::list.functions.in.file("scripts/2_descriptive_analysis_mmr.R")
packages_ML <- NCmisc::list.functions.in.file("scripts/2_Files_preparation_ML.R")
