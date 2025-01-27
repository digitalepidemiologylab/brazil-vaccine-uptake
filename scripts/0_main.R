# Script information ------------
#' Aim: Run different scripts
#' Author: Laura Espinosa
#' Date created: 24 August 2022
#' Date updated: 15 December 2024


# Packages ----------------------
## install/load "pacman" to help installing and loading other packages
message("Installing and loading packages")

while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

## load packages
p_load(tidyverse, vroom, purrr, tidyr,viridis, ggspatial, 
  graphics, stats, janitor, readr, stringr, tibble, utils,
  readxl, tools, padr, lubridate, gtsummary, DataExplorer,
  plotly, caTools, tm, tidytext, textcat, sf, fuzzyjoin, 
  vroom, webshot2, ggpubr, progress, gt, openai,
  grid, cowplot, rnaturalearth)

# Annotations of tweets and full dataset ----------
source('scripts/1b_annotations_all_tweets.R')

# Descriptive analysis -----------
source('scripts/2_descriptive_analysis_mmr.R')

# Datasets for machine learning models --------------
source('scripts/2_Files_preparation_ML.R')
