# Script information ------------
#' Aim: Run different scripts
#' Author: Laura Espinosa
#' Date created: 24 August 2022
#' Date updated: 05 February 2023


# Descriptive analysis -----------
source('scripts/1_descriptive_analysis.R')

# Datasets for machine learning models --------------
source('scripts/2_Files_preparation_ML.R')

# Check packages used in each script -------
packages_descriptive <- NCmisc::list.functions.in.file("scripts/1_descriptive_analysis.R")
packages_ML <- NCmisc::list.functions.in.file("scripts/2_Files_preparation_ML.R")
