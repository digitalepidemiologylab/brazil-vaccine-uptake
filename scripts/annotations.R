# Script information ------------
#' Aim: Compare annotations with GPT
#' Author: Laura Espinosa
#' Date created: 26 July 2021
#' Date updated: 05 February 2023

# 1 - Packages ----------------------
# install/load "pacman" to help installing and loading other packages
message("Installing and loading packages")

while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# load packages
p_load(tidyverse, vroom)

# Import and clean data -----------
## PAHO --------
message("Getting PAHO annotations")
setwd("data/local")
files_paho <- fs::dir_ls(glob = "local-batch-job-results*csv")
paho <- vroom(files_paho)
setwd("../..")

paho_clean <- paho %>% 
  filter(question_tag == "sentiment") %>% 
  group_by(text, answer_tag) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from = answer_tag,
              values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = negative + positive + neutral,
         neg_per = negative / total * 100,
         pos_per = positive / total * 100,
         neu_per = neutral / total * 100,
         agreement = case_when(neg_per >= 60 | pos_per >= 60 | neu_per >= 60 ~ 1,
                               .default = 0)) %>% 
  filter(total >= 3) %>% 
  filter(agreement == 1)

paho_clean %>% 
  write.csv("data/local/paho_tweets_filtered.csv")

## Mturk ---------
br_mturk <- read_csv("data/local/mturk-batch-job-results-paho-batch-1-15-1611993480-67863.csv") 

br_mturk_clean <- br_mturk %>% 
  filter(question_tag == "sentiment") %>% 
  group_by(text, answer_tag) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from = answer_tag,
              values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = negative + positive + neutral,
         neg_per = negative / total * 100,
         pos_per = positive / total * 100,
         neu_per = neutral / total * 100,
         agreement = case_when(neg_per >= 60 | pos_per >= 60 | neu_per >= 60 ~ 1,
                               .default = 0)) %>% 
  subset(., text %in% paho_clean$text)

br_mturk_clean %>% 
  write.csv("data/local/mturk_tweets_filtered.csv")

