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
p_load(tidyverse, vroom, caret)

# Import and clean data -----------
## PAHO annotations --------
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
  filter(agreement == 1) %>%
  mutate(sentiment_paho = case_when(positive > neutral & positive > negative ~ "positive",
                                    negative > positive & negative > neutral ~ "negative",
                                    neutral > positive & neutral > negative ~ "neutral",
                                    .default = "neutral"))

paho_clean %>% 
  write.csv("data/local/paho_tweets_filtered.csv")

## All tweets -------------
#df <- read_csv("data/local/tweets_filtered_BR_PT.csv")

## GPT 4 annotations ------------
message("Getting GPT 4 annotations")
setwd("data/local")
files_gpt <- fs::dir_ls(glob = "gpt_sentiment_paho*csv")
gpt <- vroom(files_gpt)
setwd("../..")

gpt_pt <- gpt %>% 
  filter(prompt != 3) %>% 
  select(text, sentiment_gpt, prompt, model) %>% 
  mutate(sentiment_gpt = tolower(sentiment_gpt),
         # id_tweets = id_gpt + 1,
         sentiment_gpt = str_match(sentiment_gpt, "\\b(neutro|negativo|positivo|neutra|negativa|positiva)\\b")[,2],
         sentiment_gpt = case_when(is.na(sentiment_gpt) ~ "neutro",
                                   .default = sentiment_gpt),
         sentiment_gpt = case_when(sentiment_gpt == "positivo" | sentiment_gpt == "positiva" ~ "positive",
                                       sentiment_gpt == "negativo" | sentiment_gpt == "negativa" ~ "negative",
                                       sentiment_gpt == "neutro" | sentiment_gpt == "neutra" ~ "neutral",
                                       .default = "neutral")) %>% 
  distinct(text, prompt, .keep_all = TRUE)

gpt_en <- gpt %>% 
  filter(prompt == 3) %>% 
  select(text, sentiment_gpt, prompt, model) %>% 
  mutate(sentiment_gpt = tolower(sentiment_gpt),
         # id_tweets = id_gpt + 1,
         sentiment_gpt = str_match(sentiment_gpt, "\\b(neutral|negative|positive)\\b")[,2],
         sentiment_gpt = case_when(is.na(sentiment_gpt) ~ "neutral",
                                   .default = sentiment_gpt)) %>% 
  distinct(text, prompt, .keep_all = TRUE)

gpt_clean <- gpt_pt %>% 
  rbind(gpt_en)

# gpt_na <- gpt_clean %>% 
#   filter(is.na(sentiment_gpt))

## Mixtral annotations -----------------
message("Getting mixtral annotations")
setwd("data/local")
files_mixtral <- fs::dir_ls(glob = "mixtral_sentiment_paho*csv")
mixtral <- vroom(files_mixtral) %>% 
  mutate(model = "mixtral")
setwd("../..")

mixtral_pt <- mixtral %>% 
  filter(prompt != 3) %>% 
  select(text, sentiment_mixtral, prompt, model) %>% 
  mutate(sentiment_mixtral = tolower(sentiment_mixtral),
         #sentiment_mixtral_raw = sentiment_mixtral,
         sentiment_mixtral = str_match(sentiment_mixtral, "\\b(neutro|negativo|positivo|neutra|negativa|positiva)\\b")[,2],
         sentiment_mixtral = case_when(is.na(sentiment_mixtral) ~ "neutro",
                                   .default = sentiment_mixtral),
         sentiment_mixtral = case_when(sentiment_mixtral == "positivo" | sentiment_mixtral == "positiva" ~ "positive",
                                       sentiment_mixtral == "negativo" | sentiment_mixtral == "negativa" ~ "negative",
                                       sentiment_mixtral == "neutro" | sentiment_mixtral == "neutra" ~ "neutral",
                                   .default = "neutral")) %>% 
  distinct(text, prompt, .keep_all = TRUE)

mixtral_en <- mixtral %>% 
  filter(prompt == 3) %>% 
  select(text, sentiment_mixtral, prompt, model) %>% 
  mutate(sentiment_mixtral = tolower(sentiment_mixtral),
         #sentiment_mixtral_raw = sentiment_mixtral,
         sentiment_mixtral = str_match(sentiment_mixtral, "\\b(neutral|negative|positive)\\b")[,2],
         sentiment_mixtral = case_when(is.na(sentiment_mixtral) ~ "neutral",
                                   .default = sentiment_mixtral)) %>% 
  distinct(text, prompt, .keep_all = TRUE)

mixtral_clean <- mixtral_pt %>% 
  rbind(mixtral_en)

# mixtral_na <- mixtral_clean %>%
#   filter(is.na(sentiment_mixtral))

## Join all datasets ------------
df_all <- paho_clean %>% 
  left_join(gpt_clean, by = "text") %>% 
  left_join(mixtral_clean, by = c("text", "prompt")) %>% 
  select(-model.x, -model.y) %>% 
  filter(!is.na(prompt))

# Comparison of GPT and Mixtral with PAHO annotations ------------
## Confusion matrices --------
prompts <- df_all$prompt %>% 
  unique()

### PAHO vs GPT --------------
for (i in prompts) {
  df_con_matrix <- df_all %>% 
    filter(prompt == i) %>% 
    select(sentiment_paho, sentiment_gpt) %>% 
    mutate(sentiment_paho = factor(sentiment_paho, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix',i,sep='_'), df_con_matrix)
  #prompt_loop[i] <- prompts[i]
  conf_paho_gpt <- confusionMatrix(df_con_matrix$sentiment_gpt,
                                   df_con_matrix$sentiment_paho, mode = "everything")
  conf_paho_gpt$prompt <- prompts[i]
  assign(paste('conf_paho_gpt_all',i,sep='_'),conf_paho_gpt) 
   
}

### PAHO vs Mixtral --------------
for (i in prompts) {
  df_con_matrix <- df_all %>% 
    filter(prompt == i) %>% 
    select(sentiment_paho, sentiment_mixtral) %>% 
    mutate(sentiment_paho = factor(sentiment_paho, ordered = TRUE,
                                   levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix',i,sep='_'), df_con_matrix)
  #prompt_loop[i] <- prompts[i]
  conf_paho_mixtral <- confusionMatrix(df_con_matrix$sentiment_mixtral,
                                   df_con_matrix$sentiment_paho, mode = "everything")
  conf_paho_mixtral$prompt <- prompts[i]
  assign(paste('conf_paho_mixtral_all',i,sep='_'),conf_paho_mixtral) 
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority <- df_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_epfl) %>%
  mutate(stance_majority = "neutral", 
         stance_epfl = factor(stance_epfl, ordered = TRUE,
                              levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority <- confusionMatrix(df_con_matrix_majority$stance_majority,
                                      df_con_matrix_majority$stance_epfl)

### Merging all confusion matrices -------
overall_all_fig <- as.data.frame(conf_paho_gpt_all_1$overall) %>% 
  cbind(conf_paho_gpt_all_2$overall) %>% 
  cbind(conf_paho_gpt_all_3$overall) %>% 
  cbind(conf_paho_mixtral_all_1$overall) %>% 
  cbind(conf_paho_mixtral_all_2$overall) %>% 
  cbind(conf_paho_mixtral_all_3$overall) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") 
