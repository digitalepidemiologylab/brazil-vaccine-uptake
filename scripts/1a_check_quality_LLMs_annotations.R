# Script information ------------
#' Aim: Compare annotations with GPT and mixtral
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
df <- read_csv("data/local/tweets_filtered_BR_PT.csv")

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
files_mixtral <- fs::dir_ls(glob = "mixtral_sentiment*csv")
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

## Llama3 70b annotations -----------------
message("Getting llama3 annotations")
setwd("data/local")
files_llama3 <- fs::dir_ls(glob = "llama3_70b_sentiment_paho*csv")
llama3 <- vroom(files_llama3) %>% 
  mutate(model = "llama3")
setwd("../..")

llama3_pt <- llama3 %>% 
  filter(prompt != 3) %>% 
  select(text, sentiment_llama, prompt, model) %>% 
  mutate(sentiment_llama = tolower(sentiment_llama),
         #sentiment_mixtral_raw = sentiment_mixtral,
         sentiment_llama = str_match(sentiment_llama, "\\b(neutro|negativo|positivo|neutra|negativa|positiva)\\b")[,2],
         sentiment_llama = case_when(is.na(sentiment_llama) ~ "neutro",
                                       .default = sentiment_llama),
         sentiment_llama = case_when(sentiment_llama == "positivo" | sentiment_llama == "positiva" ~ "positive",
                                     sentiment_llama == "negativo" | sentiment_llama == "negativa" ~ "negative",
                                     sentiment_llama == "neutro" | sentiment_llama == "neutra" ~ "neutral",
                                       .default = "neutral")) %>% 
  distinct(text, prompt, .keep_all = TRUE)

llama3_en <- llama3 %>% 
  filter(prompt == 3) %>% 
  select(text, sentiment_llama, prompt, model) %>% 
  mutate(sentiment_llama = tolower(sentiment_llama),
         #sentiment_mixtral_raw = sentiment_mixtral,
         sentiment_llama = str_match(sentiment_llama, "\\b(neutral|negative|positive)\\b")[,2],
         sentiment_llama = case_when(is.na(sentiment_llama) ~ "neutral",
                                       .default = sentiment_llama)) %>% 
  distinct(text, prompt, .keep_all = TRUE)

llama3_clean <- llama3_pt %>% 
  rbind(llama3_en)


## Join all datasets ------------
df_all <- paho_clean %>% 
  left_join(gpt_clean, by = "text") %>% 
  #left_join(mixtral_clean, by = c("text", "prompt")) %>% 
  left_join(llama3_clean, by = c("text", "prompt")) %>% 
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

### PAHO vs Llama3 --------------
for (i in prompts) {
  df_con_matrix <- df_all %>% 
    filter(prompt == i) %>% 
    select(sentiment_paho, sentiment_llama) %>% 
    mutate(sentiment_paho = factor(sentiment_paho, ordered = TRUE,
                                   levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix',i,sep='_'), df_con_matrix)
  #prompt_loop[i] <- prompts[i]
  conf_paho_llama <- confusionMatrix(df_con_matrix$sentiment_llama,
                                   df_con_matrix$sentiment_paho, mode = "everything")
  conf_paho_llama$prompt <- prompts[i]
  assign(paste('conf_paho_llama_all',i,sep='_'),conf_paho_llama) 
  
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

## PAHO vs selecting majority class ------------  
df_con_matrix_majority <- df_all %>% 
  select(sentiment_paho) %>%
  mutate(sentiment_majority = "neutral", 
         sentiment_paho = factor(sentiment_paho, ordered = TRUE,
                              levels = c("positive","neutral", "negative")),
         sentiment_majority = factor(sentiment_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 

conf_paho_majority <- confusionMatrix(df_con_matrix_majority$sentiment_majority,
                                      df_con_matrix_majority$sentiment_paho)

### Merging all confusion matrices -------
overall_all_fig <- as.data.frame(conf_paho_gpt_all_1$overall) %>% 
  cbind(conf_paho_gpt_all_2$overall) %>% 
  cbind(conf_paho_gpt_all_3$overall) %>% 
  cbind(conf_paho_mixtral_all_1$overall) %>%
  cbind(conf_paho_mixtral_all_2$overall) %>%
  cbind(conf_paho_mixtral_all_3$overall) %>%
  cbind(conf_paho_llama_all_1$overall) %>% 
  cbind(conf_paho_llama_all_2$overall) %>% 
  cbind(conf_paho_llama_all_2$overall) %>%
  cbind(conf_paho_llama_all_3$overall) %>%
  cbind(conf_paho_majority$overall) %>% 
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_paho_" = "", 
                                    "\\$overall" = "",
                                    "\\.overall" = "",
                                    "Majority.overall" = "Majority",
                                    "mixtral_all_2" = "Mixtral prompt 2",
                                    "mixtral_all_1" = "Mixtral prompt 1",
                                    "mixtral_all_3" = "Mixtral prompt 3",
                                    "llama_all_1" = "Llama3 70B prompt 1",
                                    "llama_all_2" = "Llama3 70B prompt 2",
                                    "llama_all_3" = "Llama3 70B prompt 3",
                                    "gpt_all_2" = "GPT 4 prompt 2",
                                    "gpt_all_1" = "GPT 4 prompt 1",
                                    "gpt_all_3" = "GPT 4 prompt 3",
                                    "majority" = "Majority"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         agreement = "Partial agreement",
         Prompt = replace_na(Prompt, "None")) %>% 
  filter(Prompt != "2.1")

overall_all <- overall_all_fig %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue") %>% 
  filter(Prompt != "2.1")

overall_all %>%
  write_csv("outputs/confusion_matrix_accuracy.csv")

### Plot accuracy ----------------
accuracy_fig <- overall_all_fig %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(
    #aes(color = Pvalue)
    ) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper
                    #,color = Pvalue
                    ),
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig
