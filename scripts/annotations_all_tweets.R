# Script information ------------
#' Aim: Analyse annotations of all tweets
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
p_load(tidyverse, vroom, caret, purrr, tidyr)

# Import data ---------
## Data annotated by GPT 4 -----------
message("Getting GPT 4 annotations for all dataset")
setwd("data/local")
files_gpt4 <- fs::dir_ls(glob = "gpt_sentiment_prompt2_all_tweets*csv")
gpt4 <- vroom(files_gpt4)
setwd("../..")

### Getting missing tweets for GPT 4 turbo ---------
df_all <- gpt4 %>% 
  mutate(annotated = 1) %>% 
  distinct(text, .keep_all = TRUE) %>% 
  select(text, annotated) %>% 
  right_join(df) %>% 
  filter(is.na(annotated)) %>% 
  distinct(text, id, .keep_all = TRUE)

#write_csv(df_all, "data/local/tweets_filtered_BR_PT_gpt4turbo.csv")

## Data annotated by GPT 4 turbo -------
message("Getting GPT 4 turbo annotations for all dataset")
setwd("data/local")
files_gpt4t <- fs::dir_ls(glob = "gpt_sentiment_prompt2b_all_tweets*csv")
gpt4t <- vroom(files_gpt4t)
setwd("../..")

gpt4t_clean <- gpt4t %>% 
  mutate(sentiment_gpt = tolower(sentiment_gpt)) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  #distinct(text_grouped, .keep_all = TRUE) %>% 
  mutate(sentiment_gpt_grouped = str_replace_all(sentiment_gpt_grouped, "``` ", ""),
         words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
                                str_detect(sentiment_gpt_grouped, pattern = "- neutro|- positivo|- negativo|- neutra|- positiva|- negativa") ~ "hyphen",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*neutro|\\*positivo|\\*negativo|\\*neutra|\\*positiva|\\*negativa") ~ "one_star_start",
                                str_detect(sentiment_gpt_grouped, pattern = ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa") ~ "hyphen",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\ neutro|\\*\\ positivo|\\*\\ negativo|\\*\\ neutra|\\*\\ positiva|\\*\\ negativa") ~ "one_star_space",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*\nneutro|\\*\npositivo|\\*\nnegativo|\\*\nneutra|\\*\npositiva|\\*\nnegativa") ~ "one_star_new_line",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\(neutro|\\*\\(positivo|\\*\\(negativo|\\*\\(neutra|\\*\\(positiva|\\*\\(negativa") ~ "one_star_bracket",
                                str_detect(sentiment_gpt_grouped, pattern = "> neutro|> positivo|> negativo|> neutra|> positiva|> negativa") ~ "more_than",
                                str_detect(sentiment_gpt_grouped, pattern = "= neutro|= positivo|= negativo|= neutra|= positiva|= negativa") ~ "equal",
                                str_detect(sentiment_gpt_grouped, pattern = ". neutro|. positivo|. negativo|. neutra|. positiva|. negativa") ~ "full_stop",
                                str_detect(sentiment_gpt_grouped, pattern = "neutro:|positivo:|negativo:|neutra:|positiva:|negativa:") ~ "colon",
                                str_detect(sentiment_gpt_grouped, pattern = "`neutro|`positivo|`negativo|`neutra|`positiva|`negativa") ~ "quotation_single",
                                str_detect(sentiment_gpt_grouped, pattern = "- neutro|- positivo|- negativo|- neutra|- positiva|- negativa") ~ "hyphen",
                                str_detect(sentiment_gpt_grouped, pattern = "neutro \\*|positivo \\*|negativo \\*|neutra \\*|positiva \\*|negativa \\*") ~ "one_star_end",
                                str_detect(sentiment_gpt_grouped, pattern = "neutro \\*\n|positivo \\*\n|negativo \\*\n|neutra \\*\n|positiva \\*\n|negativa \\*\n") ~ "one_star_end_new_line",
                                str_detect(sentiment_gpt_grouped, pattern = "\nneutro|\npositivo|\nnegativo|\nneutra|\npositiva|\nnegativa") ~ "new_line",
                                .default = "other"))

### Create function to extract sentiment ---------
extract_words <- function(data, words) {
  pattern <- paste(words, collapse = "|")
  pattern <- str_c('(?i)', pattern)
  
  data %>% 
    mutate(sentiment_gpt_standardised = str_extract_all(sentiment_gpt_grouped, pattern) %>%
             sapply(function(x) paste(x[x != ""], collapse = "***"))) %>% 
    replace_na(list(sentiment_gpt_standardised = ""))
  
}

### Function to expand rows according to the individual tweets and annotations ---
expand_rows_based_on_columns <- function(data, col1, col2) {
  # Split the columns into lists
  data_split <- data %>%
    mutate(
      split_col1 = str_split(!!sym(col1), "\\*\\*\\*"),
      split_col2 = str_split(!!sym(col2), "\\*\\*\\*")
    )
  
  # Ensure both lists in each row are of the same length
  data_padded <- data_split %>%
    mutate(
      split_col1 = map2(split_col1, split_col2, ~ {
        len <- max(length(.x), length(.y))
        c(.x, rep(NA, len - length(.x)))
      }),
      split_col2 = map2(split_col2, split_col1, ~ {
        len <- max(length(.x), length(.y))
        c(.x, rep(NA, len - length(.x)))
      })
    )
  
  # Unnest the lists to create new rows
  expanded_data <- data_padded %>%
    unnest(c(split_col1, split_col2))
  
  expanded_data
}

### Function to pad the shorter strings with '***na***'
pad_na_to_match <- function(data, cols_to_pad) {
  max_separators <- function(row, cols) {
    sapply(row[cols], function(x) str_count(x, "\\*\\*\\*")) %>%
      max(na.rm = TRUE)
  }
  
  data %>%
    rowwise() %>%
    mutate(max_sep = max_separators(c_across(all_of(cols_to_pad)), cols_to_pad)) %>%
    mutate(across(all_of(cols_to_pad), 
                  ~ if_else(str_count(.x, "\\*\\*\\*") == 0, .x, 
                            str_pad(.x, str_length(.x) + 3 * (max_sep - str_count(.x, "\\*\\*\\*")), side = "right", pad = "***na")))) %>%
    ungroup()
}


### Clean dataset -------
words_to_extract <- c("positivo", "positiva", "negativo",
                      "negativa", "neutro", "neutra")
words_to_extract1 <- c("positivo", "positiva", "negativo", "negativa", "neutro", "neutra",
                       "\\*\\*positivo\\*\\*", "\\*\\*positiva\\*\\*", "\\*\\*negativo\\*\\*",
                       "\\*\\*negativa\\*\\*", "\\*\\*neutro\\*\\*", "\\*\\*neutra\\*\\*",
                       "\\- positivo", "\\- positiva", "\\- negativo", "\\- negativa", "\\- neutro", "\\- neutra",
                       "\\— positivo", "\\— positiva", "\\— negativo", "\\— negativa", "\\— neutro", "\\— neutra",
                       "\\: positivo", "\\: positiva", "\\: negativo", "\\: negativa", "\\: neutro", "\\: neutra",
                       "\\. positivo", "\\. positiva", "\\. negativo", "\\. negativa", "\\. neutro", "\\. neutra",
                       "\\> positivo", "\\> positiva", "\\> negativo", "\\> negativa", "\\> neutro", "\\> neutra",
                       "positivo:", "positiva:", "negativo:", "negativa:", "neutro:", "neutra:")
words_to_extract2 <- c("\\*\\*positivo\\*\\*", "\\*\\*positiva\\*\\*", "\\*\\*negativo\\*\\*",
                       "\\*\\*negativa\\*\\*", "\\*\\*neutro\\*\\*", "\\*\\*neutra\\*\\*")
words_to_extract3 <- c("\\- positivo", "\\- positiva", "\\- negativo",
                       "\\- negativa", "\\- neutro", "\\- neutra",
                       "\\— positivo", "\\— positiva", "\\— negativo",
                       "\\— negativa", "\\— neutro", "\\— neutra")
words_to_extract4 <- c("\\: positivo", "\\: positiva", "\\: negativo", "\\: negativa", "\\: neutro", "\\: neutra")
words_to_extract5 <- c("\\. positivo", "\\. positiva", "\\. negativo", "\\. negativa", "\\. neutro", "\\. neutra")
words_to_extract6 <- c("\\> positivo", "\\> positiva", "\\> negativo", "\\> negativa", "\\> neutro", "\\> neutra")
words_to_extract7 <- c("positivo:", "positiva:", "negativo:", "negativa:", "neutro:", "neutra:")

gpt4t_clean_split <- gpt4t_clean %>%
  extract_words(words_to_extract1) %>%
  #select(-sentiment_gpt_grouped) %>% 
  mutate(text = text_grouped,
         n_text = str_count(text, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent)

gpt4t_clean_split_a <- gpt4t_clean_split %>% 
  filter(n_diff >= 0 & sentiment_gpt_standardised != "")

gpt4t_clean_split_b <- gpt4t_clean_split %>% 
  filter(n_diff <0 | sentiment_gpt_standardised == "") %>% 
  select(-sentiment_gpt_standardised, -n_sent, -n_diff) %>% 
  extract_words(words_to_extract1) %>% 
  mutate(sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "-", ""),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      ":", ""),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      ". ", ""),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      " ", ""),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*", "\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "^\\*+|\\*+$", ""),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_sent = ifelse(sentiment_gpt_standardised == "", 0, n_sent),
         n_diff = n_text - n_sent)

gpt4t_clean_split_b_1 <- gpt4t_clean_split_b %>% 
  filter(n_diff >= 0 & sentiment_gpt_standardised != "")

gpt4t_clean_split_b_2a <- gpt4t_clean_split_b %>% 
  filter(n_diff < 0 | sentiment_gpt_standardised == "") %>% 
  select(-sentiment_gpt_standardised, -n_sent, -n_diff) %>% 
  extract_words(words_to_extract2) %>% 
  mutate(sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*", "\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "^\\*+|\\*+$", ""),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_sent = ifelse(sentiment_gpt_standardised == "", 0, n_sent),
         n_diff = n_text - n_sent)

gpt4t_clean_split_b_2b <- gpt4t_clean_split_b_2a %>% 
  filter(sentiment_gpt_standardised == "") %>% 
  select(-sentiment_gpt_standardised, -n_sent, -n_diff) %>% 
  extract_words(words_to_extract3) %>% 
  mutate(sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*", "\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "^\\*+|\\*+$", ""),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_sent = ifelse(sentiment_gpt_standardised == "", 0, n_sent),
         n_diff = n_text - n_sent)

gpt4t_clean_split_b_2c <- gpt4t_clean_split_b_2a %>% 
  filter(sentiment_gpt_standardised == "") %>% 
  select(-sentiment_gpt_standardised, -n_sent, -n_diff) %>% 
  extract_words(words_to_extract4) %>% 
  mutate(sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*\\*", "\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "\\*\\*", "\\*\\*\\*"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "^\\*+|\\*+$", ""),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_sent = ifelse(sentiment_gpt_standardised == "", 0, n_sent),
         n_diff = n_text - n_sent)

gpt4t_clean_split_b_2_1 <- gpt4t_clean_split_b_2 %>% 
  filter(sentiment_gpt_standardised != "")

gpt4t_clean_split_b_2_2 <- gpt4t_clean_split_b_2 %>% 
  filter(sentiment_gpt_standardised == "") %>% 
  select(-sentiment_gpt_standardised, -n_sent, -n_diff) %>% 
  extract_words(words_to_extract3) %>% 
  mutate(sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      "- ", ""),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_sent = ifelse(sentiment_gpt_standardised == "", 0, n_sent),
         n_diff = n_text - n_sent)

gpt4t_clean_split_b_2_2_1 <- gpt4t_clean_split_b_2_2 %>% 
  filter(sentiment_gpt_standardised != "")

gpt4t_clean_split_b_2_2_2 <- gpt4t_clean_split_b_2_2 %>% 
  filter(sentiment_gpt_standardised == "") %>% 
  select(-sentiment_gpt_standardised, -n_sent, -n_diff) %>% 
  extract_words(words_to_extract4) %>% 
  mutate(sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      ": ", ""),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                      ". ", ""),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_sent = ifelse(sentiment_gpt_standardised == "", 0, n_sent),
         n_diff = n_text - n_sent) %>% 
  arrange(n_sent)

######## OLD

separate_rows(text, sep = "\\*\\*\\*") %>% 
  select(text_grouped, text, sentiment_gpt_standardised, sentiment_gpt, n_text, n_sent) %>% 
  separate_rows(sentiment_gpt, sep = "\\*\\*\\*") %>% 
  distinct(text, sentiment_gpt, .keep_all = TRUE)


mismatched_rows <- gpt4t_clean_split %>% 
  filter(n_text != n_sent) %>% 
  pad_na_to_match(., c("text", "sentiment_gpt"))

df_gpt4t_matched <- gpt4t_clean_split %>% 
  filter(n_text == n_sent) %>% 
  separate_rows(text, sentiment_gpt, sep = "\\*\\*\\*")

  expand_rows_based_on_columns("text_grouped", "sentiment_gpt_standardised") %>% 
  rename(text = split_col1,
         sentiment_gpt = split_col2)

gpt4t_na_text <- gpt4t_clean %>% 
  filter(is.na(text)) %>% 
  distinct(text_grouped, sentiment_gpt_standardised, .keep_all = TRUE) %>% 
  mutate(text = text_grouped) %>% 
  separate_rows(text, sep = "\\*\\*\\*")
  #expand_rows_based_on_columns("text_grouped", "sentiment_gpt_standardised")


