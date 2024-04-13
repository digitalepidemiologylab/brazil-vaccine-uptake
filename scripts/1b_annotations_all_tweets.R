# Script information ------------
#' Aim: Analyse annotations of all tweets
#' Author: Laura Espinosa
#' Date created: 26 July 2021
#' Date updated: 05 February 2023

# Fixed PT characters ------------
fix_portuguese_encoding <- function(text) {
  replacements <- c(
    "Ã�" = "Á", "Ã¡" = "á",
    "Ã‰" = "É", "Ã©" = "é",
    "Ã�" = "Í", "Ã­" = "í",
    "Ã“" = "Ó", "Ã³" = "ó",
    "Ãš" = "Ú", "Ãº" = "ú",
    "Ã‡" = "Ç", "Ã§" = "ç",
    "Ãƒ" = "Ã", "Ã£" = "ã",
    "Ã•" = "Õ", "Ãµ" = "õ",
    "Ã‚" = "Â", "Ã¢" = "â",
    "ÃŠ" = "Ê", "Ãª" = "ê",
    "Ã”" = "Ô", "Ã´" = "ô"
  )
  
  str_replace_all(text, replacements)
}


# Import data ---------
# ## Data annotated by GPT 4 -----------
message("Getting GPT 4 annotations for all dataset")
setwd("data/local")
files_gpt4 <- fs::dir_ls(glob = "gpt_sentiment_prompt2_all_tweets*csv")
gpt4 <- vroom(files_gpt4)
setwd("../..")

gpt4_clean <- gpt4 %>% 
  mutate(text = fix_portuguese_encoding(text),
         gpt_sent_presence = 1) %>% 
  rename(sent_gpt = sentiment_gpt) %>% 
  select(text, sent_gpt, gpt_sent_presence) %>% 
  mutate(sent_gpt = tolower(sent_gpt), 
         sent_gpt = str_match(sent_gpt,
                                     "\\b(neutro|positivo|negativo|neutra|positiva|negativa)\\b")[,2])

rm(gpt4)


# ### Getting missing tweets for GPT 4 turbo ---------
# df_all <- gpt4 %>% 
#   mutate(annotated = 1) %>% 
#   distinct(text, .keep_all = TRUE) %>% 
#   select(text, annotated) %>% 
#   right_join(df) %>% 
#   filter(is.na(annotated)) %>% 
#   distinct(text, id, .keep_all = TRUE)
# 
# #write_csv(df_all, "data/local/tweets_filtered_BR_PT_gpt4turbo.csv")

## Data annotated by GPT 4 turbo -------
message("Getting GPT 4 turbo annotations for all dataset")
setwd("data/local")
files_gpt4t <- fs::dir_ls(glob = "gpt_sentiment_prompt2b_all_tweets*csv")
gpt4t <- vroom(files_gpt4t)
setwd("../..")

gpt4t_clean <- gpt4t %>% 
  mutate(sentiment_gpt = tolower(sentiment_gpt),
         text = fix_portuguese_encoding(text)) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  #distinct(text_grouped, .keep_all = TRUE) %>% 
  mutate(text_grouped = str_replace_all(text_grouped, '\\*\\*\\*\\*\\*\\*\\*\\*', '\\*\\*\\*'),
         text_grouped = str_replace_all(text_grouped, '\\*\\*\\*\\*\\*\\*', '\\*\\*\\*'),
         sentiment_gpt_grouped = str_replace_all(sentiment_gpt_grouped, "``` ", ""),
         words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*neutro\\*|\\*positivo\\*|\\*negativo\\*|\\*neutra\\*|\\*positiva\\*|\\*negativa\\*") ~ "double_star",
                                str_detect(sentiment_gpt_grouped, pattern = "- neutro|- positivo|- negativo|- neutra|- positiva|- negativa") ~ "hyphen",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*neutro|\\*positivo|\\*negativo|\\*neutra|\\*positiva|\\*negativa|\\*\\ neutro|\\*\\ positivo|\\*\\ negativo|\\*\\ neutra|\\*\\ positiva|\\*\\ negativa|neutro \\*|positivo \\*|negativo \\*|neutra \\*|positiva \\*|negativa \\*") ~ "one_star",
                                str_detect(sentiment_gpt_grouped, pattern = ". neutro|. positivo|. negativo|. neutra|. positiva|. negativa") ~ "full_stop",
                                str_detect(sentiment_gpt_grouped, pattern = ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa") ~ "colon_start",
                                #str_detect(sentiment_gpt_grouped, pattern = "\\*\\ neutro|\\*\\ positivo|\\*\\ negativo|\\*\\ neutra|\\*\\ positiva|\\*\\ negativa") ~ "one_star_space",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*\nneutro|\\*\npositivo|\\*\nnegativo|\\*\nneutra|\\*\npositiva|\\*\nnegativa") ~ "one_star_new_line",
                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\(neutro|\\*\\(positivo|\\*\\(negativo|\\*\\(neutra|\\*\\(positiva|\\*\\(negativa") ~ "one_star_bracket",
                                str_detect(sentiment_gpt_grouped, pattern = "> neutro|> positivo|> negativo|> neutra|> positiva|> negativa") ~ "more_than",
                                str_detect(sentiment_gpt_grouped, pattern = "= neutro|= positivo|= negativo|= neutra|= positiva|= negativa") ~ "equal",
                                str_detect(sentiment_gpt_grouped, pattern = "neutro:|positivo:|negativo:|neutra:|positiva:|negativa:") ~ "colon_end",
                                str_detect(sentiment_gpt_grouped, pattern = "`neutro|`positivo|`negativo|`neutra|`positiva|`negativa") ~ "quotation_single",
                                str_detect(sentiment_gpt_grouped, pattern = "- neutro|- positivo|- negativo|- neutra|- positiva|- negativa") ~ "hyphen",
                                #str_detect(sentiment_gpt_grouped, pattern = "neutro \\*|positivo \\*|negativo \\*|neutra \\*|positiva \\*|negativa \\*") ~ "one_star_end",
                                #str_detect(sentiment_gpt_grouped, pattern = "neutro \\*\n|positivo \\*\n|negativo \\*\n|neutra \\*\n|positiva \\*\n|negativa \\*\n") ~ "one_star_end_new_line",
                                str_detect(sentiment_gpt_grouped, pattern = "\nneutro|\npositivo|\nnegativo|\nneutra|\npositiva|\nnegativa") ~ "new_line",
                                .default = "other"),
         sentiment_gpt_grouped = paste("***", sentiment_gpt_grouped, "***", sep = ""),
         sentiment_gpt_standardised = case_when(words_type == "single_word" ~ sentiment_gpt_grouped,
                                                words_type == "double_star" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                  "\\*neutro\\*|\\*positivo\\*|\\*negativo\\*|\\*neutra\\*|\\*positiva\\*|\\*negativa\\*"), sep = "***"),
                                                words_type == "colon_start" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa"), sep = "***"),
                                                words_type == "hyphen" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                       "- neutro|- positivo|- negativo|- neutra|- positiva|- negativa"), sep = "***"),
                                                words_type == "quotation_single" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                          "`neutro|`positivo|`negativo|`neutra|`positiva|`negativa"), sep = "***"),
                                                words_type == "one_star" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                "\\*neutro|\\*positivo|\\*negativo|\\*neutra|\\*positiva|\\*negativa|\\*\\ neutro|\\*\\ positivo|\\*\\ negativo|\\*\\ neutra|\\*\\ positiva|\\*\\ negativa|neutro \\*|positivo \\*|negativo \\*|neutra \\*|positiva \\*|negativa \\*"), sep = "***"),
                                                # words_type == "one_star_space" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                #                                                 "\\*\\ neutro|\\*\\ positivo|\\*\\ negativo|\\*\\ neutra|\\*\\ positiva|\\*\\ negativa"), sep = "***"),
                                                words_type == "full_stop" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                ". neutro|. positivo|. negativo|. neutra|. positiva|. negativa"), sep = "***"),
                                                words_type == "one_star_new_line" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                 "\\*\nneutro|\\*\npositivo|\\*\nnegativo|\\*\nneutra|\\*\npositiva|\\*\nnegativa"), sep = "***"),
                                                words_type == "colon_end" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                "neutro:|positivo:|negativo:|neutra:|positiva:|negativa:"), sep = "***"),
                                                words_type == "more_than" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                "> neutro|> positivo|> negativo|> neutra|> positiva|> negativa"), sep = "***"),
                                                words_type == "new_line" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                "\nneutro|\npositivo|\nnegativo|\nneutra|\npositiva|\nnegativa"), sep = "***"),
                                                words_type == "equal" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                  "= neutro|= positivo|= negativo|= neutra|= positiva|= negativa"), sep = "***"),
                                                words_type == "one_star_bracket" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                                                                  "\\*\\(neutro|\\*\\(positivo|\\*\\(negativo|\\*\\(neutra|\\*\\(positiva|\\*\\(negativa"), sep = "***"),
                                                # words_type == "one_star_end" ~ paste0(str_extract_all(sentiment_gpt_grouped,
                                                #                                                   "neutro \\*|positivo \\*|negativo \\*|neutra \\*|positiva \\*|negativa \\*"), sep = "***"),
                                                words_type == "other" ~ "check_step_words_type",
                                                .default = "no_words_type_class"),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                       "c|\\(|\\)|\\*| |-|:|\n|\\`|=|\\.", ""),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                       '"', ''),
         sentiment_gpt_standardised = str_replace_all(sentiment_gpt_standardised,
                                                       ',', '***'),
         sentiment_gpt_standardised = case_when(words_type == "single_word" ~ paste(sentiment_gpt_standardised, sentiment_gpt_standardised,
                                                                                                    sentiment_gpt_standardised, sentiment_gpt_standardised,
                                                                                                    sentiment_gpt_standardised, sentiment_gpt_standardised,
                                                                                                    sentiment_gpt_standardised, sentiment_gpt_standardised,
                                                                                                    sentiment_gpt_standardised, sentiment_gpt_standardised, sep = "***"),
                                            .default = sentiment_gpt_standardised),
         n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent
         
  )

rm(gpt4t)

gpt4t_clean_text <- gpt4t_clean %>% 
  select(text_grouped) %>% 
  separate_rows(text_grouped, sep = "\\*\\*\\*") %>% 
  rename(text = text_grouped)

gpt4t_clean_nodiff <- gpt4t_clean %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>%
  mutate(text = fix_portuguese_encoding(text),
         gpt_sent_presence = 1) %>% 
  distinct(text, .keep_all = TRUE)

annotations_clean <- gpt4t_clean_nodiff %>% 
  rbind(gpt4_clean) %>% 
  distinct(text, .keep_all = TRUE) %>% 
  mutate(sent_gpt = case_when(is.na(sent_gpt) ~ "neutral",
                              .default = sent_gpt),
         sent_gpt = case_when(str_detect(sent_gpt, pattern = "neutr") ~ "neutral",
                               str_detect(sent_gpt, pattern = "posit") ~ "positive",
                               str_detect(sent_gpt, pattern = "negat") ~ "negative",
                              .default = "neutral"))

rm(gpt4t_clean)



## Import cleaned Twitter data ----------------
message("Importing Twitter data")
df_raw <- read_csv('data/local/tweets_filtered_BR_PT.csv')         
gc()

df_clean <- df_raw %>% 
  #select(text) %>% 
  distinct(.keep_all = TRUE) %>% 
  left_join(annotations_clean, by = "text") %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(gpt_sent_presence == 1)

write_csv(df_clean, "data/local/tweets_BR_PT_gpt.csv")

