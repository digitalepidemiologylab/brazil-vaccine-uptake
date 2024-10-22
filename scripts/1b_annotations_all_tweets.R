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
    "Ã�" = "Í", 
    "Ã­" = "í",
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


replacements <- c("Ã¢" = "â", "Ã£" = "ã", "Ã©" = "é", "Ãª" = "ê",
                  "Ã§" = "ç", "Ã³" = "ó", "Ã´" = "ô", "Ã" = "í")

# Import data ---------
# ## Data annotated by GPT 4 -----------
message("Getting GPT 4 annotations for all dataset")
setwd("data/local")
files_gpt4 <- fs::dir_ls(glob = "gpt_sentiment_prompt2_all_tweets*csv")
gpt4 <- vroom(files_gpt4)
setwd("../..")

gpt4_clean <- gpt4 %>% 
  mutate(
    #text = fix_portuguese_encoding(text),
         gpt_sent_presence = 1,
         text = str_replace_all(text, replacements)) %>% 
  rename(sent_gpt = sentiment_gpt) %>% 
  select(text, sent_gpt, gpt_sent_presence) %>% 
  mutate(sent_gpt = tolower(sent_gpt), 
         sent_gpt = str_match(sent_gpt,
                                     "\\b(neutro|positivo|negativo|neutra|positiva|negativa)\\b")[,2],
         
         # tweets where gpt4 "could not provide a sentiment" will be classified as neutral
         sent_gpt = case_when(is.na(sent_gpt) ~ "neutro",
                              .default = sent_gpt)) 


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
         text = str_replace_all(text, replacements)) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  #distinct(text_grouped, .keep_all = TRUE) %>% 
  mutate(text_grouped = str_replace_all(text_grouped, '\\*\\*\\*\\*\\*\\*\\*\\*', '\\*\\*\\*'),
         text_grouped = str_replace_all(text_grouped, '\\*\\*\\*\\*\\*\\*', '\\*\\*\\*'),
         text_grouped = str_replace_all(text_grouped, '\\*\\*\\*\\*\\*', '\\*\\*\\*'),
         text_grouped = str_replace_all(text_grouped, '\\*\\*\\*\\*', '\\*\\*\\*'),
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
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa|:neutro|:positivo|:negativo|:neutra|:positiva|:negativa"), sep = "***"),
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
         sentiment_gpt_standardised = case_when(str_detect(sentiment_gpt_grouped, pattern ="para os tweets seguintes")  ~ "positivo***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern ="para todos os outros tweets")  ~ "positivo***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern ="para cada tweet mencionado no texto")  ~ "neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern ="os tweets fornecidos não expressam")  ~ "neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern ="para os tweets subsequentes")  ~ "negativo***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*negativo neutro\\*\\*\\*") ~ "negativo***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro positivo positivo\\*\\*\\*") ~ "neutro***positivo***positivo",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro negativo neutro\\*\\*\\*") ~ "neutro***negativo***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*negativo \\*\\*\\* neutro \\*\\*\\* negativo\\*\\*\\*") ~ "negativo***neutro***negativo",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*positivo negativo positivo\\*\\*\\*") ~ "positivo***negativo***positivo",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro positivo negativo\\*\\*\\*") ~ "neutro***positivo***negativo",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro \\*\\*\\* negativo \\*\\*\\* positivo\\*\\*\\*") ~ "neutro***negativo***positivo",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro negativo neutro\\*\\*\\*") ~ "neutro***negativo***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro negativo neutro\\*\\*\\*") ~ "neutro***negativo***negativo",
                                                str_detect(sentiment_gpt_grouped, pattern = "para cada um dos tweets mencionados, a atitude do a") ~ "positivo***positivo***positivo***positivo***positivo***positivo***positivo***positivo***positivo***positivo",
                                                str_detect(sentiment_gpt_grouped, pattern = "para cada um dos tweets mencionados no texto, a atitud") ~ "neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern = "os tweets mencionados contêm um tom predominantemte") ~ "negativo***negativo***negativo***negativo***negativo***negativo***negativo***negativo***negativo***negativo",
                                                str_detect(sentiment_gpt_grouped, pattern = "os tweets fornecidos contêm conteúdo que parece ser sa") ~ "neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro***neutro",
                                                str_detect(sentiment_gpt_grouped, pattern = "\\*\\*\\*neutro positivo neutro\\*\\*\\*") ~ "neutro***positivo***neutro",
                                                .default = sentiment_gpt_standardised),
         n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent
         
  )

rm(gpt4t)

# gpt4t_clean_text <- gpt4t_clean %>% 
#   select(text_grouped) %>% 
#   separate_rows(text_grouped, sep = "\\*\\*\\*") %>% 
#   rename(text = text_grouped)

gpt4t_clean_nodiff <- gpt4t_clean %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>%
  mutate(text = fix_portuguese_encoding(text),
         gpt_sent_presence = 1) %>% 
  distinct(text, .keep_all = TRUE)

# gpt4t_clean_nodiff %>% 
#   write.csv("data/local/gpt4turbo_annotations_no_diff.csv")

gpt4t_diff <- gpt4t_clean %>% 
  filter(n_diff != 0)

# write.csv(gpt4t_diff, "data/local/gpt4turbo_annotations_to_clean_manually.csv")
# write.csv(gpt4t_diff2, "data/local/gpt4turbo_annotations_to_clean_manually2.csv")

gpt4t_diff2 <- gpt4t_diff %>% 
  mutate(id = 1:nrow(gpt4t_diff))

gpt4t_cleaned <- read_csv("data/local/gpt4turbo_annotations_cleaned_manually.csv") %>% 
  filter(revised == 1) %>%
  select(sentiment_gpt_standardised, id, revised) %>% 
  left_join(gpt4t_diff2, by = "id") %>% 
  select(-sentiment_gpt_standardised.y, -id) %>% 
  rename(sentiment_gpt_standardised = sentiment_gpt_standardised.x) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent,
         sentiment_gpt_standardised = case_when(n_sent == 1 ~ paste0(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 2 ~ paste0(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 3 ~ paste0(sentiment_gpt_standardised, "***none***none***none***none***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 4 ~ paste0(sentiment_gpt_standardised, "***none***none***none***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 5 ~ paste0(sentiment_gpt_standardised, "***none***none***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 6 ~ paste0(sentiment_gpt_standardised, "***none***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 7 ~ paste0(sentiment_gpt_standardised, "***none***none***none",
                                                                      sep = ""),
                                                 n_sent == 8 ~ paste0(sentiment_gpt_standardised, "***none***none",
                                                                      sep = ""),
                                                 n_sent == 9 ~ paste0(sentiment_gpt_standardised, "***none",
                                                                      sep = "")),
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) 

gpt4t_cleaned_nodiff <- gpt4t_cleaned %>% 
  filter(revised == 1) %>% 
  #filter(n_diff == 0)
  mutate(sentiment_gpt_standardised = case_when(n_diff == 1 ~ paste(sentiment_gpt_standardised, "***none", sep = ""),
                                                n_diff == 2 ~ paste(sentiment_gpt_standardised, "***none***none", sep = ""),
                                                n_diff == 3 ~ paste(sentiment_gpt_standardised, "***none***none***none", sep = ""),
                                                n_diff == 4 ~ paste(sentiment_gpt_standardised, "***none***none***none***none", sep = ""),
                                                n_diff == 5 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none", sep = ""),
                                                n_diff == 6 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none", sep = ""),
                                                n_diff == 7 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none", sep = ""),
                                                n_diff == 8 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none", sep = ""),
                                                .default = sentiment_gpt_standardised)) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) %>% 
  filter(sent_gpt != "none")

#-------------
setwd("data/local")
files_gpt4t_missing <- fs::dir_ls(glob = "gpt_sentiment_missing_prompt2b*csv")
gpt4t_missing <- vroom(files_gpt4t_missing)
setwd("../..")

gpt4t_missing <- gpt4t_missing %>% 
  select(text, sentiment_gpt) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  mutate(words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
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
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa|:neutro|:positivo|:negativo|:neutra|:positiva|:negativa"), sep = "***"),
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
         n_diff = n_text - n_sent) %>% 
  rename()

gpt4t_missing_nodiff <- gpt4t_missing %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1)

gpt4t_missing_diff <- gpt4t_missing %>% 
  filter(n_diff != 0) %>% 
  mutate(id = 1:nrow(.))

# write_csv(gpt4t_missing_diff, "data/local/gpt4turbo_annotations_missing_to_clean.csv")

gpt4t_missing_cleaned <- read_csv("data/local/gpt4turbo_annotations_missing_cleaned.csv") %>% 
  select(id, sentiment_gpt_standardised, revised) %>% 
  right_join(gpt4t_missing_diff %>% select(text_grouped, id), by = "id") %>% 
  select(-id) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent)

rm(gpt4t_missing_diff)

gpt4t_missing_cleaned_nodiff <- gpt4t_missing_cleaned %>% 
  filter(revised == 1) %>% 
  #filter(n_diff == 0)
  mutate(sentiment_gpt_standardised = case_when(n_diff == 1 ~ paste(sentiment_gpt_standardised, "***none", sep = ""),
                   n_diff == 2 ~ paste(sentiment_gpt_standardised, "***none***none", sep = ""),
                   n_diff == 3 ~ paste(sentiment_gpt_standardised, "***none***none***none", sep = ""),
                   n_diff == 4 ~ paste(sentiment_gpt_standardised, "***none***none***none***none", sep = ""),
                   n_diff == 5 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none", sep = ""),
                   n_diff == 6 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none", sep = ""),
                   n_diff == 7 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none", sep = ""),
                   n_diff == 8 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none", sep = ""),
                   .default = sentiment_gpt_standardised)) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) %>% 
  filter(sent_gpt != "none")

#---  --- --- --- --- 
gpt4t_missing2 <- read_csv("data/local/gpt_sentiment___missing_prompt2b_64444_end.csv") %>% 
  select(text, sentiment_gpt) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  mutate(words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
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
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa|:neutro|:positivo|:negativo|:neutra|:positiva|:negativa"), sep = "***"),
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
         n_diff = n_text - n_sent) 

gpt4t_missing2_nodiff <- gpt4t_missing2 %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) 

gpt4t_missing2_diff <- gpt4t_missing2 %>% 
  filter(n_diff != 0) %>% 
  mutate(id = 1:nrow(.))

# write_csv(gpt4t_missing2_diff, "data/local/gpt4turbo_annotations_missing2_to_clean.csv")

gpt4t_missing2_cleaned <- read_csv("data/local/gpt4turbo_annotations_missing2_cleaned.csv") %>% 
  select(id, sentiment_gpt_standardised, revised) %>% 
  right_join(gpt4t_missing2_diff %>% select(text_grouped, id), by = "id") %>% 
  select(-id) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent)

rm(gpt4t_missing2_diff)

gpt4t_missing2_cleaned_nodiff <- gpt4t_missing2_cleaned %>% 
  filter(revised == 1) %>% 
  #filter(n_diff == 0)
  mutate(sentiment_gpt_standardised = case_when(n_diff == 1 ~ paste(sentiment_gpt_standardised, "***none", sep = ""),
                                                n_diff == 2 ~ paste(sentiment_gpt_standardised, "***none***none", sep = ""),
                                                n_diff == 3 ~ paste(sentiment_gpt_standardised, "***none***none***none", sep = ""),
                                                n_diff == 4 ~ paste(sentiment_gpt_standardised, "***none***none***none***none", sep = ""),
                                                n_diff == 5 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none", sep = ""),
                                                n_diff == 6 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none", sep = ""),
                                                n_diff == 7 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none", sep = ""),
                                                n_diff == 10 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none***none***none", sep = ""),
                                                .default = sentiment_gpt_standardised)) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) %>% 
  filter(sent_gpt != "none")

# --- --- --- --- 
gpt4t_2round <- read_csv("data/local/gpt_sentiment_missing_2round_prompt2b.csv") %>% 
  select(text, sentiment_gpt) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  mutate(words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
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
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa|:neutro|:positivo|:negativo|:neutra|:positiva|:negativa"), sep = "***"),
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
         n_diff = n_text - n_sent)

gpt4t_2round_nodiff <- gpt4t_2round %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) 

gpt4t_2round_diff <- gpt4t_2round %>% 
  filter(n_diff != 0) %>% 
  mutate(id = 1:nrow(.))

#write_csv(gpt4t_2round_diff, "data/local/gpt4turbo_annotations_2round_to_clean.csv")

gpt4t_2round_cleaned <- read_csv("data/local/gpt4turbo_annotations_2round_cleaned.csv") %>% 
  select(id, sentiment_gpt_standardised, revised) %>% 
  right_join(gpt4t_2round_diff %>% select(text_grouped, id), by = "id") %>% 
  select(-id) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent)

rm(gpt4t_2round_diff)

gpt4t_2round_cleaned_nodiff <- gpt4t_2round_cleaned %>% 
  filter(revised == 1) %>% 
  #filter(n_diff == 0)
  mutate(sentiment_gpt_standardised = case_when(n_diff == 1 ~ paste(sentiment_gpt_standardised, "***none", sep = ""),
                                                n_diff == 2 ~ paste(sentiment_gpt_standardised, "***none***none", sep = ""),
                                                n_diff == 3 ~ paste(sentiment_gpt_standardised, "***none***none***none", sep = ""),
                                                n_diff == 4 ~ paste(sentiment_gpt_standardised, "***none***none***none***none", sep = ""),
                                                n_diff == 5 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none", sep = ""),
                                                n_diff == 6 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none", sep = ""),
                                                n_diff == 7 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none", sep = ""),
                                                n_diff == 10 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none***none***none", sep = ""),
                                                .default = sentiment_gpt_standardised)) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) %>% 
  filter(sent_gpt != "none")

# ------------------------- Missing3
gpt4t_missing3 <- read_csv("data/local/gpt4turbo_annotations_missing3_0_22035.csv") %>% 
  select(text, sentiment_gpt) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  mutate(words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
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
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa|:neutro|:positivo|:negativo|:neutra|:positiva|:negativa"), sep = "***"),
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
         n_diff = n_text - n_sent)

gpt4t_missing3_nodiff <- gpt4t_missing3 %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) 

gpt4t_missing3_diff <- gpt4t_missing3 %>% 
  filter(n_diff != 0) %>% 
  mutate(id = 1:nrow(.))

#write_csv(gpt4t_missing3_diff, "data/local/gpt4turbo_annotations_missing3_to_clean.csv")

gpt4t_missing3_cleaned <- read_csv("data/local/gpt4turbo_annotations_missing3_cleaned.csv") %>% 
  select(id, sentiment_gpt_standardised, revised) %>% 
  right_join(gpt4t_missing3_diff %>% select(text_grouped, id), by = "id") %>% 
  select(-id) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent)

rm(gpt4t_missing3_diff)

gpt4t_missing3_cleaned_nodiff <- gpt4t_missing3_cleaned %>% 
  filter(revised == 1) %>% 
  #filter(n_diff == 0)
  mutate(sentiment_gpt_standardised = case_when(n_diff == 1 ~ paste(sentiment_gpt_standardised, "***none", sep = ""),
                                                n_diff == 2 ~ paste(sentiment_gpt_standardised, "***none***none", sep = ""),
                                                n_diff == 3 ~ paste(sentiment_gpt_standardised, "***none***none***none", sep = ""),
                                                n_diff == 4 ~ paste(sentiment_gpt_standardised, "***none***none***none***none", sep = ""),
                                                n_diff == 5 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none", sep = ""),
                                                n_diff == 6 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none", sep = ""),
                                                n_diff == 7 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none", sep = ""),
                                                n_diff == 10 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none***none***none", sep = ""),
                                                .default = sentiment_gpt_standardised)) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) %>% 
  filter(sent_gpt != "none")

rm(gpt4t_missing3_cleaned)

# -------------------- Missing3b
gpt4t_missing3b <- read_csv("data/local/gpt4turbo_annotations_missing3_22035_end.csv") %>% 
  select(text, sentiment_gpt) %>% 
  rename(text_grouped = text,
         sentiment_gpt_grouped = sentiment_gpt) %>% 
  mutate(words_type = case_when(str_length(sentiment_gpt_grouped) <= 9 ~ "single_word",
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
                                                                                                     ": neutro|: positivo|: negativo|: neutra|: positiva|: negativa|:neutro|:positivo|:negativo|:neutra|:positiva|:negativa"), sep = "***"),
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
         n_diff = n_text - n_sent)

gpt4t_missing3b_nodiff <- gpt4t_missing3b %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) 

gpt4t_missing3b_diff <- gpt4t_missing3b %>% 
  filter(n_diff != 0) %>% 
  mutate(id = 1:nrow(.))

#write_csv(gpt4t_missing3_diff, "data/local/gpt4turbo_annotations_missing3b_to_clean.csv")

gpt4t_missing3b_cleaned <- read_csv("data/local/gpt4turbo_annotations_missing3b_cleaned.csv") %>% 
  select(id, sentiment_gpt_standardised, revised) %>% 
  right_join(gpt4t_missing3b_diff %>% select(text_grouped, id), by = "id") %>% 
  select(-id) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent)

rm(gpt4t_missing3b_diff)

gpt4t_missing3b_cleaned_nodiff <- gpt4t_missing3b_cleaned %>% 
  filter(revised == 1) %>% 
  #filter(n_diff == 0)
  mutate(sentiment_gpt_standardised = case_when(n_diff == 1 ~ paste(sentiment_gpt_standardised, "***none", sep = ""),
                                                n_diff == 2 ~ paste(sentiment_gpt_standardised, "***none***none", sep = ""),
                                                n_diff == 3 ~ paste(sentiment_gpt_standardised, "***none***none***none", sep = ""),
                                                n_diff == 4 ~ paste(sentiment_gpt_standardised, "***none***none***none***none", sep = ""),
                                                n_diff == 5 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none", sep = ""),
                                                n_diff == 6 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none", sep = ""),
                                                n_diff == 7 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none", sep = ""),
                                                n_diff == 10 ~ paste(sentiment_gpt_standardised, "***none***none***none***none***none***none***none***none***none***none", sep = ""),
                                                .default = sentiment_gpt_standardised)) %>% 
  mutate(n_text = str_count(text_grouped, pattern = fixed("***")) + 1,
         n_sent = str_count(sentiment_gpt_standardised, pattern = fixed("***")) +1,
         n_diff = n_text - n_sent) %>% 
  filter(n_diff == 0) %>% 
  separate_rows(text_grouped, sentiment_gpt_standardised, sep = "\\*\\*\\*") %>% 
  mutate(text = text_grouped,
         sent_gpt = sentiment_gpt_standardised) %>% 
  select(text, sent_gpt) %>% 
  mutate(gpt_sent_presence = 1) %>% 
  filter(sent_gpt != "none")

rm(gpt4t_missing3b_cleaned)

### Merging all annotations ------------

annotations_clean <- gpt4t_clean_nodiff %>% 
  rbind(gpt4_clean) %>% 
  rbind(gpt4t_cleaned_nodiff) %>%
  rbind(gpt4t_missing_nodiff) %>%
  rbind(gpt4t_missing2_nodiff) %>%
  rbind(gpt4t_missing_cleaned_nodiff) %>%
  rbind(gpt4t_missing2_cleaned_nodiff) %>%
  rbind(gpt4t_2round_nodiff) %>%
  rbind(gpt4t_2round_cleaned_nodiff) %>%
  rbind(gpt4t_missing3_nodiff) %>% 
  rbind(gpt4t_missing3_cleaned_nodiff) %>% 
  rbind(gpt4t_missing3b_cleaned_nodiff) %>% 
  #distinct(text, .keep_all = TRUE) %>% 
  mutate(sent_gpt = case_when(is.na(sent_gpt) ~ "neutral",
                              .default = sent_gpt),
         sent_gpt = case_when(str_detect(sent_gpt, pattern = "neutr") ~ "neutral",
                               str_detect(sent_gpt, pattern = "posit") ~ "positive",
                               str_detect(sent_gpt, pattern = "negat") ~ "negative",
                              .default = "neutral")) %>% 
  mutate(text = fix_portuguese_encoding(text))

rm(gpt4t_clean, gpt4_clean, gpt4t_clean_nodiff, gpt4t_cleaned_nodiff)
rm(gpt4t_diff, gpt4t_diff2, gpt4t_2round_cleaned_nodiff, gpt4t_2round_cleaned)
rm(gpt4t_cleaned, gpt4t_missing_nodiff)
rm(files_gpt4, files_gpt4t, files_gpt4t_missing)
rm(gpt4t_missing_cleaned, gpt4t_missing_cleaned_nodiff)
rm(gpt4t_missing2_nodiff, gpt4t_missing, gpt4t_missing2, gpt4t_missing2_cleaned_nodiff,
   gpt4t_missing2_cleaned, gpt4t_2round, gpt4t_2round_nodiff)
rm(gpt4t_missing3, gpt4t_missing3_cleaned_nodiff, gpt4t_missing3_nodiff, gpt4t_missing3b,
   gpt4t_missing3b_cleaned_nodiff, gpt4t_missing3b_nodiff)

annotations_clean_unique <- annotations_clean %>% 
  distinct(text, .keep_all = TRUE)


## Import cleaned Twitter data ----------------
message("Importing Twitter data")
df_raw <- read_csv('data/local/tweets_filtered_BR_PT.csv') 
gc()

df_clean <- df_raw %>% 
  left_join(annotations_clean_unique, by = "text") %>% 
  filter(gpt_sent_presence == 1)

df_missing3 <- df_raw %>% 
  #select(text) %>% 
  #distinct(.keep_all = TRUE) %>% 
  left_join(annotations_clean_unique, by = "text") %>% 
  #distinct(.keep_all = TRUE) %>%  
  filter(is.na(gpt_sent_presence)) %>% 
  distinct(text)

write_csv(df_missing3, "data/local/tweets_BR_PT_missing3_for_gpt4turbo.csv")

df_raw <- df_raw[1:500000,] %>% 
  stringdist_inner_join(annotations_clean_unique, by = "text", max_dist = 6)

write_csv(df_clean, "data/local/tweets_BR_PT_gpt.csv")

df_for_gpt <- df_raw %>% 
  left_join(annotations_clean, by = "text") %>% 
  distinct(text, .keep_all = TRUE) %>% 
  filter(gpt_sent_presence == 0) 
