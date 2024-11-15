# Script information ------------
#' Aim: Descriptive analysis of keywords using NLP and preparing for GPT-4-o
#' Author: Laura Espinosa
#' Date created: 26 July 2021
#' Date updated: 05 February 2023

# 2 - Import cleaned Twitter data ----------------
message("Importing annotated data")
## Steps 2-5 can be omitted
## Sentiment predictions
df_clean <- read_csv('data/local/tweets_with_predicted_labels_filtered_BR_PT_geo_final.csv')

## Frequent words in tweets -------------
### Python lemmatisation per sentiment ----------
# Get unique sentiment labels
# sentiments <- unique(df_clean$sent_gpt)
# 
# # Initialize a list to store the word distributions
# word_distributions <- list()
# 
# # Process each sentiment separately 
# for (sentiment in sentiments) {
#   # Filter text data for the current sentiment
#   texts <- df_clean %>% filter(sent_gpt == sentiment) %>% pull(cleaned_text_lem)
#   
#   # Combine all text into a single string and split into words
#   all_words <- unlist(str_split(paste(texts, collapse = " "), "\\s+"))
#   
#   # Delete words used for the Twitter search
#   # words_remove <- c("vacina", "vacinação", "vacinado", "vacinar", "vacinal")
#   # words_remove_pattern <- paste(words_remove, collapse = "|")
#   # all_words_cleaned <- all_words[!all_words %in% words_remove]
#   
#   # Count the frequency of each word
#   word_freq <- sort(table(all_words), decreasing = TRUE)
#   
#   # Store the word frequency distribution for the sentiment
#   word_distributions[[sentiment]] <- head(word_freq, 10)  # Get the 10 most common words
# }
# 
# keywords_neg <- as.data.frame(word_distributions[["negative"]]) %>% 
#   mutate(sentiment = "negative")
# keywords_neu <- as.data.frame(word_distributions[["neutral"]]) %>% 
#   mutate(sentiment = "neutral")
# keywords_pos <- as.data.frame(word_distributions[["positive"]]) %>% 
#   mutate(sentiment = "positive")
# keywords_all <- keywords_neg %>% 
#   rbind(keywords_neu) %>% 
#   rbind(keywords_pos) %>% 
#   rename(words = all_words,
#          frequency = Freq) %>% 
#   select(sentiment, words, frequency)
# 
# keywords_wide <- keywords_all %>%
#   group_by(sentiment) %>%
#   mutate(row = row_number()) %>%  # Add a helper row identifier
#   pivot_wider(
#     names_from = sentiment,            # Pivot based on the sentiment column
#     values_from = c(words, frequency), # Pivot both words and frequency columns
#     names_glue = "{sentiment}_{.value}"  # Create new columns with the sentiment prefix
#   ) %>%
#   select(-row)  %>%  # Remove the helper row identifier
#   select(negative_words, negative_frequency, neutral_words, neutral_frequency, positive_words, positive_frequency)
# 
# # Create the GT table
# gt_table <- keywords_wide %>%
#   gt() %>% 
#   tab_spanner(
#     label = "Negative",
#     columns = c(negative_words, negative_frequency)
#   ) %>%
#   tab_spanner(
#     label = "Neutral",
#     columns = c(neutral_words, neutral_frequency)
#   ) %>%
#   tab_spanner(
#     label = "Positive",
#     columns = c(positive_words, positive_frequency)
#   ) %>%
#   # Optionally, relabel the columns if needed
#   cols_label(
#     negative_words = "Words",
#     negative_frequency = "Frequency",
#     neutral_words = "Words",
#     neutral_frequency = "Frequency",
#     positive_words = "Words",
#     positive_frequency = "Frequency"
#   ) %>%
#   # Add borders between the columns of different stances using tab_style
#   tab_style(
#     style = cell_borders(
#       sides = c("left", "right"),  # Apply borders to both sides of the cells
#       color = "black",  # Set the border color to black
#       weight = px(1)    # Set the border thickness to 1px
#     ),
#     locations = cells_body(
#       columns = everything()  # Apply to all body columns
#     )
#   ) %>%
#   tab_style(
#     style = cell_borders(
#       sides = "bottom",  # Add a bottom border
#       color = "black",
#       weight = px(1)
#     ),
#     locations = cells_body(
#       columns = everything()  # Apply to all body columns
#     )
#   ) %>%
#   # Add vertical borders to the header rows (both spanner and labels)
#   tab_style(
#     style = cell_borders(
#       sides = c("left", "right"),  # Apply vertical borders
#       color = "black",  # Set the border color to black
#       weight = px(3)    # Set the border thickness to 1px
#     ),
#     locations = list(
#       cells_column_spanners(),      # Add vertical borders to the spanner (top) row
#       cells_column_labels(columns = everything())  # Add vertical borders to the labels row
#     )
#   )%>%
#   # Add borders for the column labels
#   tab_options(
#     column_labels.border.top.color = "black",
#     column_labels.border.top.width = px(2),
#     column_labels.border.bottom.color = "black",
#     column_labels.border.bottom.width = px(2),
#     table.border.top.color = "black",
#     table.border.top.width = px(2),
#     table.border.bottom.color = "black",
#     table.border.bottom.width = px(2)
#   )
# 
# # Print the table in the RStudio viewer
# print(gt_table)
# 
# 
# gtsave(gt_table, filename = "outputs/words_distribution_table_sentiment.png")

### For time and sentiment -----------
# Process each sentiment separately with progress tracking
df_words <- df_clean %>% 
  select(cleaned_text_lem, created_at, id, sent_gpt) %>% 
  unnest_tokens(output = words, input = cleaned_text_lem) %>% 
  #filter(nchar(words) > 1) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = "")))

df_words_week <- df_words %>% 
  mutate(words = case_when(words == "vacina" | words == "vacinao" | words == "vacinaor" | words == "vacinar" | words == "vacino" ~ "vacin*",
                           .default = words)) %>% 
  group_by(year_week, sent_gpt, words) %>% 
  tally() %>% 
  arrange(year_week, sent_gpt, desc(n)) %>% 
  slice_head(n = 10)

unique(df_words_week$words)

df_words_month <- df_words %>% 
  mutate(words = case_when(words == "vacina" | words == "vacinao" | words == "vacinaor" | words == "vacinar" | words == "vacino" ~ "vacin*",
                           words == "venezuelano" | words == "venezuela" ~ "venezuel*",
                           .default = words)) %>% 
  group_by(year_month, sent_gpt, words) %>% 
  tally() %>% 
  arrange(year_month, sent_gpt, desc(n)) %>% 
  slice_head(n = 20) #%>% 
  # ungroup() %>% 
  # group_by(year_month, sent_gpt) %>% 
  # mutate(total = sum(n),
  #        percentage = n/total *100)

unique(df_words_month$words)

#### Figure ------------
df_words_month_fig <- df_words_month %>% 
  ungroup() %>% 
  mutate(year_month = factor(year_month, levels = unique(year_month))) %>% 
  ggplot(aes(x = year_month, y = percentage, fill = words)) +
  geom_bar(
    colour = "black", 
    stat = "identity",
    linewidth = 0.25) +
  geom_text(aes(label = words), size = 2, hjust = 0.5,
            #vjust = 3,
            position = position_stack(vjust = 0.5),
            angle = 90
            #, fontface = "bold"
  ) +
  theme_classic() +
  scale_x_discrete(breaks = unique(df_words_month$year_month)[seq(1, length(unique(df_words_month$year_month)), by = 3)]) +  # Fewer labels
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust = 0.5,
                                   size = 10)) +
  facet_grid(rows = vars(sent_gpt))

df_words_month_fig

ggsave("outputs/keywords_month_stance.png", 
       df_words_month_fig,
       height = 5,
       width = 9)

# Get datasets for gpt-4- summarisation ---------------
## 2019 -------------
df_clean_2019 <- df_clean %>% 
  filter(year(created_at) == 2019) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2019_pos <- df_clean_2019 %>% 
  filter(sent_gpt == "positive")
df_clean_2019_neg <- df_clean_2019 %>% 
  filter(sent_gpt == "negative")
df_clean_2019_neu <- df_clean_2019 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2019_pos$year_week)
weeks_neg <- unique(df_clean_2019_neg$year_week)
weeks_neu <- unique(df_clean_2019_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2019_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2019$year_month)) {
  df_pos_month <- df_clean_2019_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2019$year_month)) {
  df_neg_month <- df_clean_2019_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2019$year_month)) {
  df_neu_month <- df_clean_2019_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}

### Merge results per stance -------------
setwd("data")
files <- fs::dir_ls(glob = "summary_2019_*csv")
df_2019_sum <- vroom(files)
setwd("..")

df_2019_sum <- df_2019_sum %>% 
  separate(col = "dataset", into = c("year", "month", "stance"),
           sep = "_") %>% 
  mutate(year_month = paste(year, month, sep = "_"))%>% 
  select(year_month, stance, summary) %>% 
  arrange(year_month, stance)

df_2019_sum %>% write_csv("data/summary_2019_tweets.csv")

## 2018 -------------
df_clean_2018 <- df_clean %>% 
  filter(year(created_at) == 2018) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2018_pos <- df_clean_2018 %>% 
  filter(sent_gpt == "positive")
df_clean_2018_neg <- df_clean_2018 %>% 
  filter(sent_gpt == "negative")
df_clean_2018_neu <- df_clean_2018 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2018_pos$year_week)
weeks_neg <- unique(df_clean_2018_neg$year_week)
weeks_neu <- unique(df_clean_2018_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2018_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2018$year_month)) {
  df_pos_month <- df_clean_2018_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2018$year_month)) {
  df_neg_month <- df_clean_2018_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2018$year_month)) {
  df_neu_month <- df_clean_2018_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}
