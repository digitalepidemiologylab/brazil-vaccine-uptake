# Script information ------------
#' Aim: Descriptive analysis of predicted annotated tweets for Brazil and vaccine uptake data
#' Author: Laura Espinosa
#' Date created: 26 July 2021
#' Date updated: 05 February 2023

# 2 - Import cleaned Twitter data ----------------
message("Importing annotated data")
## Steps 2-5 can be omitted
## Sentiment predictions
df_clean <- read_csv('data/local/tweets_BR_PT_gpt_preprocessed.csv')

# 3 - Import other data sources ----------------
message("Importing other data sources")
## a) Import Brazil states boundary boxes -------------
br <- read_csv('data/brazil_states_boxes.csv')
# Last four states correspond to Brazilian islands

br_states <- data.frame(LocationName = c(unique(br$LocationName)),
                        StatesCode = c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 
                                       'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
                                       'RR', 'SC', 'SP', 'SE', 'TO', 'UNK'))
br <- right_join(br, br_states, by = 'LocationName') %>% 
  filter(LocationName != "Name Unknown") 

## b) Import population data -------------------
temp = tempfile(fileext = ".xls")
dataURL <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2020/serie_2001_2020_TCU.xls"
download.file(dataURL, destfile=temp, mode='wb')

df_pop <- read_excel(temp, sheet = 1, skip = 3) %>% 
  row_to_names(row_number = 1) 

df_pop_clean <- df_pop[c(2:34),] %>% 
  rename("Federal units" = names(df_pop[1])) %>% 
  arrange("Federal units") 

rm(df_pop)

df_pop_clean$`Federal units` <- toTitleCase(df_pop_clean$`Federal units`)

df_pop_clean <- df_pop_clean %>% 
  left_join(br, by = c("Federal units" = "LocationNameFull")) %>% 
  select(LocationCode, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`,
         `2019`) %>% 
  filter(!is.na(LocationCode)) %>% 
  t() %>% 
  row_to_names(row_number = 1) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  type.convert(as.is = TRUE) %>% 
  mutate(Total_pop = (rowSums(.[-1]))) %>% 
  mutate(rowname = lubridate::ymd(rowname, truncated = 2L)) %>% 
  pad(interval = "month") %>%  # fill missing dates
  fill(colnames(.), .direction = "down") # fill NA with previous values 


## d) Import vaccine uptake data -------------
## Vaccine uptake data from: http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/dpnibr.def
## MMR vaccine uptake in Brazil by states

# These commented steps were already done, no need to run them again #

# df_mmr_raw <- read_csv("data/uptake_br_mmr_2013_2019.csv")
# df_mmr <- df_mmr_raw
# df_mmr$Date <- as.Date(paste0(df_mmr$Date, "/01"), format = "%Y/%b/%d") # correct date format 
# rm(df_mmr_raw)
# 
# ### Complete missing dates with NA
# df_mmr <- df_mmr %>% 
#   rbind(c("2019-12-31", rep(NA, 41))) %>% # add last date
#   pad(interval = "day") %>%  # fill missing dates
#   tidyr::fill(colnames(df_mmr), .direction = "down") # fill NA with previous values 
# 
# 
# for (i in 2:29) {
#   df_mmr[[i]] <- as.numeric(df_mmr[[i]])
# }
# 
# # Clean and write mmr data
# df_mmr <- df_mmr %>%
#   mutate(year = year(Date),
#          Total = rowSums(.[-1]))
# 
# for (i in 2:29) {
#   df_mmr[[i]] <- round((df_mmr[[i]]/df_pop_clean[[i]]) * 100000, digits = 2)
# }
# 
# df_mmr <- df_mmr %>%
#   mutate(year_month = format(Date, "%Y-%m"))
# 
# 
# write.csv(df_mmr, "data/uptake_br_mmr_2013_2019_clean.csv", row.names = FALSE)
# 

# 
# write.csv(df_mmr_month, "data/uptake_br_mmr_2013_2019_clean_month.csv", row.names = FALSE)

### Import clean mmr data
df_mmr <- read_csv("data/uptake_br_mmr_2013_2019_clean.csv")

df_mmr_month <- df_mmr %>% 
  distinct(year_month, .keep_all = TRUE)

## e) World Bank data ------------------------------
world_bank <- read_csv("data/worldbank_br.csv") %>% 
  select(-"Country Name", -"Country Code", -"Series Code") %>%
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  janitor::clean_names() %>% 
  rownames_to_column("year") %>% 
  #separate(col = year, into = c("year", "year_code"), sep = " ") %>% 
  #select(-year_code, -na, -na_2, -na_3, -na_4, -na_5) %>% 
  mutate_all(as.numeric) 

plot_missing(world_bank, title = "Missing values",
             group = list("Remove" = 1, "Bad" = 0.8, "Good" = 0),
             missing_only = TRUE, ggtheme = theme_classic())

world_bank_clean <- world_bank %>% 
  tidyr::fill(`hospital_beds_per_1_000_people`) %>% 
  tidyr::fill(`physicians_per_1_000_people`) %>% 
  mutate(month = 1) %>%
  complete(year, month = 1:12) %>%
  fill(names(world_bank)) %>% 
  select_if(~ !any(is.na(.)))# %>% 

rm(world_bank)

#write_csv(world_bank_clean, "data/worldbank_br_clean.csv")

# 4 - Descriptive analysis --------------
message("Running the descriptive analysis")
## Twitter data ------------------
names(df_clean)
table(df_clean$sent_gpt)
summary(df_clean$created_at)

### Table for exporting in publication format
#### Number of tweets per year by label
df_table <- df_clean %>% 
  # Create year and month bariables
  mutate(year = year(created_at)) %>% 
  mutate(month = month(created_at)) %>% 
  select(year, sent_gpt) %>% 
  rename(Stance = sent_gpt) %>% 
  tbl_summary(by = year, digits = list(Stance ~ 0)) %>% 
  add_overall()
df_table

overall_total <- df_clean %>% 
  mutate(Stance = sent_gpt) %>% 
  summarise(across(Stance, list(total = ~ sum(!is.na(.))))) %>% 
  mutate(year = "Total") %>% 
  select(year, everything())

# Bind the overall total to the summary table
df_table_total <- bind_rows(df_table, overall_total)

df_table %>% as_gt() %>% gt::gtsave("outputs/data_summary_year_stance.png")

#### Summary of selected variables
df_summary <- df_clean %>% 
  # Create year and month variables
  mutate(year = year(created_at)) %>% 
  mutate(month = month(created_at)) %>% 
  select(year, sent_gpt) %>%
  rename(Stance = sent_gpt) %>% 
  tbl_summary()
df_summary 
df_summary %>% as_gt() %>% gt::gtsave("outputs/data_summary_year_stance_separately.png")

#### Figure

## Frequent words in tweets -------------
### Python lemmatisation per sentiment ----------
# Get unique sentiment labels
sentiments <- unique(df_clean$sent_gpt)

# Initialize a list to store the word distributions
word_distributions <- list()

# Process each sentiment separately 
for (sentiment in sentiments) {
  # Filter text data for the current sentiment
  texts <- df_clean %>% filter(sent_gpt == sentiment) %>% pull(cleaned_text_lem)
  
  # Combine all text into a single string and split into words
  all_words <- unlist(str_split(paste(texts, collapse = " "), "\\s+"))
  
  # Delete words used for the Twitter search
  # words_remove <- c("vacina", "vacinação", "vacinado", "vacinar", "vacinal")
  # words_remove_pattern <- paste(words_remove, collapse = "|")
  # all_words_cleaned <- all_words[!all_words %in% words_remove]
  
  # Count the frequency of each word
  word_freq <- sort(table(all_words), decreasing = TRUE)
  
  # Store the word frequency distribution for the sentiment
  word_distributions[[sentiment]] <- head(word_freq, 10)  # Get the 10 most common words
}

keywords_neg <- as.data.frame(word_distributions[["negative"]]) %>% 
  mutate(sentiment = "negative")
keywords_neu <- as.data.frame(word_distributions[["neutral"]]) %>% 
  mutate(sentiment = "neutral")
keywords_pos <- as.data.frame(word_distributions[["positive"]]) %>% 
  mutate(sentiment = "positive")
keywords_all <- keywords_neg %>% 
  rbind(keywords_neu) %>% 
  rbind(keywords_pos) %>% 
  rename(words = all_words,
         frequency = Freq) %>% 
  select(sentiment, words, frequency)

keywords_wide <- keywords_all %>%
  group_by(sentiment) %>%
  mutate(row = row_number()) %>%  # Add a helper row identifier
  pivot_wider(
    names_from = sentiment,            # Pivot based on the sentiment column
    values_from = c(words, frequency), # Pivot both words and frequency columns
    names_glue = "{sentiment}_{.value}"  # Create new columns with the sentiment prefix
  ) %>%
  select(-row)  %>%  # Remove the helper row identifier
  select(negative_words, negative_frequency, neutral_words, neutral_frequency, positive_words, positive_frequency)

# Create the GT table
gt_table <- keywords_wide %>%
  gt() %>% 
  tab_spanner(
    label = "Negative",
    columns = c(negative_words, negative_frequency)
  ) %>%
  tab_spanner(
    label = "Neutral",
    columns = c(neutral_words, neutral_frequency)
  ) %>%
  tab_spanner(
    label = "Positive",
    columns = c(positive_words, positive_frequency)
  ) %>%
  # Optionally, relabel the columns if needed
  cols_label(
    negative_words = "Words",
    negative_frequency = "Frequency",
    neutral_words = "Words",
    neutral_frequency = "Frequency",
    positive_words = "Words",
    positive_frequency = "Frequency"
  ) %>%
  # Add borders between the columns of different stances using tab_style
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),  # Apply borders to both sides of the cells
      color = "black",  # Set the border color to black
      weight = px(1)    # Set the border thickness to 1px
    ),
    locations = cells_body(
      columns = everything()  # Apply to all body columns
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",  # Add a bottom border
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      columns = everything()  # Apply to all body columns
    )
  ) %>%
  # Add vertical borders to the header rows (both spanner and labels)
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),  # Apply vertical borders
      color = "black",  # Set the border color to black
      weight = px(3)    # Set the border thickness to 1px
    ),
    locations = list(
      cells_column_spanners(),      # Add vertical borders to the spanner (top) row
      cells_column_labels(columns = everything())  # Add vertical borders to the labels row
    )
  )%>%
  # Add borders for the column labels
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2)
  )

# Print the table in the RStudio viewer
print(gt_table)


gtsave(gt_table, filename = "outputs/words_distribution_table_sentiment.png")

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
  slice_head(n = 3)

unique(df_words_week$words)

df_words_month <- df_words %>% 
  mutate(words = case_when(words == "vacina" | words == "vacinao" | words == "vacinaor" | words == "vacinar" | words == "vacino" ~ "vacin*",
                           .default = words)) %>% 
  group_by(year_month, sent_gpt, words) %>% 
  tally() %>% 
  arrange(year_month, sent_gpt, desc(n)) %>% 
  slice_head(n = 3) %>% 
  ungroup() %>% 
  group_by(year_month, sent_gpt) %>% 
  mutate(total = sum(n),
         percentage = n/total *100)

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

# ### R text cleaning per sentiment -----
# df_clean_words <- df_clean %>% 
#   #distinct(text, .keep_all = TRUE) %>% 
#   select(text, created_at, id, sent_gpt) %>%  
#   mutate(text = tolower(text),
#          text = removePunctuation(text),
#          text = removeNumbers(text),
#          text = stripWhitespace(text),
#          text = str_replace_all(text, "[^a-zA-Z\\s]", "")) %>% 
#   unnest_tokens(output = words, input = text) %>% 
#   mutate(words = if_else(words %in% stopwords("pt"), "", words)) %>% 
#   filter(words != "")
# 
# keywords_all <- df_clean_words %>% 
#   group_by(sent_gpt, words) %>% 
#   tally() %>% 
#   arrange(sent_gpt, desc(n))
# words_count_neg <- df_clean_words_neg  %>% 
#   group_by(words) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   #group_by(sent_gpt) %>% 
#   #slice_head(n = 10) %>% 
#   filter(words != "")

## Time series plots ---------------

### Time series for sentiment without geo filtering ------------
sentiment_time_allgeo <- df_clean %>%
  group_by(created_at, sent_gpt) %>%
  tally() 

sentiment_time_horiz_allgeo <- sentiment_time_allgeo %>%
  group_by(created_at_h=floor_date(created_at, "1 day"), sent_gpt) %>%
  tally() %>% 
  spread(sent_gpt, n) %>%
  select(created_at_h, positive, neutral, negative) %>% 
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  mutate(sentiment_raw = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative)) 

sentiment_time_horiz_allgeo$sentiment <- runmean(sentiment_time_horiz_allgeo$sentiment_raw, k = 7, alg = 'exact', endrule = 'keep')

sentiment_time_horiz_allgeo_cat <- sentiment_time_horiz_allgeo %>% 
  mutate(cat_sent = case_when(sentiment < 0 ~ 'negative',
                              sentiment > 0 ~ 'positive',
                              TRUE ~ 'neutral'),
         year = year(created_at_h),
         month = month(created_at_h)) %>% 
  group_by(cat_sent, year, month) %>% 
  tally() %>% 
  mutate(n = as.numeric(n)) %>% 
  arrange(desc(cat_sent)) %>% 
  adorn_totals()

total_days <- sentiment_time_horiz_allgeo_cat %>%
  filter(cat_sent != "Total") %>% 
  group_by(year) %>% 
  summarise(sum(n)) 

positive_total <- sentiment_time_horiz_allgeo_cat %>%
  filter(cat_sent == 'positive' ) %>%
  summarise(sum(n)) 

positive_year <- sentiment_time_horiz_allgeo_cat %>%
  filter(cat_sent == 'positive' ) %>%
  group_by(year) %>% 
  summarise(sum(n)) %>% 
  left_join(total_days, by = "year") %>% 
  ungroup() %>% 
  mutate(percentage = `sum(n).x` / `sum(n).y` *100)

negative_total <- sentiment_time_horiz_allgeo_cat %>%
  filter(cat_sent == 'negative' ) %>%
  summarise(sum(n))


plot_time <- sentiment_time_horiz_allgeo[, c(1,6)] %>% 
  ggplot(aes(x = created_at_h, y = sentiment)) +
  geom_hline(yintercept=0.25,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_hline(yintercept=0.50,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_hline(yintercept=-0.25,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_hline(yintercept=-0.5,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_line(colour = "dark green", linewidth = 0.7) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_datetime(date_breaks = "2 months", 
                   date_minor_breaks = "1 month", 
                   date_labels = "%d %b %Y",
                   limits = c(min(sentiment_time_horiz_allgeo$created_at_h), 
                              max(sentiment_time_horiz_allgeo$created_at_h))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", linewidth=0.5) + 
  ggtitle(paste("7-day moving average of Twitter vaccine sentiment index \nin Brazil, January 2013 to December 2019 (n = ",
                format(nrow(df_clean), big.mark = ","), ")", sep = "")) +
  labs(y = "TVS index") +
  #geom_smooth(method = "loess", se = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 22),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 30, hjust = 0.5))

# Show and save plot
plot_time

ggsave("outputs/twitter_sentiment_br.png", plot_time,
       width = 12.2, height = 5.1)

# Interactive plot (plotly)
plotly_time <- ggplotly(plot_time) 

# Show interactive plot
plotly_time


### Time series for sentiment with geo filtering ------------
sentiment_time <- df_clean %>%
  filter(location_type != "country") %>% 
  filter(!is.na(location_type)) %>% 
  group_by(created_at, sent_gpt) %>%
  tally() 

sentiment_time_horiz <- sentiment_time %>%
  group_by(created_at_h=floor_date(created_at, "1 day"), sent_gpt) %>%
  tally() %>% 
  spread(sent_gpt, n) %>%
  select(created_at_h, positive, neutral, negative) %>% 
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  mutate(sentiment_raw = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative)) 

sentiment_time_horiz$sentiment <- runmean(sentiment_time_horiz$sentiment_raw, k = 7, alg = 'exact', endrule = 'keep')

plot_time_geo <- sentiment_time_horiz[, c(1,6)] %>% 
  ggplot(aes(x = created_at_h, y = sentiment)) +
  geom_hline(yintercept=0.3,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_hline(yintercept=0.6,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_hline(yintercept=-0.3,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_hline(yintercept=-0.6,  linetype = "dashed", color = "light grey", linewidth=0.5) +
  geom_line(colour = "dark green") +
  #scale_y_continuous(
    #limits = c(-0.7, 0.7),
  #                   breaks = c(-0.6, -0.3, 0, 0.3, 0.6)) +
  scale_x_datetime(date_breaks = "2 months", 
                   date_minor_breaks = "1 month", 
                   date_labels = "%d-%m-%Y",
                   limits = c(min(sentiment_time_horiz$created_at_h), 
                              max(sentiment_time_horiz$created_at_h))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", size=0.5) +
  ggtitle(paste('7-day moving average of Twitter vaccine sentiment index in Brazil, \nJanuary 2013 to December 2019 (n = ',
                format(nrow(df_clean %>% 
                       filter(location_type != "country") %>% 
                       filter(!is.na(location_type))),
                       big.mark = ","), ')', sep = "")) +
  labs(x = "Date (day, month and year)", y = "Sentiment score") +
  #geom_smooth(method = "loess", se = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 12, hjust = 0.5))

# Show and save plot
plot_time_geo
ggsave("outputs/twitter_sentiment_geo_br.png", plot_time_geo)

# Interactive plot (plotly)
plotly_time_geo <- ggplotly(plot_time_geo) 

# Show interactive plot
plotly_time_geo

### Time series for mmr vaccine uptake --------------
mean_hline <- mean(df_mmr$Total)

plot_mmr <- df_mmr_month %>% 
  ggplot(aes(y = Total, x = Date)) +
  geom_line(color = 'blue', size = 0.7) +
  labs(x = "Date", y = "Vaccine uptake (number of doses \nper 100,000 population)") +
  #geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = mean_hline,
             linetype = "dashed", color = "red", size=0.5) +
  scale_x_date(expand = c(0,0),
                   date_breaks = "6 months",
                   date_labels = "%d %b %Y") +
  scale_y_continuous() +
  ggtitle("Diphtheria-Tetatus-Pertussis monthly vaccination uptake per 100,000 population, \nincluding mean over time period, in Brazil, 2013 to 2019") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0)) 

# Show and save plot  
plot_mmr
#ggsave("outputs/mmr_uptake_br.png", plot_mmr)

# Interactive plot
plotly_mmr <- ggplotly(plot_mmr)

# Show interactive plot
plotly_mmr

### Time series for mmr vaccine uptake and sentiment without geo filtering ----------------
hlines <- data.frame(X = c("MMR monthly uptake per 100,000 population", 
                           "Twitter vaccine sentiment"),
                     Z = c(mean(df_mmr$Total), 0))
hlines$X <- factor(hlines$X)

plot_time_mmr <- sentiment_time_horiz_allgeo[, c(1,6)] %>%
  mutate(year_month = format(created_at_h, "%Y-%m")) %>% 
  left_join(df_mmr[,c(31,29)]) %>%
  select(-year_month) %>% 
  setNames(c('Date', 'Twitter vaccine sentiment', 'mmr monthly uptake per 100,000 population')) %>% 
  gather(key = "Indicator",
         value = "value",
         -Date) %>% 
  filter(value < 40000) %>% 
  ggplot(aes(x = as.Date(Date), y = value,
             color = Indicator)) +
  scale_color_manual(values = c("blue", "darkgreen")) +
  geom_line() +
  scale_x_date(expand = c(0,0),
               date_breaks = "3 months",
               date_labels = "%b %Y") +
  scale_y_continuous() +
  ggtitle("Measles-Mumps-Rubella (MMR) vaccine uptake per 100,000 population \nand 7-day moving average of Twitter vaccine sentiment index \nin Brazil, January 2013 to December 2019") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0, 
                                   hjust = 0),
        axis.text = element_text(size = 18),
        title = element_text(size = 18),
        strip.text = element_text(size = 18),
        axis.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~Indicator,
           ncol = 1, scales = "free_y") #+
  #geom_hline(data = hlines, aes(yintercept = Z))
  

plot_time_mmr


ggsave('outputs/twitter_sentiment_mmr_br.png', plot_time_mmr,
       width = 12, height = 6)
  

plotly_time_mmr_ <- ggplotly(plot_time_mmr)

plotly_time_mmr_


plotly_time_mmr <- subplot(plotly_mmr, plotly_time, nrows = 2,
                            heights = c(0.3, 0.7), shareX = TRUE,
                            titleX = TRUE, titleY = TRUE) %>%
  layout(yaxis = list(title = list(text = "MMR vaccine \nuptake (doses)", 
                                    font = list(size = 12))),
         yaxis2 = list(title = list( text = "Twitter vaccine sentiment", 
                                    font = list(size = 12))),
         xaxis = list(title = list(text = "Date (day, month and year)",
                                   font = list(size = 12))),
         hovermode = "x unified",
         title = list(text = "MMR vaccine uptake and vaccine sentiment \n in Brazil, January 2013 to December 2019",
                      size = 12))
plotly_time_mmr

# 5 - Data analysis by Brazilian states -----------------
message("Running data analysis by Brazilian states")
## Assign LocationCode to tweets---------------
df_clean_geo_raw <- df_clean %>% 
  filter(location_type != "country") %>% 
  filter(!is.na(location_type))

df_clean_geo <- df_clean_geo_raw %>% 
  mutate(LocationCode = case_when(latitude >= br$Latitude_ymin[1] &
                                    latitude <= br$Latitude_ymax[1] &
                                    longitude >= br$Longitude_xmin[1] &
                                    longitude <= br$Longitude_xmax[1] ~
                                    br$LocationCode[1],
                                  latitude >= br$Latitude_ymin[2] &
                                    latitude <= br$Latitude_ymax[2] &
                                    longitude >= br$Longitude_xmin[2] &
                                    longitude <= br$Longitude_xmax[2] ~
                                    br$LocationCode[2],
                                  latitude >= br$Latitude_ymin[3] &
                                    latitude <= br$Latitude_ymax[3] &
                                    longitude >= br$Longitude_xmin[3] &
                                    longitude <= br$Longitude_xmax[3] ~
                                    br$LocationCode[3],
                                  latitude >= br$Latitude_ymin[4] &
                                    latitude <= br$Latitude_ymax[4] &
                                    longitude >= br$Longitude_xmin[4] &
                                    longitude <= br$Longitude_xmax[4] ~
                                    br$LocationCode[4],
                                  latitude >= br$Latitude_ymin[5] &
                                    latitude <= br$Latitude_ymax[5] &
                                    longitude >= br$Longitude_xmin[5] &
                                    longitude <= br$Longitude_xmax[5] ~
                                    br$LocationCode[5],
                                  latitude >= br$Latitude_ymin[6] &
                                    latitude <= br$Latitude_ymax[6] &
                                    longitude >= br$Longitude_xmin[6] &
                                    longitude <= br$Longitude_xmax[6] ~
                                    br$LocationCode[6],
                                  latitude >= br$Latitude_ymin[7] &
                                    latitude <= br$Latitude_ymax[7] &
                                    longitude >= br$Longitude_xmin[7] &
                                    longitude <= br$Longitude_xmax[7] ~
                                    br$LocationCode[7],
                                  latitude >= br$Latitude_ymin[8] &
                                    latitude <= br$Latitude_ymax[8] &
                                    longitude >= br$Longitude_xmin[8] &
                                    longitude <= br$Longitude_xmax[8] ~
                                    br$LocationCode[8],
                                  latitude >= br$Latitude_ymin[9] &
                                    latitude <= br$Latitude_ymax[9] &
                                    longitude >= br$Longitude_xmin[9] &
                                    longitude <= br$Longitude_xmax[9] ~
                                    br$LocationCode[9],
                                  latitude >= br$Latitude_ymin[10] &
                                    latitude <= br$Latitude_ymax[10] &
                                    longitude >= br$Longitude_xmin[10] &
                                    longitude <= br$Longitude_xmax[10] ~
                                    br$LocationCode[10],
                                  latitude >= br$Latitude_ymin[11] &
                                    latitude <= br$Latitude_ymax[11] &
                                    longitude >= br$Longitude_xmin[11] &
                                    longitude <= br$Longitude_xmax[11] ~
                                    br$LocationCode[11],
                                  latitude >= br$Latitude_ymin[12] &
                                    latitude <= br$Latitude_ymax[12] &
                                    longitude >= br$Longitude_xmin[12] &
                                    longitude <= br$Longitude_xmax[12] ~
                                    br$LocationCode[12],
                                  latitude >= br$Latitude_ymin[13] &
                                    latitude <= br$Latitude_ymax[13] &
                                    longitude >= br$Longitude_xmin[13] &
                                    longitude <= br$Longitude_xmax[13] ~
                                    br$LocationCode[13],
                                  latitude >= br$Latitude_ymin[14] &
                                    latitude <= br$Latitude_ymax[14] &
                                    longitude >= br$Longitude_xmin[14] &
                                    longitude <= br$Longitude_xmax[14] ~
                                    br$LocationCode[14],
                                  latitude >= br$Latitude_ymin[15] &
                                    latitude <= br$Latitude_ymax[15] &
                                    longitude >= br$Longitude_xmin[15] &
                                    longitude <= br$Longitude_xmax[15] ~
                                    br$LocationCode[15],
                                  latitude >= br$Latitude_ymin[16] &
                                    latitude <= br$Latitude_ymax[16] &
                                    longitude >= br$Longitude_xmin[16] &
                                    longitude <= br$Longitude_xmax[16] ~
                                    br$LocationCode[16],
                                  latitude >= br$Latitude_ymin[17] &
                                    latitude <= br$Latitude_ymax[17] &
                                    longitude >= br$Longitude_xmin[17] &
                                    longitude <= br$Longitude_xmax[17] ~
                                    br$LocationCode[17],
                                  latitude >= br$Latitude_ymin[18] &
                                    latitude <= br$Latitude_ymax[18] &
                                    longitude >= br$Longitude_xmin[18] &
                                    longitude <= br$Longitude_xmax[18] ~
                                    br$LocationCode[18],
                                  latitude >= br$Latitude_ymin[19] &
                                    latitude <= br$Latitude_ymax[19] &
                                    longitude >= br$Longitude_xmin[19] &
                                    longitude <= br$Longitude_xmax[19] ~
                                    br$LocationCode[19],
                                  latitude >= br$Latitude_ymin[20] &
                                    latitude <= br$Latitude_ymax[20] &
                                    longitude >= br$Longitude_xmin[20] &
                                    longitude <= br$Longitude_xmax[20] ~
                                    br$LocationCode[20],
                                  latitude >= br$Latitude_ymin[21] &
                                    latitude <= br$Latitude_ymax[21] &
                                    longitude >= br$Longitude_xmin[21] &
                                    longitude <= br$Longitude_xmax[21] ~
                                    br$LocationCode[21],
                                  latitude >= br$Latitude_ymin[22] &
                                    latitude <= br$Latitude_ymax[22] &
                                    longitude >= br$Longitude_xmin[22] &
                                    longitude <= br$Longitude_xmax[22] ~
                                    br$LocationCode[22],
                                  latitude >= br$Latitude_ymin[23] &
                                    latitude <= br$Latitude_ymax[23] &
                                    longitude >= br$Longitude_xmin[23] &
                                    longitude <= br$Longitude_xmax[23] ~
                                    br$LocationCode[23],
                                  latitude >= br$Latitude_ymin[24] &
                                    latitude <= br$Latitude_ymax[24] &
                                    longitude >= br$Longitude_xmin[24] &
                                    longitude <= br$Longitude_xmax[24] ~
                                    br$LocationCode[24],
                                  latitude >= br$Latitude_ymin[25] &
                                    latitude <= br$Latitude_ymax[25] &
                                    longitude >= br$Longitude_xmin[25] &
                                    longitude <= br$Longitude_xmax[25] ~
                                    br$LocationCode[25],
                                  latitude >= br$Latitude_ymin[26] &
                                    latitude <= br$Latitude_ymax[26] &
                                    longitude >= br$Longitude_xmin[26] &
                                    longitude <= br$Longitude_xmax[26] ~
                                    br$LocationCode[26],
                                  latitude >= br$Latitude_ymin[27] &
                                    latitude <= br$Latitude_ymax[27] &
                                    longitude >= br$Longitude_xmin[27] &
                                    longitude <= br$Longitude_xmax[27] ~
                                    br$LocationCode[27],
                                  latitude >= br$Latitude_ymin[28] &
                                    latitude <= br$Latitude_ymax[28] &
                                    longitude >= br$Longitude_xmin[28] &
                                    longitude <= br$Longitude_xmax[28] ~
                                    br$LocationCode[28],
                                  latitude >= br$Latitude_ymin[29] &
                                    latitude <= br$Latitude_ymax[29] &
                                    longitude >= br$Longitude_xmin[29] &
                                    longitude <= br$Longitude_xmax[29] ~
                                    br$LocationCode[29],
                                  latitude >= br$Latitude_ymin[30] &
                                    latitude <= br$Latitude_ymax[30] &
                                    longitude >= br$Longitude_xmin[30] &
                                    longitude <= br$Longitude_xmax[30] ~
                                    br$LocationCode[30],
                                  latitude >= br$Latitude_ymin[31] &
                                    latitude <= br$Latitude_ymax[31] &
                                    longitude >= br$Longitude_xmin[31] &
                                    longitude <= br$Longitude_xmax[31] ~
                                    br$LocationCode[31],
                                  TRUE ~ "NA"))


df_clean_geo <- br %>% 
  select(LocationCode, LocationName, StatesCode) %>% 
  right_join(df_clean_geo, by = c('LocationCode' = 'LocationCode'))


# Check that df_clean_geo has LocationCode
unique(df_clean_geo$LocationCode)

# Save data as csv
write_csv(df_clean_geo, 'data/local/tweets_with_predicted_labels_filtered_BR_PT_geo_final.csv')

## Group data per Brazilian state -----------------------
df_clean_geo_state <- df_clean_geo %>%
  filter(!is.na(LocationCode)) %>% 
  mutate(year = year(created_at),
         month = month(created_at),
         week = week(created_at)) %>% 
  group_by(date=floor_date(created_at, "1 day"), LocationCode, sent_gpt) %>%
  tally() %>% 
  spread(sent_gpt, n) %>%
  #select(date, LocationCode, positive, neutral, negative) %>% 
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  arrange(LocationCode) %>% 
  mutate(sentiment_raw = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative),
         year = year(date))  %>% 
  group_by(LocationCode) %>% 
  mutate(sentiment = runmean(sentiment_raw, k = 7, alg = 'exact', endrule = 'keep')) %>% 
  full_join(br[,c(2, 12)], by = 'LocationCode')

## Separate Brazilian state data per year
df_clean_geo_state_year <- df_clean_geo_state %>% 
  group_by(year) %>% 
  mutate(tweets = positive + neutral + negative) %>% 
  select(-sentiment_raw)

write_csv(df_clean_geo_state_year, 'data/tweets_with_predicted_labels_state_year.csv')

## Time series analysis per label, state and date --------------------------
df_clean_geo_state$color <- ifelse(df_clean_geo_state$sentiment < 0,
                                   "negative",
                                   ifelse(df_clean_geo_state$sentiment > 0,
                                          "positive",
                                          "neutral"))

df_clean_geo_all <- sentiment_time_horiz_allgeo %>% 
  select(created_at_h, negative, neutral, positive, sentiment, 
         sentiment_raw
         #, cat_sent
         ) %>% 
  mutate(LocationCode = "BR",
         year = year(created_at_h),
         Longitude_xmin = NA,
         color = case_when(sentiment_raw <0 ~ "negative",
                           sentiment_raw >0 ~ "positive",
                           .default = "neutral")) %>% 
  rename("date" = "created_at_h"
         #, "color" = 'cat_sent'
         ) %>% 
  rbind.data.frame(df_clean_geo_state) %>% 
  left_join(br[,c(2,5)])

df_clean_geo_all$LocationNameFull <- replace_na(df_clean_geo_all$LocationNameFull,
                                                " Brazil")

df_clean_geo_all_plot_df <- df_clean_geo %>% 
  group_by(LocationCode) %>% 
  tally() %>% 
  ungroup() %>% 
  rbind(list(LocationCode = "BR", n = nrow(df_clean))) %>%
  mutate(n = as.numeric(n)) %>% 
  rename("Total" = "n") %>% 
  right_join(df_clean_geo_all, by = "LocationCode") %>% 
  mutate(Total = case_when(is.na(Total) ~ 0,
                           TRUE ~ Total),
         label_plot = paste0(LocationNameFull, " (n = ", 
                             format(Total, big.mark = ",", trim = TRUE), 
                             " )", sep = ""))


plot_states <- df_clean_geo_all_plot_df %>% 
  # mutate(sentiment_cat = case_when(sentiment > 0 ~ "Positive",
  #                                  sentiment < 0 ~ "Negative",
  #                                  TRUE ~ "Neutral")) %>% 
  filter(!is.na(LocationNameFull)) %>% 
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(colour = "dark green", size = 0.5) +
  # geom_point(aes(x = date, y = sentiment,
  #                colour = sentiment_cat),
  #            size = 1) +
  #scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_datetime(date_breaks = "6 months", 
                   #date_minor_breaks = "1 month", 
                   date_labels = "%b-%Y",
                   limits = c(min(df_clean_geo_all$date), 
                              max(df_clean_geo_all$date))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", size=0.5) +
  ggtitle('7-day moving average of Twitter vaccine sentiment index in Brazil, January 2013 to December 2019') +
  labs(x = "Date (day, month and year)", y = "TVS index") +
  #geom_smooth(method = "loess", se = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 10)) +
  facet_wrap(vars(label_plot), 
             #scales = "free_y", 
             ncol = 4)

# Show plot
plot_states

# save plot
ggsave("outputs/twitter_sentiment_br_states.png", plot_states,
      width = 12, height = 6)

# Interactive plot (plotly)
plotly_states <- ggplotly(plot_states) 

# Show interactive plot
plotly_states

# Positives per state and year
positive_br_states <- df_clean_geo_all %>% 
  filter(color == "positive") %>% 
  select(LocationNameFull, year) %>% 
  group_by(LocationNameFull, year) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = LocationNameFull,
              names_from = year,
              values_from = n)

n_distinct(positive_br_states %>% filter(!is.na(`2013`)) %>% select(LocationNameFull))
n_distinct(positive_br_states %>% filter(!is.na(`2014`)) %>% select(LocationNameFull))
n_distinct(positive_br_states %>% filter(!is.na(`2015`)) %>% select(LocationNameFull))
n_distinct(positive_br_states %>% filter(!is.na(`2016`)) %>% select(LocationNameFull))
n_distinct(positive_br_states %>% filter(!is.na(`2017`)) %>% select(LocationNameFull))
n_distinct(positive_br_states %>% filter(!is.na(`2018`)) %>% select(LocationNameFull))
n_distinct(positive_br_states %>% filter(!is.na(`2019`)) %>% select(LocationNameFull))


# 6 - Maps in R ----------------
message("Producing maps")
## Select data for maps -----------------------

### Group data for all years per state ----------
df_clean_geo_state_year_all <- df_clean_geo_state_year %>% 
  select(LocationCode, negative, neutral, positive) %>% 
  group_by(LocationCode) %>% 
  summarise(Negative = sum(negative),
            Neutral = sum(neutral),
            Positive = sum(positive)) %>% 
  mutate(sentiment = (((1*Positive)+(-1 * Negative))/(Positive+Negative+Neutral)),
         tweets = Positive + Neutral + Negative)

# Save csv
write_csv(df_clean_geo_state_year_all, 'data/tweets_with_predicted_labels_state_year_all.csv')

### Group data by year and location and save csv ----------------
df_clean_geo_state_by_year <- df_clean_geo_state_year %>% 
  select(LocationCode, negative, neutral, positive) %>% 
  group_by(LocationCode, year) %>% 
  summarise(negative = sum(negative),
            neutral = sum(neutral),
            positive = sum(positive)) %>% 
  mutate(sentiment = (((1*positive)+(-1 * negative))/(positive+negative+neutral)),
         tweets = positive + neutral + negative)

write_csv(df_clean_geo_state_by_year, 'data/tweets_with_predicted_labels_by_state_year.csv')

### Group data for each year and save csv -----------------
df_clean_geo_state_year_2019 <- df_clean_geo_state_by_year %>% 
  filter(year == "2019") 

df_clean_geo_state_year_2018 <- df_clean_geo_state_by_year %>% 
  filter(year == "2018") 

df_clean_geo_state_year_2017 <- df_clean_geo_state_by_year %>% 
  filter(year == "2017") 

df_clean_geo_state_year_2016 <- df_clean_geo_state_by_year %>% 
  filter(year == "2016") 

df_clean_geo_state_year_2015 <- df_clean_geo_state_by_year %>% 
  filter(year == "2015") 

df_clean_geo_state_year_2014 <- df_clean_geo_state_by_year %>% 
  filter(year == "2014") 

df_clean_geo_state_year_2013 <- df_clean_geo_state_by_year %>% 
  filter(year == "2013") 

# Save csv
write_csv(df_clean_geo_state_year_2019, 'data/tweets_with_predicted_labels_state_year_2019.csv')
write_csv(df_clean_geo_state_year_2018, 'data/tweets_with_predicted_labels_state_year_2018.csv')
write_csv(df_clean_geo_state_year_2017, 'data/tweets_with_predicted_labels_state_year_2017.csv')
write_csv(df_clean_geo_state_year_2016, 'data/tweets_with_predicted_labels_state_year_2016.csv')
write_csv(df_clean_geo_state_year_2015, 'data/tweets_with_predicted_labels_state_year_2015.csv')
write_csv(df_clean_geo_state_year_2014, 'data/tweets_with_predicted_labels_state_year_2014.csv')
write_csv(df_clean_geo_state_year_2013, 'data/tweets_with_predicted_labels_state_year_2013.csv')

## Plot map -------------
#devtools::install_github("ropensci/rnaturalearthhires")
world <- ne_states(returnclass = "sf")

world_countries <- world %>%
  st_make_valid() %>%  # Attempt to fix invalid geometries
  st_simplify(dTolerance = 0.01) %>%  # Simplify geometries to remove small artefacts
  group_by(admin) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')  # Merge geometries


world_countries <- world %>%
  st_make_valid() %>%  # Attempt to fix invalid geometries
  st_simplify(dTolerance = 0.01) %>%  # Simplify geometries
  group_by(admin) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')  # Merge geometries

# Define the bounding box for Brazil and neighboring regions
bounds <- st_bbox(c(xmin = -82, xmax = -32, ymin = -35, ymax = 7), crs = st_crs(world_countries))

# Clip the world data to the bounding box
world_clipped <- st_intersection(world_countries, st_as_sfc(bounds))

### All years -------------------
world_br_all <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2)  %>% 
  mutate(name_pt_case = str_to_title(name_pt)) %>% 
  left_join(br,
            by = c("name_pt_case" = "LocationNameFull")) %>% 
  left_join(df_clean_geo_state_year_all,
            by = c("LocationCode" = "LocationCode"))


map_all <- world_br_all %>% 
  mutate(sentiment_cat = case_when(sentiment < -0.09104 ~ "< -0.09",
                                   sentiment > -0.09104 & sentiment < -0.05684 ~ "-0.09 to -0.06",
                                   sentiment > -0.05684 & sentiment < -0.04495 ~ "-0.06 to -0.04",
                                   TRUE ~ "> -0.04")) %>%
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = world_br_all$sentiment), color = "black") +
  scale_fill_viridis_c(option = "B",
                       name = "Twitter vaccine \nsentiment index") +
  #scale_fill_gradient2(midpoint = 0) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  # coord_sf(xlim = c(-72.9872354804, -33.7683777809), 
  #          ylim = c(-34.7299934555, 5.24448639569 )) +
  labs(title = 'Twitter vaccine sentiment index per \nBrazilian state, January 2013 to \nDecember 2019 (n = 1,337,642)') +
  theme_void() +
  theme(legend.position = c(1, 0.4),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        title = element_text(size = 14),
        plot.background = element_rect(fill = "white",
                                        colour = "white")) +
  coord_sf(xlim = c(-82, -32), ylim = c(-35, 7))


map_all

ggsave("outputs/map_sentiment_2013_2019.png", map_all)


### Each year -------------------
for (i in 2013:2019) {
  df_clean_geo_state_plot <- br %>% 
    select(LocationNameFull, LocationCode) %>% 
    right_join(df_clean_geo_state_by_year) %>% 
    select(LocationCode, LocationNameFull, year, negative, neutral, positive, sentiment, tweets) %>% 
    filter(year == i)
  data_plot <- world %>% 
    filter(sov_a3 == "BRA") %>% 
    arrange(iso_3166_2) %>% 
    mutate(name_pt_case = str_to_title(name_pt)) %>% 
    left_join(df_clean_geo_state_plot,
              by = c("name_pt_case" = "LocationNameFull"))
  
  map <- data_plot %>% 
    mutate(sentiment_cat = case_when(sentiment < -0.09104 ~ "< -0.09",
                                     sentiment > -0.09104 & sentiment < -0.05684 ~ "-0.09 to -0.06",
                                     sentiment > -0.05684 & sentiment < -0.04495 ~ "-0.06 to -0.04",
                                     TRUE ~ "> -0.04")) %>%
    ggplot() +
    geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
    geom_sf(aes(fill = world_br_all$sentiment), color = "black") +
    scale_fill_viridis_c(option = "B",
                         name = "Twitter vaccine \nsentiment index") +
    #scale_fill_gradient2(midpoint = 0) +
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    # coord_sf(xlim = c(-72.9872354804, -33.7683777809), 
    #          ylim = c(-34.7299934555, 5.24448639569 )) +
    labs(title = 'Twitter vaccine sentiment index per \nBrazilian state, January 2013 to \nDecember 2019 (n = 1,750,294)') +
    theme_void() +
    theme(legend.position = c(1, 0.4),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          title = element_text(size = 14),
          plot.background = element_rect(fill = "white",
                                         colour = "white")) +
    coord_sf(xlim = c(-82, -32), ylim = c(-35, 7))
  assign(x = paste("map", i, sep = "_"),
         value = map)
  print(paste0("Printing map for year ", i))
  print(map)
  ggsave(filename = paste("outputs/map_", i, ".png",
                          sep = ""))
  
}

### All years in facet ----------
world_br_all_facet <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2) %>% 
  mutate(name_pt_case = str_to_title(name_pt)) %>%
  left_join(br[, c(2,5)],
            by = c("name_pt_case" = "LocationNameFull")) %>% 
  left_join(df_clean_geo_state_by_year,
            by = c("LocationCode" = "LocationCode")) %>% 
  filter(!is.na(year)) 


df_short <- world_br_all_facet %>% 
  select(LocationCode, sentiment)


labels_plot <- NULL

for (i in 2013:2019) {
  
  df_loop <- df_clean_geo_state_by_year %>% 
    filter(year == i)
  
  new_label <- c(paste0("January-December ", i, " ", "(n=", 
                       format(sum(df_loop$tweets),
                              big.mark = ","),
                       ")", sep = ""))
  labels_plot <- c(labels_plot, new_label)
}

labels_plot_test <- c(paste(unique(world_br_all_facet$year), "test"))

map_all_facet <- world_br_all_facet %>% 
  # mutate(sentiment_cat = case_when(sentiment < -0.09104 ~ "< -0.09",
  #                                  sentiment > -0.09104 & sentiment < -0.05684 ~ "-0.09 to -0.06",
  #                                  sentiment > -0.05684 & sentiment < -0.04495 ~ "-0.06 to -0.04",
  #                                  TRUE ~ "> -0.04")) %>%
  mutate(year_label = case_when(year == 2013 ~ paste0("Jan-Dec ", 2013, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2013$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2014 ~ paste0("Jan-Dec ", 2014, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2014$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2015 ~ paste0("Jan-Dec ", 2015, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2015$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2016 ~ paste0("Jan-Dec ", 2016, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2016$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2017 ~ paste0("Jan-Dec ", 2017, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2017$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2018 ~ paste0("Jan-Dec ", 2018, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2018$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2019 ~ paste0("Jan-Dec ", 2019, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2019$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                TRUE ~ "test")) %>% 
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = sentiment), color = "black") +
  scale_fill_viridis_c(option = "B",
                       name = "Twitter vaccine \nsentiment index") +
  #scale_fill_gradient2(midpoint = 0) +
  #annotation_scale(location = "br", width_hint = 0.3) +
  # annotation_north_arrow(location = "br", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  # coord_sf(xlim = c(-72.9872354804, -33.7683777809), 
  #          ylim = c(-34.7299934555, 5.24448639569 )) +
  # labs(title = paste('Twitter vaccine sentiment index per Brazilian state and year (n = ',
  #                    format(nrow(df_clean_geo),
  #                           big.mark = ","), ")", sep = "")) +
  theme_void() +
  theme(legend.position = c(0.85, 0.3),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        title = element_text(size = 14),
        strip.text = element_text(size=14),
        plot.background = element_rect(fill = "white",
                                       colour = "white")
        #,panel.border = element_rect(color="black", fill=NA)
        ) +
  facet_wrap(~year_label,
             nrow = 2, scale = "fixed")


map_all_facet

#ggplotly(map_all_facet)

ggsave("outputs/map_sentiment_2013_2019_facet.png", 
       map_all_facet, width = 10, height = 5)

