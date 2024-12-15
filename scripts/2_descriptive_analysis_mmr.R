# Script information ------------
#' Aim: Descriptive analysis of predicted annotated tweets for Brazil and vaccine uptake data
#' Author: Laura Espinosa
#' Date created: 26 July 2021
#' Date updated: 15 December 2024

# 2 - Import cleaned Twitter data ----------------
message("Importing annotated data")
## Sentiment predictions
df_clean <- read_csv('data/local/tweets_BR_PT_gpt_preprocessed.csv')

# Store tweets id -------
id_tweets <- df_clean %>% select(id)
id_tweets %>% write_csv("data/id_tweets_study.csv")

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

df_pop_year <- df_pop_clean %>% 
  mutate(year = year(rowname)) %>% 
  select(-rowname, -Total_pop) %>% 
  distinct(year, .keep_all = TRUE) %>% 
  pivot_longer(cols = starts_with("BR"),
               names_to = "LocationCode",
               values_to = "population") %>% 
  left_join(br[c('LocationCode', 'StatesCode')]) %>% 
  select(-LocationCode)

## d) Import vaccine coverage data ---------
### Import mmr coverage data 
load("./data/immunization_master_data.RData")

# https://www.synapse.org/Synapse:syn25148356/files/
df_mmr <- sipni_data %>% 
  filter(SCOPE == 3) %>% 
  filter(INDICATOR == "FL_Y1_MMR1") %>%
  filter(YEAR >= 2012 & YEAR <= 2019) %>% 
  select(LOCAL_NAME, YEAR, PC_COVERAGE) %>% 
  rename(StatesCode = LOCAL_NAME) %>% 
  left_join(br_states, by = "StatesCode") %>% 
  select(-StatesCode) %>% 
  rename(year = YEAR) %>% 
  left_join(br[c("LocationCode", "LocationName")], by = "LocationName")  

df_mmr_trend <- df_mmr %>% 
  group_by(LocationName) %>% 
  arrange(LocationName, year) %>% 
  mutate(trend_mmr = case_when(PC_COVERAGE > lag(PC_COVERAGE) ~ "increased",
                               PC_COVERAGE == lag(PC_COVERAGE) ~ "equal",
                               PC_COVERAGE < lag(PC_COVERAGE) ~ "decreased",
                               .default = NA_character_)) %>% 
  ungroup()
  


## f) Import measles cases per region and year ----------------
# https://www.gov.br/saude/pt-br/assuntos/saude-de-a-a-z/s/sarampo/situacao-epidemiologica/casos-confirmados-de-sarampo-brasil-grandes-regioes-e-unidades-federadas-1990-2024
df_measles_raw <- read_csv("data/measles_1990_2024.csv") %>% 
  left_join(br[c('LocationName', 'StatesCode')]) %>% 
  filter(!is.na(LocationName)) %>%  
  pivot_longer(cols = `1990`:`2024`,
               names_to = "year",
               values_to = "measles") 

df_measles <- df_measles_raw %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(df_pop_year, by = c('StatesCode', 'year')) %>% 
  filter(!is.na(population)) %>% 
  mutate(measles_rate = case_when(measles > 0 ~ round(measles/population * 100000, 1),
                                  measles == 0 ~ round(0, 0)))

df_measles_trend <- df_measles_raw %>% 
  group_by(LocationName) %>% 
  arrange(LocationName, year) %>% 
  mutate(trend_measles = case_when(measles > lag(measles) ~ "increased",
                               measles == lag(measles) ~ "equal",
                               measles < lag(measles) ~ "decreased",
                               .default = NA_character_)) %>% 
  ungroup()


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
#df_table_total <- bind_rows(df_table, overall_total)

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
#plotly_time <- ggplotly(plot_time) 

# Show interactive plot
#plotly_time


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
#plotly_time_geo <- ggplotly(plot_time_geo) 

# Show interactive plot
#plotly_time_geo

#plotly_time_mmr <- subplot(plotly_mmr, plotly_time, nrows = 2,
#                            heights = c(0.3, 0.7), shareX = TRUE,
#                            titleX = TRUE, titleY = TRUE) %>%
#  layout(yaxis = list(title = list(text = "MMR vaccine \nuptake (doses)", 
#                                    font = list(size = 12))),
#         yaxis2 = list(title = list( text = "Twitter vaccine sentiment", 
#                                    font = list(size = 12))),
#         xaxis = list(title = list(text = "Date (day, month and year)",
#                                   font = list(size = 12))),
#         hovermode = "x unified",
#         title = list(text = "MMR vaccine uptake and vaccine sentiment \n in Brazil, January 2013 to December 2019",
#                      size = 12))
#plotly_time_mmr

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

value_range_plot_states <- range(df_clean_geo_all_plot_df$sentiment, na.rm = TRUE)

plot_states <- df_clean_geo_all_plot_df %>% 
  #filter(LocationCode != "BR") %>% 
  filter(LocationNameFull != "Rio Grande Do Norte") %>% # Because there are no tweets
  filter(!is.na(LocationNameFull)) %>% 
  ggplot(aes(x = date, y = sentiment, colour = sentiment)) +
  geom_line(
    #colour = "dark green", 
    size = 0.5) +
  # geom_point(aes(x = date, y = sentiment,
  #                colour = sentiment_cat),
  #            size = 1) +
  #scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_color_gradientn(
    colors = c("#4B0000", "#FF6666", "#FFFFFF", "#6699FF", "#000033"),  
    limits = c(value_range_plot_states[1], value_range_plot_states[2]), 
    values = scales::rescale(c(value_range_plot_states[1], -0.01, 0, 0.01, value_range_plot_states[2])),
    name = "Twitter vaccine sentiment index (TVS)"
  ) +
  scale_x_datetime(date_breaks = "6 months", 
                   #date_minor_breaks = "1 month", 
                   date_labels = "%b-%Y",
                   limits = c(min(df_clean_geo_all$date), 
                              max(df_clean_geo_all$date))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", size=0.5) +
  #ggtitle('7-day moving average of Twitter vaccine sentiment index in Brazil, January 2013 to December 2019') +
  labs(x = "Date (day, month and year)", y = "TVS index") +
  #geom_smooth(method = "loess", se = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 10, hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 10)) +
  facet_wrap(vars(label_plot), 
             #scales = "free_y", 
             #ncol = 4
             ncol = 3)

# Show plot
plot_states

# save plot
ggsave("outputs/twitter_sentiment_br_states.png", plot_states,
      width = 10, height = 12)

# Summary of positives and negatives per state
trend_time_state <- df_clean_geo_all_plot_df %>% 
  group_by(LocationNameFull, color) %>% 
  tally() %>% 
  arrange(LocationNameFull, desc(n))

# Interactive plot (plotly)
#plotly_states <- ggplotly(plot_states) 

# Show interactive plot
#plotly_states

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

df_clean_geo_state_by_year_trend <- df_clean_geo_state_by_year %>% 
  #filter(LocationNameFull != " Brazil") %>% 
  select(year, sentiment, LocationCode) %>% 
  ungroup() %>% 
  group_by(LocationCode) %>% 
  arrange(LocationCode, year) %>% 
  mutate(trend_sentiment = case_when(sentiment > lag(sentiment) ~ "increased",
                               sentiment == lag(sentiment) ~ "equal",
                               sentiment < lag(sentiment) ~ "decreased",
                               .default = NA_character_)) %>% 
  ungroup()

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

ggsave("outputs/map_sentiment_2013_2019.png", map_all, width = 10, height = 5)


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
  
}

### All years in facet ----------
# data
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

# title labels
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


# map
value_range <- range(world_br_all_facet$sentiment)
extended_limits <- c(-0.2, max(world_br_all_facet$sentiment, na.rm = TRUE))

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
  #scale_fill_viridis_c(option = "B",
  #                     name = "Twitter vaccine \nsentiment index") +
  scale_fill_gradientn(
    colors = c("#4B0000", "#FF6666", "#FFDADA", "#6699FF", "#000033"),
    values = scales::rescale(c(value_range[1], -0.01, 0, 0.01, value_range[2])),
    #limits = extended_limits,
    breaks = c(-0.15, 0, 0.15, 0.30, 0.45),  
    #breaks = c(-0.2, 0, 0.2, 0.4),  
    #labels = c("-0.2", "0", "0.2", "0.4"),  # Format labels
    name = "Twitter vaccine \nsentiment index"
  ) +
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

### All years in facet (trend) --------------
world_br_all_facet_trend <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2) %>% 
  mutate(name_pt_case = str_to_title(name_pt)) %>%
  left_join(br[, c(2,5)],
            by = c("name_pt_case" = "LocationNameFull")) %>% 
  left_join(df_clean_geo_state_by_year_trend,
            by = c("LocationCode" = "LocationCode")) %>% 
  filter(!is.na(year)) 


map_all_facet_trend <- world_br_all_facet_trend %>% 
  # mutate(sentiment_cat = case_when(sentiment < -0.09104 ~ "< -0.09",
  #                                  sentiment > -0.09104 & sentiment < -0.05684 ~ "-0.09 to -0.06",
  #                                  sentiment > -0.05684 & sentiment < -0.04495 ~ "-0.06 to -0.04",
  #                                  TRUE ~ "> -0.04")) %>%
  mutate(year_label = case_when(year == 2013 ~ paste0("  ",2013, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2013$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2014 ~ paste0(2014, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2014$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2015 ~ paste0(2015, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2015$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2016 ~ paste0(2016, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2016$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2017 ~ paste0(2017, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2017$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2018 ~ paste0(2018, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2018$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2019 ~ paste0(2019, " ", "(n=", 
                                                      format(sum(df_clean_geo_state_year_2019$tweets),
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                TRUE ~ "test")) %>% 
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = trend_sentiment), color = "black") +
  scale_fill_manual(name = "(A) Trend of Twitter vaccine sentiment index",
                    values = c("#a50f15", "#0868ac")) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        title = element_text(size = 14),
        strip.text = element_text(size=14),
        plot.background = element_rect(fill = "white",
                                       colour = "white")
        #,panel.border = element_rect(color="black", fill=NA)
  ) +
  facet_wrap(~year_label,
             nrow = 1, scale = "fixed")


map_all_facet_trend

#ggplotly(map_all_facet)

ggsave("outputs/map_trend_sentiment_2013_2019_facet.png", 
       map_all_facet_trend, width = 10, height = 5)



## Maps for measles cases -----------
# Create a continuous color scale with white for zeros
viridis_colors <- viridis::viridis(193, option = "B")
# Add white at the beginning of the color scale
class_colors <- c("white","#bae4b3", "#74c476", 
                  "#31a354", "#006d2c")

df_measles_year <- df_measles_raw %>% 
  group_by(year) %>% 
  summarise(measles = sum(measles))

# Plot map
quartiles <- df_measles %>% 
  filter(year >= 2013 & year <= 2019) %>% 
  filter(measles != 0) %>% 
  select(measles) %>% 
  quantile(probs = seq(0, 1, by = 0.25), na.rm = TRUE)

map_measles <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2)  %>% 
  mutate(name_pt_case = str_to_title(name_pt)) %>% 
  left_join(br,
            by = c("name_pt_case" = "LocationNameFull")) %>% 
  left_join(df_measles,
            by = c("StatesCode", "LocationName"))  %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(measles_class = case_when(
    measles == 0 ~ "0",
    TRUE ~ cut(measles, 
               breaks = quartiles, 
               include.lowest = TRUE, 
               labels = c(
                 paste0("Quartile 1 (", round(quartiles[1], 2), ", ", round(quartiles[2], 2), "]"),
                 paste0("Quartile 2 (", round(quartiles[2], 2), ", ", round(quartiles[3], 2), "]"),
                 paste0("Quartile 3 (", round(quartiles[3], 2), ", ", round(quartiles[4], 2), "]"),
                 paste0("Quartile 4 (", round(quartiles[4], 2), ", ", round(quartiles[5], 2), "]")
               ))
  )) %>% 
  filter(!is.na(measles_class)) %>% 
  filter(year >= 2013 & year <= 2019) %>% 
  mutate(year_label = case_when(year == 2013 ~ paste0("Jan-Dec ", 2013, 
                                                      #" ", 
                                                      # "(n=", 
                                                      # format(df_measles_year[24,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                year == 2014 ~ paste0("Jan-Dec ", 2014, 
                                                      # " ", "(n=", 
                                                      # format(df_measles_year[25,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                year == 2015 ~ paste0("Jan-Dec ", 2015, 
                                                      # " ", "(n=", 
                                                      # format(df_measles_year[26,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                year == 2016 ~ paste0("Jan-Dec ", 2016, 
                                                      # " ", "(n=", 
                                                      # format(df_measles_year[27,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                year == 2017 ~ paste0("Jan-Dec ", 2017, 
                                                      # " ", "(n=", 
                                                      # format(df_measles_year[28,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                year == 2018 ~ paste0("Jan-Dec ", 2018, 
                                                      # " ", "(n=", 
                                                      # format(df_measles_year[29,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                year == 2019 ~ paste0("Jan-Dec ", 2019, 
                                                      # " ", "(n=", 
                                                      # format(df_measles_year[30,2][[1]],
                                                      #        big.mark = ","),
                                                      # ")", 
                                                      sep = ""),
                                TRUE ~ "test")) %>% 
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = measles_class), color = "black") +
  #scale_fill_viridis_c(option = "B",
  #                     name = "Measles cases") +
  scale_fill_manual(values = class_colors, name = "Measles cases per \n100,000 population") +
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
  facet_wrap(~year_label, nrow = 2, scale = "fixed")

map_measles

ggsave("outputs/map_measles_2013_2019_facet.png", 
       map_measles, width = 10, height = 5)

### Maps for trends of measles cases -----------
map_measles_trend <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2)  %>% 
  mutate(name_pt_case = str_to_title(name_pt)) %>% 
  left_join(br,
            by = c("name_pt_case" = "LocationNameFull")) %>% 
  left_join(df_measles_trend,
            by = c("StatesCode", "LocationName"))  %>% 
  mutate(year = as.numeric(year)) %>%
  #filter(!is.na(measles_class)) %>% 
  filter(year >= 2013 & year <= 2019) %>% 
  mutate(year_label = case_when(year == 2013 ~ paste0(2013, " ", "(n=", 
                                                      format(df_measles_year[24,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2014 ~ paste0(2014, " ", "(n=", 
                                                      format(df_measles_year[25,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2015 ~ paste0(2015, " ", "(n=", 
                                                      format(df_measles_year[26,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2016 ~ paste0(2016, " ", "(n=", 
                                                      format(df_measles_year[27,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2017 ~ paste0(2017, " ", "(n=", 
                                                      format(df_measles_year[28,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2018 ~ paste0(2018, " ", "(n=", 
                                                      format(df_measles_year[29,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                year == 2019 ~ paste0(2019, " ", "(n=", 
                                                      format(df_measles_year[30,2][[1]],
                                                             big.mark = ","),
                                                      ")", sep = ""),
                                TRUE ~ "test")) %>% 
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = trend_measles), color = "black") +
  #scale_fill_viridis_c(option = "B",
  #                     name = "Measles cases") +
  scale_fill_manual(name = "(C) Trend of measles cases per 100,000 population",
                      values = c("#0868ac", "white", "#a50f15")) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        title = element_text(size = 14),
        strip.text = element_text(size=14),
        plot.background = element_rect(fill = "white",
                                       colour = "white")
        #,panel.border = element_rect(color="black", fill=NA)
  ) +
  facet_wrap(~year_label, nrow = 1, scale = "fixed")

map_measles_trend

ggsave("outputs/map_trend_measles_2013_2019_facet.png", 
       map_measles_trend, width = 10, height = 5)

## Maps for mmr coverage -----------

# Plot map
map_mmr <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2)  %>% 
  left_join(br,
            by = c("name_pt" = "LocationNameFull")) %>% 
  left_join(df_mmr,
            by = c("LocationName"))  %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2013) %>% 
  filter(!is.na(PC_COVERAGE)) %>% 
  mutate(name_pt_case = str_to_title(name_pt),
         coverage_class = case_when(PC_COVERAGE < 80 ~ " < 80%",
                                    PC_COVERAGE >= 80 & PC_COVERAGE < 90 ~ " 80-89%",
                                    PC_COVERAGE >= 90 & PC_COVERAGE <95 ~ " 90-94%",
                                    PC_COVERAGE >= 95 & PC_COVERAGE <=100 ~ " 95-100%",
                                    PC_COVERAGE > 100 ~ ">100%",
                                    .default = NA)) %>% 
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = coverage_class), color = "black") +
  # scale_fill_viridis_c(option = "B",
  #                      name = "MMR vaccination coverage") +
  scale_fill_manual(name = "MMR vaccination \ncoverage",
                       values = c("#f2f0f7","#cbc9e2", "#9e9ac8",
                                  "#756bb1", "#54278f")) +
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
  facet_wrap(~year, nrow = 2, scale = "fixed")

map_mmr

ggsave("outputs/map_mmr_coverage_facet.png", 
       map_mmr, width = 10, height = 5)

### Map for trend of mmr coverage -------------
map_mmr_trend <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2)  %>% 
  left_join(br,
            by = c("name_pt" = "LocationNameFull")) %>% 
  left_join(df_mmr_trend,
            by = c("LocationName"))  %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2013) %>% 
  filter(!is.na(trend_mmr)) %>% 
  ggplot() +
  geom_sf(data = world_clipped, fill = "gray90", color = "gray") +  # Adding this line plots the whole world
  geom_sf(aes(fill = trend_mmr), color = "black") +
  # scale_fill_viridis_c(option = "B",
  #                      name = "MMR vaccination coverage") +
  scale_fill_manual(name = "(B) Trend of MMR vaccination coverage",
                    values = c("#a50f15", "#0868ac")) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        title = element_text(size = 14),
        strip.text = element_text(size=14),
        plot.background = element_rect(fill = "white",
                                       colour = "white")
        #,panel.border = element_rect(color="black", fill=NA)
  ) +
  facet_wrap(~year, nrow = 1, scale = "fixed")

map_mmr_trend

ggsave("outputs/map_trend_mmr_coverage_facet.png", 
       map_mmr_trend, width = 10, height = 5)

## Comparative maps of trends ---------
plot_arrange <- lapply(list(map_all_facet_trend, 
                            map_mmr_trend,
                            map_measles_trend), 
                       function(p) p+theme(plot.background = element_rect(colour = "black")))

map_trend_all <- ggarrange(plotlist = plot_arrange,
                           #labels = c("(A)", "(B)", "(C)"),
                           vjust = 2.2, hjust = 0, 
                           ncol = 1, nrow = 3)
  
map_trend_all  

ggsave("outputs/map_trend_all.png", 
       map_trend_all, width = 15, height = 8)
