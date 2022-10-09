# Script information ------------
#' Aim: Descriptive analysis of predicted annotated tweets for Brazil and vaccine uptake data
#' Author: Laura Espinosa
#' Date created: 26 July 2021
#' Date updated: 22 August 2022

# 1 - Packages ----------------------
# install/load "pacman" to help installing and loading other packages
while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# load packages
p_load(tidyverse, readr, lubridate, plotly, slider, zoo, ggpubr, caTools, gtsummary,
       padr, forecast, gridExtra, maps, ggforce, geobr, rnaturalearth,
       ggmap, sf, rnaturalearth, rnaturalearthdata, ggspatial)

# 2 - Import data --------------------
# This csv is a copy of the file '/drives/sdd/vaccine_paho_project/vaccine_classification/data/predict_data/precovid_paho_290621_sentiment/all_features_predictions_2021-07-13_09-40-16_018009/tweets_with_predicted_labels_2013-01-01_to_2019-12-31.csv'
# The below csv is not included in the Github repo; the path should be changed accordingly
df <- read_csv('data/big_files/tweets_with_predicted_labels_2013-01-01_to_2019-12-31.csv')

df_an_all <- read_csv('data/cleaned_labels_min-labels-cutoff-3_majority_all.csv')
df_an_mturk <- read_csv('data/cleaned_labels_min-labels-cutoff-3_majority_mturk.csv')
df_an_local <- read_csv('data/cleaned_labels_min-labels-cutoff-3_majority_local.csv')
df_an_public <- read_csv('data/cleaned_labels_min-labels-cutoff-3_majority_public.csv')


# 3 - Clean data ------------------
## Annotations
df_an_all$annotation <- "all"
df_an_mturk$annotation <- "mturk"
df_an_local$annotation <- "local"
df_an_public$annotation <- "public"

df_an <- rbind(df_an_mturk, df_an_local, df_an_public) %>% 
  filter(question_tag == "sentiment")

df_an_all_clean <- df_an_all %>% 
  filter(question_tag == "sentiment")

## Subset with all tweets from Brazil in Portuguese
df_clean <- df %>%  # Select dataframe
  filter(lang == "pt", # Select Portuguese tweets
         country_code == "BR") %>%  #Select tweets from Brazil
         #is_retweet==FALSE) # Discard retweets (this is currently not applied)
  # Alvorada had wrong latitude
  mutate(latitude = case_when(latitude == 29.9904 ~ -29.9904,
                              TRUE ~ latitude))

## Specific filtering according to geo_type
df_clean_geo1 <- df_clean %>% 
  filter(geo_type == 1)

df_clean_geo2 <- df_clean %>% 
  filter(geo_type == 2) %>% 
  filter(location_type != 'country')

df_clean_geo3 <- df_clean %>% 
  filter(geo_type == 3) %>% 
  filter(location_type == 'city' | 
           location_type == 'place' |
           location_type == 'admin1'|
           location_type == 'admin2'| 
           location_type == 'admin3' |
           location_type == 'admin4' |
           location_type == 'admin5' |
           location_type == 'admin6'|
           location_type == 'admin_other')

### Merging three dataframes
df_clean_geo <- rbind(df_clean_geo1, df_clean_geo2, df_clean_geo3) %>% 
  mutate(latitude = case_when(latitude == 29.9904 ~ -29.9904,
                              TRUE ~ latitude))

# 4 - Save cleaned data -----------------
write_csv(df_clean, 'data/tweets_with_predicted_labels_filtered_BR_PT.csv')
write_csv(df_clean_geo, 'data/tweets_with_predicted_labels_filtered_BR_PT_geo.csv')

# 5 - Import cleaned data ----------------
## Steps 2-5 can be omitted
## Sentiment predictions
df_clean <- read_csv('data/big_files/tweets_with_predicted_labels_filtered_BR_PT.csv')
df_clean_geo <- read_csv('data/big_files/tweets_with_predicted_labels_filtered_BR_PT_geo.csv')

# 6 - Select coverage data -------------
# Vaccine uptake data from: http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/dpnibr.def
# Vaccination coverage from: http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/cpnibr.def
## DTP vaccine uptake in Brazil by states
df_dtp_raw <- read_csv("data/uptake_br_dtp_2013_2019.csv")
df_dtp <- df_dtp_raw
df_dtp$Date <- as.Date(paste0(df_dtp$Date, "/01"), format = "%Y/%b/%d") # correct date format 

### Complete missing dates with NA
df_dtp <- df_dtp %>% 
  rbind(c("2019-12-31", rep(NA, 41))) %>% # add last date
  pad(interval = "day") %>%  # fill missing dates
  fill(colnames(df_dtp), .direction = "down") # fill NA with previous values 


for (i in 2:29) {
  df_dtp[[i]] <- as.numeric(df_dtp[[i]])
}

write.csv(df_dtp, "data/uptake_br_dtp_2013_2019_clean.csv", row.names = FALSE)

# 7 - Descriptive analysis --------------
## Annotations ---------------
names(df_an_all_clean)

length(unique(df_an_all_clean$id)) # number of unique tweets

table(df_an$annotation)

## Twitter data ------------------
names(df_clean)
table(df_clean$label)
summary(df_clean$created_at)

### Table for exporting in publication format
#### Number of tweets per year by label
df_table <- df_clean %>% 
  # Create year and month bariables
  mutate(year = year(created_at)) %>% 
  mutate(month = month(created_at)) %>% 
  select(year, label) %>% 
  tbl_summary(by = year, digits = list(label ~ c(1,1)))
df_table

#### Summary of selected variables
df_summmary <- df_clean %>% 
  # Create year and month variables
  mutate(year = year(created_at)) %>% 
  mutate(month = month(created_at)) %>% 
  select(year, label) %>% 
  tbl_summary()
df_summmary 

#### Figure

# 8 - Time series plots ---------------

## Time series for sentiment without geo filtering ------------
sentiment_time_allgeo <- df_clean %>%
  group_by(created_at, label) %>%
  tally() 

sentiment_time_horiz_allgeo <- sentiment_time_allgeo %>%
  group_by(created_at_h=floor_date(created_at, "1 day"), label) %>%
  tally() %>% 
  spread(label, n) %>%
  select(created_at_h, positive, neutral, negative) %>% 
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  mutate(sentiment_raw = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative)) 

sentiment_time_horiz_allgeo$sentiment <- runmean(sentiment_time_horiz_allgeo$sentiment_raw, k = 7, alg = 'exact', endrule = 'keep')

plot_time <- sentiment_time_horiz_allgeo[, c(1,6)] %>% 
  ggplot(aes(x = created_at_h, y = sentiment)) +
  geom_line(colour = "dark green", size = 0.7) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_datetime(date_breaks = "2 months", 
                   date_minor_breaks = "1 month", 
                   date_labels = "%d %b %Y",
                   limits = c(min(sentiment_time_horiz_allgeo$created_at_h), 
                              max(sentiment_time_horiz_allgeo$created_at_h))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", size=0.5) +
  ggtitle('7-day moving average of Twitter vaccine sentiment index \nin Brazil, January 2013 to December 2019 (n = 2,197,090)') +
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


## Time series for sentiment with geo filtering ------------
sentiment_time <- df_clean_geo %>%
  group_by(created_at, label) %>%
  tally() 

sentiment_time_horiz <- sentiment_time %>%
  group_by(created_at_h=floor_date(created_at, "1 day"), label) %>%
  tally() %>% 
  spread(label, n) %>%
  select(created_at_h, positive, neutral, negative) %>% 
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  mutate(sentiment_raw = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative)) 

sentiment_time_horiz$sentiment <- runmean(sentiment_time_horiz$sentiment_raw, k = 7, alg = 'exact', endrule = 'keep')

plot_time_geo <- sentiment_time_horiz[, c(1,6)] %>% 
  ggplot(aes(x = created_at_h, y = sentiment)) +
  geom_line(colour = "dark green") +
  #scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_datetime(date_breaks = "2 months", 
                   date_minor_breaks = "1 month", 
                   date_labels = "%d-%m-%Y",
                   limits = c(min(sentiment_time_horiz$created_at_h), 
                              max(sentiment_time_horiz$created_at_h))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", size=0.5) +
  ggtitle('Vaccination coverage in Brazil, \nJanuary 2013 to December 2019') +
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

## Time series for DTP vaccine uptake --------------
plot_dtp <- df_dtp %>% 
  ggplot(aes(y = Total, x = Date)) +
  geom_line() +
  labs(x = "Date (month, year)", y = "Vaccine uptake \n(number of doses)") +
  #geom_smooth(method = "loess", se = FALSE) +
  scale_x_date(expand = c(0,0),
                   date_breaks = "6 months",
                   date_labels = "%d %b %Y") +
  scale_y_continuous() +
  ggtitle("Diphtheria-Tetatus-Pertussis \nvaccination uptake in Brazil, 2013 to 2019") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0)) 

# Show and save plot  
plot_dtp
ggsave("outputs/dtp_uptake_br.png", plot_dtp)

# Interactive plot
plotly_dtp <- ggplotly(plot_dtp)

# Show interactive plot
plotly_dtp

## Time series for DTP vaccine uptake and sentiment without geo filtering ----------------
plot_time_dtp <- sentiment_time_horiz_allgeo[, c(1,6)] %>% 
  left_join(df_dtp[,c(1,29)], by = c("created_at_h" = "Date")) %>% 
  setNames(c('Date', 'Sentiment', 'DTP uptake')) %>% 
  gather(key = "Indicator",
         value = "value",
         -Date) %>% 
  ggplot(aes(x = as.Date(Date), y = value)) +
  geom_line() +
  scale_x_date(expand = c(0,0),
               date_breaks = "6 months",
               date_labels = "%b %Y") +
  scale_y_continuous() +
  ggtitle("Diphtheria-Tetatus-Pertussis vaccination uptake and 
          Twitter vaccine sentiment in Brazil, 2013 to 2019") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0, 
                                   hjust = 0),
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18),
        axis.title = element_blank()) +
  facet_wrap(~Indicator,
           ncol = 1, scales = "free_y")
  
  
plot_time_dtp

ggsave('outputs/twitter_sentiment_dtp_br.png', plot_time_dtp,
       width = 10, height = 6)

plotly_time_dtp <- subplot(plotly_dtp, plotly_time, nrows = 2,
                            heights = c(0.3, 0.7), shareX = TRUE,
                            titleX = TRUE, titleY = TRUE) %>%
  layout(yaxis = list(title = list(text = "DTP vaccine \nuptake (doses)", 
                                    font = list(size = 12))),
         yaxis2 = list(title = list( text = "Twitter vaccine sentiment", 
                                    font = list(size = 12))),
         xaxis = list(title = list(text = "Date (day, month and year)",
                                   font = list(size = 12))),
         hovermode = "x unified",
         title = list(text = "DTP vaccine uptake and vaccine sentiment \n in Brazil, January 2013 to December 2019",
                      size = 12))
plotly_time_dtp

# 9 - Data analysis by Brazilian states -----------------
## Import Brazil states boundary boxes -------------
br <- read_csv('data/brazil_states_boxes.csv')
# Last four states correspond to Brazilian islands

br_states <- data.frame(LocationName = c(unique(br$LocationName)),
                        StatesCode = c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT', 
                                            'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
                                            'RR', 'SC', 'SP', 'SE', 'TO', 'UNK'))
br <- right_join(br, br_states, by = 'LocationName')

## Assign LocationCode to tweets---------------
## Interim solution since for loop is not giving expected results
df_clean_geo <- df_clean_geo %>% 
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
df_clean_geo_raw <- df_clean_geo

df_clean_geo <- br %>% 
  select(LocationCode, LocationName, StatesCode) %>% 
  right_join(df_clean_geo, by = c('LocationCode' = 'LocationCode'))

# Function to include LocationCode
case_when_br <- function(df, geo_df, br_state){
  df_clean_geo <- df
  br <- geo_df
  i <- br_state
  df_clean_geo <- df_clean_geo %>% 
    mutate(LocationCode = case_when(latitude >= br$Latitude_ymin[i] &
                                      latitude <= br$Latitude_ymax[i] &
                                      longitude >= br$Longitude_xmin[i] &
                                      longitude <= br$Longitude_xmax[i] ~
                                      br$LocationCode[i],
                                    TRUE ~ 'NA'))
}

case_when_br(df_clean_geo, br, 20)

# Check that df_clean_geo has LocationCode
unique(df_clean_geo$LocationCode)

# Save data as csv
write_csv(df_clean_geo, 'data/big_files/tweets_with_predicted_labels_filtered_BR_PT_geo_final.csv')

## Group data per Brazilian state -----------------------
df_clean_geo_state <- df_clean_geo %>%
  filter(!is.na(LocationCode)) %>% 
  mutate(year = year(created_at),
         month = month(created_at),
         week = week(created_at)) %>% 
  group_by(date=floor_date(created_at, "1 day"), StatesCode, label) %>%
  tally() %>% 
  spread(label, n) %>%
  #select(date, LocationCode, positive, neutral, negative) %>% 
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  mutate(sentiment_raw = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative),
         year = year(date))  %>% 
  rename(StatesCode = StatesCode)

# Add rolling average sentiment
df_clean_geo_state$sentiment <- runmean(df_clean_geo_state$sentiment_raw, k = 20, alg = 'exact', endrule = 'keep')  

# Add names of Brasilian states
df_clean_geo_state <- full_join(df_clean_geo_state, br[,c(4, 12)], by = 'StatesCode')

## Separate Brazilian state data per year
df_clean_geo_state_year <- df_clean_geo_state %>% 
  group_by(year) %>% 
  mutate(tweets = positive + neutral + negative) %>% 
  select(-sentiment_raw)

write.csv(df_clean_geo_state_year, 'data/tweets_with_predicted_labels_state_year.csv')

## Time series analysis per label, state and date --------------------------
df_clean_geo_state$color <- ifelse(df_clean_geo_state$sentiment < 0,
                                   "negative",
                                   ifelse(df_clean_geo_state$sentiment > 0,
                                          "positive",
                                          "neutral"))

plot_states <- df_clean_geo_state %>% 
  # mutate(sentiment_cat = case_when(sentiment > 0 ~ "Positive",
  #                                  sentiment < 0 ~ "Negative",
  #                                  TRUE ~ "Neutral")) %>% 
  filter(!is.na(LocationCode)) %>% 
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(colour = "dark green") +
  # geom_point(aes(x = date, y = sentiment,
  #                colour = sentiment_cat),
  #            size = 1) +
  #scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_datetime(date_breaks = "6 months", 
                   #date_minor_breaks = "1 month", 
                   date_labels = "%b-%Y",
                   limits = c(min(df_clean_geo_state$date), 
                              max(df_clean_geo_state$date))) +
  geom_hline(yintercept=0,  linetype = "dashed", color = "red", size=0.5) +
  ggtitle('7-day moving average of Twitter vaccine sentiment index in Brazil, January 2013 to December 2019') +
  labs(x = "Date (day, month and year)", y = "Vaccine sentiment index") +
  #geom_smooth(method = "loess", se = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  facet_wrap(vars(LocationCode), scales = "free_y", ncol = 4)

# Show plot
plot_states

# Interactive plot (plotly)
plotly_states <- ggplotly(plot_states) 

# Show interactive plot
plotly_states

# Maps in R ----------------
## Select data for maps -----------------------

### Group data for all years per state ----------
df_clean_geo_state_year_all <- df_clean_geo_state_year %>% 
  select(StatesCode, negative, neutral, positive) %>% 
  group_by(StatesCode) %>% 
  summarise(Negative = sum(negative),
            Neutral = sum(neutral),
            Positive = sum(positive)) %>% 
  mutate(sentiment = (((1*Positive)+(-1 * Negative))/(Positive+Negative+Neutral)),
         tweets = Positive + Neutral + Negative)

# Save csv
write.csv(df_clean_geo_state_year_all, 'data/tweets_with_predicted_labels_state_year_all.csv')

### Group data by year and location and save csv ----------------
df_clean_geo_state_by_year <- df_clean_geo_state_year %>% 
  select(StatesCode, negative, neutral, positive) %>% 
  group_by(StatesCode, year) %>% 
  summarise(negative = sum(negative),
            neutral = sum(neutral),
            positive = sum(positive)) %>% 
  mutate(sentiment = (((1*positive)+(-1 * negative))/(positive+negative+neutral)),
         tweets = positive + neutral + negative)

write.csv(df_clean_geo_state_by_year, 'data/tweets_with_predicted_labels_by_state_year.csv')

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
write.csv(df_clean_geo_state_year_2019, 'data/tweets_with_predicted_labels_state_year_2019.csv')
write.csv(df_clean_geo_state_year_2018, 'data/tweets_with_predicted_labels_state_year_2018.csv')
write.csv(df_clean_geo_state_year_2017, 'data/tweets_with_predicted_labels_state_year_2017.csv')
write.csv(df_clean_geo_state_year_2016, 'data/tweets_with_predicted_labels_state_year_2016.csv')
write.csv(df_clean_geo_state_year_2015, 'data/tweets_with_predicted_labels_state_year_2015.csv')
write.csv(df_clean_geo_state_year_2014, 'data/tweets_with_predicted_labels_state_year_2014.csv')
write.csv(df_clean_geo_state_year_2013, 'data/tweets_with_predicted_labels_state_year_2013.csv')

## Plot map -------------
#devtools::install_github("ropensci/rnaturalearthhires")
world <- ne_states(returnclass = "sf")

world_br <- world %>% 
  filter(sov_a3 == "BRA") %>% 
  arrange(iso_3166_2)

world_br$sentiment <- df_clean_geo_state_year_all_RN$sentiment


map <- world_br %>% 
  mutate(sentiment_cat = case_when(sentiment < -0.09104 ~ "< -0.09",
                                   sentiment > -0.09104 & sentiment < -0.05684 ~ "-0.09 to -0.06",
                                   sentiment > -0.05684 & sentiment < -0.04495 ~ "-0.06 to -0.04",
                                   TRUE ~ "> -0.04")) %>% 
ggplot() +
  geom_sf(aes(fill = world_br$sentiment)) +
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
        title = element_text(size = 14))


map

# https://mran.microsoft.com/snapshot/2020-01-29/web/packages/geobr/vignettes/intro_to_geobr.html

# CODE UNDER REVIEW, NOT FINAL ------------------

# Map in R ---------------------
mymap <- maps::map("world", fill=TRUE, col="white", bg="white", ylim=c(-60, 90), mar=c(0,0,0,0))
points(df_clean_2019$Long, df_clean_2019$Lat, col = "red", cex = 0.2)


## Using brazilmaps------------------
states <- get_brmap(geo = c('State'),
                    #geo.filter = list(State != 0),
                    class = 'sf')
plot_brmap(states)
View(states)

# Sentiment score per geoname id -----------------
sentiment_geo <- df_clean %>%
  group_by(geoname_id, label) %>%
  tally() %>%
  spread(label, n) %>%
  select(geoname_id, positive, neutral, negative) %>%
  replace_na(list(positive = 0, neutral = 0, negative = 0)) %>%
  mutate(sentiment = ((1 * positive) + (0 * neutral) + (-1 * negative))/ (positive + neutral + negative),
         sentiment2 = ((1 * positive) + (-1 * negative))/ (positive + negative),
         sent_average = (1 * positive) + (0 * neutral) + (-1 * negative))

# Check new variable
hist(sentiment_geo$sentiment)
summary(sentiment_geo$sentiment)
hist(sentiment_geo$sentiment2)
summary(sentiment_geo$sentiment2)

