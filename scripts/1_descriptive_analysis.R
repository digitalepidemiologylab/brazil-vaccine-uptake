# Script information ------------
#' Aim: Descriptive analysis of predicted annotated tweets for Brazil and vaccine uptake data
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
p_load(caTools, data.table, DataExplorer, ggpubr, ggspatial, graphics, 
       gtsummary, janitor, lubridate, padr, plotly, readr, readxl, 
       rnaturalearth, stats, tidyverse, tools, utils, zoo)

# 2 - Import cleaned Twitter data ----------------
message("Importing Twitter data")
## Steps 2-5 can be omitted
## Sentiment predictions
df_clean <- read_csv('data/local/tweets_with_predicted_labels_filtered_BR_PT.csv')
df_clean_geo <- read_csv('data/local/tweets_with_predicted_labels_filtered_BR_PT_geo.csv')

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

# Clean and write dtp data
df_dtp <- df_dtp %>%
  mutate(year = year(Date),
         Total = rowSums(.[-1]))

for (i in 2:29) {
  df_dtp[[i]] <- round((df_dtp[[i]]/df_pop_clean[[i]]) * 100000, digits = 2)
}

df_dtp <- df_dtp %>%
  mutate(year_month = format(Date, "%Y-%m"))


write.csv(df_dtp, "data/uptake_br_dtp_2013_2019_clean.csv", row.names = FALSE)

### Import clean dtp data
df_dtp <- read_csv("data/uptake_br_dtp_2013_2019_clean.csv")

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

write_csv(world_bank_clean, "data/worldbank_br_clean.csv")

# 4 - Descriptive analysis --------------
message("Running the descriptive analysis")
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

## Time series plots ---------------

### Time series for sentiment without geo filtering ------------
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


### Time series for sentiment with geo filtering ------------
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
  ggtitle('7-day moving average of Twitter vaccine sentiment index in Brazil, \nJanuary 2013 to December 2019') +
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

### Time series for DTP vaccine uptake --------------
mean_hline <- mean(df_dtp$Total)

plot_dtp <- df_dtp %>% 
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
plot_dtp
ggsave("outputs/dtp_uptake_br.png", plot_dtp)

# Interactive plot
plotly_dtp <- ggplotly(plot_dtp)

# Show interactive plot
plotly_dtp

### Time series for DTP vaccine uptake and sentiment without geo filtering ----------------
hlines <- data.frame(X = c("DTP monthly uptake per 100,000 population", 
                           "Twitter vaccine sentiment"),
                     Z = c(mean(df_dtp$Total), 0))
hlines$X <- factor(hlines$X)

plot_time_dtp <- sentiment_time_horiz_allgeo[, c(1,6)] %>%
  mutate(year_month = format(created_at_h, "%Y-%m")) %>% 
  left_join(df_dtp[,c(31,29)]) %>%
  select(-year_month) %>% 
  setNames(c('Date', 'Twitter vaccine sentiment', 'DTP monthly uptake per 100,000 population')) %>% 
  gather(key = "Indicator",
         value = "value",
         -Date) %>% 
  ggplot(aes(x = as.Date(Date), y = value,
             color = Indicator)) +
  scale_color_manual(values = c("blue", "orange")) +
  geom_line() +
  scale_x_date(expand = c(0,0),
               date_breaks = "3 months",
               date_labels = "%b %Y") +
  scale_y_continuous() +
  ggtitle("Diphtheria-Tetatus-Pertussis (DTP) vaccine uptake per 100,000 population \nand 7-day moving average of Twitter vaccine sentiment index \nin Brazil, January 2013 to December 2019 (n = 2,197,090)") +
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
  

plot_time_dtp


ggsave('outputs/twitter_sentiment_dtp_br.png', plot_time_dtp,
       width = 12, height = 6)
  

plotly_time_dtp_ <- ggplotly(plot_time_dtp)

plotly_time_dtp_


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

# 5 - Data analysis by Brazilian states -----------------
message("Running data analysis by Brazilian states")
## Assign LocationCode to tweets---------------
df_clean_geo_raw <- df_clean_geo

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
  group_by(date=floor_date(created_at, "1 day"), LocationCode, label) %>%
  tally() %>% 
  spread(label, n) %>%
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
  geom_sf(aes(fill = world_br_all$sentiment)) +
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
                                        colour = "white"))


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
    geom_sf(aes(fill = data_plot$sentiment)) +
    scale_fill_viridis_c(option = "B",
                         name = "Twitter vaccine \nsentiment index") +
    # scale_fill_gradient(limits = c(0, -0.35),
    #                     #breaks = c(-0.09104, 0.05684, 0.04495)
    #                     breaks = c(0.04495, 0.05684, -0.09104)) +
    #scale_fill_gradient2(midpoint = 0) +
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    # coord_sf(xlim = c(-72.9872354804, -33.7683777809), 
    #          ylim = c(-34.7299934555, 5.24448639569 )) +
    labs(title = paste('Twitter vaccine sentiment index per \nBrazilian state, January ', 
                       i, ' to \nDecember ', i, ' (n = ', 
                       format(sum(df_clean_geo_state_plot$tweets), big.mark = ','),
                       ')', sep = "")) +
    theme_void() +
    theme(legend.position = c(1, 0.4),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          title = element_text(size = 14),
          plot.background = element_rect(fill = "white",
                                         colour = "white"))
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
  geom_sf(aes(fill = sentiment)) +
  scale_fill_viridis_c(option = "B",
                       name = "Twitter vaccine \nsentiment index") +
  #scale_fill_gradient2(midpoint = 0) +
  #annotation_scale(location = "br", width_hint = 0.3) +
  # annotation_north_arrow(location = "br", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  # coord_sf(xlim = c(-72.9872354804, -33.7683777809), 
  #          ylim = c(-34.7299934555, 5.24448639569 )) +
  labs(title = 'Twitter vaccine sentiment index per Brazilian state and year (n = 1,750,294)') +
  theme_void() +
  theme(legend.position = c(0.85, 0.3),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        title = element_text(size = 14),
        strip.text = element_text(size=14),
        plot.background = element_rect(fill = "white",
                                       colour = "white"),
        panel.border = element_rect(color="black", fill=NA)) +
  facet_wrap(~year_label,
             nrow = 2, scale = "fixed")


map_all_facet

ggplotly(map_all_facet)

ggsave("outputs/map_sentiment_2013_2019_facet.png", 
       map_all_facet, width = 10, height = 5)

