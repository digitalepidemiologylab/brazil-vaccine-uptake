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

## 2017 -------------
df_clean_2017 <- df_clean %>% 
  filter(year(created_at) == 2017) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2017_pos <- df_clean_2017 %>% 
  filter(sent_gpt == "positive")
df_clean_2017_neg <- df_clean_2017 %>% 
  filter(sent_gpt == "negative")
df_clean_2017_neu <- df_clean_2017 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2017_pos$year_week)
weeks_neg <- unique(df_clean_2017_neg$year_week)
weeks_neu <- unique(df_clean_2017_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2017_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2017$year_month)) {
  df_pos_month <- df_clean_2017_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2017$year_month)) {
  df_neg_month <- df_clean_2017_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2017$year_month)) {
  df_neu_month <- df_clean_2017_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}

## 2016 -------------
df_clean_2016 <- df_clean %>% 
  filter(year(created_at) == 2016) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2016_pos <- df_clean_2016 %>% 
  filter(sent_gpt == "positive")
df_clean_2016_neg <- df_clean_2016 %>% 
  filter(sent_gpt == "negative")
df_clean_2016_neu <- df_clean_2016 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2016_pos$year_week)
weeks_neg <- unique(df_clean_2016_neg$year_week)
weeks_neu <- unique(df_clean_2016_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2016_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2016$year_month)) {
  df_pos_month <- df_clean_2016_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2016$year_month)) {
  df_neg_month <- df_clean_2016_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2016$year_month)) {
  df_neu_month <- df_clean_2016_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}

## 2015 -------------
df_clean_2015 <- df_clean %>% 
  filter(year(created_at) == 2015) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2015_pos <- df_clean_2015 %>% 
  filter(sent_gpt == "positive")
df_clean_2015_neg <- df_clean_2015 %>% 
  filter(sent_gpt == "negative")
df_clean_2015_neu <- df_clean_2015 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2015_pos$year_week)
weeks_neg <- unique(df_clean_2015_neg$year_week)
weeks_neu <- unique(df_clean_2015_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2015_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2015$year_month)) {
  df_pos_month <- df_clean_2015_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2015$year_month)) {
  df_neg_month <- df_clean_2015_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2015$year_month)) {
  df_neu_month <- df_clean_2015_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}

## 2014 -------------
df_clean_2014 <- df_clean %>% 
  filter(year(created_at) == 2014) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2014_pos <- df_clean_2014 %>% 
  filter(sent_gpt == "positive")
df_clean_2014_neg <- df_clean_2014 %>% 
  filter(sent_gpt == "negative")
df_clean_2014_neu <- df_clean_2014 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2014_pos$year_week)
weeks_neg <- unique(df_clean_2014_neg$year_week)
weeks_neu <- unique(df_clean_2014_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2014_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2014$year_month)) {
  df_pos_month <- df_clean_2014_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2014$year_month)) {
  df_neg_month <- df_clean_2014_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2014$year_month)) {
  df_neu_month <- df_clean_2014_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}

## 2013 -------------
df_clean_2013 <- df_clean %>% 
  filter(year(created_at) == 2013) %>% 
  mutate(month = month(created_at),
         year = year(created_at),
         week = week(created_at),
         year_month = case_when(month <= 9 ~ paste(year, "-0", month, sep = ""),
                                .default = paste(year, "-", month, sep = "")),
         year_week = case_when(week <= 9 ~ paste(year, "-w0", week, sep = ""),
                               .default = paste(year, "-w", week, sep = ""))) 

df_clean_2013_pos <- df_clean_2013 %>% 
  filter(sent_gpt == "positive")
df_clean_2013_neg <- df_clean_2013 %>% 
  filter(sent_gpt == "negative")
df_clean_2013_neu <- df_clean_2013 %>% 
  filter(sent_gpt == "neutral")

### Per week --------------
weeks_pos <- unique(df_clean_2013_pos$year_week)
weeks_neg <- unique(df_clean_2013_neg$year_week)
weeks_neu <- unique(df_clean_2013_neu$year_week)

for (i in weeks_pos) {
  df_pos <- df_clean_2013_pos %>% 
    filter(year_week == i) 
  assign(paste('df_pos',i,sep='_'), df_pos) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

### Per month --------------
for (i in unique(df_clean_2013$year_month)) {
  df_pos_month <- df_clean_2013_pos %>% 
    filter(year_month == i) 
  assign(paste('df_pos',i,sep='_'), df_pos_month) %>% 
    write_csv(paste("data/local/df_pos_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2013$year_month)) {
  df_neg_month <- df_clean_2013_neg %>% 
    filter(year_month == i) 
  assign(paste('df_neg',i,sep='_'), df_neg_month) %>% 
    write_csv(paste("data/local/df_neg_", i, ".csv", sep = ""))
}

for (i in unique(df_clean_2013$year_month)) {
  df_neu_month <- df_clean_2013_neu %>% 
    filter(year_month == i) 
  assign(paste('df_neu',i,sep='_'), df_neu_month) %>% 
    write_csv(paste("data/local/df_neu_", i, ".csv", sep = ""))
}

## Merge results for all years -------------
# setwd("data")
# files_all_sum <- fs::dir_ls(glob = "summary_2*csv")
# df_all_sum <- vroom(files_all_sum)
# setwd("..")
# 
# df_all_sum <- df_all_sum %>% 
#   separate(col = "dataset", into = c("year", "month", "stance"),
#            sep = "_") %>% 
#   mutate(year_month = paste(year, month, sep = "_"))%>% 
#   select(year_month, stance, summary) %>% 
#   arrange(year_month, stance)
# 
# df_all_sum %>% write_csv("data/summary_all_tweets.csv")

