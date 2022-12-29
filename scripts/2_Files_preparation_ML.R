# Script information ------------
#' Aim: Prepare files for ML
#' Author: Laura Espinosa
#' Date created: 27 August 2022
#' Date updated: 27 August 2022


# 1 - Packages ----------------------
# install/load "pacman" to help installing and loading other packages
while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# load packages
p_load(tidyverse, readr, lubridate)

# 2 - Import data --------------------
df_clean_geo <- read_csv('data/local/tweets_with_predicted_labels_filtered_BR_PT_geo_final.csv')

world_br_ml <- read_csv("data/worldbank_br_clean.csv") %>% 
  select(-...1) 

# 3 - Group tweets per month and calculate variables --------------
names(df_clean_geo)

df_clean_geo_month <- df_clean_geo

df_clean_geo_month$year_month <- floor_date(df_clean_geo_month$created_at,
                                            "month")

# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/overview
df_clean_geo_ml_variables <- df_clean_geo_month %>% 
  group_by(year_month) %>% 
  dplyr::summarise(in_reply_to_status_count = sum(!is.na(in_reply_to_status_id)),
                   in_reply_to_user_count = sum(!is.na(in_reply_to_user_id)),
                   in_quotation_user_count = sum(!is.na(quoted_user_id)),
                   in_quotation_status_count = sum(!is.na(quoted_status_id)),
                   in_retweet_user_count = sum(!is.na(retweeted_user_id)),
                   in_retweet_status_count = sum(!is.na(retweeted_status_id)),
                   entities_count = sum(!is.na(entities.user_mentions)),
                   followers_mean = mean(user.num_followers),
                   following_mean = mean (user.num_following),
                   user_statuses_mean = mean(user.statuses_count),
                   verified_users_count = sum(user.is_verified == TRUE),
                   retweets_count = sum(is_retweet == TRUE),
                   quotes_count = sum(has_quote == TRUE),
                   replies_count = sum(is_reply == TRUE),
                   num_retweets_mean = mean(num_retweets),
                   num_quotes_mean = mean(num_quotes),
                   num_replies_mean = mean(num_replies)) %>% 
  ungroup()

df_clean_geo_ml_region <- df_clean_geo_month %>% 
  group_by(year_month, label, LocationCode) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(key = label, value = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(sentiment = (((positive * 1)+ (negative * -1))/(positive+negative+neutral))
         ,total = positive + negative + neutral
         ) %>% 
  select(-positive, -negative, -neutral) %>% 
  tidyr::pivot_wider(names_from = LocationCode, 
                     values_from = c(sentiment, total),
                     values_fill = 0)

df_clean_geo_ml <- df_clean_geo_month %>% 
  group_by(year_month, label) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(key = label, value = n) %>% 
  replace(is.na(.), 0) %>%
  mutate(sentiment_BR = (((positive * 1)+ (negative * -1))/(positive+negative+neutral)),
         total_BR = positive + negative + neutral) %>% 
  select(-positive, -negative, -neutral) %>% 
  left_join(df_clean_geo_ml_region, 
            by = c('year_month' = 'year_month')) %>% 
  mutate(year = year(year_month),
         month = month(year_month)) %>% 
  left_join(world_br_ml, by = c('year' = 'year',
                             'month' = 'month'))




# 4 - Save new dataset ----------------
write_csv(df_clean_geo_ml, 'data/df_monthly_ml.csv')
