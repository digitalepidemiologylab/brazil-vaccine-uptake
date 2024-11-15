# Script information ------------
#' Aim: Prepare files for ML
#' Author: Laura Espinosa
#' Date created: 27 August 2022
#' Date updated: 05 February 2023


# 1 - Packages ----------------------
message("Installing and loading packages")
# install/load "pacman" to help installing and loading other packages
while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# load packages
p_load(tidyverse, readr, lubridate)

# 2 - Import data --------------------
message("Importing data")

df_clean_geo <- read_csv('data/local/tweets_with_predicted_labels_filtered_BR_PT_geo_final.csv') # Without geo filter
#df_clean_geo <- read_csv('data/local/tweets_with_predicted_labels_filtered_BR_PT_geo_final.csv')

world_bank_clean <- read_csv("data/worldbank_br_clean.csv") 

# 3 - Group tweets per month and calculate variables --------------
message("Prepare datasets for machine learning model")
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
  group_by(year_month, sent_gpt, LocationName) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(key = sent_gpt, value = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(sentiment = (((positive * 1)+ (negative * -1))/(positive+negative+neutral))
         ,total = positive + negative + neutral
         ) %>% 
  select(-positive, -negative, -neutral) %>% 
  tidyr::pivot_wider(names_from = LocationName, 
                     values_from = c(sentiment, total),
                     values_fill = 0)

df_clean_geo_ml <- df_clean_geo_month %>% 
  group_by(year_month, sent_gpt) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(key = sent_gpt, value = n) %>% 
  replace(is.na(.), 0) %>%
  mutate(sentiment_BR = (((positive * 1)+ (negative * -1))/(positive+negative+neutral)),
         total_BR = positive + negative + neutral) %>% 
  select(-positive, -negative, -neutral) %>% 
  left_join(df_clean_geo_ml_region, 
            by = c('year_month' = 'year_month')) %>% 
  mutate(year = year(year_month),
         month = month(year_month)) %>% 
  left_join(world_bank_clean, by = c('year' = 'year',
                             'month' = 'month')) %>% 
  select(-surface_area_sq_km, -population_total, -gni_atlas_method_current_us,
         -gni_per_capita_atlas_method_current_us, -gni_ppp_current_international,
         -immunization_measles_percent_of_children_ages_12_23_months,
         -prevalence_of_hiv_total_percent_of_population_ages_15_49,
         -forest_area_sq_km, -energy_use_kg_of_oil_equivalent_per_capita,
         -co2_emissions_metric_tons_per_capita, -electric_power_consumption_k_wh_per_capita,
         -gdp_current_us, -inflation_gdp_deflator_annual_percent, 
         -agriculture_forestry_and_fishing_value_added_percent_of_gdp, 
         -industry_including_construction_value_added_percent_of_gdp, 
         -exports_of_goods_and_services_percent_of_gdp, 
         -imports_of_goods_and_services_percent_of_gdp, 
         -revenue_excluding_grants_percent_of_gdp, 
         -time_required_to_start_a_business_days, -tax_revenue_percent_of_gdp,
         -military_expenditure_percent_of_gdp, 
         -contraceptive_prevalence_any_method_percent_of_married_women_ages_15_49,
         -high_technology_exports_percent_of_manufactured_exports, 
         #-statistical_capacity_score_overall_average_scale_0_100,
         -merchandise_trade_percent_of_gdp, 
         -net_barter_terms_of_trade_index_2000_100, 
         -external_debt_stocks_total_dod_current_us,
         -total_debt_service_percent_of_exports_of_goods_services_and_primary_income,
         -net_migration, -personal_remittances_received_current_us,
         -foreign_direct_investment_net_inflows_bo_p_current_us, 
         -net_official_development_assistance_and_official_aid_received_current_us)

df_clean_geo_ml_full <- df_clean_geo_ml %>% 
  left_join(df_clean_geo_ml_variables,
            by = "year_month")

df_clean_geo_ml_short <- df_clean_geo_ml %>% 
  select(-starts_with("total_BRG"))




# 4 - Save new datasets ----------------
message("Saving new datasets for machine learning model")
write_csv(df_clean_geo_ml, 'data/df_monthly_ml.csv')
write_csv(df_clean_geo_ml_full, 'data/df_monthly_ml_full.csv')
write_csv(df_clean_geo_ml_short, 'data/df_monthly_ml_short.csv')

