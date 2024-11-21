### LOAD PACKAGES

t0 <- Sys.time()

library(tidyverse)
library(hablar)
library(rio)


### DATA IMPORT

load("./data/immunization_master_data.RData")
load("./data/sipni_final_data.RData")
load("./data/sipni_crude_data.RData")

### DATA CLEANING (I)
# https://www.synapse.org/Synapse:syn25148356/files/
mmr_coverage <- sipni_data %>% 
  filter(SCOPE == 3) %>% 
  filter(INDICATOR == "FL_Y1_MMR1") %>% 
  select(LOCAL_NAME, YEAR, INDICATOR, PC_COVERAGE)
sipni_final <- 
  sipni_final %>% 
  filter(between(YEAR, 2013, 2019)) %>% 
  mutate(AGE = recode(AGE, "0-30 dias" = "0-1 ano"))

aux_data <- 
  sipni_data %>% 
  filter(SCOPE == 3) %>% # Data at State level  
  select(LOCAL_CODE, YEAR, POPULATION_BIRTH) %>% 
  #mutate(POPULATION_Y4 = NA_integer_) %>% 
  pivot_longer(cols = c("POPULATION_BIRTH"), 
               names_to = "AGE", values_to = "POPULATION_TARGET")  


### DATA INTEGRATION AND HARMONIZATION

sipni_data_clean <- 
  sipni_final %>% 
  right_join(aux_data, by = c("LOCAL_CODE", "YEAR")) %>% 
  filter(INDICATOR == "FL_Y1_SR") %>% 
  
  #mutate(AGE = case_when(INDICATOR == "FL_U30_HB" ~ "0-30 dias", TRUE ~ AGE)) %>% 
  #mutate(AGE = fct_relevel(AGE, "0-30 dias", "0-1 ano", "1 ano", "4 anos")) %>% 
  mutate(NU_DOSE_FINAL = replace_na(NU_DOSE_FINAL, 0L)) %>% 
  mutate(PC_COVERAGE = round((NU_DOSE_FINAL / POPULATION_TARGET) * 100, 2)) %>% 
  rationalize(PC_DROPOUT, PC_COVERAGE) %>% 
  mutate(NU_DROPOUT = ifelse(NU_DROPOUT < 0, 0, NU_DROPOUT)) %>% 
  mutate(PC_DROPOUT = ifelse(PC_DROPOUT < 0, 0, PC_DROPOUT)) %>% 
  #mutate(FL_COV150 = ifelse(PC_COVERAGE >= 150, 1L, 0L)) %>% 
  mutate(PC_COVERAGE = ifelse(PC_COVERAGE >= 150, 150, PC_COVERAGE)) %>% 
  mutate(FL_GOAL = case_when(PC_COVERAGE >= 100 & INDICATOR == "FL_U1_FA" ~ 1L,
                             PC_COVERAGE >= 90 & INDICATOR %in% c("FL_U1_BCG", "FL_U1_RV") ~ 1L,
                             PC_COVERAGE >= 95 & !INDICATOR %in% c("FL_U1_FA", "FL_U1_BCG", "FL_U1_RV") ~ 1L,
                             TRUE ~ 0L)) %>% 
  mutate(CAT_GOAL = case_when(PC_COVERAGE < 50 ~ 1L, 
                              PC_COVERAGE >= 50 & PC_COVERAGE < 100 & INDICATOR == "FL_U1_FA" ~ 2L, 
                              PC_COVERAGE >= 50 & PC_COVERAGE < 90 & INDICATOR %in% c("FL_U1_BCG", "FL_U1_RV") ~ 2L, 
                              PC_COVERAGE >= 50 & PC_COVERAGE < 95 & !INDICATOR %in% c("FL_U1_FA", "FL_U1_BCG", "FL_U1_RV") ~ 2L, 
                              PC_COVERAGE >= 90 & PC_COVERAGE <= 120 & INDICATOR %in% c("FL_U1_BCG", "FL_U1_RV") ~ 3L, 
                              PC_COVERAGE >= 95 & PC_COVERAGE <= 120 & !INDICATOR %in% c("FL_U1_FA", "FL_U1_BCG", "FL_U1_RV") ~ 3L, 
                              PC_COVERAGE >= 100 & PC_COVERAGE <= 120 & INDICATOR == "FL_U1_FA" ~ 3L, 
                              PC_COVERAGE > 120 ~ 4L, 
                              TRUE ~ NA_integer_))  
  

### DATA CLEANING (II)

df_homo <- 
  bind_rows(
    sipni_data %>% 
      filter(SCOPE %in% c(7,8)) %>% 
      mutate(LOCAL_CODE = 76L) %>% 
      group_by(LOCAL_CODE, YEAR, INDICATOR) %>% 
      summarise(NU_MUNIC = n(),
                NU_MUNIC_GOAL = sum_(FL_GOAL), .groups = "drop"),
    
    sipni_data %>% 
      filter(SCOPE %in% c(7,8)) %>% 
      mutate(LOCAL_CODE = as.integer(str_sub(LOCAL_CODE, end = 1))) %>% 
      group_by(LOCAL_CODE, YEAR, INDICATOR) %>% 
      summarise(NU_MUNIC = n(),
                NU_MUNIC_GOAL = sum_(FL_GOAL), .groups = "drop"),
    
    sipni_data %>% 
      filter(SCOPE %in% c(7,8)) %>% 
      mutate(LOCAL_CODE = as.integer(str_sub(LOCAL_CODE, end = 2))) %>% 
      group_by(LOCAL_CODE, YEAR, INDICATOR) %>% 
      summarise(NU_MUNIC = n(),
                NU_MUNIC_GOAL = sum_(FL_GOAL), .groups = "drop")) %>% 
  mutate(PC_MUNIC_HOMO = round((NU_MUNIC_GOAL / NU_MUNIC)*100, 2))


sipni_data <- 
  sipni_data %>% 
  left_join(df_homo, by = c("LOCAL_CODE", "YEAR", "INDICATOR")) %>% 
  mutate(INDICATOR = case_when(INDICATOR == "FL_U30_HB" ~ "FL_U30_HepB",
                               INDICATOR == "FL_U1_S" ~ "FL_U1_M",
                               INDICATOR == "FL_U1_FA" ~ "FL_U1_YF",
                               INDICATOR == "FL_U1_HB" ~ "FL_U1_HepB",
                               INDICATOR == "FL_U1_HIB" ~ "FL_U1_Hib",
                               INDICATOR == "FL_U1_MEN" ~ "FL_U1_MenC",
                               INDICATOR == "FL_Y1_SR" ~ "FL_Y1_MR",
                               INDICATOR == "FL_Y1_SCR1" ~ "FL_Y1_MMR1",
                               INDICATOR == "FL_Y1_SCR2" ~ "FL_Y1_MMR2",
                               INDICATOR == "FL_Y1_HA" ~ "FL_Y1_HepA",
                               INDICATOR == "FL_Y1_MEN" ~ "FL_Y1_MenC",
                               TRUE ~ INDICATOR))


glimpse(sipni_data) # 2,442,863

# nrow(sipni_final) - nrow(sipni_data) # 8588
# sipni_final %>% mutate(INDICATOR = case_when(INDICATOR == "FL_U30_HB" ~ "FL_U30_HepB", INDICATOR == "FL_U1_S" ~ "FL_U1_M", INDICATOR == "FL_U1_FA" ~ "FL_U1_YF", INDICATOR == "FL_U1_HB" ~ "FL_U1_HepB", INDICATOR == "FL_U1_HIB" ~ "FL_U1_Hib", INDICATOR == "FL_U1_MEN" ~ "FL_U1_MenC", INDICATOR == "FL_Y1_SR" ~ "FL_Y1_MR", INDICATOR == "FL_Y1_SCR1" ~ "FL_Y1_MMR1", INDICATOR == "FL_Y1_SCR2" ~ "FL_Y1_MMR2", INDICATOR == "FL_Y1_HA" ~ "FL_Y1_HepA", INDICATOR == "FL_Y1_MEN" ~ "FL_Y1_MenC", TRUE ~ INDICATOR)) %>% anti_join(sipni_data, by = c("LOCAL_CODE", "YEAR", "INDICATOR")) -> x


### DATA SELFIE

format(object.size(sipni_data), units = "MB") # 512.9 Mb
print(summarytools::dfSummary(sipni_data, graph.col = F), method = "viewer", file = "./master/immunization/data_files/immunization_master_dataselfie.html")


### SAVE DATA

save(sipni_data, file = "./master/immunization/data_sets/immunization_master_data.RData")
export(sipni_data, file = "./master/immunization/data_sets/immunization_master_data.csv")

Sys.time() - t0 # 1.463788 min
