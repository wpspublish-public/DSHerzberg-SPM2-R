suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt", 
  # age_range
  NA, "03.5 to 6 mo", "03.5 to 10 mo", "07 to 10.5 mo", "09.5 to 20 mo",  "11 to 31.5 mo", 
  "21 to 31.5 mo", "5 to 8 years", "9 to 12 years", "12 to 13 years", "14 to 15 years", 
  "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")


# HOME --------------------------------------------------------------------

Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Home-allData-desamp.csv")
  )))

Teen_1221_Home_age_TOT_raw_desc <-
  describeBy(Teen_1221_Home$TOT_raw, Teen_1221_Home$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Home$TOT_raw))/sd(Teen_1221_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
         ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())
  
Teen_1221_Home_gender_TOT_raw_desc <-
  describeBy(Teen_1221_Home$TOT_raw, Teen_1221_Home$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Home$TOT_raw))/sd(Teen_1221_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Home_SES_TOT_raw_desc <-
  describeBy(Teen_1221_Home$TOT_raw, Teen_1221_Home$ParentHighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Home$TOT_raw))/sd(Teen_1221_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Home_Ethnic_TOT_raw_desc <-
  describeBy(Teen_1221_Home$TOT_raw, Teen_1221_Home$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Home$TOT_raw))/sd(Teen_1221_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Home_Region_TOT_raw_desc <-
  describeBy(Teen_1221_Home$TOT_raw, Teen_1221_Home$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Home$TOT_raw))/sd(Teen_1221_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Home_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    Teen_1221_Home_age_TOT_raw_desc,
    Teen_1221_Home_gender_TOT_raw_desc,
    Teen_1221_Home_SES_TOT_raw_desc,
    Teen_1221_Home_Ethnic_TOT_raw_desc,
    Teen_1221_Home_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/TEEN/MOD-EFFECTS/Teen-1221-Home-TOT-raw-mod-effects.csv'
  ),
  na = '')

# SCHOOL --------------------------------------------------------------------

Teen_1221_School <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-School-allData-desamp.csv")
  )))

Teen_1221_School_age_TOT_raw_desc <-
  describeBy(Teen_1221_School$TOT_raw, Teen_1221_School$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_School$TOT_raw))/sd(Teen_1221_School$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_School_gender_TOT_raw_desc <-
  describeBy(Teen_1221_School$TOT_raw, Teen_1221_School$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_School$TOT_raw))/sd(Teen_1221_School$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_School_Ethnic_TOT_raw_desc <-
  describeBy(Teen_1221_School$TOT_raw, Teen_1221_School$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_School$TOT_raw))/sd(Teen_1221_School$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_School_Region_TOT_raw_desc <-
  describeBy(Teen_1221_School$TOT_raw, Teen_1221_School$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_School$TOT_raw))/sd(Teen_1221_School$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_School_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    Teen_1221_School_age_TOT_raw_desc,
    Teen_1221_School_gender_TOT_raw_desc,
    Teen_1221_School_Ethnic_TOT_raw_desc,
    Teen_1221_School_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/TEEN/MOD-EFFECTS/Teen-1221-School-TOT-raw-mod-effects.csv'
  ),
  na = '')

# SELF --------------------------------------------------------------------

Teen_1221_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Self-allData-desamp.csv")
  )))

Teen_1221_Self_age_TOT_raw_desc <-
  describeBy(Teen_1221_Self$TOT_raw, Teen_1221_Self$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Self$TOT_raw))/sd(Teen_1221_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Self_gender_TOT_raw_desc <-
  describeBy(Teen_1221_Self$TOT_raw, Teen_1221_Self$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Self$TOT_raw))/sd(Teen_1221_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Self_SES_TOT_raw_desc <-
  describeBy(Teen_1221_Self$TOT_raw, Teen_1221_Self$ParentHighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Self$TOT_raw))/sd(Teen_1221_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Self_Ethnic_TOT_raw_desc <-
  describeBy(Teen_1221_Self$TOT_raw, Teen_1221_Self$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Self$TOT_raw))/sd(Teen_1221_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Self_Region_TOT_raw_desc <-
  describeBy(Teen_1221_Self$TOT_raw, Teen_1221_Self$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Teen_1221_Self$TOT_raw))/sd(Teen_1221_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Teen_1221_Self_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    Teen_1221_Self_age_TOT_raw_desc,
    Teen_1221_Self_gender_TOT_raw_desc,
    Teen_1221_Self_SES_TOT_raw_desc,
    Teen_1221_Self_Ethnic_TOT_raw_desc,
    Teen_1221_Self_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/TEEN/MOD-EFFECTS/Teen-1221-Self-TOT-raw-mod-effects.csv'
  ),
  na = '')


