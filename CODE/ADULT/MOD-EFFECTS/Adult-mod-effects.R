suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt", 
  # age_range
  NA, "3.5 to 6 mo", "03.5 to 10 mo", "7 to 10.5 mo", "09.5 to 20 mo",  "11 to 31.5 mo", 
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


# SELF --------------------------------------------------------------------

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Self-allData-desamp.csv")
  )))

Adult_Self_age_TOT_raw_desc <-
  describeBy(Adult_Self$TOT_raw, Adult_Self$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Self$TOT_raw))/sd(Adult_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
         ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())
  
Adult_Self_gender_TOT_raw_desc <-
  describeBy(Adult_Self$TOT_raw, Adult_Self$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Self$TOT_raw))/sd(Adult_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Self_SES_TOT_raw_desc <-
  describeBy(Adult_Self$TOT_raw, Adult_Self$HighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Self$TOT_raw))/sd(Adult_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Self_Ethnic_TOT_raw_desc <-
  describeBy(Adult_Self$TOT_raw, Adult_Self$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Self$TOT_raw))/sd(Adult_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Self_Region_TOT_raw_desc <-
  describeBy(Adult_Self$TOT_raw, Adult_Self$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Self$TOT_raw))/sd(Adult_Self$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Self_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    Adult_Self_age_TOT_raw_desc,
    Adult_Self_gender_TOT_raw_desc,
    Adult_Self_SES_TOT_raw_desc,
    Adult_Self_Ethnic_TOT_raw_desc,
    Adult_Self_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/ADULT/MOD-EFFECTS/Adult-Self-TOT-raw-mod-effects.csv'
  ),
  na = '')

# OTHER --------------------------------------------------------------------

Adult_Other <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Other-allData-desamp.csv")
  )))

Adult_Other_age_TOT_raw_desc <-
  describeBy(Adult_Other$TOT_raw, Adult_Other$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Other$TOT_raw))/sd(Adult_Other$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Other_gender_TOT_raw_desc <-
  describeBy(Adult_Other$TOT_raw, Adult_Other$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Other$TOT_raw))/sd(Adult_Other$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Other_SES_TOT_raw_desc <-
  describeBy(Adult_Other$TOT_raw, Adult_Other$HighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Other$TOT_raw))/sd(Adult_Other$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Other_Ethnic_TOT_raw_desc <-
  describeBy(Adult_Other$TOT_raw, Adult_Other$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Other$TOT_raw))/sd(Adult_Other$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Other_Region_TOT_raw_desc <-
  describeBy(Adult_Other$TOT_raw, Adult_Other$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(Adult_Other$TOT_raw))/sd(Adult_Other$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

Adult_Other_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    Adult_Other_age_TOT_raw_desc,
    Adult_Other_gender_TOT_raw_desc,
    Adult_Other_SES_TOT_raw_desc,
    Adult_Other_Ethnic_TOT_raw_desc,
    Adult_Other_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/ADULT/MOD-EFFECTS/Adult-Other-TOT-raw-mod-effects.csv'
  ),
  na = '')
