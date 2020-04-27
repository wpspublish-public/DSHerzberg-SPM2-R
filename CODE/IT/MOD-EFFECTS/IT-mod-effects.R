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


# READ DATA --------------------------------------------------------------------

IT_49_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-49-Home-allData-desamp.csv")
  ))) %>% 
  select(-(contains('q0')))

IT_1030_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-1030-Home-allData-desamp.csv")
  ))) %>% 
  select(-(contains('q0')))

IT_Caregiver_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-Caregiver-combo-norms-input.csv")
  )))
IT_Caregiver_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SP-NORMS-INPUT/IT-Caregiver-Sp-norms-input.csv")
  )))

IT_Caregiver <- bind_rows(
  IT_Caregiver_Eng,
  IT_Caregiver_Sp
) %>% 
  arrange(IDNumber)

rm(IT_Caregiver_Eng, IT_Caregiver_Sp)

IT_430_Home <- 
  bind_rows(
    IT_49_Home,
    IT_1030_Home
  ) %>% 
  arrange(IDNumber)

# HOME 430 --------------------------------------------------------------------

IT_430_Home_age_TOT_raw_desc <-
  describeBy(IT_430_Home$TOT_raw, IT_430_Home$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_430_Home$TOT_raw))/sd(IT_430_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_430_Home_gender_TOT_raw_desc <-
  describeBy(IT_430_Home$TOT_raw, IT_430_Home$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_430_Home$TOT_raw))/sd(IT_430_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_430_Home_SES_TOT_raw_desc <-
  describeBy(IT_430_Home$TOT_raw, IT_430_Home$ParentHighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_430_Home$TOT_raw))/sd(IT_430_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_430_Home_Ethnic_TOT_raw_desc <-
  describeBy(IT_430_Home$TOT_raw, IT_430_Home$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_430_Home$TOT_raw))/sd(IT_430_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_430_Home_Region_TOT_raw_desc <-
  describeBy(IT_430_Home$TOT_raw, IT_430_Home$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_430_Home$TOT_raw))/sd(IT_430_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_430_Home_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    IT_430_Home_age_TOT_raw_desc,
    IT_430_Home_gender_TOT_raw_desc,
    IT_430_Home_SES_TOT_raw_desc,
    IT_430_Home_Ethnic_TOT_raw_desc,
    IT_430_Home_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/IT/MOD-EFFECTS/IT-430-Home-TOT-raw-mod-effects.csv'
  ),
  na = '')

# HOME 49 --------------------------------------------------------------------

IT_49_Home_age_TOT_raw_desc <-
  describeBy(IT_49_Home$TOT_raw, IT_49_Home$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_49_Home$TOT_raw))/sd(IT_49_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_49_Home_gender_TOT_raw_desc <-
  describeBy(IT_49_Home$TOT_raw, IT_49_Home$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_49_Home$TOT_raw))/sd(IT_49_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_49_Home_SES_TOT_raw_desc <-
  describeBy(IT_49_Home$TOT_raw, IT_49_Home$ParentHighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_49_Home$TOT_raw))/sd(IT_49_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_49_Home_Ethnic_TOT_raw_desc <-
  describeBy(IT_49_Home$TOT_raw, IT_49_Home$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_49_Home$TOT_raw))/sd(IT_49_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_49_Home_Region_TOT_raw_desc <-
  describeBy(IT_49_Home$TOT_raw, IT_49_Home$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_49_Home$TOT_raw))/sd(IT_49_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_49_Home_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    IT_49_Home_age_TOT_raw_desc,
    IT_49_Home_gender_TOT_raw_desc,
    IT_49_Home_SES_TOT_raw_desc,
    IT_49_Home_Ethnic_TOT_raw_desc,
    IT_49_Home_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/IT/MOD-EFFECTS/IT-49-Home-TOT-raw-mod-effects.csv'
  ),
  na = '')

# HOME 1030 --------------------------------------------------------------------

IT_1030_Home_age_TOT_raw_desc <-
  describeBy(IT_1030_Home$TOT_raw, IT_1030_Home$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_1030_Home$TOT_raw))/sd(IT_1030_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_1030_Home_gender_TOT_raw_desc <-
  describeBy(IT_1030_Home$TOT_raw, IT_1030_Home$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_1030_Home$TOT_raw))/sd(IT_1030_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_1030_Home_SES_TOT_raw_desc <-
  describeBy(IT_1030_Home$TOT_raw, IT_1030_Home$ParentHighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_1030_Home$TOT_raw))/sd(IT_1030_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_1030_Home_Ethnic_TOT_raw_desc <-
  describeBy(IT_1030_Home$TOT_raw, IT_1030_Home$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_1030_Home$TOT_raw))/sd(IT_1030_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_1030_Home_Region_TOT_raw_desc <-
  describeBy(IT_1030_Home$TOT_raw, IT_1030_Home$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_1030_Home$TOT_raw))/sd(IT_1030_Home$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_1030_Home_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    IT_1030_Home_age_TOT_raw_desc,
    IT_1030_Home_gender_TOT_raw_desc,
    IT_1030_Home_SES_TOT_raw_desc,
    IT_1030_Home_Ethnic_TOT_raw_desc,
    IT_1030_Home_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/IT/MOD-EFFECTS/IT-1030-Home-TOT-raw-mod-effects.csv'
  ),
  na = '')

# CAREGIVER --------------------------------------------------------------------

IT_Caregiver_age_TOT_raw_desc <-
  describeBy(IT_Caregiver$TOT_raw, IT_Caregiver$age_range, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_Caregiver$TOT_raw))/sd(IT_Caregiver$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Age Range',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_Caregiver_gender_TOT_raw_desc <-
  describeBy(IT_Caregiver$TOT_raw, IT_Caregiver$Gender, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_Caregiver$TOT_raw))/sd(IT_Caregiver$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Gender',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_Caregiver_SES_TOT_raw_desc <-
  describeBy(IT_Caregiver$TOT_raw, IT_Caregiver$ParentHighestEducation, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_Caregiver$TOT_raw))/sd(IT_Caregiver$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'SES',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_Caregiver_Ethnic_TOT_raw_desc <-
  describeBy(IT_Caregiver$TOT_raw, IT_Caregiver$Ethnicity, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_Caregiver$TOT_raw))/sd(IT_Caregiver$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Ethnicity',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_Caregiver_Region_TOT_raw_desc <-
  describeBy(IT_Caregiver$TOT_raw, IT_Caregiver$Region, fast = T, mat = T) %>%
  rownames_to_column() %>%
  arrange(match(group1, cat_order)) %>%
  select(group1, n, mean, sd) %>% 
  mutate(ES_lag = round((mean - lag(mean))/((sd + lag(sd))/2),2), 
         ES_grand = round((mean - mean(IT_Caregiver$TOT_raw))/sd(IT_Caregiver$TOT_raw),2),
         mod_var = case_when(
           is.na(lag(n)) ~ 'Region',
           TRUE ~ ''
         )
  ) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>%
  rename(mod_val = group1, mean_TOT = mean, sd_TOT = sd) %>% 
  select(mod_var, everything())

IT_Caregiver_TOT_raw_mod_effects <- suppressWarnings(
  bind_rows(
    IT_Caregiver_age_TOT_raw_desc,
    IT_Caregiver_gender_TOT_raw_desc,
    IT_Caregiver_SES_TOT_raw_desc,
    IT_Caregiver_Ethnic_TOT_raw_desc,
    IT_Caregiver_Region_TOT_raw_desc
  )
) %>%
  write_csv(here(
    'OUTPUT-FILES/IT/MOD-EFFECTS/IT-Caregiver-TOT-raw-mod-effects.csv'
  ),
  na = '')

