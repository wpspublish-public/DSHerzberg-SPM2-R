
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# 49 DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

IT_49_Home_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

IT_49_Home_Stand_raw_desc <-
  IT_49_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Infant Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
       mean_Stand = mean,
       sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

IT_49_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

# WRITE RAW SCORE DESCRIPTIVES

IT_49_Home_Clin_raw_desc <-
  IT_49_Home_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Infant Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

IT_49_Home_raw_desc <- bind_cols(IT_49_Home_Stand_raw_desc,
                                 IT_49_Home_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

# 1030 DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

IT_1030_Home_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

# WRITE RAW SCORE DESCRIPTIVES

IT_1030_Home_Stand_raw_desc <-
  IT_1030_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Toddler Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

IT_1030_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

# WRITE RAW SCORE DESCRIPTIVES

IT_1030_Home_Clin_raw_desc <-
  IT_1030_Home_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Toddler Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

IT_1030_Home_raw_desc <- bind_cols(IT_1030_Home_Stand_raw_desc,
                                   IT_1030_Home_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

# CAREGIVER DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

IT_Caregiver_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

IT_Caregiver_Stand_raw_desc <-
  IT_Caregiver_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Caregiver Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

IT_Caregiver_Clin <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-clin-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

IT_Caregiver_Clin_raw_desc <-
  IT_Caregiver_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Caregiver Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

IT_Caregiver_raw_desc <- bind_cols(IT_Caregiver_Stand_raw_desc,
                                   IT_Caregiver_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

# WRITE COMBINED OUTPUT TABLE ---------------------------------------------

IT_430_raw_desc <- bind_rows(
  IT_49_Home_raw_desc,
  IT_1030_Home_raw_desc,
  IT_Caregiver_raw_desc
)

write_csv(IT_430_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t406-IT-49-1030-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

