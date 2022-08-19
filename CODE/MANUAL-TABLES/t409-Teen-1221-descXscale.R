suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# HOME DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

Teen_1221_Home_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  )))

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_Home_Stand_raw_desc <-
  Teen_1221_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Home Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
       mean_Stand = mean,
       sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

Teen_1221_Home_Clin <- 
    suppressMessages(as_tibble(read_csv(
      here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv")
    )))
  
# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_Home_Clin_raw_desc <-
  Teen_1221_Home_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Home Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

Teen_1221_Home_raw_desc <- bind_cols(Teen_1221_Home_Stand_raw_desc,
                                 Teen_1221_Home_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

# SCHOOL DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

Teen_1221_School_Stand <- 
  suppressMessages(as_tibble(read_csv(
      here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
    )))
  
# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_School_Stand_raw_desc <-
  Teen_1221_School_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "School Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

Teen_1221_School_Clin <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv")
  )))
  
# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_School_Clin_raw_desc <-
  Teen_1221_School_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "School Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

Teen_1221_School_raw_desc <- bind_cols(Teen_1221_School_Stand_raw_desc,
                                          Teen_1221_School_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

# SELF DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

Teen_1221_Self_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_Self_Stand_raw_desc <-
  Teen_1221_Self_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Self Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

Teen_1221_Self_Clin <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_Self_Clin_raw_desc <-
  Teen_1221_Self_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Self Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

Teen_1221_Self_raw_desc <- bind_cols(Teen_1221_Self_Stand_raw_desc,
                                     Teen_1221_Self_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))


# WRITE COMBINED OUTPUT TABLE ---------------------------------------------

Teen_1221_raw_desc <- bind_rows(
  Teen_1221_Home_raw_desc,
  Teen_1221_School_raw_desc,
  Teen_1221_Self_raw_desc
)

write_csv(Teen_1221_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t409-Teen-1221-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

