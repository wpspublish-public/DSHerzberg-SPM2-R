suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# SELF DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

Adult_Self_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv")
  )))

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

Adult_Self_Stand_raw_desc <-
  Adult_Self_Stand %>% 
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

Adult_Self_Clin <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-clin-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

Adult_Self_Clin_raw_desc <-
  Adult_Self_Clin %>% 
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

Adult_Self_raw_desc <- bind_cols(Adult_Self_Stand_raw_desc,
                                     Adult_Self_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))


# OTHER DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

Adult_Other_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

Adult_Other_Stand_raw_desc <-
  Adult_Other_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Other Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

Adult_Other_Clin <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-clin-T-Scores-per-case.csv")
  )))

# WRITE RAW SCORE DESCRIPTIVES

Adult_Other_Clin_raw_desc <-
  Adult_Other_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Other Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

Adult_Other_raw_desc <- bind_cols(Adult_Other_Stand_raw_desc,
                                     Adult_Other_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

# WRITE COMBINED OUTPUT TABLE ---------------------------------------------

Adult_raw_desc <- bind_rows(
  Adult_Self_raw_desc,
  Adult_Other_raw_desc
)

write_csv(Adult_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t410-Adult-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

