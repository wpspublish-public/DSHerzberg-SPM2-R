###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## IT HOME 49 DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-stand.R"))

IT_49_Home_stand <-  IT_49_Home_stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
IT_49_Home_B <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-IRR-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-IRR-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

IT_49_Home_IRR <- IT_49_Home_stand %>% 
  inner_join(
    IT_49_Home_B,
    by = 'IDNumber',
    suffix = c('_stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- IT_49_Home_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_s-TOT_NT_I', 'SOC_NT_s-SOC_NT_I', 'VIS_NT_s-VIS_NT_I',
             'HEA_NT_s-HEA_NT_I', 'TOU_NT_s-TOU_NT_I', 'TS_NT_s-TS_NT_I',
             'BOD_NT_s-BOD_NT_I', 'BAL_NT_s-BAL_NT_I', 'PLA_NT_s-PLA_NT_I')

IT_49_Home_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'IT-49-Home',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


