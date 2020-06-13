###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
## PRESCHOOL 25 Home DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))

Preschool_25_Home_Stand <-  Preschool_25_Home_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Preschool_25_Home_B <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-IRR-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-IRR-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Preschool_25_Home_IRR <- Preschool_25_Home_Stand %>% 
  inner_join(
    Preschool_25_Home_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- Preschool_25_Home_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Preschool_25_Home_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Preschool-25-Home',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

