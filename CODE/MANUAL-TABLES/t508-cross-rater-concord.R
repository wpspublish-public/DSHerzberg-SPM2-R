###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## PRESCHOOL 25 ---------------------------------------------------------------------

source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))

Home_T <- Preschool_25_Home_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_Home"))

School_T <- Preschool_25_School_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_School"))

Home_School_T <- Home_T %>% 
  inner_join(School_T, by = "IDNumber") %>% 
  select(-IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Home - TOT_NT_School)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 20)

cor_cols <- Home_School_T %>% 
  select(-TOT_NT_dif)

cor_row <- c('TOT_NT_H-TOT_NT_S', 'SOC_NT_H-SOC_NT_S', 'VIS_NT_H-VIS_NT_S',
             'HEA_NT_H-HEA_NT_S', 'TOU_NT_H-TOU_NT_S', 'TS_NT_H-TS_NT_S',
             'BOD_NT_H-BOD_NT_S', 'BAL_NT_H-BAL_NT_S', 'PLA_NT_H-PLA_NT_S')

Preschool_25_cross_rater_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Preschool-25',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = "cross")))

## CHILD 512 ---------------------------------------------------------------------

source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))

Home_T <- Child_512_Home_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_Home"))

School_T <- Child_512_School_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_School"))

Home_School_T <- Home_T %>% 
  inner_join(School_T, by = "IDNumber") %>% 
  select(-IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Home - TOT_NT_School)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 20)

cor_cols <- Home_School_T %>% 
  select(-TOT_NT_dif)

cor_row <- c('TOT_NT_H-TOT_NT_S', 'SOC_NT_H-SOC_NT_S', 'VIS_NT_H-VIS_NT_S',
             'HEA_NT_H-HEA_NT_S', 'TOU_NT_H-TOU_NT_S', 'TS_NT_H-TS_NT_S',
             'BOD_NT_H-BOD_NT_S', 'BAL_NT_H-BAL_NT_S', 'PLA_NT_H-PLA_NT_S')

Child_512_cross_rater_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Child-512',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = "cross")))

## TEEN 1221 ---------------------------------------------------------------------

source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))

Home_T <- Teen_1221_Home_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_Home"))

School_T <- Teen_1221_School_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_School"))

Self_T <- Teen_1221_Self_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_Self"))

# Home corr with School

Home_School_T <- Home_T %>% 
  inner_join(School_T, by = "IDNumber") %>% 
  select(-IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Home - TOT_NT_School)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 20)

cor_cols <- Home_School_T %>% 
  select(-TOT_NT_dif)

cor_row <- c('TOT_NT_H-TOT_NT_S', 'SOC_NT_H-SOC_NT_S', 'VIS_NT_H-VIS_NT_S',
             'HEA_NT_H-HEA_NT_S', 'TOU_NT_H-TOU_NT_S', 'TS_NT_H-TS_NT_S',
             'BOD_NT_H-BOD_NT_S', 'BAL_NT_H-BAL_NT_S', 'PLA_NT_H-PLA_NT_S')

Teen_1221_Home_School_cross_rater_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Teen-1221-Home-School',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

# Home corr with Self

Home_Self_T <- Home_T %>% 
  inner_join(Self_T, by = "IDNumber") %>% 
  select(-IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Home - TOT_NT_Self)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 20)

cor_cols <- Home_Self_T %>% 
  select(-TOT_NT_dif)

cor_row <- c('TOT_NT_H-TOT_NT_S', 'SOC_NT_H-SOC_NT_S', 'VIS_NT_H-VIS_NT_S',
             'HEA_NT_H-HEA_NT_S', 'TOU_NT_H-TOU_NT_S', 'TS_NT_H-TS_NT_S',
             'BOD_NT_H-BOD_NT_S', 'BAL_NT_H-BAL_NT_S', 'PLA_NT_H-PLA_NT_S')

Teen_1221_Home_Self_cross_rater_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Teen-1221-Home-Self',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

# School corr with Self

School_Self_T <- School_T %>% 
  inner_join(Self_T, by = "IDNumber") %>% 
  select(-IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_School - TOT_NT_Self)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 20)

cor_cols <- School_Self_T %>% 
  select(-TOT_NT_dif)

cor_row <- c('TOT_NT_Sc-TOT_NT_Sl', 'SOC_NT_Sc-SOC_NT_Sl', 'VIS_NT_Sc-VIS_NT_Sl',
             'HEA_NT_Sc-HEA_NT_Sl', 'TOU_NT_Sc-TOU_NT_Sl', 'TS_NT_Sc-TS_NT_Sl',
             'BOD_NT_Sc-BOD_NT_Sl', 'BAL_NT_Sc-BAL_NT_Sl', 'PLA_NT_Sc-PLA_NT_Sl')

Teen_1221_School_Self_cross_rater_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Teen-1221-School-Self',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = "cross")))

## ADULT ---------------------------------------------------------------------

source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Self-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Stand.R"))

Self_T <- Adult_Self_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_Self"))

Other_T <- Adult_Other_Stand %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_Other"))

# Self corr with Other

Self_Other_T <- Self_T %>% 
  inner_join(Other_T, by = "IDNumber") %>% 
  select(-IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Self - TOT_NT_Other)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 20)

cor_cols <- Self_Other_T %>% 
  select(-TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_O', 'SOC_NT_S-SOC_NT_O', 'VIS_NT_S-VIS_NT_O',
             'HEA_NT_S-HEA_NT_O', 'TOU_NT_S-TOU_NT_O', 'TS_NT_S-TS_NT_O',
             'BOD_NT_S-BOD_NT_O', 'BAL_NT_S-BAL_NT_O', 'PLA_NT_S-PLA_NT_O')

Adult_Self_Other_cross_rater_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Adult-Self-Other',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = "cross")))


###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
cross_rater_cor_table <- bind_rows(
  Preschool_25_cross_rater_cor_table,
  Child_512_cross_rater_cor_table,
  Teen_1221_Home_School_cross_rater_cor_table,
  Teen_1221_Home_Self_cross_rater_cor_table,
  Teen_1221_School_Self_cross_rater_cor_table,
  Adult_Self_Other_cross_rater_cor_table
)

write_csv(cross_rater_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t508-cross-rater-concord-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
