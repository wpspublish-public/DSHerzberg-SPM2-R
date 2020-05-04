# LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# ADULT SELF DATA ---------------------------------------------

# read stand

Adult_Self_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2

Adult_Self_time2 <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-time2-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber, contains('_NT'))

Adult_Self_TRT <- Adult_Self_Stand %>% 
  inner_join(
    Adult_Self_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>% 
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- Adult_Self_TRT %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT_1-TOT_NT_2', 'SOC_NT_1-SOC_NT_2', 'VIS_NT_1-VIS_NT_2',
             'HEA_NT_1-HEA_NT_2', 'TOU_NT_1-TOU_NT_2', 'TS_NT_1-TS_NT_2',
             'BOD_NT_1-BOD_NT_2', 'BAL_NT_1-BAL_NT_2', 'PLA_NT_1-PLA_NT_2')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

Adult_Self_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>%
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Adult Self', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

Adult_Self_t1_T_desc <-
  Adult_Self_TRT %>% 
  select(contains('_NT_t1')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Self Form t1",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n = n,
         mean_t1 = mean,
         sd_t1 = sd)

Adult_Self_t2_T_desc <-
  Adult_Self_TRT %>% 
  select(contains('_NT_t2')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, mean, sd) %>% 
  rename(mean_t2 = mean,
         sd_t2 = sd)

Adult_Self_TRT_T_desc <- Adult_Self_t1_T_desc %>% 
  full_join(
    Adult_Self_t2_T_desc,
    by = 'scale'
  )  %>%
  select(-form) %>% 
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2)))) %>%
  mutate_at(vars(mean_t1, sd_t1, mean_t2, sd_t2, ES), ~
              (round(., 2)))
