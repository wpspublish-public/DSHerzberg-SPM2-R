## TEEN 1221 HOME DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))

Teen_1221_Home_Stand <-  Teen_1221_Home_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Teen_1221_Home_B <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-IRR-T-Scores-per-case.csv')
  ))) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Teen_1221_Home_IRR <- Teen_1221_Home_Stand %>% 
  inner_join(
    Teen_1221_Home_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 11)

cor_cols <- Teen_1221_Home_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Teen_1221_Home_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Teen-1221-Home',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


