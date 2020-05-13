###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
## IT HOME 430 DATA ---------------------------------------------

# read stand data
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Stand.R"))

IT_49_Home_Stand <-  IT_49_Home_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))
IT_1030_Home_Stand <-  IT_1030_Home_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

IT_430_Home_Stand <- bind_rows(
  IT_49_Home_Stand,
  IT_1030_Home_Stand
) %>% 
  arrange(IDNumber)

# read IRR (Form B)
IT_49_Home_B <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-IRR-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-IRR-T-Scores-per-case.csv')
  )))
) %>% 
  select(IDNumber, contains('_NT'))

IT_1030_Home_B <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-IRR-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-IRR-T-Scores-per-case.csv')
  )))
) %>% 
  select(IDNumber, contains('_NT'))

IT_430_Home_B <- bind_rows(
  IT_49_Home_B,
  IT_1030_Home_B
) %>% 
  arrange(IDNumber)

IT_430_Home_IRR <- IT_430_Home_Stand %>% 
  inner_join(
    IT_430_Home_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- IT_430_Home_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

IT_430_Home_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'IT-430-Home',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

## IT CAREGIVER DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-Caregiver-Stand.R"))

IT_Caregiver_Stand <-  IT_Caregiver_Stand  %>%
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
IT_Caregiver_B <-
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-IRR-T-Scores-per-case.csv')
  ))) %>%
  arrange(IDNumber) %>%
  select(IDNumber, contains('_NT'))

IT_Caregiver_IRR <- IT_Caregiver_Stand %>%
  inner_join(
    IT_Caregiver_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>%
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>%
  select(TOT_NT_dif, everything()) %>%
  arrange(TOT_NT_dif) %>%
  filter(TOT_NT_dif < 10)

cor_cols <- IT_Caregiver_IRR %>%
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

IT_Caregiver_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'IT-Caregiver',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

## PRESCHOOL 25 SCHOOL DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))

Preschool_25_School_Stand <-  Preschool_25_School_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Preschool_25_School_B <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-IRR-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-IRR-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Preschool_25_School_IRR <- Preschool_25_School_Stand %>% 
  inner_join(
    Preschool_25_School_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- Preschool_25_School_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Preschool_25_School_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Preschool-25-School',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

## CHILD 512 HOME DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))

Child_512_Home_Stand <-  Child_512_Home_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Child_512_Home_B <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-IRR-T-Scores-per-case.csv')
  ))) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Child_512_Home_IRR <- Child_512_Home_Stand %>% 
  inner_join(
    Child_512_Home_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- Child_512_Home_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Child_512_Home_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Child-512-Home',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


## CHILD 512 SCHOOL DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))

Child_512_School_Stand <-  Child_512_School_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Child_512_School_B <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-IRR-T-Scores-per-case.csv')
  ))) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Child_512_School_IRR <- Child_512_School_Stand %>% 
  inner_join(
    Child_512_School_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 11)

cor_cols <- Child_512_School_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Child_512_School_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Child-512-School',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


## TEEN 1221 SCHOOL DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))

Teen_1221_School_Stand <-  Teen_1221_School_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Teen_1221_School_B <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-IRR-T-Scores-per-case.csv')
  ))) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Teen_1221_School_IRR <- Teen_1221_School_Stand %>% 
  inner_join(
    Teen_1221_School_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 12)

cor_cols <- Teen_1221_School_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Teen_1221_School_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


## ADULT OTHER DATA ---------------------------------------------
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Stand.R"))

Adult_Other_Stand <-  Adult_Other_Stand  %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read IRR (Form B)
Adult_Other_B <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-IRR-T-Scores-per-case.csv')
  ))) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

Adult_Other_IRR <- Adult_Other_Stand %>% 
  inner_join(
    Adult_Other_B,
    by = 'IDNumber',
    suffix = c('_Stand', '_IRR')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_Stand - TOT_NT_IRR)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

cor_cols <- Adult_Other_IRR %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_S-TOT_NT_I', 'SOC_NT_S-SOC_NT_I', 'VIS_NT_S-VIS_NT_I',
             'HEA_NT_S-HEA_NT_I', 'TOU_NT_S-TOU_NT_I', 'TS_NT_S-TS_NT_I',
             'BOD_NT_S-BOD_NT_I', 'BAL_NT_S-BAL_NT_I', 'PLA_NT_S-PLA_NT_I')

Adult_Other_IRR_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Adult-Other',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
IRR_table <- bind_rows(
  IT_430_Home_IRR_cor_table,
  IT_Caregiver_IRR_cor_table,
  Preschool_25_School_IRR_cor_table,
  Child_512_Home_IRR_cor_table,
  Child_512_School_IRR_cor_table,
  Teen_1221_Home_IRR_cor_table,
  Teen_1221_School_IRR_cor_table,
  Adult_Other_IRR_cor_table
)

write_csv(IRR_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t530-IRR-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
