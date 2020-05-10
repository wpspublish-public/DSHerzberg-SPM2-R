###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
### Child-512-Home-Stand DATA ---------------------------------------------------------
# READ SPM FORMS---------------------------------------
SPM_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

source(here("CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R"))

Child_512_Home_Stand_SPM_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/SPM_Home_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPM"))

# Evaluate whether de-sampled stand data can be used, or whether you need to
# recover cases from original non-desampled input files. Designate chosen sample
# as `orig_data`

# find shared cases with Child-512-Home-Stand original input files
orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/CHILD/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))
# 
# orig_data <- Child_512_Home_Stand

# READ SPM-2 FORMS, OBTAIN T-SCORES ---------------------------------------
# join with SPM2 data, obtain SPM2 raw scores
Child_512_Home_Stand_SPM_T_SPM2_raw <- Child_512_Home_Stand_SPM_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, Age, contains("_NT"), contains("_raw"), -TS_raw)

# read raw-to-t lookup tables, create lookup cols by scale
Child_512_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-Home-raw-T-lookup.csv")
  )))

map_df(
  SPM_score_names,
  ~
    Child_512_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Child_512_Home_Stand_SPM_SPM2_T <- map_dfc(SPM_score_names,
                                         ~
                                           Child_512_Home_Stand_SPM_T_SPM2_raw %>% left_join(eval(as.name(
                                             str_c(.x, '_512_lookup_col')
                                           )),
                                           by = str_c(.x, '_raw')) %>%
                                           select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_Home_Stand_SPM_T_SPM2_raw, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPM)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 11)

rm(list = ls(pattern = "col"))

# GENERATE SPM2-SPM CORR TABLE -------------------------------------
cor_cols <- Child_512_Home_Stand_SPM_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Child_512_Home_Stand_SPM_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Child-512-Home-Stand',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

### Child-512-Home-Clin DATA ---------------------------------------------------------
# READ SPM FORMS---------------------------------------
SPM_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

Child_512_Home_Clin_SPM_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/SPM_Home_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPM")) 

# READ SPM-2 FORMS ---------------------------------------
# read clin data
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Clin.R"))

# join with SPM
Child_512_Home_Clin_SPM_SPM2_T <- Child_512_Home_Clin_SPM_T %>% 
  inner_join(Child_512_Home_Clin, by = "IDNumber") %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPM)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# GENERATE SPM2-SPM CORR TABLE -------------------------------------
cor_cols <- Child_512_Home_Clin_SPM_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Child_512_Home_Clin_SPM_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Child-512-Home-Clin',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

### Child-512-School-Stand DATA ---------------------------------------------------------
# READ SPM FORMS---------------------------------------
SPM_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

source(here("CODE/ITEM-VECTORS/Child-512-School-item-vectors.R"))

Child_512_School_Stand_SPM_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/SPM_Main_Classroom_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPM"))

# Evaluate whether de-sampled stand data can be used, or whether you need to
# recover cases from original non-desampled input files. Designate chosen sample
# as `orig_data`

# find shared cases with Child-512-School-Stand original input files
# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/CHILD/SM-ONLY-NORMS-INPUT/Child-512-School-SM-only-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))

orig_data <- Child_512_School_Stand

# READ SPM-2 FORMS, OBTAIN T-SCORES ---------------------------------------
# join with SPM2 data, obtain SPM2 raw scores
Child_512_School_Stand_SPM_SPM2_T <- Child_512_School_Stand_SPM_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPM)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# GENERATE SPM2-SPM CORR TABLE -------------------------------------
cor_cols <- Child_512_School_Stand_SPM_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Child_512_School_Stand_SPM_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Child-512-School-Stand',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

### Child-512-School-Clin DATA ---------------------------------------------------------
# READ SPM FORMS---------------------------------------
SPM_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

Child_512_School_Clin_SPM_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/SPM_Main_Classroom_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPM")) %>% 
  select(-V20)

# READ SPM-2 FORMS ---------------------------------------
# read clin data
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Clin.R"))

# join with SPM
Child_512_School_Clin_SPM_SPM2_T <- Child_512_School_Clin_SPM_T %>% 
  inner_join(Child_512_School_Clin, by = "IDNumber") %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPM)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# GENERATE SPM2-SPM CORR TABLE -------------------------------------
cor_cols <- Child_512_School_Clin_SPM_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Child_512_School_Clin_SPM_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Child-512-School-Clin',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))
###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
SPM_SPM2_cor_table <- bind_rows(
  Child_512_Home_Stand_SPM_SPM2_cor_table,
  Child_512_Home_Clin_SPM_SPM2_cor_table,
  Child_512_School_Stand_SPM_SPM2_cor_table,
  Child_512_School_Clin_SPM_SPM2_cor_table
)

write_csv(SPM_SPM2_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t516-SPM-SPM2-cor-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


