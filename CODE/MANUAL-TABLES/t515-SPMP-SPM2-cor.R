###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
### Preschool-25-Home-Stand DATA ---------------------------------------------------------
# READ SPMP FORMS---------------------------------------
SPMP_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

source(here("CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R"))

Preschool_25_Home_Stand_SPMP_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/PAPER-FORMS/SPMP_Home_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename(VIS_TS = VIS_TC) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPMP"))

# Evaluate whether de-sampled stand data can be used, or whether you need to
# recover cases from original non-desampled input files. Designate chosen sample
# as `orig_data`

# find shared cases with Preschool-25-Home-Stand original input files
orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/PRESCHOOL/SM-QUAL-COMBO-NORMS-INPUT/Preschool-25-Home-combo-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))
# 
# orig_data <- Preschool_25_Home_Stand

# READ SPM-2 FORMS, OBTAIN T-SCORES ---------------------------------------
# join with SPM2 data, obtain SPM2 raw scores
Preschool_25_Home_Stand_SPMP_T_SPM2_raw <- Preschool_25_Home_Stand_SPMP_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_Home]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_Home]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_Home]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_Home]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_Home]),
    # TS_raw = rowSums(.[TS_items_Preschool_25_Home]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_Home]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_Home]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_Home])
  ) %>% 
  select(IDNumber, Age, contains("_NT"),contains("_raw"))

# split sample by age_range

Preschool_24_Home_Stand_SPMP_T_SPM2_raw <- Preschool_25_Home_Stand_SPMP_T_SPM2_raw %>% filter(Age <= 4)
Preschool_5_Home_Stand_SPMP_T_SPM2_raw <- Preschool_25_Home_Stand_SPMP_T_SPM2_raw %>% filter(Age >= 5)

# 24 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_24_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-24-Home-raw-T-lookup.csv")
  )))

map_df(
  SPMP_score_names,
  ~
    Preschool_24_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_24_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Preschool_24_Home_Stand_SPMP_SPM2_T <- map_dfc(SPMP_score_names,
                                         ~
                                           Preschool_24_Home_Stand_SPMP_T_SPM2_raw %>% left_join(eval(as.name(
                                             str_c(.x, '_24_lookup_col')
                                           )),
                                           by = str_c(.x, '_raw')) %>%
                                           select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_Home_Stand_SPMP_T_SPM2_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# 5 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_5_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-5-Home-raw-T-lookup.csv")
  )))

map_df(
  SPMP_score_names,
  ~
    Preschool_5_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_5_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Preschool_5_Home_Stand_SPMP_SPM2_T <- map_dfc(SPMP_score_names,
                                        ~
                                          Preschool_5_Home_Stand_SPMP_T_SPM2_raw %>% left_join(eval(as.name(
                                            str_c(.x, '_5_lookup_col')
                                          )),
                                          by = str_c(.x, '_raw')) %>%
                                          select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_Home_Stand_SPMP_T_SPM2_raw, .) %>% 
  select(IDNumber, contains("_NT"))

# Combine 24, 5 SPMP T-scores
Preschool_25_Home_Stand_SPMP_SPM2_T <- bind_rows(
  Preschool_24_Home_Stand_SPMP_SPM2_T,
  Preschool_5_Home_Stand_SPMP_SPM2_T
) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPMP)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 11)

rm(list = ls(pattern = "col"))

# GENERATE SPM2-SPMP CORR TABLE -------------------------------------
cor_cols <- Preschool_25_Home_Stand_SPMP_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Preschool_25_Home_Stand_SPMP_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Preschool-25-Home-Stand',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


### Preschool-25-Home-Clin DATA ---------------------------------------------------------
# READ SPMP FORMS---------------------------------------
SPMP_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

Preschool_25_Home_Clin_SPMP_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/PAPER-FORMS/SPMP_Home_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename(VIS_TS = VIS_TC) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPMP")) %>% 
  select(-V20)

# READ SPM-2 FORMS ---------------------------------------
# read clin data
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Clin.R"))

# join with SPMP
Preschool_25_Home_Clin_SPMP_SPM2_T <- Preschool_25_Home_Clin_SPMP_T %>% 
  inner_join(Preschool_25_Home_Clin, by = "IDNumber") %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPMP)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# GENERATE SPM2-SPMP CORR TABLE -------------------------------------
cor_cols <- Preschool_25_Home_Clin_SPMP_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Preschool_25_Home_Clin_SPMP_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Preschool-25-Home-Clin',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))



### Preschool-25-School-Stand DATA ---------------------------------------------------------
# READ SPMP FORMS---------------------------------------
SPMP_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

source(here("CODE/ITEM-VECTORS/Preschool-25-School-item-vectors.R"))

Preschool_25_School_Stand_SPMP_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/PAPER-FORMS/SPMP_School_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPMP"))

# Evaluate whether de-sampled stand data can be used, or whether you need to
# recover cases from original non-desampled input files. Designate chosen sample
# as `orig_data`

# find shared cases with Preschool-25-School-Stand original input files
# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/PRESCHOOL/SM-ONLY-NORMS-INPUT/Preschool-25-School-SM-only-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))

orig_data <- Preschool_25_School_Stand

# READ SPM-2 FORMS, OBTAIN T-SCORES ---------------------------------------
# join with SPM2 data, obtain SPM2 raw scores
Preschool_25_School_Stand_SPMP_SPM2_T <- Preschool_25_School_Stand_SPMP_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPMP)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# GENERATE SPM2-SPMP CORR TABLE -------------------------------------
cor_cols <- Preschool_25_School_Stand_SPMP_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Preschool_25_School_Stand_SPMP_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Preschool-25-School-Stand',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

### Preschool-25-School-Clin DATA ---------------------------------------------------------
# READ SPMP FORMS---------------------------------------
SPMP_score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "BOD", "BAL", "PLA")

Preschool_25_School_Clin_SPMP_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/PAPER-FORMS/SPMP_School_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("TS")), ~ str_replace(., "TS", "NT_SPMP")) %>% 
  select(-V20)

# READ SPM-2 FORMS ---------------------------------------
# read clin data
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Clin.R"))

# join with SPMP
Preschool_25_School_Clin_SPMP_SPM2_T <- Preschool_25_School_Clin_SPMP_T %>% 
  inner_join(Preschool_25_School_Clin, by = "IDNumber") %>% 
  select(IDNumber, contains("_NT")) %>% 
  arrange(IDNumber) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_SPMP)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# GENERATE SPM2-SPMP CORR TABLE -------------------------------------
cor_cols <- Preschool_25_School_Clin_SPMP_SPM2_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Preschool_25_School_Clin_SPMP_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Preschool-25-School-Clin',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))
###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
SPMP_SPM2_cor_table <- bind_rows(
  Preschool_25_Home_Stand_SPMP_SPM2_cor_table,
  Preschool_25_Home_Clin_SPMP_SPM2_cor_table,
  Preschool_25_School_Stand_SPMP_SPM2_cor_table,
  Preschool_25_School_Clin_SPMP_SPM2_cor_table
)

write_csv(SPMP_SPM2_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t515-SPMP-SPM2-cor-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


