###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## IT-49-Home DATA ---------------------------------------------------------
# READ PAPER FORMS, OBTAIN T-SCORES ---------------------------------------
source(here('CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R'))

# Read paper forms
IT_49_Home_paper <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/SPM-2 Infant 4-9.csv")
  ))) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>%
  mutate_at(
    SOC_rev_items_IT_49_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_49_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_49_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_49_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_49_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_49_Home]),
    TS_raw = rowSums(.[TS_items_IT_49_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_49_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_49_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_49_Home])
  ) %>% 
  select(IDNumber, contains("_raw"))

# Extract IDNumbers
IT_49_Home_paper_ID <- IT_49_Home_paper %>% 
  select(IDNumber)

# find shared cases with IT-49-Home-Stand
orig_data <- 
    suppressMessages(as_tibble(read_csv(
      here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv')
    )))

paper_AgeInMonths <- IT_49_Home_paper_ID %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths)

# Add AgeInMonths to paper data
IT_49_Home_paper <- IT_49_Home_paper %>% 
  full_join(paper_AgeInMonths, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths, everything())

# split sample by age_range

IT_46_Home_paper <- IT_49_Home_paper %>% filter(AgeInMonths <= 6)
IT_79_Home_paper <- IT_49_Home_paper %>% filter(AgeInMonths >= 7)

# 46 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_46_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-46-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_46_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_46_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_46_Home_paper_T <- map_dfc(score_names,
                              ~
                                IT_46_Home_paper %>% left_join(eval(as.name(
                                  str_c(.x, '_46_lookup_col')
                                )),
                                by = str_c(.x, '_raw')) %>%
                                select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_46_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))

# 79 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_79_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-79-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_79_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_79_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_79_Home_paper_T <- map_dfc(score_names,
                              ~
                                IT_79_Home_paper %>% left_join(eval(as.name(
                                  str_c(.x, '_79_lookup_col')
                                )),
                                by = str_c(.x, '_raw')) %>%
                                select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_79_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))

# Combine 46, 79 paper T-scores
IT_49_Home_paper_T <- bind_rows(
  IT_46_Home_paper_T,
  IT_79_Home_paper_T
) %>% 
  arrange(IDNumber)

# READ DIGITAL FORMS, OBTAIN T-SCORES ---------------------------------------
# join with digital data, obtain digital raw scores
IT_49_Home_paper_T_dig_raw <- IT_49_Home_paper_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_49_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_49_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_49_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_49_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_49_Home]),
    TS_raw = rowSums(.[TS_items_IT_49_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_49_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_49_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_49_Home])
  ) %>% 
  select(IDNumber, AgeInMonths, contains("_NT"),contains("_raw"))

# split sample by age_range

IT_46_Home_paper_T_dig_raw <- IT_49_Home_paper_T_dig_raw %>% filter(AgeInMonths <= 6)
IT_79_Home_paper_T_dig_raw <- IT_49_Home_paper_T_dig_raw %>% filter(AgeInMonths >= 7)

# 46 DATA

# create lookup cols by scale
map_df(
  score_names,
  ~
    IT_46_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_46_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_46_Home_paper_dig_T <- map_dfc(score_names,
                              ~
                                IT_46_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                  str_c(.x, '_46_lookup_col')
                                )),
                                by = str_c(.x, '_raw')) %>%
                                select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_46_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# 79 DATA

# read raw-to-t lookup tables, create lookup cols by scale
map_df(
  score_names,
  ~
    IT_79_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_79_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_79_Home_paper_dig_T <- map_dfc(score_names,
                              ~
                                IT_79_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                  str_c(.x, '_79_lookup_col')
                                )),
                                by = str_c(.x, '_raw')) %>%
                                select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_79_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# Combine 46, 79 paper T-scores
IT_49_Home_paper_dig_T <- bind_rows(
  IT_46_Home_paper_dig_T,
  IT_79_Home_paper_dig_T
) %>% 
  arrange(IDNumber)


# GENERATE DIG-PAPER EQUIV CORR TABLE -------------------------------------
cor_cols <- IT_49_Home_paper_dig_T %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

IT_49_Home_paper_dig_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'IT-49-Home',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))

## IT-1030-Home DATA ---------------------------------------------------------
# READ PAPER FORMS, OBTAIN T-SCORES ---------------------------------------
source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

# Read paper forms
IT_1030_Home_paper <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/SPM-2 Toddler 10-30.csv")
  ))) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  mutate_at(
    SOC_rev_items_IT_1030_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_1030_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_1030_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_1030_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_1030_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_1030_Home]),
    TS_raw = rowSums(.[TS_items_IT_1030_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_1030_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_1030_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_1030_Home])
  ) %>% 
  select(IDNumber, contains("_raw"))

# Extract IDNumbers
IT_1030_Home_paper_ID <- IT_1030_Home_paper %>% 
  select(IDNumber)

# find shared cases with IT-1030-Home-Stand
orig_data <- 
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv')
  )))

paper_AgeInMonths <- IT_1030_Home_paper_ID %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths)

# Add AgeInMonths to paper data
IT_1030_Home_paper <- IT_1030_Home_paper %>% 
  full_join(paper_AgeInMonths, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths, everything()) %>% 
  drop_na()

# split sample by age_range

IT_1020_Home_paper <- IT_1030_Home_paper %>% filter(AgeInMonths <= 20)
IT_2130_Home_paper <- IT_1030_Home_paper %>% filter(AgeInMonths >= 21)

# 1020 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_1020_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-1020-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_1020_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1020_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_1020_Home_paper_T <- map_dfc(score_names,
                                ~
                                  IT_1020_Home_paper %>% left_join(eval(as.name(
                                    str_c(.x, '_1020_lookup_col')
                                  )),
                                  by = str_c(.x, '_raw')) %>%
                                  select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_1020_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))

# 2130 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_2130_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-2130-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_2130_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_2130_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_2130_Home_paper_T <- map_dfc(score_names,
                                ~
                                  IT_2130_Home_paper %>% left_join(eval(as.name(
                                    str_c(.x, '_2130_lookup_col')
                                  )),
                                  by = str_c(.x, '_raw')) %>%
                                  select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_2130_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))

# Combine 1020, 2130 paper T-scores
IT_1030_Home_paper_T <- bind_rows(
  IT_1020_Home_paper_T,
  IT_2130_Home_paper_T
) %>% 
  arrange(IDNumber)

# READ DIGITAL FORMS, OBTAIN T-SCORES ---------------------------------------
# join with digital data, obtain digital raw scores
IT_1030_Home_paper_T_dig_raw <- IT_1030_Home_paper_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_1030_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_1030_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_1030_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_1030_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_1030_Home]),
    TS_raw = rowSums(.[TS_items_IT_1030_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_1030_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_1030_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_1030_Home])
  ) %>% 
  select(IDNumber, AgeInMonths, contains("_NT"),contains("_raw"))

# split sample by age_range

IT_1020_Home_paper_T_dig_raw <- IT_1030_Home_paper_T_dig_raw %>% filter(AgeInMonths <= 20)
IT_2130_Home_paper_T_dig_raw <- IT_1030_Home_paper_T_dig_raw %>% filter(AgeInMonths >= 21)

# 1020 DATA

# create lookup cols by scale
map_df(
  score_names,
  ~
    IT_1020_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1020_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_1020_Home_paper_dig_T <- map_dfc(score_names,
                                    ~
                                      IT_1020_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                        str_c(.x, '_1020_lookup_col')
                                      )),
                                      by = str_c(.x, '_raw')) %>%
                                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_1020_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# 2130 DATA

# read raw-to-t lookup tables, create lookup cols by scale
map_df(
  score_names,
  ~
    IT_2130_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_2130_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_2130_Home_paper_dig_T <- map_dfc(score_names,
                                    ~
                                      IT_2130_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                        str_c(.x, '_2130_lookup_col')
                                      )),
                                      by = str_c(.x, '_raw')) %>%
                                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_2130_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# Combine 1020, 2130 paper T-scores
IT_1030_Home_paper_dig_T <- bind_rows(
  IT_1020_Home_paper_dig_T,
  IT_2130_Home_paper_dig_T
) %>% 
  arrange(IDNumber)
# GENERATE DIG-PAPER EQUIV CORR TABLE -------------------------------------
cor_cols <- IT_1030_Home_paper_dig_T %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

IT_1030_Home_paper_dig_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'IT-1030-Home',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))





## IT-Caregiver DATA ---------------------------------------------------------
# READ PAPER FORMS, OBTAIN T-SCORES ---------------------------------------
source(here('CODE/ITEM-VECTORS/IT-Caregiver-item-vectors.R'))

# Read paper forms
IT_Caregiver_paper <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/SPM2_InfantToddler_Caregiver_Combo.csv")
  ))) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  mutate_at(
    SOC_rev_items_IT_Caregiver,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_Caregiver]),
    SOC_raw = rowSums(.[SOC_items_IT_Caregiver]),
    VIS_raw = rowSums(.[VIS_items_IT_Caregiver]),
    HEA_raw = rowSums(.[HEA_items_IT_Caregiver]),
    TOU_raw = rowSums(.[TOU_items_IT_Caregiver]),
    TS_raw = rowSums(.[TS_items_IT_Caregiver]),
    BOD_raw = rowSums(.[BOD_items_IT_Caregiver]),
    BAL_raw = rowSums(.[BAL_items_IT_Caregiver]),
    PLA_raw = rowSums(.[PLA_items_IT_Caregiver])
  ) %>% 
  select(IDNumber, contains("_raw"))

# Extract IDNumbers
IT_Caregiver_paper_ID <- IT_Caregiver_paper %>% 
  select(IDNumber)

# find shared cases with IT-Caregiver-Stand
orig_data <- 
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-Caregiver-combo-norms-input.csv')
  )))

paper_AgeInMonths <- IT_Caregiver_paper_ID %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths)

# Add AgeInMonths to paper data
IT_Caregiver_paper <- IT_Caregiver_paper %>% 
  full_join(paper_AgeInMonths, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths, everything()) %>% 
  drop_na()

# read raw-to-t lookup tables, create lookup cols by scale
IT_Caregiver_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-Caregiver-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_Caregiver_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Caregiver_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_Caregiver_paper_T <- map_dfc(score_names,
                                ~
                                  IT_Caregiver_paper %>% left_join(eval(as.name(
                                    str_c(.x, '_Caregiver_lookup_col')
                                  )),
                                  by = str_c(.x, '_raw')) %>%
                                  select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_Caregiver_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))


# READ DIGITAL FORMS, OBTAIN T-SCORES ---------------------------------------
# join with digital data, obtain digital raw scores
IT_Caregiver_paper_T_dig_raw <- IT_Caregiver_paper_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_Caregiver]),
    SOC_raw = rowSums(.[SOC_items_IT_Caregiver]),
    VIS_raw = rowSums(.[VIS_items_IT_Caregiver]),
    HEA_raw = rowSums(.[HEA_items_IT_Caregiver]),
    TOU_raw = rowSums(.[TOU_items_IT_Caregiver]),
    TS_raw = rowSums(.[TS_items_IT_Caregiver]),
    BOD_raw = rowSums(.[BOD_items_IT_Caregiver]),
    BAL_raw = rowSums(.[BAL_items_IT_Caregiver]),
    PLA_raw = rowSums(.[PLA_items_IT_Caregiver])
  ) %>% 
  select(IDNumber, AgeInMonths, contains("_NT"),contains("_raw"))

# create lookup cols by scale
map_df(
  score_names,
  ~
    IT_Caregiver_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Caregiver_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_Caregiver_paper_dig_T <- map_dfc(score_names,
                                    ~
                                      IT_Caregiver_paper_T_dig_raw %>% left_join(eval(as.name(
                                        str_c(.x, '_Caregiver_lookup_col')
                                      )),
                                      by = str_c(.x, '_raw')) %>%
                                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_Caregiver_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# GENERATE DIG-PAPER EQUIV CORR TABLE -------------------------------------
cor_cols <- IT_Caregiver_paper_dig_T %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

IT_Caregiver_paper_dig_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'IT-Caregiver',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


## Preschool-25-Home DATA ---------------------------------------------------------
# READ PAPER FORMS, OBTAIN T-SCORES ---------------------------------------
source(here('CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R'))

# Read paper forms
Preschool_25_Home_paper <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/PAPER-FORMS/SPM-2 Preschool Home 2-5.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  mutate_at(
    SOC_rev_items_Preschool_25_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_Home]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_Home]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_Home]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_Home]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_Home]),
    TS_raw = rowSums(.[TS_items_Preschool_25_Home]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_Home]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_Home]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_Home])
  ) %>% 
  select(IDNumber, contains("_raw"))

# Extract IDNumbers
Preschool_25_Home_paper_ID <- Preschool_25_Home_paper %>% 
  select(IDNumber)

# find shared cases with Preschool-25-Home-Stand
orig_data <- 
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/PRESCHOOL/SM-QUAL-COMBO-NORMS-INPUT/Preschool-25-Home-combo-norms-input.csv')
  )))

paper_Age <- Preschool_25_Home_paper_ID %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, Age)

# Add Age to paper data
Preschool_25_Home_paper <- Preschool_25_Home_paper %>% 
  full_join(paper_Age, by = "IDNumber") %>% 
  select(IDNumber, Age, everything()) %>% 
  drop_na()

# split sample by age_range

Preschool_24_Home_paper <- Preschool_25_Home_paper %>% filter(Age <= 4)
Preschool_5_Home_paper <- Preschool_25_Home_paper %>% filter(Age >= 5)

# 24 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_24_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-24-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_24_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_24_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Preschool_24_Home_paper_T <- map_dfc(score_names,
                                     ~
                                       Preschool_24_Home_paper %>% left_join(eval(as.name(
                                         str_c(.x, '_24_lookup_col')
                                       )),
                                       by = str_c(.x, '_raw')) %>%
                                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))

# 5 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_5_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-5-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_5_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_5_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Preschool_5_Home_paper_T <- map_dfc(score_names,
                                    ~
                                      Preschool_5_Home_paper %>% left_join(eval(as.name(
                                        str_c(.x, '_5_lookup_col')
                                      )),
                                      by = str_c(.x, '_raw')) %>%
                                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))

# Combine 24, 5 paper T-scores
Preschool_25_Home_paper_T <- bind_rows(
  Preschool_24_Home_paper_T,
  Preschool_5_Home_paper_T
) %>% 
  arrange(IDNumber)

# READ DIGITAL FORMS, OBTAIN T-SCORES ---------------------------------------
# join with digital data, obtain digital raw scores
Preschool_25_Home_paper_T_dig_raw <- Preschool_25_Home_paper_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_Home]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_Home]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_Home]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_Home]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_Home]),
    TS_raw = rowSums(.[TS_items_Preschool_25_Home]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_Home]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_Home]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_Home])
  ) %>% 
  select(IDNumber, Age, contains("_NT"),contains("_raw"))

# split sample by age_range

Preschool_24_Home_paper_T_dig_raw <- Preschool_25_Home_paper_T_dig_raw %>% filter(Age <= 4)
Preschool_5_Home_paper_T_dig_raw <- Preschool_25_Home_paper_T_dig_raw %>% filter(Age >= 5)

# 24 DATA

# create lookup cols by scale
map_df(
  score_names,
  ~
    IT_24_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_24_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Preschool_24_Home_paper_dig_T <- map_dfc(score_names,
                                         ~
                                           Preschool_24_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                             str_c(.x, '_24_lookup_col')
                                           )),
                                           by = str_c(.x, '_raw')) %>%
                                           select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# 5 DATA

# read raw-to-t lookup tables, create lookup cols by scale
map_df(
  score_names,
  ~
    IT_5_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_5_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Preschool_5_Home_paper_dig_T <- map_dfc(score_names,
                                        ~
                                          Preschool_5_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                            str_c(.x, '_5_lookup_col')
                                          )),
                                          by = str_c(.x, '_raw')) %>%
                                          select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# Combine 24, 5 paper T-scores
Preschool_25_Home_paper_dig_T <- bind_rows(
  Preschool_24_Home_paper_dig_T,
  Preschool_5_Home_paper_dig_T
) %>% 
  arrange(IDNumber)
# GENERATE DIG-PAPER EQUIV CORR TABLE -------------------------------------
cor_cols <- Preschool_25_Home_paper_dig_T %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Preschool_25_Home_paper_dig_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Preschool-25-Home',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))





