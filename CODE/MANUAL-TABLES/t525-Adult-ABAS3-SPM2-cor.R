###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
#### Adult-Self-Other-Stand DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# ADULT SELF DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Adult-Self-item-vectors.R"))

Adult_Self_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/PAPER-FORMS/ABAS_3_Adult_Self_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/ADULT/SM-QUAL-COMBO-NORMS-INPUT/Adult-Self-combo-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Self-Stand.R"))
# 
# orig_data <- Adult_Self_Stand

# obtain SPM2 T scores and join with ABAS3 data
Adult_Self_Stand_ABAS3_SPM2_T <- Adult_Self_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_raw, VIS_raw, HEA_raw, TOU_raw, 
         TS_raw, BOD_raw, BAL_raw, PLA_raw, TOT_raw) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  select(-r.CommunityUse_ss)

# read raw-to-t lookup tables, create lookup cols by scale
Adult_Self_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/RAW-T-LOOKUP-TABLES/Adult-Self-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    Adult_Self_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Adult_Self_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_Adult_Self <- map_dfc(score_names,
                             ~
                               Adult_Self_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                                 str_c(.x, '_Adult_Self_lookup_col')
                               )),
                               by = str_c(.x, '_raw')) %>%
                               select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Adult_Self_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-(contains("_raw")))

scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Adult_Self_Stand_ABAS3_ss_SPM2_T <- output_Adult_Self %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# ADULT OTHER  DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Adult-Other-item-vectors.R"))

Adult_Other_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/PAPER-FORMS/ABAS_3_Adult_Other_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/ADULT/SM-ONLY-NORMS-INPUT/Adult-Other-SM-only-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Stand.R"))
# 
# orig_data <- Adult_Other_Stand

# obtain SPM2 T scores and join with ABAS3 data
Adult_Other_Stand_ABAS3_SPM2_T <- Adult_Other_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_raw, VIS_raw, HEA_raw, TOU_raw, 
         TS_raw, BOD_raw, BAL_raw, PLA_raw, TOT_raw) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  select(-r.CommunityUse_ss)

# read raw-to-t lookup tables, create lookup cols by scale
Adult_Other_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/RAW-T-LOOKUP-TABLES/Adult-Other-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    Adult_Other_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Adult_Other_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_Adult_Other <- map_dfc(score_names,
                              ~
                                Adult_Other_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                                  str_c(.x, '_Adult_Other_lookup_col')
                                )),
                                by = str_c(.x, '_raw')) %>%
                                select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Adult_Other_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-(contains("_raw")))

scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Adult_Other_Stand_ABAS3_ss_SPM2_T <- output_Adult_Other %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order)

rm(list = setdiff(ls(), ls(pattern = "ss")))

## GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack Self, Other data
Adult_Stand_ABAS3_ss_SPM2_T <- bind_rows(
  Adult_Self_Stand_ABAS3_ss_SPM2_T,
  Adult_Other_Stand_ABAS3_ss_SPM2_T,
)

cor_cols <- Adult_Stand_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

Adult_Stand_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Adult-Self-Other-Stand',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

#### Adult-Self-Other-Clin DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# ADULT SELF DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Adult-Self-item-vectors.R"))

Adult_Self_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/PAPER-FORMS/ABAS_3_Adult_Self_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/ADULT/SM-QUAL-COMBO-NORMS-INPUT/Adult-Self-combo-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Self-Clin.R"))

orig_data <- Adult_Self_Clin

# obtain SPM2 T scores and join with ABAS3 data
scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Adult_Self_Clin_ABAS3_ss_SPM2_T <- Adult_Self_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

# ADULT OTHER DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Adult-Other-item-vectors.R"))

Adult_Other_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/PAPER-FORMS/ABAS_3_Adult_Other_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/ADULT/SM-QUAL-COMBO-NORMS-INPUT/Adult-Other-combo-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Clin.R"))

orig_data <- Adult_Other_Clin

# obtain SPM2 T scores and join with ABAS3 data
scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Adult_Other_Clin_ABAS3_ss_SPM2_T <- Adult_Other_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

## GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack Self, Other data
Adult_Clin_ABAS3_ss_SPM2_T <- bind_rows(
  Adult_Self_Clin_ABAS3_ss_SPM2_T,
  Adult_Other_Clin_ABAS3_ss_SPM2_T
)

cor_cols <- Adult_Clin_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

Adult_Clin_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Adult-Home-School-Clin',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
Adult_ABAS3_SPM2_cor_table <- bind_rows(
  Adult_Stand_ABAS3_SPM2_cor_table,
  Adult_Clin_ABAS3_SPM2_cor_table
  )

write_csv(Adult_ABAS3_SPM2_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t525-Adult-ABAS3-SPM2-cor-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

rm(list = ls())

