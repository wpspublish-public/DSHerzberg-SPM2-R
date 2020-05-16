###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
#### Teen-1221-Home-School-Self-Stand DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# 1221 HOME DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R"))

Teen_1221_Home_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/PAPER-FORMS/ABAS_3_Teen_Parent_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss")) %>% 
  rename(HomeSchool_ss = Home_ss)

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/TEEN/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Home-combo-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))

orig_data <- Teen_1221_Home_Stand

# obtain SPM2 raw scores and join with ABAS3 data
scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Teen_1221_Home_Stand_ABAS3_ss_SPM2_T <- Teen_1221_Home_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# 1221 SCHOOL DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R"))

Teen_1221_School_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/PAPER-FORMS/ABAS_3_Teen_Teacher_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))%>% 
  rename(HomeSchool_ss = School_ss)

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/TEEN/SM-ONLY-NORMS-INPUT/Teen-1221-School-SM-only-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))

orig_data <- Teen_1221_School_Stand

# obtain SPM2 T scores and join with ABAS3 data
Teen_1221_School_Stand_ABAS3_ss_SPM2_T <- Teen_1221_School_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  select(-r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# 1221 SELF DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R"))

Teen_1221_Self_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/PAPER-FORMS/ABAS_3_Teen_Self_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss")) %>% 
  rename(HomeSchool_ss = Home_ss)

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/TEEN/SM-ONLY-NORMS-INPUT/Teen-1221-Self-SM-only-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))

orig_data <- Teen_1221_Self_Stand

# obtain SPM2 T scores and join with ABAS3 data
Teen_1221_Self_Stand_ABAS3_ss_SPM2_T <- Teen_1221_Self_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  select(-r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss")))

## GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack Home, School, Self data
Teen_1221_Stand_ABAS3_ss_SPM2_T <- bind_rows(
  Teen_1221_Home_Stand_ABAS3_ss_SPM2_T,
  Teen_1221_School_Stand_ABAS3_ss_SPM2_T,
  Teen_1221_Self_Stand_ABAS3_ss_SPM2_T
)

cor_cols <- Teen_1221_Stand_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

Teen_1221_Stand_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Teen-1221-Home-School-Self-Stand',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

#### Teen-1221-Home-School-Clin DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# 1221 HOME DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R"))

Teen_1221_Home_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/PAPER-FORMS/ABAS_3_Teen_Parent_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss")) %>% 
  rename(HomeSchool_ss = Home_ss)

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/TEEN/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Home-combo-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Clin.R"))

orig_data <- Teen_1221_Home_Clin

# obtain SPM2 T scores and join with ABAS3 data
scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Teen_1221_Home_Clin_ABAS3_ss_SPM2_T <- Teen_1221_Home_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

# 1221 SCHOOL DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R"))

Teen_1221_School_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/PAPER-FORMS/ABAS_3_Teen_Teacher_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss")) %>% 
  rename(HomeSchool_ss = School_ss)

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Clin.R"))

orig_data <- Teen_1221_School_Clin

# obtain SPM2 T scores and join with ABAS3 data
Teen_1221_School_Clin_ABAS3_ss_SPM2_T <- Teen_1221_School_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  select(-r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

# 1221 SELF DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R"))

Teen_1221_Self_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/PAPER-FORMS/ABAS_3_Teen_Self_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss")) %>% 
  rename(HomeSchool_ss = Home_ss)

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/TEEN/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Self-combo-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Clin.R"))

orig_data <- Teen_1221_Self_Clin

# obtain SPM2 T scores and join with ABAS3 data
scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Teen_1221_Self_Clin_ABAS3_ss_SPM2_T <- Teen_1221_Self_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

## GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack Home, School, Self data
Teen_1221_Clin_ABAS3_ss_SPM2_T <- bind_rows(
  Teen_1221_Home_Clin_ABAS3_ss_SPM2_T,
  Teen_1221_School_Clin_ABAS3_ss_SPM2_T,
  Teen_1221_Self_Clin_ABAS3_ss_SPM2_T
)

cor_cols <- Teen_1221_Clin_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

Teen_1221_Clin_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Teen-1221-Home-School-Self-Clin',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
Teen_1221_ABAS3_SPM2_cor_table <- bind_rows(
  Teen_1221_Stand_ABAS3_SPM2_cor_table,
  Teen_1221_Clin_ABAS3_SPM2_cor_table
  )

write_csv(Teen_1221_ABAS3_SPM2_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t524-Teen-ABAS3-SPM2-cor-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

rm(list = ls())

