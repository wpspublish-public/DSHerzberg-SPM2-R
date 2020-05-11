### IT-430-Home-Clin DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# 49 DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R"))

IT_49_Home_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/ABAS_3_Infant_PPC_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  select(-(CommunityUse_ss:Home_ss)) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Clin.R"))

orig_data <- IT_49_Home_Clin

# obtain SPM2 T scores and join with ABAS3 data
IT_49_Home_Clin_ABAS3_ss_SPM2_T <- IT_49_Home_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .))
  
rm(list = setdiff(ls(), ls(pattern = "ss")))

# 1030 DATA -----------------------------------------------------------------

source(here("CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R"))

IT_1030_Home_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/ABAS_3_Toddler_PPC_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  select(-Diagnosis, -(CommunityUse_ss:Home_ss)) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss")) 

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Clin.R"))

orig_data <- IT_1030_Home_Clin

# obtain SPM2 T scores and join with ABAS3 data
IT_1030_Home_Clin_ABAS3_ss_SPM2_T <- IT_1030_Home_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .))

rm(list = setdiff(ls(), ls(pattern = "ss")))

# GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack 49 and 1030 data
IT_430_Home_Clin_ABAS3_ss_SPM2_T <- bind_rows(
  IT_49_Home_Clin_ABAS3_ss_SPM2_T,
  IT_1030_Home_Clin_ABAS3_ss_SPM2_T
)

cor_cols <- IT_430_Home_Clin_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

IT_430_Home_Clin_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'IT-430-Home-Clin',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ (round(., 3))) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))



