### IT-1030-Home-Clin DATA ---------------------------------------------------------
# READ SP2 FORMS---------------------------------------
source(here("CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R"))

IT_1030_Home_Clin_SP2_raw <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/SP2_Toddler_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber)

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Clin.R"))

orig_data <- IT_1030_Home_Clin

# READ SPM-2 FORMS, OBTAIN T-SCORES ---------------------------------------
# join with SP2 data, obtain SPM2 raw scores

IT_1030_Home_Clin_SP2_SPM2_raw <- IT_1030_Home_Clin_SP2_raw %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("RS"), SOC_raw, VIS_raw, HEA_raw, TOU_raw, 
         TS_raw, BOD_raw, BAL_raw, PLA_raw, TOT_raw) %>% 
  rename_at(vars(contains("raw")), ~ str_c("c.", .))%>% 
  rename_at(vars(contains("RS")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("RS")), ~ str_replace(., "RS", "raw"))


# GENERATE SP2-SPM CORR TABLE -------------------------------------
cor_cols <- IT_1030_Home_Clin_SP2_SPM2_raw %>% 
  select(contains('_raw'))

IT_1030_Home_Clin_SP2_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace_all(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'IT-1030-Home-Clin',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(SP2_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))


