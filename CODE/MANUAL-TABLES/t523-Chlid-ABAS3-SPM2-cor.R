###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
### Child-512-Home-School-Stand DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# 512 HOME DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R"))

Child_512_Home_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/ABAS_3_Child_Parent_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/CHILD/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))
# 
# orig_data <- Child_512_Home_Stand

# obtain SPM2 raw scores and join with ABAS3 data
Child_512_Home_Stand_ABAS3_SPM2_T <- Child_512_Home_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, age_range, contains("ss"), SOC_raw, VIS_raw, HEA_raw, TOU_raw, 
         TS_raw, BOD_raw, BAL_raw, PLA_raw, TOT_raw) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .))

# read raw-to-t lookup tables, create lookup cols by scale
Child_512_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-Home-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    Child_512_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_512 <- map_dfc(score_names,
                      ~
                        Child_512_Home_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                          str_c(.x, '_512_lookup_col')
                        )),
                        by = str_c(.x, '_raw')) %>%
                        select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_Home_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-age_range, -(contains("_raw")))

scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Child_512_Home_Stand_ABAS3_ss_SPM2_T <- output_512 %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss, -r.Home_ss)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# 512 SCHOOL DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Child-512-School-item-vectors.R"))

Child_512_School_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/ABAS_3_Child_Teacher_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/CHILD/SM-ONLY-NORMS-INPUT/Child-512-School-SM-only-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))

orig_data <- Child_512_School_Stand

# obtain SPM2 T scores and join with ABAS3 data
Child_512_School_Stand_ABAS3_ss_SPM2_T <- Child_512_School_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  select(-r.School_ss, -r.CommunityUse_ss)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack Home and School data
Child_512_Stand_ABAS3_ss_SPM2_T <- bind_rows(
  Child_512_Home_Stand_ABAS3_ss_SPM2_T,
  Child_512_School_Stand_ABAS3_ss_SPM2_T
)

cor_cols <- Child_512_Stand_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

Child_512_Stand_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Child-512-Home-School-Stand',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

### Child-512-Home-School-Clin DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# 512 HOME DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R"))

Child_512_Home_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/ABAS_3_Child_Parent_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

# orig_data <-
#   suppressMessages(as_tibble(read_csv(
#     here('INPUT-FILES/CHILD/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv')
#   )))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Clin.R"))

orig_data <- Child_512_Home_Clin

# obtain SPM2 T scores and join with ABAS3 data
scale_order <- c("c.SOC_NT", "c.VIS_NT", "c.HEA_NT", "c.TOU_NT", 
                 "c.TS_NT", "c.BOD_NT", "c.BAL_NT", "c.PLA_NT", "c.TOT_NT")

Child_512_Home_Clin_ABAS3_ss_SPM2_T <- Child_512_Home_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:r.Prac_ss, scale_order, -r.CommunityUse_ss, -r.Home_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

# 512 SCHOOL DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Child-512-School-item-vectors.R"))

Child_512_School_Clin_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/ABAS_3_Child_Teacher_Clinical.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

# find shared cases with desampled data that has T-scores already
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Clin.R"))

orig_data <- Child_512_School_Clin

# obtain SPM2 T scores and join with ABAS3 data
Child_512_School_Clin_ABAS3_ss_SPM2_T <- Child_512_School_Clin_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, contains("ss"), SOC_NT, VIS_NT, HEA_NT, TOU_NT, 
         TS_NT, BOD_NT, BAL_NT, PLA_NT, TOT_NT) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .)) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>% 
  select(-r.School_ss)

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

# GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack Home and School data
Child_512_Clin_ABAS3_ss_SPM2_T <- bind_rows(
  Child_512_Home_Clin_ABAS3_ss_SPM2_T,
  Child_512_School_Clin_ABAS3_ss_SPM2_T
)

cor_cols <- Child_512_Clin_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

Child_512_Clin_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'Child-512-Home-School-Clin',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
Child_512_ABAS3_SPM2_cor_table <- bind_rows(
  Child_512_Stand_ABAS3_SPM2_cor_table,
  Child_512_Clin_ABAS3_SPM2_cor_table
  )

write_csv(Child_512_ABAS3_SPM2_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t523-Child-ABAS3-SPM2-cor-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

rm(list = ls())

