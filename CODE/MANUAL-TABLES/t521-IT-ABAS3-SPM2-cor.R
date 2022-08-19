###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
### IT-430-Home-Stand DATA ---------------------------------------------------------
## READ ABAS3 FORMS, JOIN WITH SPM-2 T-SCORES ----------------------------------
# 49 DATA -----------------------------------------------------------------
source(here("CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R"))

IT_49_Home_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/ABAS_3_Infant_PPC_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  select(-(CommunityUse_ss:Home_ss), -V31) %>% 
  rename_at(vars(contains("_StanS")), ~ str_replace(., "_StanS", "_ss"))

orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Clin.R"))
# 
# orig_data <- IT_49_Home_Clin

# obtain SPM2 T scores and join with ABAS3 data
IT_49_Home_Stand_ABAS3_SPM2_T <- IT_49_Home_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, age_range, contains("ss"), SOC_raw, VIS_raw, HEA_raw, TOU_raw, 
         TS_raw, BOD_raw, BAL_raw, PLA_raw, TOT_raw) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .))

# split sample by age_range

IT_46_Home_Stand_ABAS3_SPM2_T <- IT_49_Home_Stand_ABAS3_SPM2_T %>% filter(age_range == "03.5 to 6 mo")
IT_79_Home_Stand_ABAS3_SPM2_T <- IT_49_Home_Stand_ABAS3_SPM2_T %>% filter(age_range == "07 to 10.5 mo")

# 46 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_46_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-46-Home-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    IT_46_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_46_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_46 <- map_dfc(score_names,
                     ~
                       IT_46_Home_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                         str_c(.x, '_46_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_46_Home_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-age_range, -(contains("_raw")))

rm(list = ls(pattern = "col"))

# 79 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_79_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-79-Home-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    IT_79_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_79_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_79 <- map_dfc(score_names,
                     ~
                       IT_79_Home_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                         str_c(.x, '_79_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_79_Home_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-age_range, -(contains("_raw")))

rm(list = ls(pattern = "col"))

# recombine 49 data
IT_49_Home_Stand_ABAS3_ss_SPM2_T <- bind_rows(
  output_46,
  output_79
) %>% arrange(IDNumber)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# 1030 DATA -----------------------------------------------------------------

source(here("CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R"))

IT_1030_Home_Stand_ABAS3_T <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/PAPER-FORMS/ABAS_3_Toddler_PPC_Standard.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  select(-(CommunityUse_ss:Home_ss)) %>% 
  rename_at(vars(contains("_StandS")), ~ str_replace(., "_StandS", "_ss")) %>% 
  rename(Con_ss = Conceptual_ss, Prac_ss= Practical_ss)

orig_data <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv')
  )))

# find shared cases with desampled data that has T-scores already
# source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Clin.R"))
# 
# orig_data <- IT_1030_Home_Clin

# obtain SPM2 T scores and join with ABAS3 data
IT_1030_Home_Stand_ABAS3_SPM2_T <- IT_1030_Home_Stand_ABAS3_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, age_range, contains("ss"), SOC_raw, VIS_raw, HEA_raw, TOU_raw, 
         TS_raw, BOD_raw, BAL_raw, PLA_raw, TOT_raw) %>% 
  rename_at(vars(contains("ss")), ~ str_c("r.", .))

# split sample by age_range

IT_1020_Home_Stand_ABAS3_SPM2_T <- IT_1030_Home_Stand_ABAS3_SPM2_T %>% filter(age_range == "09.5 to 20 mo")
IT_2130_Home_Stand_ABAS3_SPM2_T <- IT_1030_Home_Stand_ABAS3_SPM2_T %>% filter(age_range == "21 to 31.5 mo")

# 1020 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_1020_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-1020-Home-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    IT_1020_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1020_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_1020 <- map_dfc(score_names,
                       ~
                         IT_1020_Home_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                           str_c(.x, '_1020_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_1020_Home_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-age_range, -(contains("_raw")))

rm(list = ls(pattern = "col"))

# 2130 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_2130_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-2130-Home-raw-T-lookup.csv")
  )))

map_df(
  scale_order,
  ~
    IT_2130_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_2130_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_2130 <- map_dfc(score_names,
                       ~
                         IT_2130_Home_Stand_ABAS3_SPM2_T %>% left_join(eval(as.name(
                           str_c(.x, '_2130_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_2130_Home_Stand_ABAS3_SPM2_T, .) %>% 
  arrange(IDNumber) %>% 
  rename_at(vars(contains("NT")), ~ str_c("c.", .)) %>%
  select(-age_range, -(contains("_raw")))

rm(list = ls(pattern = "col"))

# recombine 1030 data
IT_1030_Home_Stand_ABAS3_ss_SPM2_T <- bind_rows(
  output_1020,
  output_2130
) %>% arrange(IDNumber)

rm(list = setdiff(ls(), ls(pattern = "ss")))

# GENERATE ABAS3-SPM CORR TABLE -------------------------------------
# stack 49 and 1030 data
IT_430_Home_Stand_ABAS3_ss_SPM2_T <- bind_rows(
  IT_49_Home_Stand_ABAS3_ss_SPM2_T,
  IT_1030_Home_Stand_ABAS3_ss_SPM2_T
)

cor_cols <- IT_430_Home_Stand_ABAS3_ss_SPM2_T %>% 
  select(-IDNumber)

IT_430_Home_Stand_ABAS3_SPM2_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  separate(pair, c("row", "col"), sep = "-") %>%
  filter((str_detect(row, "r.") &
            str_detect(col, "c.")) & !str_detect(col, "r.")) %>%
  mutate_at(vars(row, col), ~ str_replace(str_sub(., 3), "_", "")) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'IT-430-Home-Stand',
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  select(form, n, col, row, r, p) %>% 
  rename(ABAS3_col_label = row, SPM2_row_label = col)

rm(list = setdiff(ls(), ls(pattern = 'table')))

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

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

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

rm(list = setdiff(ls(), ls(pattern = "ss|table")))

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

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
IT_430_Home_ABAS3_SPM2_cor_table <- bind_rows(
  IT_430_Home_Stand_ABAS3_SPM2_cor_table,
  IT_430_Home_Clin_ABAS3_SPM2_cor_table
  )

write_csv(IT_430_Home_ABAS3_SPM2_cor_table,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t521-IT-ABAS3-SPM2-cor-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

rm(list = ls())

