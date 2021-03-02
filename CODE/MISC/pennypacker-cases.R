suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

pp_id_nums <- suppressMessages(read_csv(
  here("INPUT-FILES/DATA_CLEANUP_FILES/pennypacker-spm2-id-nums.csv")
)) %>% rename(IDNumber = id)

forms <- pp_id_nums %>% 
  select(form) %>% 
  as_vector() %>% 
  unique()

id_form <- map(
  forms,
 ~ pp_id_nums %>% filter(form == .x) 
) %>% 
  set_names(forms)


# PRESCHOOL_24_HOME -------------------------------------------------------

Preschool_24_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-Home-allData-desamp.csv")
  ))) %>% 
  filter(Age %in% c(2:4))%>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

ps_24_home_pp <- inner_join(id_form$`preschool-home`, Preschool_24_Home, by = "IDNumber" )

# result: no pennypacker cases present.

# PRESCHOOL_5_HOME --------------------------------------------------------

Preschool_5_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-Home-allData-desamp.csv")
  ))) %>% 
  filter(Age == 5) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

ps_5_home_pp <- inner_join(id_form$`preschool-home`, Preschool_5_Home, by = "IDNumber" )

# result: no pennypacker cases present.

# PRESCHOOL_24_SCHOOL -------------------------------------------------------

Preschool_24_School <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-School-allData-desamp.csv")
  ))) %>% 
  filter(Age %in% c(2:4))%>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

ps_24_school_pp <- inner_join(id_form$`preschool-school`, Preschool_24_School, by = "IDNumber" )

# result: 10 pennypacker cases present.

# PRESCHOOL_5_SCHOOL --------------------------------------------------------

Preschool_5_School <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-School-allData-desamp.csv")
  ))) %>% 
  filter(Age == 5) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

ps_5_school_pp <- inner_join(id_form$`preschool-school`, Preschool_5_School, by = "IDNumber" )

# result: 5 pennypacker cases present.

#

########## STAND

# READ FINALIZED SAMPLE

Preschool_25_School_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  # remove 15 pennypacker cases
  anti_join(id_form$`preschool-school`, by = "IDNumber")

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES without pennypacker cases

Preschool_25_School_Stand_raw_desc <-
  Preschool_25_School_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "School Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

# conclusion: with pennypacker cases removed, scale means change by less than a
# 10th of a raw score point. Therefore NO NEED TO REDO PUBLISHED NORMS TABLES,
# ANALYSES WITH PENNYPACKER CASES REMOVED.


