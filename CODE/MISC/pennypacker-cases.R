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

rm(list = ls(pattern = ("Preschool_|ps_")))

# CHILD_512_HOME -------------------------------------------------------

# READ FINALIZED STAND SAMPLE ---------------------------------------------

Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-Home-allData-desamp.csv")
  )))  %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range") 

child_512_home_pp <- inner_join(id_form$`child-home`, Child_512_Home, by = "IDNumber" )

# result: 5 pennypacker cases present.

Child_512_Home_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case.csv")
  ))) %>% 
  arrange(IDNumber) %>% 
  # remove 5 pennypacker cases
  anti_join(id_form$`child-home`, by = "IDNumber")


scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

Child_512_Home_Stand_raw_desc <-
  Child_512_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Home Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

# conclusion: with pennypacker cases removed, scale means change by less than a
# 10th of a raw score point. Therefore NO NEED TO REDO PUBLISHED NORMS TABLES,
# ANALYSES WITH PENNYPACKER CASES REMOVED.

# CHILD_512_SCHOOL -------------------------------------------------------

Child_512_School <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-School-allData-desamp.csv")
  ))) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

child_512_school_pp <- inner_join(id_form$`child-school`, Child_512_School, by = "IDNumber" )

# result: 40 pennypacker cases present.

Child_512_School_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-T-Scores-per-case.csv")
  ))) %>% 
  arrange(IDNumber) %>% 
  # remove 40 pennypacker cases
  anti_join(id_form$`child-school`, by = "IDNumber")


scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

Child_512_School_Stand_raw_desc <-
  Child_512_School_Stand %>% 
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

# conclusion: with 40 pennypacker cases removed, scale means change by .2 to .4
# raw score points for subscales, and .6 raw score points for TOT. Effect here
# is bigger than previous, because more cases were removed, but still no cause
# for concern. Therefore NO NEED TO REDO PUBLISHED NORMS TABLES, ANALYSES WITH
# PENNYPACKER CASES REMOVED.

rm(list = ls(pattern = ("Child_|child_")))

# TEEN_HOME -------------------------------------------------------

Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Home-allData-desamp.csv")
  ))) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

adolescent_1221_home_pp <- inner_join(id_form$`adolescent-home`, Teen_1221_Home, by = "IDNumber" )

# result: 0 pennypacker cases present.

# TEEN_SCHOOL -------------------------------------------------------

Teen_1221_School <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-School-allData-desamp.csv")
  ))) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

adolescent_1221_school_pp <- inner_join(id_form$`adolescent-school`, Teen_1221_School, by = "IDNumber" )

# result: 15 pennypacker cases present.

Teen_1221_School_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
  ))) %>% 
  arrange(IDNumber) %>% 
  # remove 15 pennypacker cases
  anti_join(id_form$`adolescent-school`, by = "IDNumber")

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

Teen_1221_School_Stand_raw_desc <-
  Teen_1221_School_Stand %>% 
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

# TEEN_SELF -------------------------------------------------------

Teen_1221_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Self-allData-desamp.csv")
  ))) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

adolescent_1221_self_pp <- inner_join(id_form$`adolescent-self`, Teen_1221_Self, by = "IDNumber" )

# result: 0 pennypacker cases present.

rm(list = ls(pattern = ("adolescent_|Teen_")))

# ADULT_RATER -------------------------------------------------------

Adult_Other <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Other-allData-desamp.csv")
  )))%>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

adult_rater_pp <- inner_join(id_form$`adult-rater`, Adult_Other, by = "IDNumber" )

# result: 0 pennypacker cases present.

# ADULT_RATER -------------------------------------------------------

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Self-allData-desamp.csv")
  )))%>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

adult_self_pp <- inner_join(id_form$`adult-self`, Adult_Self, by = "IDNumber" )

# result: 0 pennypacker cases present.


