suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

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

ps_24_home_pp <- semi_join(id_form$`preschool-home`, Preschool_24_Home, by = "IDNumber" )

# result: no penny packer cases present.

# PRESCHOOL_5_HOME --------------------------------------------------------

Preschool_5_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-Home-allData-desamp.csv")
  ))) %>% 
  filter(Age == 5) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

ps_5_home_pp <- semi_join(id_form$`preschool-home`, Preschool_5_Home, by = "IDNumber" )

# result: no penny packer cases present.


