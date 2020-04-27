suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# HOME DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

Teen_1221_Home_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Home-allData-desamp.csv")
  )))

# Teen_1221_Home_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Home-allData-desamp.csv")
#   ))) 

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Teen_1221_Home_raw_desc <-
  Teen_1221_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Home Form",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) 

# SCHOOL DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

Teen_1221_School_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-School-allData-desamp.csv")
  )))

# Teen_1221_School_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-School-allData-desamp.csv")
#   ))) 


# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Teen_1221_School_raw_desc <-
  Teen_1221_School_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "School Form",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) 

# SELF DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

Teen_1221_Self_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Self-allData-desamp.csv")
  )))

# Teen_1221_Self_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/TEEN/ALLDATA-DESAMP-NORMS-INPUT/Teen-1221-Self-allData-desamp.csv")
#   ))) 


# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Teen_1221_Self_raw_desc <-
  Teen_1221_Self_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Self-Report Form",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) 

Teen_1221_raw_desc <- bind_rows(
  Teen_1221_Home_raw_desc,
  Teen_1221_School_raw_desc,
  Teen_1221_Self_raw_desc
)

write_csv(Teen_1221_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t409-Teen-1221-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

