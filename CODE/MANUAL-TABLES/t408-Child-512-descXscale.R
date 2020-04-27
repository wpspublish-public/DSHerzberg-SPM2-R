suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# HOME DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

Child_512_Home_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-Home-allData-desamp.csv")
  )))

# Child_512_Home_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-Home-allData-desamp.csv")
#   ))) 

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Child_512_Home_raw_desc <-
  Child_512_Home_Stand %>% 
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

Child_512_School_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-School-allData-desamp.csv")
  )))

# Child_512_School_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-School-allData-desamp.csv")
#   ))) 


# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Child_512_School_raw_desc <-
  Child_512_School_Stand %>% 
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

Child_512_raw_desc <- bind_rows(
  Child_512_Home_raw_desc,
  Child_512_School_raw_desc
)

write_csv(Child_512_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t408-Child-512-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

