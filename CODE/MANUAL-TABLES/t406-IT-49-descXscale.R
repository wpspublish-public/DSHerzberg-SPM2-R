suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# 49 DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

IT_49_Home_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-49-Home-allData-desamp.csv")
  )))

# IT_49_Home_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-49-Home-allData-desamp.csv")
#   ))) 

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

IT_49_Home_raw_desc <-
  IT_49_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Infant Form",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) 

# 1030 DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

IT_1030_Home_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-1030-Home-allData-desamp.csv")
  )))

# IT_1030_Home_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-1030-Home-allData-desamp.csv")
#   ))) 


# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

IT_1030_Home_raw_desc <-
  IT_1030_Home_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Toddler Form",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) 

IT_430_Home_raw_desc <- bind_rows(
  IT_49_Home_raw_desc,
  IT_1030_Home_raw_desc
)

write_csv(IT_430_Home_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t406-IT-49-1030-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

