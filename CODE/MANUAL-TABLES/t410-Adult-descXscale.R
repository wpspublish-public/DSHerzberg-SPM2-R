suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# HOME DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

Adult_Other_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Other-allData-desamp.csv")
  )))

# Adult_Other_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Other-allData-desamp.csv")
#   ))) 

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Adult_Other_raw_desc <-
  Adult_Other_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "Other Form",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) 

# SELF DATA ---------------------------------------------

# READ FINALIZED STAND AND CLINICAL SAMPLES

Adult_Self_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Self-allData-desamp.csv")
  )))

# Adult_Self_Clin <-
#   suppressMessages(as_tibble(read_csv(
#     here("INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Self-allData-desamp.csv")
#   ))) 


# WRITE RAW SCORE DESCRIPTIVES TABLE WITH ES

Adult_Self_raw_desc <-
  Adult_Self_Stand %>% 
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

Adult_1221_raw_desc <- bind_rows(
  Adult_Self_raw_desc,
  Adult_Other_raw_desc
)

write_csv(Adult_1221_raw_desc, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t410-Adult-1221-descXscale-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)

