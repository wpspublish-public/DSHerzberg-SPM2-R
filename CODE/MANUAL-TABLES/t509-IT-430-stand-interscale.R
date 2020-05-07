###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## IT 49 HOME STAND--------------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

IT_49_Home_Stand_T_scores <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(contains("_NT"))

IT_49_Home_Stand_output <- data.frame(cor(IT_49_Home_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "IT-49-Home",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))
  
## IT 1030 HOME STAND----------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

IT_1030_Home_Stand_T_scores <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(contains("_NT"))

IT_1030_Home_Stand_output <- data.frame(cor(IT_1030_Home_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "IT-1030-Home",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))

## IT CAREGIVER STAND----------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

IT_Caregiver_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case.csv")
  ))) %>% 
  select(contains("_NT"))

IT_Caregiver_Stand_output <- data.frame(cor(IT_Caregiver_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "IT-Caregiver",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))
###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

IT_output <- bind_rows(
  IT_49_Home_Stand_output,
  IT_1030_Home_Stand_output,
  IT_Caregiver_Stand_output
)

write_csv(IT_output,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t509-IT-430-stand-interscale-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
