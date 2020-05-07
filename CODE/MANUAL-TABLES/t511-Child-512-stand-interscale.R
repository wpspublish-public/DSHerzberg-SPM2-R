###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## CHILD 512 HOME STAND--------------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Child_512_Home_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case.csv")
  ))
) %>% 
  select(contains("_NT"))

Child_512_Home_Stand_output <- data.frame(cor(Child_512_Home_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "Child-512-Home",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))
  
## CHILD 512 SCHOOL STAND----------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Child_512_School_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-T-Scores-per-case.csv")
  ))
) %>% 
  select(contains("_NT"))

Child_512_School_Stand_output <- data.frame(cor(Child_512_School_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "Child-512-School",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

Child_512_output <- bind_rows(
  Child_512_Home_Stand_output,
  Child_512_School_Stand_output
)

write_csv(Child_512_output,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t511-Child-512-stand-interscale-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
