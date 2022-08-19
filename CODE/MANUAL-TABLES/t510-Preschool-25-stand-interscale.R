###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## PRESHOOL 25 HOME STAND--------------------------------------------------------------
# scale_order_old <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
#                  "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")
scale_order <- c("VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "TOT_NT", "PLA_NT", "SOC_NT")

Preschool_25_Home_Stand_T_scores <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(contains("_NT"))

Preschool_25_Home_Stand_output <- data.frame(cor(Preschool_25_Home_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(
    form = case_when(rownames(.) == "1" ~ "Preschool-25-Home",
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ nrow(Preschool_25_Home_Stand_T_scores),
                  T ~ NA_integer_)
  ) %>%
  select(form, n, scale, all_of(scale_order))

rm(list = setdiff(ls(), ls(pattern = "output")))
  
## PRESCHOOL 25 SCHOOL STAND----------------------------------------------------------
scale_order <- c("VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "TOT_NT", "PLA_NT", "SOC_NT")

Preschool_25_School_Stand_T_scores <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(contains("_NT"))

Preschool_25_School_Stand_output <- data.frame(cor(Preschool_25_School_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(
    form = case_when(rownames(.) == "1" ~ "Preschool-25-School",
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ nrow(Preschool_25_School_Stand_T_scores),
                  T ~ NA_integer_)
  ) %>%
  select(form, n, scale, all_of(scale_order))

rm(list = setdiff(ls(), ls(pattern = "output")))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

Preschool_output <- bind_rows(
  Preschool_25_Home_Stand_output,
  Preschool_25_School_Stand_output
)

write_csv(Preschool_output,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t510-Preschool-25-stand-interscale-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
