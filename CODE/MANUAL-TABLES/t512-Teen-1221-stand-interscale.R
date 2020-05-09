###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## TEEN 1221 HOME STAND--------------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Teen_1221_Home_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  ))
) %>% 
  select(contains("_NT"))

Teen_1221_Home_Stand_output <- data.frame(cor(Teen_1221_Home_Stand_T_scores)) %>%
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "Teen-1221-Home",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))
  
## TEEN 1221 SCHOOL STAND----------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Teen_1221_School_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
  ))
) %>% 
  select(contains("_NT"))

Teen_1221_School_Stand_output <- data.frame(cor(Teen_1221_School_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "Teen-1221-School",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))

## TEEN 1221 SELF STAND--------------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Teen_1221_Self_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  ))
  ) %>% 
  select(contains("_NT"))

Teen_1221_Self_Stand_output <- data.frame(cor(Teen_1221_Self_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_NT" ~ "Teen-1221-Self",
    T ~ NA_character_
  )) %>% 
  select(form, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))


###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

Teen_1221_output <- bind_rows(
  Teen_1221_Self_Stand_output,
  Teen_1221_Home_Stand_output,
  Teen_1221_School_Stand_output
)

write_csv(Teen_1221_output,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t512-Teen-1221-stand-interscale-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
