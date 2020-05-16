###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## ADULT SELF STAND--------------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Adult_Self_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv")
  ))
  ) %>% 
  select(contains("_NT"))

Adult_Self_Stand_output <- data.frame(cor(Adult_Self_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(
    form = case_when(rownames(.) == "1" ~ "Adult-Self",
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ nrow(Adult_Self_Stand_T_scores),
                  T ~ NA_integer_)
  ) %>%
  select(form, n, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))

## ADULT OTHER STAND--------------------------------------------------------------
scale_order <- c("SOC_NT", "VIS_NT", "HEA_NT", "TOU_NT", 
                 "TS_NT", "BOD_NT", "BAL_NT", "PLA_NT", "TOT_NT")

Adult_Other_Stand_T_scores <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-T-Scores-per-case.csv")
  ))
  ) %>% 
  select(contains("_NT"))

Adult_Other_Stand_output <- data.frame(cor(Adult_Other_Stand_T_scores)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(
    form = case_when(rownames(.) == "1" ~ "Adult-Other",
                     T ~ NA_character_),
    n = case_when(rownames(.) == "1" ~ nrow(Adult_Other_Stand_T_scores),
                  T ~ NA_integer_)
  ) %>%
  select(form, n, scale, scale_order)

rm(list = setdiff(ls(), ls(pattern = "output")))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

Adult_output <- bind_rows(
  Adult_Self_Stand_output,
  Adult_Other_Stand_output
)

write_csv(Adult_output,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t513-Adult-stand-interscale-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
