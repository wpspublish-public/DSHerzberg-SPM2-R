suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(reportROC))

# read conditional probabability input data for SPD, set binary predictors for
# different t-score cutpoints
cond_prob_SPD_input <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/COND-PROB-SPD/cond-prob-SPD-inputData.csv")
  ))) %>%
  mutate(
    TOT_NT_cut55 = case_when(TOT_NT >= 55 ~ 1,
                             T ~ 0),
    TOT_NT_cut60 = case_when(TOT_NT >= 60 ~ 1,
                             T ~ 0),
    TOT_NT_cut65 = case_when(TOT_NT >= 65 ~ 1,
                             T ~ 0),
    TOT_NT_cut70 = case_when(TOT_NT >= 70 ~ 1,
                             T ~ 0),
    TOT_NT_cut75 = case_when(TOT_NT >= 75 ~ 1,
                             T ~ 0),
  )

# use reportROC::reportROC() to obtain table rows that contain sens, spec for
# different t-score cutpoints
output_cut55 <-
  reportROC(
    gold = cond_prob_SPD_input$case,
    predictor.binary = cond_prob_SPD_input$TOT_NT_cut55,
    plot = F
  ) %>% mutate(T_score_cut = 55) %>% 
  select(T_score_cut, SEN, SPE)


