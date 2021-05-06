suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# read in final normative sample for child-home form.
Child_512_Home_desamp <- read_csv(
  here(
    "INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp.csv"
    ))

# use dplyr::sample_frac() to get a 60% sample. By grouping on all demo vars, we
# roughly preserve demographic proportions in smaller sample.
set.seed(1234)
sample_60perc <- Child_512_Home_desamp %>% 
  group_by(Age, Gender, ParentHighestEducation, Ethnicity, Region) %>%
  sample_frac(0.6)

# quick check on counts/distribution for each demo variables, across full sample
# and 60% sample.
table(Child_512_Home_desamp$Age)
table(sample_60perc$Age)

table(Child_512_Home_desamp$Gender)
table(sample_60perc$Gender)

table(Child_512_Home_desamp$ParentHighestEducation)
table(sample_60perc$ParentHighestEducation)

table(Child_512_Home_desamp$Ethnicity)
table(sample_60perc$Ethnicity)

table(Child_512_Home_desamp$Region)
table(sample_60perc$Region)

# write .csv of 60% sample for use in other procedures
write_csv(sample_60perc, here(
  "INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp-60perc.csv"
))

# Comp raw-score descriptives between full sample and 60% sample
T_per_case_full_sample <- read_csv(
  here(
    "OUTPUT-FILES/NORMS-OUTPUT-4080T/Child-512-Home-T-Scores-per-case-4080T.csv"
  )
)

T_per_case_60perc_sample <- read_csv(
  here(
    "OUTPUT-FILES/NORMS-OUTPUT-4080T/Child-512-Home-T-Scores-per-case-4080T-60perc.csv"
  )
)

raw_score_desc_full_sample <- T_per_case_full_sample %>% 
  select(contains("raw")) %>%
  describe(fast = TRUE) %>%
  rownames_to_column(var = "scale") %>%
  select(scale, n, mean, sd)

raw_score_desc_60perc_sample <- T_per_case_60perc_sample %>% 
  select(contains("raw")) %>%
  describe(fast = TRUE) %>%
  rownames_to_column(var = "scale") %>%
  select(scale, n, mean, sd)

raw_score_desc_comp <- raw_score_desc_full_sample %>%
  left_join(
    raw_score_desc_60perc_sample,
    by = "scale",
    suffix = c("_full", "_60perc")
  ) %>%
  mutate(ES = (mean_full - mean_60perc) /
           ((sd_full + sd_60perc) / 2)) %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))

# write .csv of raw score desc comp
write_csv(raw_score_desc_comp, here(
  "INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp-60perc.csv"
))


