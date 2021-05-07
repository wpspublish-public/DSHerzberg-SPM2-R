suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

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

# write .csv of 60% sample for use in other procedures
write_csv(sample_60perc, here(
  "INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp-60perc.csv"
))

# Comp demos between full sample and 60% sample
ageXgender_full <- Child_512_Home_desamp %>%
  group_by(Age, Gender) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Gender, values_from = n)

ageXpeduc_full <- Child_512_Home_desamp %>%
  group_by(Age, ParentHighestEducation) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = ParentHighestEducation, values_from = n)

ageXethnic_full <- Child_512_Home_desamp %>%
  group_by(Age, Ethnicity) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Ethnicity, values_from = n)

ageXregion_full <- Child_512_Home_desamp %>%
  group_by(Age, Region) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Region, values_from = n)

demos_full <- list(ageXgender_full,
                   ageXpeduc_full,
                   ageXethnic_full,
                   ageXregion_full) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(Child_512_Home_desamp),
      TRUE ~ NA_integer_
    )
  ) %>%
  relocate(c(sample, n), .before = "Age")

ageXgender_60_perc <- sample_60perc %>%
  group_by(Age, Gender) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Gender, values_from = n)

ageXpeduc_60_perc <- sample_60perc %>%
  group_by(Age, ParentHighestEducation) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = ParentHighestEducation, values_from = n)

ageXethnic_60_perc <- sample_60perc %>%
  group_by(Age, Ethnicity) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Ethnicity, values_from = n)

ageXregion_60_perc <- sample_60perc %>%
  group_by(Age, Region) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Region, values_from = n)

demos_60_perc <- list(ageXgender_60_perc,
                      ageXpeduc_60_perc,
                      ageXethnic_60_perc,
                      ageXregion_60_perc) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "60_perc",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(sample_60perc),
      TRUE ~ NA_integer_
    )
  ) %>%
  relocate(c(sample, n), .before = "Age")

demos_comp <- bind_rows(demos_full,
                        demos_60_perc) %>%
  mutate(across(sample,
                ~ case_when(
                  lag(sample) == sample ~ NA_character_, 
                  TRUE ~ .x
                )))

# write .csv of demos comp
write_csv(
  demos_comp,
  here(
    "OUTPUT-FILES/CHILD/COMP-60PERC-SAMPLE/Child-512-Home-demos-full-60perc-comp.csv"
  ),
  na = ""
)

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
  "OUTPUT-FILES/CHILD/COMP-60PERC-SAMPLE/Child-512-Home-raw-desc-full-60perc-comp.csv"
))


