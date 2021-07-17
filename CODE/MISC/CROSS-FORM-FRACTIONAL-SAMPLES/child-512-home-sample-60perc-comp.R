suppressMessages(library(here))
suppressMessages(library(splitstackshape))
suppressMessages(library(psych))
suppressMessages(suppressWarnings(library(tidyverse)))

input_file_name <- "Child-512-Home-allData-desamp"
input_file_path <- "CODE/MISC/CROSS-FORM-FRACTIONAL-SAMPLES/INPUT-FILES/"
output_file_path <- "CODE/MISC/CROSS-FORM-FRACTIONAL-SAMPLES/OUTPUT-FILES/"

sample_full <- suppressMessages(read_csv(here(str_c(
  input_file_path, input_file_name, ".csv"
))))

set.seed(1234)
sample_60perc <- stratified(
  sample_full,
  c(
    "Age",
    "Gender",
    "ParentHighestEducation",
    "Ethnicity",
    "Region"
  ),
  size = .6
)

write_csv(sample_60perc, here(
  str_c(output_file_path, input_file_name, "-sample-60perc.csv")
  ))

demos_full <- map(
  c("Gender", "ParentHighestEducation", "Ethnicity", "Region"),
  ~
    sample_full %>%
    group_by(Age,!!sym(.x)) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(row_number() == 1 ~ nrow(sample_full),
                  TRUE ~ NA_integer_)
  ) %>%
  relocate(c(sample, n), .before = "Age")

demos_60_perc <- map(
  c("Gender", "ParentHighestEducation", "Ethnicity", "Region"),
  ~
    sample_60perc %>%
    group_by(Age,!!sym(.x)) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "60_perc",
                       TRUE ~ NA_character_),
    n = case_when(row_number() == 1 ~ nrow(sample_60perc),
                  TRUE ~ NA_integer_)
  ) %>%
  relocate(c(sample, n), .before = "Age")

demos_comp <- bind_rows(demos_full,
                        demos_60_perc) 

write_csv(demos_comp, here(
  str_c(
    output_file_path,
    input_file_name,
    "-demos-full-60perc-comp.csv"
  )
),
na = "")


############START HERE

# Comp raw-score descriptives between full sample and 60% sample
T_per_case_full_sample <- read_csv(
  here(
    "OUTPUT-FILES/NORMS-OUTPUT-4080T/Child-512-Home-T-Scores-per-case-4080T.csv"
  )
)

# The file to be read in here is the t-scores per case for the 60perc sample
# that was written out above as "CHILD-512-Home-allData-desamp-60perc.csv". It
# needs to be generated from this latter file using the long script for creating
# raw-to-T lookup tables. A specialized version of this script, just for this
# purpose, has been saved as
# "Child-512-Home-Norms-Lookup-Tables-4080T-60perc-SPM2.R"
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
  mutate(ES = abs((mean_full - mean_60perc) /
           sqrt(((n_full*(sd_full^2)) + (n_60perc*(sd_60perc^2))) / (n_full + n_60perc)))) %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))

# write .csv of raw score desc comp
write_csv(raw_score_desc_comp, here(
  "OUTPUT-FILES/CHILD/COMP-60PERC-SAMPLE/Child-512-Home-raw-desc-full-60perc-comp.csv"
))


