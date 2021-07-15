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

# write .csv of 60% sample for use in other procedures
write_csv(sample_60perc, here(
  str_c(output_file_path)
  ))

# Comp demos between full sample and 60% sample
# Prepare table of demographics counts for the full sample. We first call map()
# to return a list of four dfs, with the case counts by age and category for
# each of the four demographic variables. We map over a vector holding the names
# of these four demo vars. The input to this mapping procedure is the full
# standardization sample. We group this input by age and the .x argument to
# map(), that is, the currently iterated demo var. We can then call summarize()
# to get the case counts (n()) for each category of the demo var, within each
# ageyear. At this point the summary table is in nested format, where the
# categories of the demo vars are nested with in each value of age, resulting in
# a long table. We call pivot_wider() to transform the table into a more
# conventional demographic table format, in which there is one row for each age
# year, each category of the demographic variable has its own column, and the
# cells contain the person counts for each crossing of age X demographic
# category.
demos_full <- map(
  c("Gender", "ParentHighestEducation", "Ethnicity", "Region"),
  ~
    Child_512_Home_desamp %>%
    group_by(Age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  # At this point the data object is list of data frames, all of which have
  # identical values in the age column. We can use purrr:reduce() to iteratively
  # apply left_join(), joining the tables together by the age column. The result
  # is a single df with age column on the far left, and the columns of
  # categories of the four demo vars proceeding to the right, each holding the
  # person counts for each value of age.
  reduce(left_join, by = "Age") %>%
  # We ungroup to facilitate a table structure that is more readdable. The
  # mutate() call creates sample and n columns that are only filled in the top
  # cell.
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(Child_512_Home_desamp),
      TRUE ~ NA_integer_
    )
  ) %>%
  # relocate provides the desired column sequence in the output table.
  relocate(c(sample, n), .before = "Age")

# documentation for code block below is analogous to that for previous (creation
# of demos_full)
demos_60_perc <- map(
  c("Gender", "ParentHighestEducation", "Ethnicity", "Region"),
  ~
    sample_60perc %>%
    group_by(Age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
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

# Use bind_rows() to stack the tables from the full and 60_perc samples, and
# mutate() the existing sample column to keep it readable, by having the sample
# label only appear in the first row of the stacked table for each sample.
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


