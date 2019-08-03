# Examine SPM-2 data to determine need for age-stratified norms.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS
library(bestNormalize) # NORMALIZATION METHODS

# Generate raw-to-T lookup table. Input is stand sample with raw scores and
# normalized T scores for each case. Group bases by raw score, relationship
# between raw and T is many-to-one.
TOT_lookup <- Adult_Other %>% group_by(
  TOT_raw
) %>% 
  # Because raw-to-T is many to one, all values of T are identical for each raw,
  # and summarizing by the min value of T per raw yields the ONLY value of T per
  # raw. But we need the raw column to contain all possible values of raw, and
  # not all possible values of raw are represented in the stand sample. Thus
  # current data object jumps possible raw values (e.g, raw = 62 and raw = 65
  # might be adjacent rows in this table)
  summarise(
    TOT_NT = min(TOT_NT)
  ) %>% 
  # complete expands the table vertically, filling in missing values of raw
  # within the range given. This leaves NA cells for T for those rows that
  # didn't have raw values in the input object.
  complete(
    TOT_raw = 10:240
  ) %>% 
  # fill replaces NA in T going down the table, with values from the last
  # preceding (lagging) cell that was not NA.
  fill(
    TOT_NT
  ) %>% 
  # A second call of fill is needed to handle inputs where the first cell(s) of
  # T are NA. 2nd fill call is uses direction up to fill those first NA cells
  # with the value from the first subsequent (leading) cell that is not NA.
  fill(
    TOT_NT,
    .direction = "up"
  ) %>% 
  rename(
    raw = TOT_raw
  ) %>% 
  mutate_at(
    vars(TOT_NT), ~ case_when(
      raw < 60 ~ NA_real_,
      TRUE ~ .x
    )
  )

# NEXT APPLY MUTATE_AT TO SUBSCALES, DO WE NEED ASSIGN?

subscale_names <- score_names[2:9]

subscale_lookup <- map(
  subscale_names, 
~ Adult_Other %>% group_by(
  !!as.name(paste0(.x, '_raw'))
) %>% 
  summarise(
    !!as.name(paste0(.x, '_NT')) := min(!!as.name(paste0(.x, '_NT')))
  ) %>% 
  complete(
    !!as.name(paste0(.x, '_raw')) := 10:240
  ) %>% 
  fill(
    paste0(.x, '_NT')
  ) %>% 
  fill(
    paste0(.x, '_NT'),
    .direction = "up"
  ) %>% 
  rename(
    raw = !!as.name(paste0(.x, '_raw'))
    ) %>% 
  mutate_at(
    vars(!!as.name(paste0(.x, '_NT'))), ~ case_when(
      raw > 40 ~ NA_real_,
      TRUE ~ .x
      )
    )
) %>% 
  reduce(
    left_join, 
    by = 'raw'
    )

all_lookup <- full_join(TOT_lookup, subscale_lookup, by = 'raw')

all_lookup_col_names <- c(paste0(score_names, '_raw'))

all_lookup_pub <- all_lookup %>% 
  # gather collapses wide table into three-column tall table with key-value
  # pairs: rawscore, score_name(key var, many rows for each score_name), T(value
  # var, one row for each value of T within each score_name)
  gather(score_name, T,-raw) %>% 
  group_by(score_name) %>%
  # expand the table vertically, adding new rows, so there's a row for every possible T value
  complete(T = 25:75) %>% 
  ungroup() %>%
  # regroup table by two levels
  group_by(score_name, T) %>%
  # filter step retains all 1-row groups, and the first and last rows of any
  # multi-row groups. n() == 1 returns 1-row groups; n() > 1 & row_number()
  # %in% c(1, n()) returns rows of multi-row groups with the row number of
  # either 1 (first row), or n() which is the number of rows and also the
  # number of the last row. The first and last rows hold the min and max
  # values of raw for that value of T (the grouping variable)
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  # Summarise creates a table with one row per group (one row per
  # possible value of T). For the 1-row groups, str_c simply passes the
  # value of raw as a string; for the multi-row groups, str_c joins the min
  # and max values of raw with the '--' separator.
  summarise(raw = str_c(raw, collapse = '--')) %>%
  # recode missing values of raw to '-'
  mutate_at(vars(raw), ~ case_when(is.na(.x) ~ '-', TRUE ~ .x)) %>%
  # sort on two levels
  arrange(score_name, desc(T)) %>% 
  # spread table back to wide, all values of T (one row for each), score_name
  # columns filled with values of rawscore
  spread(score_name, raw) %>%
  # sort descending on T
  arrange(desc(T)) %>% 
  # apply desired final column names
  rename_at(vars(ends_with('_NT')), ~ all_lookup_col_names)
