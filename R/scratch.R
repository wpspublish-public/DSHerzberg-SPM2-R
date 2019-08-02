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
  assign(paste0(.x, '_lookup'), ., envir = .GlobalEnv)
) %>% 
  reduce(
    left_join, 
    by = 'raw'
    )

all_lookup <- full_join(TOT_lookup, subscale_lookup, by = 'raw')


# fill empty raw-to-T lookup with standard scores by age strat.
raw_to_T_lookup <- raw_to_T_lookup_empty %>%
  # gather collapses the empty wide rawscore by agestrat table into tall table
  # with three columns, rawscore, agestrat, and an empty T column. Rawscore
  # sequence is repeated down its column, once per agestrat, and value of
  # agestrat column is uniform for the entire rawscore sequence. `-rawscore` is
  # `dplyr::select` code that drops all vars except rawscore.
  gather(score, T, -rawscore) %>%
  # This tall table can now be joined with `final_med_SD`, which contains
  # smoothed meds and SDs per agestrat, because both tables have an `agestrat`
  # column that can be used as a `by` var. In the newly-constituted tall table,
  # `median_sm`, `lo_SD_sm`, and `hi_SD_sm` cols hold the correct values for
  # each agestrat.
  left_join(final_med_SD, by = 'agestrat') %>%
  # calculate T above and below the median. `meidan_SM = NULL` drops this now
  # unnecessary column from the piped object.
  mutate(T = case_when(
    rawscore <= median_sm ~ round(100 + (((rawscore - median_sm) / lo_SD_sm) *15)),
    TRUE ~ round(100 + (((rawscore - median_sm) / hi_SD_sm) *15))
  ), median_sm = NULL, lo_SD_sm = NULL, hi_SD_sm = NULL) %>%
  # truncate T distribution at 40 and 160.
  mutate_at(
    vars(T), ~ case_when(
      .x < 40 ~ 40,
      .x > 160 ~ 160,
      TRUE ~ .x
    )
  ) %>%
  # spread converts table back from tall to wide. Resulting table has one row
  # per `rawscore`. Each value of `agestrat` gets its own column, and each of
  # these columns is populated with the value of `T` that matches `rawscore`,
  # within that `agestrat`.
  spread(agestrat, T) %>%
  # select reorders vars so to give the correct sequence of `agestrat` going
  # left-to-right. That order of agestrats is given by the char vec
  # `c(final_med_SD$agestrat)`.
  select(rawscore, c(final_med_SD$agestrat))

