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
test <- Adult_Other %>% group_by(
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
    TOT_raw = 60:240
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
  )
  
