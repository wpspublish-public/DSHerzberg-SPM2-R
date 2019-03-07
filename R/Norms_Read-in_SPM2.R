# Examine SPM-2 data to determine need for age-stratified norms.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
library(moderndive) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
library(magrittr) # PIPE OPERATORS
# note use of `suppressWarnings` to silence chatter during interactive session
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

Adult_Self <-
  suppressMessages(read_csv(here('DATA/SPM-2 Adult ages 1690 Self-Report Questionnaire.csv')))
