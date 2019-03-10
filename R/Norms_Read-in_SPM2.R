# Examine SPM-2 data to determine need for age-stratified norms.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
library(moderndive) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

# Scale vectors with item names

TOT_items_Adult_Self_unscored <- c("q0013", "q0014", "q0016", "q0018", "q0020", "q0021", "q0023", 
                          "q0024", "q0026", "q0027", "q0029", "q0030", "q0031", "q0032", 
                          "q0033", "q0034", "q0036", "q0037", "q0040", "q0041", "q0043", 
                          "q0044", "q0046", "q0047", "q0049", "q0050", "q0051", "q0053", 
                          "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", 
                          "q0063", "q0064", "q0067", "q0068", "q0069", "q0073", "q0075", 
                          "q0076", "q0077", "q0081", "q0082", "q0083", "q0084", "q0086", 
                          "q0087", "q0089", "q0090", "q0091", "q0092", "q0093", "q0096", 
                          "q0098", "q0099", "q0100", "q0101", "q0103", "q0104", "q0105", 
                          "q0106", "q0108", "q0111", "q0113", "q0114", "q0115", "q0117", 
                          "q0118", "q0119", "q0122", "q0126", "q0127", "q0128", "q0129", 
                          "q0130", "q0131", "q0132")

TOT_items_Adult_Self <- c("i013", "i014", "i016", "i018", "i020", "i021", "i023", 
                                 "i024", "i026", "i027", "i029", "i030", "i031", "i032", 
                                 "i033", "i034", "i036", "i037", "i040", "i041", "i043", 
                                 "i044", "i046", "i047", "i049", "i050", "i051", "i053", 
                                 "i054", "i057", "i058", "i059", "i060", "i061", "i062", 
                                 "i063", "i064", "i067", "i068", "i069", "i073", "i075", 
                                 "i076", "i077", "i081", "i082", "i083", "i084", "i086", 
                                 "i087", "i089", "i090", "i091", "i092", "i093", "i096", 
                                 "i098", "i099", "i100", "i101", "i103", "i104", "i105", 
                                 "i106", "i108", "i111", "i113", "i114", "i115", "i117", 
                                 "i118", "i119", "i122", "i126", "i127", "i128", "i129", 
                                 "i130", "i131", "i132")

SOC_items_Adult_Self <- c("i013", "i014", "i016", "i018", "i020", "i021", "i023", 
                          "i024", "i026", "i027")

VIS_items_Adult_Self <- c("i029", "i030", "i031", "i032", "i033", "i034", "i036", 
                          "i037", "i040", "i041")

HEA_items_Adult_Self <- c("i043", "i044", "i046", "i047", "i049", "i050", "i051", 
                          "i053", "i054", "i057")

HEA_items_Adult_Self <- c("i043", "i044", "i046", "i047", "i049", "i050", "i051", 
                          "i053", "i054", "i057")

TOU_items_Adult_Self <- c("i058", "i059", "i060", "i061", "i062", "i063", "i064", 
                          "i067", "i068", "i069")

TS_items_Adult_Self <- c("i073", "i075", "i076", "i077", "i081", "i082", "i083", 
                         "i084", "i086", "i087")

BOD_items_Adult_Self <- c("i089", "i090", "i091", "i092", "i093", "i096", "i098", 
                          "i099", "i100", "i101")

BAL_items_Adult_Self <- c("i103", "i104", "i105", "i106", "i108", "i111", "i113", 
                          "i114", "i115", "i117")

PLA_items_Adult_Self <- c("i118", "i119", "i122", "i126", "i127", "i128", "i129", 
                          "i130", "i131", "i132")

# Read data

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here('DATA/SPM-2 Adult ages 1690 Self-Report Questionnaire.csv')
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    TOT_items_Adult_Self_unscored
  ) %>% mutate_at(TOT_items_Adult_Self, .funs=~NA_real_) %>% print()

# Check for duplicate IDnumber.

Adult_Self_dup <- Adult_Self %>% count(IDNumber) %>% filter(n > 1)
