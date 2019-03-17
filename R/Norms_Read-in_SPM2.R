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

TOT_items_Adult_Self <- c("q0013", "q0014", "q0016", "q0018", "q0020", "q0021", "q0023", 
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

SOC_items_Adult_Self <- c("q0013", "q0014", "q0016", "q0018", "q0020", "q0021", "q0023", 
                          "q0024", "q0026", "q0027")

SOC_rev_items_Adult_Self <- c("q0013", "q0014", "q0018", "q0020", "q0027")

VIS_items_Adult_Self <- c("q0029", "q0030", "q0031", "q0032", "q0033", "q0034", "q0036", 
                          "q0037", "q0040", "q0041")

HEA_items_Adult_Self <- c("q0043", "q0044", "q0046", "q0047", "q0049", "q0050", "q0051", 
                          "q0053", "q0054", "q0057")

TOU_items_Adult_Self <- c("q0058", "q0059", "q0060", "q0061", "q0062", "q0063", "q0064", 
                          "q0067", "q0068", "q0069")

TS_items_Adult_Self <- c("q0073", "q0075", "q0076", "q0077", "q0081", "q0082", "q0083", 
                         "q0084", "q0086", "q0087")

BOD_items_Adult_Self <- c("q0089", "q0090", "q0091", "q0092", "q0093", "q0096", "q0098", 
                          "q0099", "q0100", "q0101")

BAL_items_Adult_Self <- c("q0103", "q0104", "q0105", "q0106", "q0108", "q0111", "q0113", 
                          "q0114", "q0115", "q0117")

PLA_items_Adult_Self <- c("q0118", "q0119", "q0122", "q0126", "q0127", "q0128", "q0129", 
                          "q0130", "q0131", "q0132")

# Read data

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("DATA/SPM-2 Adult ages 1690 Self-Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    TOT_items_Adult_Self
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_Adult_Self,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # recode reverse-scored items
  mutate_at(
    SOC_rev_items_Adult_Self,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_Adult_Self,
            ~ as.integer(.x)) %>% print()


# Check for duplicate IDnumber.

Adult_Self_dup <- Adult_Self %>% count(IDNumber) %>% filter(n > 1)
