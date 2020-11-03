# SPM-2: derived normalized T-scores for each case in standardization sample;
# create raw-to-T lookup tables.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS
library(bestNormalize) # NORMALIZATION METHODS
suppressMessages(library(psych)) # DESCRIPTIVE TABLES

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Child_512_School <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023",
                                "q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039",
                                "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050",
                                "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065",
                                "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079",
                                "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092",
                                "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108",
                                "q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

TOT_items_Child_512_School <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039",
                                "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050",
                                "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065",
                                "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079",
                                "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092",
                                "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

SOC_items_Child_512_School <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

SOC_rev_items_Child_512_School <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

VIS_items_Child_512_School <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039")

HEA_items_Child_512_School <- c("q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050")

TOU_items_Child_512_School <- c("q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065")

TS_items_Child_512_School <- c("q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079")

BOD_items_Child_512_School <- c("q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092")

BAL_items_Child_512_School <- c("q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

PLA_items_Child_512_School <- c("q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Child_512_School_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/SPM-2 Child ages 512 School Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Child_512_School
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Child_512_School,
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
    SOC_rev_items_Child_512_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Child_512_School,
            ~ as.integer(.x)) %>%
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years")
  ) %>%
  # select(-AgeGroup) %>%
# Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_School])`: when used
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_School]),
    SOC_raw = rowSums(.[SOC_items_Child_512_School]),
    VIS_raw = rowSums(.[VIS_items_Child_512_School]),
    HEA_raw = rowSums(.[HEA_items_Child_512_School]),
    TOU_raw = rowSums(.[TOU_items_Child_512_School]),
    TS_raw = rowSums(.[TS_items_Child_512_School]),
    BOD_raw = rowSums(.[BOD_items_Child_512_School]),
    BAL_raw = rowSums(.[BAL_items_Child_512_School]),
    PLA_raw = rowSums(.[PLA_items_Child_512_School])
  ) %>%
  # Create data var
  mutate(data = 'SM') %>%
  select(IDNumber, data, everything()) %>%
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) %>%
  # filter(!(TOT_raw >= 138 & data == 'Qual')) %>%
  write_csv(here('INPUT-FILES/CHILD/SM-ONLY-NORMS-INPUT/Child-512-School-SM-only-norms-input.csv'),
            na = "")
