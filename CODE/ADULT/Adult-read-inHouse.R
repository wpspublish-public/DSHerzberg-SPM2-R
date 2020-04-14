suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# Self

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Adult_Self <- c("q0013", "q0014", "q0016", "q0018", "q0020", "q0021", "q0023", 
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

TOT_items_Adult_Self <- c("q0029", "q0030", "q0031", "q0032", 
                          "q0033", "q0034", "q0036", "q0037", "q0040", "q0041", "q0043", 
                          "q0044", "q0046", "q0047", "q0049", "q0050", "q0051", "q0053", 
                          "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", 
                          "q0063", "q0064", "q0067", "q0068", "q0069", "q0073", "q0075", 
                          "q0076", "q0077", "q0081", "q0082", "q0083", "q0084", "q0086", 
                          "q0087", "q0089", "q0090", "q0091", "q0092", "q0093", "q0096", 
                          "q0098", "q0099", "q0100", "q0101", "q0103", "q0104", "q0105", 
                          "q0106", "q0108", "q0111", "q0113", "q0114", "q0115", "q0117")

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

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/WPS SPM-2 Adult ages 1690 Self-Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Self
  ) %>%
  # filter out youngest age group
  filter(AgeGroup != "16.00 to 20.99 years") %>% 
  rename(age_range = AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Adult_Self,
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
  mutate_at(All_items_Adult_Self,
            ~ as.integer(.x)) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Adult_Self])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Self]),
    SOC_raw = rowSums(.[SOC_items_Adult_Self]),
    VIS_raw = rowSums(.[VIS_items_Adult_Self]),
    HEA_raw = rowSums(.[HEA_items_Adult_Self]),
    TOU_raw = rowSums(.[TOU_items_Adult_Self]),
    TS_raw = rowSums(.[TS_items_Adult_Self]),
    BOD_raw = rowSums(.[BOD_items_Adult_Self]),
    BAL_raw = rowSums(.[BAL_items_Adult_Self]),
    PLA_raw = rowSums(.[PLA_items_Adult_Self])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw
  filter(TOT_raw <200) %>%
  write_csv(here('INPUT-FILES/ADULT/INHOUSE-NORMS-INPUT/Adult-Self-inHouse-norms-input.csv'),
            na = "")

rm(list = ls())

# Other

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Adult_Other <- c("q0014", "q0015", "q0016", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027", "q0028",
                           "q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042", 
                           "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057", 
                           "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072", 
                           "q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087", 
                           "q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", 
                           "q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114",
                           "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125", "q0129", "q0131")

TOT_items_Adult_Other <- c("q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042", 
                           "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057", 
                           "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072", 
                           "q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087", 
                           "q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", 
                           "q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114")

SOC_items_Adult_Other <- c("q0014", "q0015", "q0016", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027", "q0028")

SOC_rev_items_Adult_Other <- c("q0014", "q0015", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027")

VIS_items_Adult_Other <- c("q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042")

HEA_items_Adult_Other <- c("q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057")

TOU_items_Adult_Other <- c("q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072")

TS_items_Adult_Other <- c("q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087")

BOD_items_Adult_Other <- c("q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100")

BAL_items_Adult_Other <- c("q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114")

PLA_items_Adult_Other <- c("q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125", "q0129", "q0131")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Adult_Other <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/WPS SPM-2 Adult ages 1690 Other Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Other
  ) %>%
  # filter out youngest age group
  filter(AgeGroup != "16.00 to 20.99 years") %>% 
  rename(age_range = AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Adult_Other,
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
    SOC_rev_items_Adult_Other,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Adult_Other,
            ~ as.integer(.x)) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Adult_Other])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Other]),
    SOC_raw = rowSums(.[SOC_items_Adult_Other]),
    VIS_raw = rowSums(.[VIS_items_Adult_Other]),
    HEA_raw = rowSums(.[HEA_items_Adult_Other]),
    TOU_raw = rowSums(.[TOU_items_Adult_Other]),
    TS_raw = rowSums(.[TS_items_Adult_Other]),
    BOD_raw = rowSums(.[BOD_items_Adult_Other]),
    BAL_raw = rowSums(.[BAL_items_Adult_Other]),
    PLA_raw = rowSums(.[PLA_items_Adult_Other])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw
  filter(TOT_raw <200) %>%
  write_csv(here('INPUT-FILES/ADULT/INHOUSE-NORMS-INPUT/Adult-Other-inHouse-norms-input.csv'),
            na = "")

rm(list = ls())
