suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# HOME

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Teen_1221_Home <- c("q0015", "q0016", "q0017", "q0018", "q0019", "q0021", "q0022", "q0024", "q0025", "q0026", 
                              "q0028", "q0029", "q0030", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", "q0040", 
                              "q0041", "q0042", "q0043", "q0044", "q0047", "q0048", "q0049", "q0050", "q0052", "q0053", 
                              "q0055", "q0056", "q0057", "q0058", "q0059", "q0061", "q0062", "q0063", "q0064", "q0065", 
                              "q0067", "q0068", "q0069", "q0071", "q0072", "q0074", "q0076", "q0077", "q0078", "q0079", 
                              "q0080", "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0091", "q0092", 
                              "q0094", "q0095", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102", "q0103", "q0104",
                              "q0106", "q0107", "q0108", "q0109", "q0111", "q0112", "q0113", "q0114", "q0115", "q0117")

TOT_items_Teen_1221_Home <- c("q0028", "q0029", "q0030", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", "q0040", 
                              "q0041", "q0042", "q0043", "q0044", "q0047", "q0048", "q0049", "q0050", "q0052", "q0053", 
                              "q0055", "q0056", "q0057", "q0058", "q0059", "q0061", "q0062", "q0063", "q0064", "q0065", 
                              "q0067", "q0068", "q0069", "q0071", "q0072", "q0074", "q0076", "q0077", "q0078", "q0079", 
                              "q0080", "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0091", "q0092", 
                              "q0094", "q0095", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102", "q0103", "q0104")

SOC_items_Teen_1221_Home <- c("q0015", "q0016", "q0017", "q0018", "q0019", "q0021", "q0022", "q0024", "q0025", "q0026")

SOC_rev_items_Teen_1221_Home <- c("q0015", "q0016", "q0017", "q0021", "q0022")

VIS_items_Teen_1221_Home <- c("q0028", "q0029", "q0030", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", "q0040")

HEA_items_Teen_1221_Home <- c("q0041", "q0042", "q0043", "q0044", "q0047", "q0048", "q0049", "q0050", "q0052", "q0053")

TOU_items_Teen_1221_Home <- c("q0055", "q0056", "q0057", "q0058", "q0059", "q0061", "q0062", "q0063", "q0064", "q0065")

TS_items_Teen_1221_Home <- c("q0067", "q0068", "q0069", "q0071", "q0072", "q0074", "q0076", "q0077", "q0078", "q0079")

BOD_items_Teen_1221_Home <- c("q0080", "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0091", "q0092")

BAL_items_Teen_1221_Home <- c("q0094", "q0095", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102", "q0103", "q0104")

PLA_items_Teen_1221_Home <- c("q0106", "q0107", "q0108", "q0109", "q0111", "q0112", "q0113", "q0114", "q0115", "q0117")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Teen_1221_Home_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/WPS SPM-2 Teen ages 1221 Home Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Teen_1221_Home,
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
    SOC_rev_items_Teen_1221_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Teen_1221_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Teen_1221_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_Home]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_Home]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_Home]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_Home]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_Home]),
    TS_raw = rowSums(.[TS_items_Teen_1221_Home]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_Home]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_Home]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) %>% 
  # filter(!(TOT_raw >= 155 & age_range == '18 to 21 years' & data == 'SM')) %>%
  write_csv(here('INPUT-FILES/TEEN/INHOUSE-NORMS-INPUT/Teen-1221-Home-inHouse-norms-input.csv'),
            na = "")

rm(list = ls())

# SCHOOL

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Teen_1221_School <- c("q0012", "q0013", "q0014", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023", "q0024", 
                                "q0025", "q0026", "q0028", "q0029", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", 
                                "q0038", "q0039", "q0040", "q0041", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", 
                                "q0052", "q0053", "q0055", "q0056", "q0057", "q0059", "q0060", "q0061", "q0062", "q0063", 
                                "q0064", "q0065", "q0066", "q0067", "q0068", "q0071", "q0072", "q0074", "q0075", "q0076", 
                                "q0077", "q0078", "q0079", "q0080", "q0081", "q0082", "q0083", "q0084", "q0086", "q0089", 
                                "q0090", "q0092", "q0093", "q0094", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102",
                                "q0103", "q0104", "q0105", "q0106", "q0107", "q0108", "q0112", "q0113", "q0115", "q0116")

TOT_items_Teen_1221_School <- c("q0025", "q0026", "q0028", "q0029", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", 
                                "q0038", "q0039", "q0040", "q0041", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", 
                                "q0052", "q0053", "q0055", "q0056", "q0057", "q0059", "q0060", "q0061", "q0062", "q0063", 
                                "q0064", "q0065", "q0066", "q0067", "q0068", "q0071", "q0072", "q0074", "q0075", "q0076", 
                                "q0077", "q0078", "q0079", "q0080", "q0081", "q0082", "q0083", "q0084", "q0086", "q0089", 
                                "q0090", "q0092", "q0093", "q0094", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102")

SOC_items_Teen_1221_School <- c("q0012", "q0013", "q0014", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023", "q0024")

SOC_rev_items_Teen_1221_School <- c("q0012", "q0013", "q0014", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

VIS_items_Teen_1221_School <- c("q0025", "q0026", "q0028", "q0029", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037")

HEA_items_Teen_1221_School <- c("q0038", "q0039", "q0040", "q0041", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049")

TOU_items_Teen_1221_School <- c("q0052", "q0053", "q0055", "q0056", "q0057", "q0059", "q0060", "q0061", "q0062", "q0063")

TS_items_Teen_1221_School <- c("q0064", "q0065", "q0066", "q0067", "q0068", "q0071", "q0072", "q0074", "q0075", "q0076")

BOD_items_Teen_1221_School <- c("q0077", "q0078", "q0079", "q0080", "q0081", "q0082", "q0083", "q0084", "q0086", "q0089")

BAL_items_Teen_1221_School <- c("q0090", "q0092", "q0093", "q0094", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102")

PLA_items_Teen_1221_School <- c("q0103", "q0104", "q0105", "q0106", "q0107", "q0108", "q0112", "q0113", "q0115", "q0116")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Teen_1221_School_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/WPS SPM-2 Teen ages 1221 School Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_School
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Teen_1221_School,
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
    SOC_rev_items_Teen_1221_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Teen_1221_School,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Teen_1221_School])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_School]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_School]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_School]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_School]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_School]),
    TS_raw = rowSums(.[TS_items_Teen_1221_School]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_School]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_School]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_School])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) %>% 
  # filter(!(TOT_raw >= 155 & age_range == '18 to 21 years' & data == 'SM')) %>%
  write_csv(here('INPUT-FILES/TEEN/INHOUSE-NORMS-INPUT/Teen-1221-School-inHouse-norms-input.csv'),
            na = "")

rm(list = ls())

# SELF

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Teen_1221_Self <- c("q0014", "q0015", "q0016", "q0017", "q0019", "q0020", "q0021", "q0022", "q0024", "q0025", 
                              "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0033", "q0036", "q0037", "q0038", 
                              "q0040", "q0042", "q0043", "q0044", "q0045", "q0047", "q0049", "q0050", "q0051", "q0052", 
                              "q0053", "q0054", "q0056", "q0058", "q0059", "q0060", "q0061", "q0062", "q0063", "q0064", 
                              "q0066", "q0067", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                              "q0079", "q0080", "q0081", "q0082", "q0083", "q0084", "q0085", "q0086", "q0088", "q0090", 
                              "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", "q0103", "q0104", "q0105",
                              "q0106", "q0107", "q0108", "q0109", "q0110", "q0112", "q0114", "q0115", "q0116", "q0117")

TOT_items_Teen_1221_Self <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0033", "q0036", "q0037", "q0038", 
                              "q0040", "q0042", "q0043", "q0044", "q0045", "q0047", "q0049", "q0050", "q0051", "q0052", 
                              "q0053", "q0054", "q0056", "q0058", "q0059", "q0060", "q0061", "q0062", "q0063", "q0064", 
                              "q0066", "q0067", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                              "q0079", "q0080", "q0081", "q0082", "q0083", "q0084", "q0085", "q0086", "q0088", "q0090", 
                              "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", "q0103", "q0104", "q0105")

SOC_items_Teen_1221_Self <- c("q0014", "q0015", "q0016", "q0017", "q0019", "q0020", "q0021", "q0022", "q0024", "q0025")

SOC_rev_items_Teen_1221_Self <- c("q0014", "q0015", "q0016", "q0017", "q0021")

VIS_items_Teen_1221_Self <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0033", "q0036", "q0037", "q0038")

HEA_items_Teen_1221_Self <- c("q0040", "q0042", "q0043", "q0044", "q0045", "q0047", "q0049", "q0050", "q0051", "q0052")

TOU_items_Teen_1221_Self <- c("q0053", "q0054", "q0056", "q0058", "q0059", "q0060", "q0061", "q0062", "q0063", "q0064")

TS_items_Teen_1221_Self <- c("q0066", "q0067", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078")

BOD_items_Teen_1221_Self <- c("q0079", "q0080", "q0081", "q0082", "q0083", "q0084", "q0085", "q0086", "q0088", "q0090")

BAL_items_Teen_1221_Self <- c("q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", "q0103", "q0104", "q0105")

PLA_items_Teen_1221_Self <- c("q0106", "q0107", "q0108", "q0109", "q0110", "q0112", "q0114", "q0115", "q0116", "q0117")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Teen_1221_Self_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/WPS SPM-2 Teen ages 1221 Self-Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_Self
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Teen_1221_Self,
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
    SOC_rev_items_Teen_1221_Self,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Teen_1221_Self,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Teen_1221_Self])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_Self]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_Self]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_Self]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_Self]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_Self]),
    TS_raw = rowSums(.[TS_items_Teen_1221_Self]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_Self]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_Self]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_Self])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) %>% 
  # filter(!(TOT_raw >= 155 & age_range == '18 to 21 years' & data == 'SM')) %>%
  write_csv(here('INPUT-FILES/TEEN/INHOUSE-NORMS-INPUT/Teen-1221-Self-inHouse-norms-input.csv'),
            na = "")

