# Preschool School 25 read SM only data for downstream processing

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Preschool_25_School <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0024", 
                                   "q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0033", "q0034", "q0035", 
                                   "q0038", "q0039", "q0041", "q0042", "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", 
                                   "q0052", "q0053", "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", "q0063", 
                                   "q0064", "q0065", "q0066", "q0067", "q0068", "q0069", "q0070", "q0071", "q0074", "q0076", 
                                   "q0077", "q0078", "q0079", "q0080", "q0081", "q0083", "q0084", "q0086", "q0088", "q0090", 
                                   "q0091", "q0093", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", "q0101", "q0104", 
                                   "q0105", "q0106", "q0108", "q0110", "q0111", "q0112", "q0113", "q0114", "q0115", "q0117")

TOT_items_Preschool_25_School <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0033", "q0034", "q0035", 
                                   "q0038", "q0039", "q0041", "q0042", "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", 
                                   "q0052", "q0053", "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", "q0063", 
                                   "q0064", "q0065", "q0066", "q0067", "q0068", "q0069", "q0070", "q0071", "q0074", "q0076", 
                                   "q0077", "q0078", "q0079", "q0080", "q0081", "q0083", "q0084", "q0086", "q0088", "q0090", 
                                   "q0091", "q0093", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", "q0101", "q0104")

SOC_items_Preschool_25_School <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0024")

SOC_rev_items_Preschool_25_School <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0024")

VIS_items_Preschool_25_School <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0033", "q0034", "q0035")

HEA_items_Preschool_25_School <- c("q0038", "q0039", "q0041", "q0042", "q0043", "q0045", "q0046", "q0047", "q0048", "q0050")

TOU_items_Preschool_25_School <- c("q0052", "q0053", "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", "q0063")

TS_items_Preschool_25_School <- c("q0064", "q0065", "q0066", "q0067", "q0068", "q0069", "q0070", "q0071", "q0074", "q0076")

BOD_items_Preschool_25_School <- c("q0077", "q0078", "q0079", "q0080", "q0081", "q0083", "q0084", "q0086", "q0088", "q0090")

BAL_items_Preschool_25_School <- c("q0091", "q0093", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", "q0101", "q0104")

PLA_items_Preschool_25_School <- c("q0105", "q0106", "q0108", "q0110", "q0111", "q0112", "q0113", "q0114", "q0115", "q0117")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Preschool_25_School <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/SPM-2 Preschooler ages 25 School Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Preschool_25_School
  ) %>%
  # filter for age stratification
  # filter(Age == 5) %>%
  # mutate(age_range = "5 yr") %>%
  # select(everything(), age_range) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Preschool_25_School,
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
    SOC_rev_items_Preschool_25_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Preschool_25_School,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 4 ~ "2 to 4 years",
    TRUE ~ "5 years") 
    )%>% 
    # Compute raw scores. Note use of `rowSums(.[TOT_items_Preschool_25_School])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_School]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_School]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_School]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_School]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_School]),
    TS_raw = rowSums(.[TS_items_Preschool_25_School]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_School]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_School]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_School])
  ) %>% 
  # Create data var 
  mutate(data = 'SM') %>% 
  select(IDNumber, data, everything()) %>% 
  filter(TOT_raw <200) %>% 
  write_csv(
    here(
      'INPUT-FILES/PRESCHOOL/SM-ONLY-NORMS-INPUT/Preschool-25-School-SM-only-norms-input.csv'
      ),
    na = ""
    )



