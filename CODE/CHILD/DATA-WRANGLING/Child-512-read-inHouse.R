suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# HOME

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023", "q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039", 
                              "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050", 
                              "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065", 
                              "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079", 
                              "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092", 
                              "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108", "q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

TOT_items_Child_512_Home <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039", 
                              "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050", 
                              "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065", 
                              "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079", 
                              "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092", 
                              "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

SOC_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

SOC_rev_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

VIS_items_Child_512_Home <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039")

HEA_items_Child_512_Home <- c("q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050")

TOU_items_Child_512_Home <- c("q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065")

TS_items_Child_512_Home <- c("q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079")

BOD_items_Child_512_Home <- c("q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092")

BAL_items_Child_512_Home <- c("q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

PLA_items_Child_512_Home <- c("q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

# English

Child_512_Home_inHouse_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/WPS SPM-2 Child ages 512 Home Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Child_512_Home)
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Child_512_Home,
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
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Child_512_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house-Eng') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from different data sources)
  filter(TOT_raw < 200)

# Spanish

Child_512_Home_inHouse_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/WPS SPM-2 Child ages 512 Home Report Questionnaire sp.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Child_512_Home)
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Child_512_Home,
    ~ case_when(
      .x == "Nunca" ~ 1,
      .x == "Ocasionalmente" ~ 2,
      .x == "Frecuentemente" ~ 3,
      .x == "Siempre" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # recode reverse-scored items
  mutate_at(
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # recode gender and educ
  mutate(
    Gender = case_when(
      Gender == "Masculino" ~ "Male",
      Gender == "Femenino" ~ "Female",
      TRUE ~ NA_character_
    ),
    ParentHighestEducation = case_when(
      ParentHighestEducation == "No terminé la escuela secundaria (no obtuve el diploma)" ~ "Did not complete high school (no diploma)",
      ParentHighestEducation == "Graduado/a de secundaria (incluye diploma de educación general o GED)" ~ "High school graduate (including GED)",
      ParentHighestEducation == "Alguna educación superior o grado asociado (associate degree)" ~ "Some college or associate degree",
      ParentHighestEducation == "Licenciatura o grado más alto" ~ "Bachelor's degree or higher",
      TRUE ~ NA_character_
    )
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(All_items_Child_512_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = "In-House-Sp") %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200)

# write combined English spanish norms input file

Child_512_Home_inHouse <- bind_rows(Child_512_Home_inHouse_Eng, Child_512_Home_inHouse_Sp) %>% 
  write_csv(
    here(
      'INPUT-FILES/CHILD/INHOUSE-NORMS-INPUT/Child-512-Home-inHouse-norms-input.csv'
    ),
    na = ""
  )

rm(list = ls())

# SCHOOL

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Child_512_School <- c("q0012", "q0013", "q0015", "q0016", "q0017", "q0020", "q0021", "q0023", "q0024", "q0025", 
                                "q0027", "q0028", "q0030", "q0031", "q0034", "q0035", "q0036", "q0037", "q0038", "q0039", 
                                "q0040", "q0042", "q0045", "q0046", "q0047", "q0049", "q0050", "q0051", "q0052", "q0053", 
                                "q0056", "q0057", "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0067", 
                                "q0068", "q0069", "q0072", "q0073", "q0074", "q0075", "q0077", "q0078", "q0079", "q0080", 
                                "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0092", "q0093", "q0094", 
                                "q0095", "q0096", "q0097", "q0099", "q0100", "q0101", "q0103", "q0104", "q0107", "q0108", 
                                "q0109", "q0110", "q0112", "q0113", "q0114", "q0115", "q0118", "q0119", "q0120", "q0121")

TOT_items_Child_512_School <- c("q0027", "q0028", "q0030", "q0031", "q0034", "q0035", "q0036", "q0037", "q0038", "q0039", 
                                "q0040", "q0042", "q0045", "q0046", "q0047", "q0049", "q0050", "q0051", "q0052", "q0053", 
                                "q0056", "q0057", "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0067", 
                                "q0068", "q0069", "q0072", "q0073", "q0074", "q0075", "q0077", "q0078", "q0079", "q0080", 
                                "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0092", "q0093", "q0094", 
                                "q0095", "q0096", "q0097", "q0099", "q0100", "q0101", "q0103", "q0104", "q0107", "q0108")

SOC_items_Child_512_School <- c("q0012", "q0013", "q0015", "q0016", "q0017", "q0020", "q0021", "q0023", "q0024", "q0025")

SOC_rev_items_Child_512_School <- c("q0012", "q0013", "q0015", "q0016", "q0017", "q0020", "q0021", "q0023", "q0024", "q0025")

VIS_items_Child_512_School <- c("q0027", "q0028", "q0030", "q0031", "q0034", "q0035", "q0036", "q0037", "q0038", "q0039")

HEA_items_Child_512_School <- c("q0040", "q0042", "q0045", "q0046", "q0047", "q0049", "q0050", "q0051", "q0052", "q0053")

TOU_items_Child_512_School <- c("q0056", "q0057", "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0067")

TS_items_Child_512_School <- c("q0068", "q0069", "q0072", "q0073", "q0074", "q0075", "q0077", "q0078", "q0079", "q0080")

BOD_items_Child_512_School <- c("q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0092", "q0093", "q0094")

BAL_items_Child_512_School <- c("q0095", "q0096", "q0097", "q0099", "q0100", "q0101", "q0103", "q0104", "q0107", "q0108")

PLA_items_Child_512_School <- c("q0109", "q0110", "q0112", "q0113", "q0114", "q0115", "q0118", "q0119", "q0120", "q0121")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Child_512_School_inHouse_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/WPS SPM-2 Child ages 512 School Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Child_512_School)
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
  mutate(data = 'In-house-Eng') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) %>% 
# filter(!(TOT_raw >= 155 & age_range == '18 to 21 years' & data == 'SM')) %>%
  write_csv(here('INPUT-FILES/CHILD/INHOUSE-NORMS-INPUT/Child-512-School-inHouse-norms-input.csv'),
            na = "")

rm(list = ls())

