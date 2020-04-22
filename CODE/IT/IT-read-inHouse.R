suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# HOME 49

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023", 
                          "q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                          "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                          "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                          "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                          "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                          "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105", 
                          "q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")


TOT_items_IT_49_Home <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                          "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                          "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                          "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                          "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                          "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105")

SOC_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023")

SOC_rev_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023")

VIS_items_IT_49_Home <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038")

HEA_items_IT_49_Home <- c("q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050")

TOU_items_IT_49_Home <- c("q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066")

TS_items_IT_49_Home <- c("q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078")

BOD_items_IT_49_Home <- c("q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091")

BAL_items_IT_49_Home <- c("q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105")

PLA_items_IT_49_Home <- c("q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

# English

IT_49_Home_inHouse_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/WPS SPM-2 InfantToddler 49 Months.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_49_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_49_Home,
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
    SOC_rev_items_IT_49_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_IT_49_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 6 ~ "03.5 to 6 mo",
    TRUE ~ "07 to 10.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_49_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_49_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_49_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_49_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_49_Home]),
    TS_raw = rowSums(.[TS_items_IT_49_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_49_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_49_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_49_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house-Eng') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from different data sources)
  filter(TOT_raw < 200)

# Spanish

IT_49_Home_inHouse_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/WPS SPM-2 InfantToddler 49 Months sp.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_49_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_49_Home,
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
    SOC_rev_items_IT_49_Home,
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
  mutate_at(All_items_IT_49_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 6 ~ "03.5 to 6 mo",
    TRUE ~ "07 to 10.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_49_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_49_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_49_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_49_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_49_Home]),
    TS_raw = rowSums(.[TS_items_IT_49_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_49_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_49_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_49_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house-Sp') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from different data sources)
  filter(TOT_raw < 200)

IT_49_Home_inHouse <-
  bind_rows(IT_49_Home_inHouse_Eng, IT_49_Home_inHouse_Sp) %>% write_csv(
    here(
      'INPUT-FILES/IT/INHOUSE-NORMS-INPUT/IT-49-Home-inHouse-norms-input.csv'
    ),
    na = ""
  )

rm(list = ls())


# HOME 1030

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_IT_1030_Home <- c("q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0022", 
                            "q0024", "q0025", "q0026", "q0027", "q0028", "q0029", "q0032", "q0034", "q0035", "q0036", 
                            "q0037", "q0038", "q0039", "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", 
                            "q0052", "q0054", "q0055", "q0056", "q0061", "q0062", "q0063", "q0066", "q0067", "q0068", 
                            "q0069", "q0071", "q0073", "q0075", "q0076", "q0077", "q0079", "q0080", "q0081", "q0082", 
                            "q0086", "q0087", "q0088", "q0089", "q0091", "q0093", "q0095", "q0096", "q0097", "q0098", 
                            "q0100", "q0101", "q0102", "q0103", "q0104", "q0105", "q0106", "q0107", "q0110", "q0112", 
                            "q0114", "q0117", "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125")

TOT_items_IT_1030_Home <- c("q0024", "q0025", "q0026", "q0027", "q0028", "q0029", "q0032", "q0034", "q0035", "q0036", 
                            "q0037", "q0038", "q0039", "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", 
                            "q0052", "q0054", "q0055", "q0056", "q0061", "q0062", "q0063", "q0066", "q0067", "q0068", 
                            "q0069", "q0071", "q0073", "q0075", "q0076", "q0077", "q0079", "q0080", "q0081", "q0082", 
                            "q0086", "q0087", "q0088", "q0089", "q0091", "q0093", "q0095", "q0096", "q0097", "q0098", 
                            "q0100", "q0101", "q0102", "q0103", "q0104", "q0105", "q0106", "q0107", "q0110", "q0112")

SOC_items_IT_1030_Home <- c("q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0022")

SOC_rev_items_IT_1030_Home <- c("q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0022")

VIS_items_IT_1030_Home <- c("q0024", "q0025", "q0026", "q0027", "q0028", "q0029", "q0032", "q0034", "q0035", "q0036")

HEA_items_IT_1030_Home <- c("q0037", "q0038", "q0039", "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046")

TOU_items_IT_1030_Home <- c("q0052", "q0054", "q0055", "q0056", "q0061", "q0062", "q0063", "q0066", "q0067", "q0068")

TS_items_IT_1030_Home <- c("q0069", "q0071", "q0073", "q0075", "q0076", "q0077", "q0079", "q0080", "q0081", "q0082")

BOD_items_IT_1030_Home <- c("q0086", "q0087", "q0088", "q0089", "q0091", "q0093", "q0095", "q0096", "q0097", "q0098")

BAL_items_IT_1030_Home <- c("q0100", "q0101", "q0102", "q0103", "q0104", "q0105", "q0106", "q0107", "q0110", "q0112")

PLA_items_IT_1030_Home <- c("q0114", "q0117", "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

IT_1030_Home_inHouse_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/WPS SPM-2 InfantToddler 1030 Months.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_1030_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_1030_Home,
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
    SOC_rev_items_IT_1030_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_IT_1030_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 20 ~ "09.5 to 20 mo",
    TRUE ~ "21 to 31.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_1030_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_1030_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_1030_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_1030_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_1030_Home]),
    TS_raw = rowSums(.[TS_items_IT_1030_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_1030_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_1030_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_1030_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house-Eng') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from different data sources)
  filter(TOT_raw < 200)

# Spanish

IT_1030_Home_inHouse_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/WPS SPM-2 InfantToddler 1030 Months sp.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_1030_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_1030_Home,
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
    SOC_rev_items_IT_1030_Home,
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
  mutate_at(All_items_IT_1030_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 6 ~ "03.5 to 6 mo",
    TRUE ~ "07 to 10.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_1030_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_1030_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_1030_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_1030_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_1030_Home]),
    TS_raw = rowSums(.[TS_items_IT_1030_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_1030_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_1030_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_1030_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'In-house-Sp') %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from different data sources)
  filter(TOT_raw < 200)

IT_1030_Home_inHouse <-
  bind_rows(IT_1030_Home_inHouse_Eng, IT_1030_Home_inHouse_Sp) %>% write_csv(
    here(
      'INPUT-FILES/IT/INHOUSE-NORMS-INPUT/IT-1030-Home-inHouse-norms-input.csv'
    ),
    na = ""
  )

rm(list = ls())

