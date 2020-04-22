# read in IT home(parent rating) and daycare rating data, equalize item numbers,
# keep only unique daycare ratings (i.e., those that DON'T also have a parent
# rating), compare TOT means between parent and unique daycare ratings to
# determine whether to include the latter in normative sample.

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))


# IT-49 DATA --------------------------------------------------------------

All_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023", 
                     "q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                     "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                     "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                     "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                     "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                     "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105", 
                     "q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")

# use purrr::map_chr() to apply a function to the home item numbers
# (All_items_IT_49_Home) that increments the numerical part of the item number
# by 1. This maps the daycare item numbers onto the home item numbers, and
# allows the daycare items to be renamed with the names of the home items.
# map_chr() returns a char vec.
All_items_IT_49_Daycare <- map_chr(All_items_IT_49_Home, ~ str_sub(.x, 2, 5) %>% 
          as.integer() %>% 
          `+`(1) %>% 
          str_pad(4, pad = '0') %>% 
          str_c('q', .))

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


IT_49_Daycare_SM_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SPM-2 InfantToddler 49 Months Daycare Provider.csv")
  ))) %>% select(
    All_items_IT_49_Daycare
  )

IT_49_Daycare_inHouse_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/WPS SPM-2 InfantToddler 49 Months Daycare Provider.csv")
  ))) %>% select(
    All_items_IT_49_Daycare
  )

IT_49_Daycare_items <- bind_rows(IT_49_Daycare_SM_items, IT_49_Daycare_inHouse_items)

# rename the daycare items to the names of the home items, so the two samples
# can be stacked into a single df
names(IT_49_Daycare_items) <- All_items_IT_49_Home

# Stack key columsn from SM and inhouse data to combine two data sources for scoring
IT_49_Daycare_SM <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/IT/SPM-2 InfantToddler 49 Months Daycare Provider.csv"
    )
  ))) %>% 
  select(
    IDNumber,
    AgeInMonths,
    Gender,
    Ethnicity,
    Region
  )

IT_49_Daycare_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/IT/WPS SPM-2 InfantToddler 49 Months Daycare Provider.csv"
    )
  ))) %>% 
 select(
    IDNumber,
    AgeInMonths,
    Gender,
    Ethnicity,
    Region
  )

IT_49_Daycare_SM_inHouse <- bind_rows(IT_49_Daycare_SM, IT_49_Daycare_inHouse)

IT_49_Daycare <-IT_49_Daycare_SM_inHouse %>% 
  # bring in the renamed daycare items
  bind_cols(
    IT_49_Daycare_items
  ) %>%
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
  # Compute raw scores. Note use of `rowSums(.[TOT_items_IT_49_Home])`: when used 
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
  # Create columns to match Home data
  mutate(
    data = "Daycare",
    ParentHighestEducation = NA
    ) %>% 
  select(IDNumber, data, AgeInMonths, Gender, ParentHighestEducation, everything())# %>%
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  # filter(TOT_raw < 190) %>% 
  # filter(!(TOT_raw >= 100 & age_range == '7 to 10.5 mo' & data == 'Qual')) %>%
  # write_csv(here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv'),
  #           na = "")

# read scored home(parent-rating) data, to which the columns of the daycare data
# have been matched.
IT_49_Home <- read_csv(here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv'))

# stack the home and daycare rows, identify dup IDNumbers using
# data.table::duplicated(), which returns a logical vector where TRUE indicates
# a duplicate, mutate() puts that vector into a column so it can be filtered,
# then filter out the daycare rows that are TRUE on dup
IT_49_comp <- bind_rows(IT_49_Home, IT_49_Daycare) %>% 
  mutate(data = case_when(
    data == "Qual" ~ "Home",
    data == "SM" ~ "Home",
    TRUE ~ data
  )) %>% 
  arrange(IDNumber) %>% 
  mutate(dup = duplicated(IDNumber)) %>% 
  select(IDNumber, dup, everything()) %>% 
  filter(!(data == "Daycare" & dup == TRUE))

# confirmation step that dup IDNumbers are gone. data.table::uniqueN() returns
# the number of non-dup rows on IDnumber, this value should equal the number of
# rows in the main data object
uniqueN(IT_49_comp, by = "IDNumber")

# write raw score descriptives for all scales (using psych::describeBy)
IT_comp_TOT_raw_desc <-
  IT_49_comp %>% 
  select(contains('raw')) %>% 
  describeBy(IT_49_comp$data, fast = T, mat = T) %>%
  rownames_to_column() %>%
  rename(scale = rowname, data = group1) %>% 
  arrange(data) %>% 
  select(scale, data, n, mean, sd) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  mutate_at(vars(scale), ~str_sub(., 1, 3) %>% 
              str_replace(., '_', ''))

rm(list = ls())

# IT-1030 DATA --------------------------------------------------------------

All_items_IT_1030_Home <- c("q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0022", 
                            "q0024", "q0025", "q0026", "q0027", "q0028", "q0029", "q0032", "q0034", "q0035", "q0036", 
                            "q0037", "q0038", "q0039", "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", 
                            "q0052", "q0054", "q0055", "q0056", "q0061", "q0062", "q0063", "q0066", "q0067", "q0068", 
                            "q0069", "q0071", "q0073", "q0075", "q0076", "q0077", "q0079", "q0080", "q0081", "q0082", 
                            "q0086", "q0087", "q0088", "q0089", "q0091", "q0093", "q0095", "q0096", "q0097", "q0098", 
                            "q0100", "q0101", "q0102", "q0103", "q0104", "q0105", "q0106", "q0107", "q0110", "q0112", 
                            "q0114", "q0117", "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125")

# use purrr::map_chr() to apply a function to the home item numbers
# (All_items_IT_1030_Home) that increments the numerical part of the item number
# by 1. This maps the daycare item numbers onto the home item numbers, and
# allows the daycare items to be renamed with the names of the home items.
# map_chr() returns a char vec.
All_items_IT_1030_Daycare <- map_chr(All_items_IT_1030_Home, ~ str_sub(.x, 2, 5) %>% 
                                       as.integer() %>% 
                                       `+`(1) %>% 
                                       str_pad(4, pad = '0') %>% 
                                       str_c('q', .))

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


IT_1030_Daycare_SM_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SPM-2 InfantToddler 1030 Months Daycare Provider.csv")
  ))) %>% select(
    All_items_IT_1030_Daycare
  )

IT_1030_Daycare_inHouse_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/WPS SPM-2 InfantToddler 1030 Months Daycare Provider.csv")
  ))) %>% select(
    All_items_IT_1030_Daycare
  )

IT_1030_Daycare_items <- bind_rows(IT_1030_Daycare_SM_items, IT_1030_Daycare_inHouse_items)

# rename the daycare items to the names of the home items, so the two samples
# can be stacked into a single df
names(IT_1030_Daycare_items) <- All_items_IT_1030_Home

# Stack key columsn from SM and inhouse data to combine two data sources for scoring
IT_1030_Daycare_SM <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/IT/SPM-2 InfantToddler 1030 Months Daycare Provider.csv"
    )
  ))) %>% 
  select(
    IDNumber,
    AgeInMonths,
    Gender,
    Ethnicity,
    Region
  )

IT_1030_Daycare_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/IT/WPS SPM-2 InfantToddler 1030 Months Daycare Provider.csv"
    )
  ))) %>% 
  select(
    IDNumber,
    AgeInMonths,
    Gender,
    Ethnicity,
    Region
  )

IT_1030_Daycare_SM_inHouse <- bind_rows(IT_1030_Daycare_SM, IT_1030_Daycare_inHouse)

IT_1030_Daycare <-IT_1030_Daycare_SM_inHouse %>% 
  # bring in the renamed daycare items
  bind_cols(
    IT_1030_Daycare_items
  ) %>%
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
    AgeInMonths <= 6 ~ "03.5 to 6 mo",
    TRUE ~ "07 to 10.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_IT_1030_Home])`: when used 
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
  # Create columns to match Home data
  mutate(
    data = "Daycare",
    ParentHighestEducation = NA
  ) %>% 
  select(IDNumber, data, AgeInMonths, Gender, ParentHighestEducation, everything())# %>%
# Exclude outliers on TOT_raw (also exlude by data source to equalize samples
# from diiferent data sources)
# filter(TOT_raw < 190) %>% 
# filter(!(TOT_raw >= 100 & age_range == '7 to 10.5 mo' & data == 'Qual')) %>%
# write_csv(here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv'),
#           na = "")

# read scored home(parent-rating) data, to which the columns of the daycare data
# have been matched.
IT_1030_Home <- read_csv(here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv'))

# stack the home and daycare rows, identify dup IDNumbers using
# data.table::duplicated(), which returns a logical vector where TRUE indicates
# a duplicate, mutate() puts that vector into a column so it can be filtered,
# then filter out the daycare rows that are TRUE on dup
IT_1030_comp <- bind_rows(IT_1030_Home, IT_1030_Daycare) %>% 
  mutate(data = case_when(
    data == "Qual" ~ "Home",
    data == "SM" ~ "Home",
    TRUE ~ data
  )) %>% 
  arrange(IDNumber) %>% 
  mutate(dup = duplicated(IDNumber)) %>% 
  select(IDNumber, dup, everything()) %>% 
  filter(!(data == "Daycare" & dup == TRUE))

# confirmation step that dup IDNumbers are gone. data.table::uniqueN() returns
# the number of non-dup rows on IDnumber, this value should equal the number of
# rows in the main data object
uniqueN(IT_1030_comp, by = "IDNumber")

# write raw score descriptives for all scales (using psych::describeBy)
IT_comp_TOT_raw_desc <-
  IT_1030_comp %>% 
  select(contains('raw')) %>% 
  describeBy(IT_1030_comp$data, fast = T, mat = T) %>%
  rownames_to_column() %>%
  rename(scale = rowname, data = group1) %>% 
  arrange(data) %>% 
  select(scale, data, n, mean, sd) %>% 
  mutate_at(vars(mean, sd), ~(round(., 2))) %>% 
  mutate_at(vars(scale), ~str_sub(., 1, 3) %>% 
              str_replace(., '_', ''))
