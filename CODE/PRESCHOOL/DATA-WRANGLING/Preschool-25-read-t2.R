# LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# PRESCHOOL 25 HOME DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R'))

Preschool_25_Home_time2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/TIME-2/SPM-2 Preschooler ages 25 Home Report Questionnaire time2.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Preschool_25_Home
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Preschool_25_Home,
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
    SOC_rev_items_Preschool_25_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Preschool_25_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 4 ~ "2 to 4 years",
    TRUE ~ "5 years")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_Home]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_Home]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_Home]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_Home]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_Home]),
    TS_raw = rowSums(.[TS_items_Preschool_25_Home]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_Home]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_Home]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_Home])
  ) %>% 
  # Create data var 
  mutate(data = 'time2',
         clin_status = 'typ',
         clin_dx = NA
         ) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# split sample by age_range

Preschool_24_Home_time2 <- Preschool_25_Home_time2 %>% filter(age_range == "2 to 4 years")
Preschool_5_Home_time2 <- Preschool_25_Home_time2 %>% filter(age_range == "5 years")

# 24 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_24_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-24-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Preschool_24_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_24_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_24 <- map_dfc(score_names,
                     ~
                       Preschool_24_Home_time2 %>% left_join(eval(as.name(
                         str_c(.x, '_24_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_Home_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_24,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

# 5 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_5_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-5-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Preschool_5_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_5_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_5 <- map_dfc(score_names,
                     ~
                       Preschool_5_Home_time2 %>% left_join(eval(as.name(
                         str_c(.x, '_5_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_Home_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_5,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls())


# PRESCHOOL 25 SCHOOL DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Preschool-25-School-item-vectors.R'))

Preschool_25_School_time2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/TIME-2/SPM-2 Preschooler ages 25 School Report Questionnaire time2.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Preschool_25_School
  ) %>%
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
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_School])`: when used 
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
  mutate(data = 'time2',
         clin_status = 'typ',
         clin_dx = NA
  ) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# split sample by age_range

Preschool_24_School_time2 <- Preschool_25_School_time2 %>% filter(age_range == "2 to 4 years")
Preschool_5_School_time2 <- Preschool_25_School_time2 %>% filter(age_range == "5 years")

# 24 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_24_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-24-School-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Preschool_24_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_24_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_24 <- map_dfc(score_names,
                     ~
                       Preschool_24_School_time2 %>% left_join(eval(as.name(
                         str_c(.x, '_24_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_School_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_24,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

# 5 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_5_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-5-School-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Preschool_5_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_5_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_5 <- map_dfc(score_names,
                    ~
                      Preschool_5_School_time2 %>% left_join(eval(as.name(
                        str_c(.x, '_5_lookup_col')
                      )),
                      by = str_c(.x, '_raw')) %>%
                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_School_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_5,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls())

