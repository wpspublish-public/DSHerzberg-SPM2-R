suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# HOME DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R'))

Teen_1221_Home_clin <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/FORM-C/SPM-2 Teen ages 1221 Home Report Questionnaire C.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_Home,
    q0010_other
  ) %>%
  rename(clin_dx = q0010_other) %>% 
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
  mutate(data = 'clin',
         clin_status = 'clin') %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
Teen_1221_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/RAW-T-LOOKUP-TABLES/Teen-1221-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Teen_1221_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1221_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                     ~
                       Teen_1221_Home_clin %>% left_join(eval(as.name(
                         str_c(.x, '_1221_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Teen_1221_Home_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_1221 <- dx_recode_out

# write output for analysis

write_csv(
  output_1221,
  here(
    'OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv'
  )
)

rm(list = ls())

# SCHOOL DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R'))

Teen_1221_School_clin <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/FORM-C/SPM-2 Teen ages 1221 School Report Questionnaire C.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_School,
    q0010_other
  ) %>%
  rename(clin_dx = q0010_other) %>% 
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
  mutate(data = 'clin',
         clin_status = 'clin') %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
Teen_1221_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/RAW-T-LOOKUP-TABLES/Teen-1221-School-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Teen_1221_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1221_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                     ~
                       Teen_1221_School_clin %>% left_join(eval(as.name(
                         str_c(.x, '_1221_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Teen_1221_School_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_1221 <- dx_recode_out

# write output for analysis

write_csv(
  output_1221,
  here(
    'OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv'
  )
)

rm(list = ls())

# SELF DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R'))

Teen_1221_Self_clin <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/FORM-C/SPM-2 Teen ages 1221 Self-Report Questionnaire C.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_Self,
    q0010_other
  ) %>%
  rename(clin_dx = q0010_other) %>% 
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
  mutate(data = 'clin',
         clin_status = 'clin') %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
Teen_1221_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/RAW-T-LOOKUP-TABLES/Teen-1221-Self-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Teen_1221_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1221_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                       ~
                         Teen_1221_Self_clin %>% left_join(eval(as.name(
                           str_c(.x, '_1221_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Teen_1221_Self_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_1221 <- dx_recode_out

# write output for analysis

write_csv(
  output_1221,
  here(
    'OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv'
  )
)


