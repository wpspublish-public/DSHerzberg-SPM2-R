suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# HOME 49 DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R'))

IT_49_Home_clin <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/FORM-C/SPM-2 InfantToddler 49 Months C.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_49_Home,
    q0009_other
  ) %>%
  rename(clin_dx= q0009_other) %>% 
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
  mutate(data = 'clin',
         clin_status = 'clin') %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# split sample by age_range

IT_46_Home_clin <- IT_49_Home_clin %>% filter(age_range == "03.5 to 6 mo")
IT_79_Home_clin <- IT_49_Home_clin %>% filter(age_range == "07 to 10.5 mo")

# 46 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_46_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-46-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_46_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_46_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                     ~
                       IT_46_Home_clin %>% left_join(eval(as.name(
                         str_c(.x, '_46_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_46_Home_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_46 <- dx_recode_out

# write output for analysis

write_csv(
  output_46,
  here(
    'OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-clin-T-Scores-per-case.csv'
  )
)

rm(list = ls(pattern = 'col|recode'))

# 79 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_79_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-79-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_79_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_79_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                     ~
                       IT_79_Home_clin %>% left_join(eval(as.name(
                         str_c(.x, '_79_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_79_Home_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_79 <- dx_recode_out

# write output for analysis

write_csv(
  output_79,
  here(
    'OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-clin-T-Scores-per-case.csv'
  )
)

rm(list = ls())

# HOME 1030 DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

IT_1030_Home_clin <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/FORM-C/SPM-2 InfantToddler 1030 Months C.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_1030_Home,
    q0009_other
  ) %>%
  rename(clin_dx = q0009_other) %>% 
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
  mutate(data = 'clin',
         clin_status = 'clin') %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# split sample by age_range

IT_1020_Home_clin <- IT_1030_Home_clin %>% filter(age_range == "09.5 to 20 mo")
IT_2130_Home_clin <- IT_1030_Home_clin %>% filter(age_range == "21 to 31.5 mo")

# 1020 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_1020_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-1020-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_1020_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1020_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                       ~
                         IT_1020_Home_clin %>% left_join(eval(as.name(
                           str_c(.x, '_1020_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_1020_Home_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_1020 <- dx_recode_out

# write output for analysis

write_csv(
  output_1020,
  here(
    'OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-clin-T-Scores-per-case.csv'
  )
)

rm(list = ls(pattern = 'col|recode'))

# 2130 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_2130_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-2130-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_2130_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_2130_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                       ~
                         IT_2130_Home_clin %>% left_join(eval(as.name(
                           str_c(.x, '_2130_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_2130_Home_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_2130 <- dx_recode_out

# write output for analysis

write_csv(
  output_2130,
  here(
    'OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-clin-T-Scores-per-case.csv'
  )
)

rm(list = ls())

# CAREGIVER DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/IT-Caregiver-item-vectors.R'))

IT_Caregiver_clin <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/FORM-C/SPM2_InfantToddler_Caregiver_Combo C.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_Caregiver,
    q0009_other
  ) %>%
  rename(clin_dx = q0009_other) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_Caregiver,
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
    SOC_rev_items_IT_Caregiver,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_IT_Caregiver,
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
    TOT_raw = rowSums(.[TOT_items_IT_Caregiver]),
    SOC_raw = rowSums(.[SOC_items_IT_Caregiver]),
    VIS_raw = rowSums(.[VIS_items_IT_Caregiver]),
    HEA_raw = rowSums(.[HEA_items_IT_Caregiver]),
    TOU_raw = rowSums(.[TOU_items_IT_Caregiver]),
    TS_raw = rowSums(.[TS_items_IT_Caregiver]),
    BOD_raw = rowSums(.[BOD_items_IT_Caregiver]),
    BAL_raw = rowSums(.[BAL_items_IT_Caregiver]),
    PLA_raw = rowSums(.[PLA_items_IT_Caregiver])
  ) %>% 
  # Create data var 
  mutate(data = 'clin',
         clin_status = 'clin') %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
IT_Caregiver_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-Caregiver-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_Caregiver_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Caregiver_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
dx_recode_in <- map_dfc(score_names,
                       ~
                         IT_Caregiver_clin %>% left_join(eval(as.name(
                           str_c(.x, '_Caregiver_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_Caregiver_clin, .)

source(here('CODE/MISC/clin-dx-orig-map-clin-dx-rev.R'))

output_Caregiver <- dx_recode_out

# write output for analysis

write_csv(
  output_Caregiver,
  here(
    'OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-clin-T-Scores-per-case.csv'
  ), 
  na = '(missing)'
)


