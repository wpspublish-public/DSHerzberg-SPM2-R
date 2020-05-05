# LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# CHILD 512 HOME DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R'))

Child_512_Home_time2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/TIME-2/SPM-2 Child ages 512 Home Report Questionnaire time2.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Child_512_Home
  ) %>%
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
  mutate(data = 'time2',
         clin_status = 'typ',
         clin_dx = NA
         ) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
Child_512_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Child_512_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_512 <- map_dfc(score_names,
                     ~
                       Child_512_Home_time2 %>% left_join(eval(as.name(
                         str_c(.x, '_512_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_Home_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_512,
  here(
    'OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls())


# CHILD 512 SCHOOL DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Child-512-School-item-vectors.R'))

Child_512_School_time2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/TIME-2/SPM-2 Child ages 512 School Report Questionnaire time2.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Child_512_School
  ) %>%
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
  mutate(data = 'time2',
         clin_status = 'typ',
         clin_dx = NA
  ) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
Child_512_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-School-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Child_512_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_512 <- map_dfc(score_names,
                       ~
                         Child_512_School_time2 %>% left_join(eval(as.name(
                           str_c(.x, '_512_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_School_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_512,
  here(
    'OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls())


