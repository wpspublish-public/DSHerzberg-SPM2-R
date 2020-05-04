# ADULT OTHER DATA ------------------------------------------------------------

source(here('CODE/ITEM-VECTORS/Adult-Other-item-vectors.R'))

Adult_Other_time2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/TIME-2/SPM-2 Adult ages 1690 Other Report Questionnaire time2.csv")
  ))) %>% select(
    IDNumber,
    Age,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Other
  ) %>%
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
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 30 ~ "21.00 to 30.99 years",
    between(Age, 31, 40) ~ "31.00 to 40.99 years",
    between(Age, 41, 50) ~ "41.00 to 50.99 years",
    between(Age, 51, 64) ~ "51.00 to 64.99 years",
    TRUE ~ "65.00 to 99.99 years")) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Other])`: when used 
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
  mutate(data = 'time2',
         clin_status = 'typ',
         clin_dx = NA
  ) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# read raw-to-t lookup tables, create lookup cols by scale
Adult_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/RAW-T-LOOKUP-TABLES/Adult-Other-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Adult_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1690_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_1690 <- map_dfc(score_names,
                       ~
                         Adult_Other_time2 %>% left_join(eval(as.name(
                           str_c(.x, '_1690_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Adult_Other_time2, .) %>% 
  arrange(IDNumber)

# write outuput for analysis

write_csv(
  output_1690,
  here(
    'OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-time2-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls())


