suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

adult_driving_items <-
  c(
    'q0133',
    'q0134',
    'q0135',
    'q0136',
    'q0137',
    'q0138',
    'q0139',
    'q0140',
    'q0141',
    'q0142',
    'q0143',
    'q0144',
    'q0145',
    'q0146',
    'q0147',
    'q0148',
    'q0149',
    'q0150'
  )


# READ, SCORE ENGLISH DATA ------------------------------------------------

adult_driving_data_Eng <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/ADULT/SPM-2 Adult Driving SR Other Combo.csv'
  )
)) %>%
  select(IDNumber, DriveForm, adult_driving_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    adult_driving_items,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>% 
  # recode reverse-scored items
  # mutate_at(
  #   adult_driving_rev_items,
  #   ~ case_when(.x == 4 ~ 1,
  #               .x == 3 ~ 2,
  #               .x == 2 ~ 3,
  #               .x == 1 ~ 4,
  #               TRUE ~ NA_real_)
  # ) %>% 
  # Convert scored item vars to integers
  mutate_at(adult_driving_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[adult_driving_items])
  ) %>% 
  # add data var
  mutate(data = "Eng") %>% 
  select(IDNumber, data, everything())

# READ, SCORE SPANISH DATA ------------------------------------------------

adult_driving_data_Sp <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/ADULT/SPM-2 Adult Driving SR Other Combo SP.csv'
  )
)) %>%
  select(IDNumber, DriveForm, adult_driving_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    adult_driving_items,
    ~ case_when(
      .x == "Nunca" ~ 1,
      .x == "Ocasionalmente" ~ 2,
      .x == "Frecuentemente" ~ 3,
      .x == "Siempre" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # recode reverse-scored items
  # mutate_at(
  #   adult_driving_rev_items,
  #   ~ case_when(.x == 4 ~ 1,
  #               .x == 3 ~ 2,
  #               .x == 2 ~ 3,
  #               .x == 1 ~ 4,
  #               TRUE ~ NA_real_)
  # ) %>% 
  # Convert scored item vars to integers
  mutate_at(adult_driving_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[adult_driving_items])
  ) %>% 
  # add data var
  mutate(data = "Sp") %>% 
  select(IDNumber, data, everything())

# RECOMBINE DATA BY FORM --------------------------------------------------

adult_driving_data_self <- 
  bind_rows(
    (adult_driving_data_Eng %>% filter(DriveForm == "AdultSR")),
     (adult_driving_data_Sp %>% filter(DriveForm == "AdultSRsp"))
  ) %>% 
  arrange(IDNumber) %>% 
  filter(!is.na(TOT_raw)) %>% 
  # remove outliers
  filter(TOT_raw < 45)

adult_driving_data_other <- 
  # adult_driving_data_Eng %>% 
  # filter(DriveForm == "AdultOther") %>% 
  bind_rows(
    (adult_driving_data_Eng %>% filter(DriveForm == "AdultOther")),
    (adult_driving_data_Sp %>% filter(DriveForm == "AdultOthersp"))
  ) %>% 
  arrange(IDNumber) %>% 
  filter(!is.na(TOT_raw)) %>% 
  # remove outliers
  filter(TOT_raw < 45)

# write data
write_csv(adult_driving_data_self, 
          here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/adult-driving-self-data.csv'))
write_csv(adult_driving_data_other, 
          here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/adult-driving-other-data.csv'))

# # Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID encountered)
# anyDuplicated(adult_driving_data$IDNumber)
# # Check for any NAs on IDNumber, returns TRUE if NA exist
# any(is.na(adult_driving_data$IDNumber))



# CREATE SUMMARY TABLE SHOWING OPTIMAL CUTOFF SCORE -----------------------

# SELF

# Freq table
adult_driving_self_TOT_freq <- adult_driving_data_self %>% 
  count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4),
    diff = case_when(
      cum_per <= 90 & lead(cum_per) >= 90 & lag(cum_per) <= 90 ~ 90 - cum_per,
      cum_per > 90 & lead(cum_per) >= 90 & lag(cum_per) <= 90 ~ cum_per -90,
      TRUE ~ NA_real_
    ),
    cutoff = case_when(
      is.na(lag(diff)) & diff < lead(diff) ~ 'cutoff',
      is.na(lead(diff)) & diff < lag(diff) ~ 'cutoff',
      TRUE ~ NA_character_
    )
  ) %>% 
  select(-diff)

# Descriptives table
adult_driving_self_TOT_desc_1row <- adult_driving_data_self %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
adult_driving_self_TOT_desc <- adult_driving_self_TOT_desc_1row %>% 
  add_row(total_n = rep(NA_integer_, nrow(adult_driving_self_TOT_freq) - 1))
adult_driving_self_summary <- bind_cols(adult_driving_self_TOT_freq, adult_driving_self_TOT_desc)

# write summary table
write_csv(
  adult_driving_self_summary,
  here('OUTPUT-FILES/SCHOOL-ENVIRON-DRIVING/adult-driving-self-cutoff-summary.csv'),
  na = ''
)

# OTHER

# Freq table
adult_driving_other_TOT_freq <- adult_driving_data_other %>% 
  count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4),
    diff = case_when(
      cum_per <= 90 & lead(cum_per) >= 90 & lag(cum_per) <= 90 ~ 90 - cum_per,
      cum_per > 90 & lead(cum_per) >= 90 & lag(cum_per) <= 90 ~ cum_per -90,
      TRUE ~ NA_real_
    ),
    cutoff = case_when(
      is.na(lag(diff)) & diff < lead(diff) ~ 'cutoff',
      is.na(lead(diff)) & diff < lag(diff) ~ 'cutoff',
      TRUE ~ NA_character_
    )
  ) %>% 
  select(-diff)

# Descriptives table
adult_driving_other_TOT_desc_1row <- adult_driving_data_other %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
adult_driving_other_TOT_desc <- adult_driving_other_TOT_desc_1row %>% 
  add_row(total_n = rep(NA_integer_, nrow(adult_driving_other_TOT_freq) - 1))
adult_driving_other_summary <- bind_cols(adult_driving_other_TOT_freq, adult_driving_other_TOT_desc)

# write summary table
write_csv(
  adult_driving_other_summary,
  here('OUTPUT-FILES/SCHOOL-ENVIRON-DRIVING/adult-driving-other-cutoff-summary.csv'),
  na = ''
)



