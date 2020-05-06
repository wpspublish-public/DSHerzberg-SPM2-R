suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

teen_driving_items <-
  c(
    'q0120',
    'q0121',
    'q0122',
    'q0123',
    'q0124',
    'q0125',
    'q0126',
    'q0127',
    'q0128',
    'q0129',
    'q0130',
    'q0131',
    'q0132',
    'q0133',
    'q0134',
    'q0135',
    'q0136',
    'q0137'
  )

# READ, SCORE ENGLISH DATA ------------------------------------------------

teen_driving_data_Eng <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/TEEN/SPM-2 Teen Driving SR Home Combo.csv'
  )
)) %>%
  select(IDNumber, DriveForm, teen_driving_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    teen_driving_items,
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
  #   teen_driving_rev_items,
  #   ~ case_when(.x == 4 ~ 1,
  #               .x == 3 ~ 2,
  #               .x == 2 ~ 3,
  #               .x == 1 ~ 4,
  #               TRUE ~ NA_real_)
  # ) %>% 
  # Convert scored item vars to integers
  mutate_at(teen_driving_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[teen_driving_items])
  ) %>% 
  # add data var
  mutate(data = "Eng") %>% 
  select(IDNumber, data, everything())

# READ, SCORE SPANISH DATA ------------------------------------------------

teen_driving_data_Sp <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/TEEN/SPM-2 Teen Driving SR Home Combo SP.csv'
  )
)) %>%
  select(IDNumber, DriveForm, teen_driving_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    teen_driving_items,
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
  #   teen_driving_rev_items,
  #   ~ case_when(.x == 4 ~ 1,
  #               .x == 3 ~ 2,
  #               .x == 2 ~ 3,
  #               .x == 1 ~ 4,
  #               TRUE ~ NA_real_)
  # ) %>% 
  # Convert scored item vars to integers
  mutate_at(teen_driving_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[teen_driving_items])
  ) %>% 
  # add data var
  mutate(data = "Sp") %>% 
  select(IDNumber, data, everything())

# RECOMBINE DATA BY FORM --------------------------------------------------

teen_driving_data_self <- 
  bind_rows(
    (teen_driving_data_Eng %>% filter(DriveForm == "TeenSR")),
     (teen_driving_data_Sp %>% filter(DriveForm == "TeenSRsp"))
  ) %>% 
  arrange(IDNumber) %>% 
  filter(!is.na(TOT_raw)) %>% 
  # remove outliers
  filter(TOT_raw < 45)

teen_driving_data_home <- teen_driving_data_Eng %>% 
  bind_rows(
    (teen_driving_data_Eng %>% filter(DriveForm == "TeenHome")),
    (teen_driving_data_Sp %>% filter(DriveForm == "TeenHomesp"))
  ) %>% 
  arrange(IDNumber) %>% 
  filter(!is.na(TOT_raw)) %>% 
  # remove outliers
  filter(TOT_raw < 45)

# write data
write_csv(teen_driving_data_home, 
          here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/teen-driving-home-data.csv'))
write_csv(teen_driving_data_self, 
          here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/teen-driving-self-data.csv'))

# # Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID encountered)
# anyDuplicated(teen_driving_data$IDNumber)
# # Check for any NAs on IDNumber, returns TRUE if NA exist
# any(is.na(teen_driving_data$IDNumber))



# CREATE SUMMARY TABLE SHOWING OPTIMAL CUTOFF SCORE -----------------------

# SELF

# Freq table
teen_driving_self_TOT_freq <- teen_driving_data_self %>% 
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
teen_driving_self_TOT_desc <- teen_driving_data_self %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
teen_driving_self_TOT_desc[nrow(teen_driving_self_TOT_desc) + (nrow(teen_driving_self_TOT_freq) - 1),] <- NA
teen_driving_self_summary <- bind_cols(teen_driving_self_TOT_freq, teen_driving_self_TOT_desc)

# write summary table
write_csv(
  teen_driving_self_summary,
  here('OUTPUT-FILES/TEEN/MISC/teen-driving-self-cutoff-summary.csv'),
  na = ''
)

# HOME

# Freq table
teen_driving_home_TOT_freq <- teen_driving_data_home %>% 
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
teen_driving_home_TOT_desc <- teen_driving_data_home %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
teen_driving_home_TOT_desc[nrow(teen_driving_home_TOT_desc) + (nrow(teen_driving_home_TOT_freq) - 1),] <- NA
teen_driving_home_summary <- bind_cols(teen_driving_home_TOT_freq, teen_driving_home_TOT_desc)

# write summary table
write_csv(
  teen_driving_home_summary,
  here('OUTPUT-FILES/TEEN/MISC/teen-driving-home-cutoff-summary.csv'),
  na = ''
)


