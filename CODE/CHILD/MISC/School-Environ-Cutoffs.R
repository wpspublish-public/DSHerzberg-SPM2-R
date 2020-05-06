suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# Code for each School Environments form: specify item sets, read in data,
# search for and remove outliers, generate summary table for JW to evaluate.

# ART -------------------------------------------

ART_items <-
  c(
    'q0011_0001',
    'q0013_0001',
    'q0014_0001',
    'q0016_0001',
    'q0017_0001',
    'q0018_0001',
    'q0019_0001',
    'q0020_0001',
    'q0022_0001',
    'q0024_0001',
    'q0025_0001',
    'q0027_0001',
    'q0028_0001',
    'q0029_0001',
    'q0030_0001'
  )

ART_rev_items <-
  c(
    'q0011_0001',
    'q0013_0001',
    'q0014_0001'
  )

ART_data <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SPM-2 Child ages 512 ART Class Report Questionnaire.csv'
  )
)) %>%
  select(IDNumber, ART_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    ART_items,
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
    ART_rev_items,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(ART_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[ART_items])
  ) %>% 
  # remove outliers
  filter(TOT_raw < 50)

# write data
write_csv(ART_data, here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/ART-data.csv'))

# Freq table
ART_TOT_freq <- ART_data %>% 
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
ART_TOT_desc <- ART_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
ART_TOT_desc[nrow(ART_TOT_desc) + (nrow(ART_TOT_freq) - 1),] <- NA
ART_summary <- bind_cols(ART_TOT_freq, ART_TOT_desc)

# write summary table
write_csv(
  ART_summary,
  here('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/ART-cutoff-summary.csv'),
  na = ''
)


# BUS ---------------------------------------------------------------------

BUS_items <-
  c(
    'q0011_0001', 
    'q0012_0001', 
    'q0013_0001', 
    'q0014_0001', 
    'q0015_0001', 
    'q0016_0001', 
    'q0017_0001', 
    'q0018_0001', 
    'q0019_0001', 
    'q0020_0001', 
    'q0021_0001', 
    'q0022_0001', 
    'q0023_0001', 
    'q0024_0001', 
    'q0025_0001'
  )

BUS_rev_items <-
  c(
    'q0011_0001', 
    'q0012_0001', 
    'q0013_0001', 
    'q0014_0001'
  )

BUS_data <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SPM-2 Child ages 512 School Bus Report Questionnaire.csv'
  )
)) %>%
  select(IDNumber, BUS_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    BUS_items,
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
    BUS_rev_items,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(BUS_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[BUS_items])
  ) #%>% 
  # remove outliers
  # filter(TOT_raw < 50)

# write data
write_csv(BUS_data, here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/BUS-data.csv'))

# Freq table
BUS_TOT_freq <- BUS_data %>% 
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
BUS_TOT_desc <- BUS_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
BUS_TOT_desc[nrow(BUS_TOT_desc) + (nrow(BUS_TOT_freq) - 1),] <- NA
BUS_summary <- bind_cols(BUS_TOT_freq, BUS_TOT_desc)

# write summary table
write_csv(
  BUS_summary,
  here('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/BUS-cutoff-summary.csv'),
  na = ''
)

# CAF ---------------------------------------------------------------------

CAF_items <-
  c(
    'q0011_0001', 
    'q0013_0001', 
    'q0014_0001', 
    'q0016_0001', 
    'q0018_0001', 
    'q0019_0001', 
    'q0022_0001', 
    'q0023_0001', 
    'q0024_0001', 
    'q0025_0001', 
    'q0026_0001', 
    'q0027_0001', 
    'q0028_0001', 
    'q0029_0001', 
    'q0030_0001'
  )

CAF_rev_items <-
  c(
    'q0011_0001', 
    'q0013_0001', 
    'q0014_0001'
  )

CAF_data <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SPM-2 Child ages 512 Cafeteria Report Questionnaire.csv'
  )
)) %>%
  select(IDNumber, CAF_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    CAF_items,
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
    CAF_rev_items,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(CAF_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[CAF_items])
  ) %>% 
# remove outliers
filter(TOT_raw < 45)

# write data
write_csv(CAF_data, here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/CAF-data.csv'))

CAF_TOT_freq <- CAF_data %>% 
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
CAF_TOT_desc <- CAF_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
CAF_TOT_desc[nrow(CAF_TOT_desc) + (nrow(CAF_TOT_freq) - 1),] <- NA
CAF_summary <- bind_cols(CAF_TOT_freq, CAF_TOT_desc)

# write summary table
write_csv(
  CAF_summary,
  here('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/CAF-cutoff-summary.csv'),
  na = ''
)

# MUS ---------------------------------------------------------------------

MUS_items <-
  c(
    'q0011_0001', 
    'q0013_0001', 
    'q0015_0001', 
    'q0016_0001', 
    'q0017_0001', 
    'q0019_0001', 
    'q0020_0001', 
    'q0021_0001', 
    'q0023_0001', 
    'q0024_0001', 
    'q0025_0001', 
    'q0026_0001', 
    'q0027_0001', 
    'q0028_0001', 
    'q0029_0001'
  )

MUS_rev_items <-
  c(
    'q0011_0001', 
    'q0013_0001'
  )

MUS_data <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SPM-2 Child ages 512 Music Class Report Questionnaire.csv'
  )
)) %>%
  select(IDNumber, MUS_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    MUS_items,
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
    MUS_rev_items,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(MUS_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[MUS_items])
  ) #%>% 
  # remove outliers
  # filter(TOT_raw < 40)

# write data
write_csv(MUS_data, here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/MUS-data.csv'))

MUS_TOT_freq <- MUS_data %>% 
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
MUS_TOT_desc <- MUS_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
MUS_TOT_desc[nrow(MUS_TOT_desc) + (nrow(MUS_TOT_freq) - 1),] <- NA
MUS_summary <- bind_cols(MUS_TOT_freq, MUS_TOT_desc)

# write summary table
write_csv(
  MUS_summary,
  here('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/MUS-cutoff-summary.csv'),
  na = ''
)

# PHY ---------------------------------------------------------------------

PHY_items <-
  c(
    'q0011_0001', 
    'q0012_0001', 
    'q0013_0001', 
    'q0015_0001', 
    'q0016_0001', 
    'q0017_0001', 
    'q0020_0001', 
    'q0021_0001', 
    'q0023_0001', 
    'q0025_0001', 
    'q0026_0001', 
    'q0027_0001', 
    'q0028_0001', 
    'q0029_0001', 
    'q0030_0001'
  )

PHY_rev_items <-
  c(
    'q0011_0001', 
    'q0012_0001', 
    'q0013_0001'
  )

PHY_data <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SPM-2 Child ages 512 Physical Education Class Report Questionnaire.csv'
  )
)) %>%
  select(IDNumber, PHY_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    PHY_items,
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
    PHY_rev_items,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(PHY_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[PHY_items])
  ) %>% 
# remove outliers
filter(TOT_raw < 40)

# write data
write_csv(PHY_data, here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/PHY-data.csv'))

PHY_TOT_freq <- PHY_data %>% 
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
PHY_TOT_desc <- PHY_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
PHY_TOT_desc[nrow(PHY_TOT_desc) + (nrow(PHY_TOT_freq) - 1),] <- NA
PHY_summary <- bind_cols(PHY_TOT_freq, PHY_TOT_desc)

# write summary table
write_csv(
  PHY_summary,
  here('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/PHY-cutoff-summary.csv'),
  na = ''
)

# REC ---------------------------------------------------------------------

REC_items <-
  c(
    'q0012_0001', 
    'q0013_0001', 
    'q0014_0001', 
    'q0015_0001', 
    'q0016_0001', 
    'q0017_0001', 
    'q0018_0001', 
    'q0019_0001', 
    'q0020_0001', 
    'q0021_0001', 
    'q0022_0001', 
    'q0023_0001', 
    'q0025_0001', 
    'q0029_0001', 
    'q0031_0001'
  )

REC_rev_items <-
  c(
    'q0012_0001', 
    'q0013_0001'
  )

REC_data <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SPM-2 Child ages 512 RecessPlayground Report Questionnaire.csv'
  )
)) %>%
  select(IDNumber, REC_items) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    REC_items,
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
    REC_rev_items,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(REC_items,
            ~ as.integer(.x)) %>% 
  # calculate total score
  mutate(
    TOT_raw = rowSums(.[REC_items])
  ) #%>% 
  # remove outliers
  # filter(TOT_raw < 40)

# write data
write_csv(REC_data, here('INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/REC-data.csv'))

REC_TOT_freq <- REC_data %>% 
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
REC_TOT_desc <- REC_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

# Add enough rows to descriptive table so that it can be combined with freq
# table into single summary table.
REC_TOT_desc[nrow(REC_TOT_desc) + (nrow(REC_TOT_freq) - 1),] <- NA
REC_summary <- bind_cols(REC_TOT_freq, REC_TOT_desc)

# write summary table
write_csv(
  REC_summary,
  here('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/REC-cutoff-summary.csv'),
  na = ''
)



