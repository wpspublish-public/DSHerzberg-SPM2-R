suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# ITEM SETS BY SCHOOL ENVIRONMENT FORM -------------------------------------------

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


# READ DATA ---------------------------------------------------------------

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


# EXAMINE DATA ------------------------------------------------------------

# Freq table
ART_TOT_freq <- ART_data %>% 
  count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4),
    cutoff = case_when(
      cum_per < 90 & lead(cum_per) >= 90 & lag(cum_per) <= 90 ~ 'cutoff',
      TRUE ~ NA_character_
      )
    )

# Descriptives table
ART_TOT_desc <- ART_data %>% 
  summarise(total_n = n(),
            median_TOT_raw = round(median(TOT_raw), 2),
            mean_TOT_raw = round(mean(TOT_raw), 2),
            sd_TOT_raw = round(sd(TOT_raw), 2))

 ART_TOT_desc[nrow(ART_TOT_desc) + (nrow(ART_TOT_freq) - 1),] <- NA
 
 ART_summary <- bind_cols(ART_TOT_freq, ART_TOT_desc)

