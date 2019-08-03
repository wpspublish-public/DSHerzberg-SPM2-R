# Examine SPM-2 data to determine need for age-stratified norms.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
library(moderndive) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

# Scale vectors with item names

TOT_items_IT_1030_Caregiver <- c("q0129", "q0130", "q0132", "q0133", "q0134", "q0135", "q0138", "q0139", "q0140", "q0141", 
                                 "q0142", "q0143", "q0145", "q0146", "q0147", "q0148", "q0149", "q0151", "q0153", "q0154", 
                                 "q0155", "q0156", "q0158", "q0159", "q0160", "q0161", "q0162", "q0163", "q0165", "q0167", 
                                 "q0169", "q0171", "q0173", "q0174", "q0175", "q0176", "q0177", "q0178", "q0179", "q0182", 
                                 "q0183", "q0184", "q0185", "q0188", "q0190", "q0193", "q0194", "q0195", "q0196", "q0197", 
                                 "q0199", "q0200", "q0201", "q0202", "q0203", "q0204", "q0205", "q0206", "q0207", "q0209", 
                                 "q0211", "q0213", "q0214", "q0215", "q0217", "q0219", "q0220", "q0221", "q0222", "q0223", 
                                 "q0224", "q0225", "q0226", "q0228", "q0229", "q0231", "q0233", "q0234", "q0235", "q0236")

SOC_items_IT_1030_Caregiver <- c("q0129", "q0130", "q0132", "q0133", "q0134", "q0135", "q0138", "q0139", "q0140", "q0141")

SOC_rev_items_IT_1030_Caregiver <- c("q0129", "q0130", "q0132", "q0133", "q0134", "q0135")

VIS_items_IT_1030_Caregiver <- c("q0142", "q0143", "q0145", "q0146", "q0147", "q0148", "q0149", "q0151", "q0153", "q0154")

HEA_items_IT_1030_Caregiver <- c("q0155", "q0156", "q0158", "q0159", "q0160", "q0161", "q0162", "q0163", "q0165", "q0167")

TOU_items_IT_1030_Caregiver <- c("q0169", "q0171", "q0173", "q0174", "q0175", "q0176", "q0177", "q0178", "q0179", "q0182")

TS_items_IT_1030_Caregiver <- c("q0183", "q0184", "q0185", "q0188", "q0190", "q0193", "q0194", "q0195", "q0196", "q0197")

BOD_items_IT_1030_Caregiver <- c("q0199", "q0200", "q0201", "q0202", "q0203", "q0204", "q0205", "q0206", "q0207", "q0209")

BAL_items_IT_1030_Caregiver <- c("q0211", "q0213", "q0214", "q0215", "q0217", "q0219", "q0220", "q0221", "q0222", "q0223")

PLA_items_IT_1030_Caregiver <- c("q0224", "q0225", "q0226", "q0228", "q0229", "q0231", "q0233", "q0234", "q0235", "q0236")

# Read data, recode item vars, calculate TOT.
IT_1030_Caregiver <-
  suppressMessages(as_tibble(read_csv(
    here("DATA/SPM-2 InfantToddler 1030 Months.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    AgeGroup,
    Gender,
    Ethnicity,
    Region,
    TOT_items_IT_1030_Caregiver
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_IT_1030_Caregiver,
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
    SOC_rev_items_IT_1030_Caregiver,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_IT_1030_Caregiver,
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_IT_1030_Caregiver])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(TOT_raw = rowSums(.[TOT_items_IT_1030_Caregiver])) %>% 
  # Compute AgeStrat variable for grouping over AgeInMonths.
  mutate(AgeStrat = case_when(
    AgeInMonths >= 10 & AgeInMonths <= 15 ~ "10-15_mo",
    AgeInMonths >= 16 & AgeInMonths <= 20 ~ "16-20_mo",
    AgeInMonths >= 21 & AgeInMonths <= 25 ~ "21-25_mo",
    AgeInMonths >= 26 & AgeInMonths <= 30 ~ "26-30_mo",
    TRUE ~ NA_character_
  )) %>% print()

# Create frequency tables for TOT_raw by AgeInMonths
IT_1030_Caregiver_TOT_freq_AgeInMonths <- IT_1030_Caregiver %>% group_by(AgeStrat) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeInMonths
IT_1030_Caregiver_TOT_desc_AgeInMonths <-
  IT_1030_Caregiver %>% group_by(AgeStrat) %>% arrange(AgeInMonths) %>% summarise(n = n(),
                                                         median = round(median(TOT_raw), 2),
                                                         mean = round(mean(TOT_raw), 2),
                                                         sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:4))

AgeStrat <- IT_1030_Caregiver_TOT_desc_AgeInMonths %>% pull(AgeStrat)

# Plot TOT_raw means, SDs by AgeInMonths
mean_plot <- ggplot(data = IT_1030_Caregiver_TOT_desc_AgeInMonths, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 4, 1), labels = AgeStrat) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeStrat", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(mean_plot)


# Check for duplicate IDnumber, missing on AgeInMonths.

IT_1030_Caregiver_dup_IDnumber <- IT_1030_Caregiver %>% count(IDNumber) %>% filter(n > 1)
write_csv(IT_1030_Caregiver_dup_IDnumber, here("DATA/DATA_CLEANUP_FILES/IT_1030_Caregiver_dup_IDnumber.csv"))

IT_1030_Caregiver_missing_AgeInMonths <- IT_1030_Caregiver %>% filter(is.na(AgeInMonths)) %>% select(IDNumber)
write_csv(IT_1030_Caregiver_missing_AgeInMonths, here("DATA/DATA_CLEANUP_FILES/IT_1030_Caregiver_missing_AgeInMonths.csv"))

