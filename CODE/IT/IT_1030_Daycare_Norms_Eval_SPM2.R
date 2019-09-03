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

TOT_items_IT_1030_Daycare <- c("q0024", "q0025", "q0026", "q0027", "q0028", "q0029", "q0032", "q0034", "q0035", "q0036", 
                       "q0037", "q0038", "q0039", "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", 
                       "q0052", "q0054", "q0055", "q0056", "q0061", "q0062", "q0063", "q0066", "q0067", "q0068", 
                       "q0069", "q0071", "q0073", "q0075", "q0076", "q0077", "q0079", "q0080", "q0081", "q0082", 
                       "q0086", "q0087", "q0088", "q0089", "q0091", "q0093", "q0095", "q0096", "q0097", "q0098", 
                       "q0100", "q0101", "q0102", "q0103", "q0104", "q0105", "q0106", "q0107", "q0110", "q0112")

SOC_items_IT_1030_Daycare <- c("q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0022")

SOC_rev_items_IT_1030_Daycare <- c("q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0022")

VIS_items_IT_1030_Daycare <- c("q0024", "q0025", "q0026", "q0027", "q0028", "q0029", "q0032", "q0034", "q0035", "q0036")

HEA_items_IT_1030_Daycare <- c("q0037", "q0038", "q0039", "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046")

TOU_items_IT_1030_Daycare <- c("q0052", "q0054", "q0055", "q0056", "q0061", "q0062", "q0063", "q0066", "q0067", "q0068")

TS_items_IT_1030_Daycare <- c("q0069", "q0071", "q0073", "q0075", "q0076", "q0077", "q0079", "q0080", "q0081", "q0082")

BOD_items_IT_1030_Daycare <- c("q0086", "q0087", "q0088", "q0089", "q0091", "q0093", "q0095", "q0096", "q0097", "q0098")

BAL_items_IT_1030_Daycare <- c("q0100", "q0101", "q0102", "q0103", "q0104", "q0105", "q0106", "q0107", "q0110", "q0112")

PLA_items_IT_1030_Daycare <- c("q0114", "q0117", "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125")

# Read data, recode item vars, calculate TOT.
IT_1030_Daycare <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SPM-2 InfantToddler 1030 Months Daycare Provider.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    AgeGroup,
    Gender,
    Ethnicity,
    Region,
    SOC_items_IT_1030_Daycare,
    TOT_items_IT_1030_Daycare,
    PLA_items_IT_1030_Daycare
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_IT_1030_Daycare,
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
    SOC_rev_items_IT_1030_Daycare,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_IT_1030_Daycare,
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_IT_1030_Daycare])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(TOT_raw = rowSums(.[TOT_items_IT_1030_Daycare])) %>% 
  # Compute AgeStrat variable for grouping over AgeInMonths.
  mutate(AgeStrat = case_when(
    AgeInMonths >= 10 & AgeInMonths <= 15 ~ "10-15_mo",
    AgeInMonths >= 16 & AgeInMonths <= 20 ~ "16-20_mo",
    AgeInMonths >= 21 & AgeInMonths <= 25 ~ "21-25_mo",
    AgeInMonths >= 26 & AgeInMonths <= 30 ~ "26-30_mo",
    TRUE ~ NA_character_
  )) %>% print()

# Create frequency tables for TOT_raw by AgeInMonths
IT_1030_Daycare_TOT_freq_AgeInMonths <- IT_1030_Daycare %>% group_by(AgeStrat) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeInMonths
IT_1030_Daycare_TOT_desc_AgeInMonths <-
  IT_1030_Daycare %>% group_by(AgeStrat) %>% arrange(AgeInMonths) %>% summarise(n = n(),
                                                         median = round(median(TOT_raw), 2),
                                                         mean = round(mean(TOT_raw), 2),
                                                         sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:4))

AgeStrat <- IT_1030_Daycare_TOT_desc_AgeInMonths %>% pull(AgeStrat)

# Plot TOT_raw means, SDs by AgeInMonths
mean_plot <- ggplot(data = IT_1030_Daycare_TOT_desc_AgeInMonths, aes(group, mean)) +
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

IT_1030_Daycare_dup_IDnumber <- IT_1030_Daycare %>% count(IDNumber) %>% filter(n > 1)
write_csv(IT_1030_Daycare_dup_IDnumber, here("INPUT-FILES/DATA_CLEANUP_FILES/IT_1030_Daycare_dup_IDnumber.csv"))

IT_1030_Daycare_missing_AgeInMonths <- IT_1030_Daycare %>% filter(is.na(AgeInMonths)) %>% select(IDNumber)
write_csv(IT_1030_Daycare_missing_AgeInMonths, here("INPUT-FILES/DATA_CLEANUP_FILES/IT_1030_Daycare_missing_AgeInMonths.csv"))

# Regroup into two AgeGroups (AG2): 10-20 mo, 21-30 mo, evaluate these as norm strata.

# Create frequency tables for TOT_raw by AG2
IT_1030_Daycare_TOT_freq_AG2 <- IT_1030_Daycare %>% mutate(AG2 = case_when(
  AgeInMonths <= 20 ~ "10-20 mo",
  AgeInMonths >= 21 ~ "21-30 mo",
  TRUE ~ NA_character_
)) %>% group_by(AG2) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AG2
IT_1030_Daycare_TOT_desc_AG2 <-
  IT_1030_Daycare %>% mutate(AG2 = case_when(
    AgeInMonths <= 20 ~ "10-20 mo",
    AgeInMonths >= 21 ~ "21-30 mo",
    TRUE ~ NA_character_
  )) %>% group_by(AG2) %>% arrange(AG2) %>% summarise(n = n(),
                                                      median = round(median(TOT_raw), 2),
                                                      mean = round(mean(TOT_raw), 2),
                                                      sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:2))


