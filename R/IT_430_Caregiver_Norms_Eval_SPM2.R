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

TOT_items_IT_430_Caregiver <- c("QT14", "QT15", 
                                "QT17", "QT18", "QT19", "QT20", "QT21", "QT23", "QT25", "QT26", "QT27", "QT28", "QT30", 
                                "QT31", "QT32", "QT33", "QT34", "QT35", "QT37", "QT39", "QT41", "QT43", "QT45", "QT46", 
                                "QT47", "QT48", "QT49", "QT50", "QT51", "QT54", "QT55", "QT56", "QT57", "QT60", "QT62", 
                                "QT65", "QT66", "QT67", "QT68", "QT69", "QT71", "QT72", "QT73", "QT74", "QT75", "QT76", 
                                "QT77", "QT78", "QT79", "QT81", "QT83", "QT85", "QT86", "QT87", "QT89", "QT91", "QT92", 
                                "QT93", "QT94", "QT95")

SOC_items_IT_430_Caregiver <- c("QT1", "QT2", "QT4", "QT5", "QT6", "QT7", "QT10", "QT11", "QT12", "QT13")

SOC_rev_items_IT_430_Caregiver <- c("QT1", "QT2", "QT4", "QT5", "QT6", "QT7")

VIS_items_IT_430_Caregiver <- c("QT14", "QT15", "QT17", "QT18", "QT19", "QT20", "QT21", "QT23", "QT25", "QT26")

HEA_items_IT_430_Caregiver <- c("QT27", "QT28", "QT30", "QT31", "QT32", "QT33", "QT34", "QT35", "QT37", "QT39")

TOU_items_IT_430_Caregiver <- c("QT41", "QT43", "QT45", "QT46", "QT47", "QT48", "QT49", "QT50", "QT51", "QT54")

TS_items_IT_430_Caregiver <- c("QT55", "QT56", "QT57", "QT60", "QT62", "QT65", "QT66", "QT67", "QT68", "QT69")

BOD_items_IT_430_Caregiver <- c("QT71", "QT72", "QT73", "QT74", "QT75", "QT76", "QT77", "QT78", "QT79", "QT81")

BAL_items_IT_430_Caregiver <- c("QT83", "QT85", "QT86", "QT87", "QT89", "QT91", "QT92", "QT93", "QT94", "QT95")

PLA_items_IT_430_Caregiver <- c("QT96", "QT97", "QT98", "QT100", "QT101", "QT103", "QT105", "QT106", "QT107", "QT108")

# Read data, recode item vars, calculate TOT.
IT_430_Caregiver <-
  suppressMessages(as_tibble(read_csv(
    here("DATA/SPM2_InfantToddler_Caregiver_Combo.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    AgeGroup,
    Gender,
    Ethnicity,
    Region,
    TOT_items_IT_430_Caregiver
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_IT_430_Caregiver,
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
    SOC_rev_items_IT_430_Caregiver,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_IT_430_Caregiver,
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_IT_430_Caregiver])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(TOT_raw = rowSums(.[TOT_items_IT_430_Caregiver])) %>% 
  # Compute AgeStrat variable for grouping over AgeInMonths.
  mutate(AgeStrat = case_when(
    AgeInMonths >= 4 & AgeInMonths <= 9 ~ "04-09_mo",
    AgeInMonths >= 10 & AgeInMonths <= 15 ~ "10-15_mo",
    AgeInMonths >= 16 & AgeInMonths <= 20 ~ "16-20_mo",
    AgeInMonths >= 21 & AgeInMonths <= 25 ~ "21-25_mo",
    AgeInMonths >= 26 & AgeInMonths <= 30 ~ "26-30_mo",
    TRUE ~ NA_character_
  )) %>% print()

# Create frequency tables for TOT_raw by AgeInMonths
IT_430_Caregiver_TOT_freq_AgeInMonths <- IT_430_Caregiver %>% group_by(AgeStrat) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeInMonths
IT_430_Caregiver_TOT_desc_AgeInMonths <-
  IT_430_Caregiver %>% group_by(AgeStrat) %>% arrange(AgeInMonths) %>% summarise(n = n(),
                                                         median = round(median(TOT_raw), 2),
                                                         mean = round(mean(TOT_raw), 2),
                                                         sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:5))

AgeStrat <- IT_430_Caregiver_TOT_desc_AgeInMonths %>% pull(AgeStrat)

# Plot TOT_raw means, SDs by AgeInMonths
mean_plot <- ggplot(data = IT_430_Caregiver_TOT_desc_AgeInMonths, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 5, 1), labels = AgeStrat) +
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

IT_430_Caregiver_dup_IDnumber <- IT_430_Caregiver %>% count(IDNumber) %>% filter(n > 1)
write_csv(IT_430_Caregiver_dup_IDnumber, here("DATA/DATA_CLEANUP_FILES/IT_430_Caregiver_dup_IDnumber.csv"))

IT_430_Caregiver_missing_AgeInMonths <- IT_430_Caregiver %>% filter(is.na(AgeInMonths)) %>% select(IDNumber)
write_csv(IT_430_Caregiver_missing_AgeInMonths, here("DATA/DATA_CLEANUP_FILES/IT_430_Caregiver_missing_AgeInMonths.csv"))

