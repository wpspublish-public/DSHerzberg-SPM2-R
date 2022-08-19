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

TOT_items_IT_49_Caregiver <- c("q0123", "q0124", "q0126", "q0127", "q0128", "q0129", "q0132", "q0133", "q0134", "q0135", 
                               "q0136", "q0137", "q0139", "q0140", "q0141", "q0142", "q0143", "q0145", "q0147", "q0148", 
                               "q0149", "q0150", "q0152", "q0153", "q0154", "q0155", "q0156", "q0157", "q0159", "q0161", 
                               "q0163", "q0165", "q0167", "q0168", "q0169", "q0170", "q0171", "q0172", "q0173", "q0176", 
                               "q0177", "q0178", "q0179", "q0182", "q0184", "q0187", "q0188", "q0189", "q0190", "q0191", 
                               "q0193", "q0194", "q0195", "q0196", "q0197", "q0198", "q0199", "q0200", "q0201", "q0203", 
                               "q0205", "q0207", "q0208", "q0209", "q0211", "q0213", "q0214", "q0215", "q0216", "q0217", 
                               "q0218", "q0219", "q0220", "q0222", "q0223", "q0225", "q0227", "q0228", "q0229", "q0230")

SOC_items_IT_49_Caregiver <- c("q0123", "q0124", "q0126", "q0127", "q0128", "q0129", "q0132", "q0133", "q0134", "q0135")

SOC_rev_items_IT_49_Caregiver <- c("q0123", "q0124", "q0126", "q0127", "q0128", "q0129")

VIS_items_IT_49_Caregiver <- c("q0136", "q0137", "q0139", "q0140", "q0141", "q0142", "q0143", "q0145", "q0147", "q0148")

HEA_items_IT_49_Caregiver <- c("q0149", "q0150", "q0152", "q0153", "q0154", "q0155", "q0156", "q0157", "q0159", "q0161")

TOU_items_IT_49_Caregiver <- c("q0163", "q0165", "q0167", "q0168", "q0169", "q0170", "q0171", "q0172", "q0173", "q0176")

TS_items_IT_49_Caregiver <- c("q0177", "q0178", "q0179", "q0182", "q0184", "q0187", "q0188", "q0189", "q0190", "q0191")

BOD_items_IT_49_Caregiver <- c("q0193", "q0194", "q0195", "q0196", "q0197", "q0198", "q0199", "q0200", "q0201", "q0203")

BAL_items_IT_49_Caregiver <- c("q0205", "q0207", "q0208", "q0209", "q0211", "q0213", "q0214", "q0215", "q0216", "q0217")

PLA_items_IT_49_Caregiver <- c("q0218", "q0219", "q0220", "q0222", "q0223", "q0225", "q0227", "q0228", "q0229", "q0230")

# Read data, recode item vars, calculate TOT.
IT_49_Caregiver <-
  suppressMessages(as_tibble(read_csv(
    here("DATA/SPM-2 InfantToddler 49 Months.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    AgeGroup,
    Gender,
    Ethnicity,
    Region,
    TOT_items_IT_49_Caregiver
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_IT_49_Caregiver,
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
    SOC_rev_items_IT_49_Caregiver,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_IT_49_Caregiver,
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_IT_49_Caregiver])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(TOT_raw = rowSums(.[TOT_items_IT_49_Caregiver])) %>% print()

# Create frequency tables for TOT_raw by AgeInMonths
IT_49_Caregiver_TOT_freq_AgeInMonths <- IT_49_Caregiver %>% group_by(AgeInMonths) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeInMonths
IT_49_Caregiver_TOT_desc_AgeInMonths <-
  IT_49_Caregiver %>% group_by(AgeInMonths) %>% arrange(AgeInMonths) %>% summarise(n = n(),
                                                         median = round(median(TOT_raw), 2),
                                                         mean = round(mean(TOT_raw), 2),
                                                         sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:6))

AgeInMonths <- IT_49_Caregiver_TOT_desc_AgeInMonths %>% pull(AgeInMonths)

# Plot TOT_raw means, SDs by AgeInMonths
mean_plot <- ggplot(data = IT_49_Caregiver_TOT_desc_AgeInMonths, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 6, 1), labels = AgeInMonths) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeInMonths", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(mean_plot)


# Check for duplicate IDnumber, missing on AgeInMonths.

IT_49_Caregiver_dup_IDnumber <- IT_49_Caregiver %>% count(IDNumber) %>% filter(n > 1)
write_csv(IT_49_Caregiver_dup_IDnumber, here("DATA/DATA_CLEANUP_FILES/IT_49_Caregiver_dup_IDnumber.csv"))

IT_49_Caregiver_missing_AgeInMonths <- IT_49_Caregiver %>% filter(is.na(AgeInMonths)) %>% select(IDNumber)
write_csv(IT_49_Caregiver_missing_AgeInMonths, here("DATA/DATA_CLEANUP_FILES/IT_49_Caregiver_missing_AgeInMonths.csv"))

