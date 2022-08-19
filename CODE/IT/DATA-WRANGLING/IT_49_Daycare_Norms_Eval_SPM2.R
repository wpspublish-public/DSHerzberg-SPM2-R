# Examine SPM-2 data to determine need for age-stratified norms.

suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# Scale vectors with item names

All_items_IT_49_Daycare <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023",
                             "q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                             "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                             "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                             "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                             "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                             "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105",
                             "q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")

TOT_items_IT_49_Daycare <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                             "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                             "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                             "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                             "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                             "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105")

SOC_items_IT_49_Daycare <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023")

SOC_rev_items_IT_49_Daycare <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023")

VIS_items_IT_49_Daycare <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038")

HEA_items_IT_49_Daycare <- c("q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050")

TOU_items_IT_49_Daycare <- c("q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066")

TS_items_IT_49_Daycare <- c("q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078")

BOD_items_IT_49_Daycare <- c("q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091")

BAL_items_IT_49_Daycare <- c("q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105")

PLA_items_IT_49_Daycare <- c("q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")

# Read data, recode item vars, calculate TOT.
IT_49_Daycare <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SPM-2 InfantToddler 49 Months Daycare Provider.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    AgeGroup,
    Gender,
    Ethnicity,
    Region,
    SOC_items_IT_49_Daycare,
    TOT_items_IT_49_Daycare,
    PLA_items_IT_49_Daycare
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_IT_49_Daycare,
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
    SOC_rev_items_IT_49_Daycare,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_IT_49_Daycare,
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_IT_49])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(TOT_raw = rowSums(.[TOT_items_IT_49_Daycare])) %>% print()

# Create frequency tables for TOT_raw by AgeInMonths
IT_49_Daycare_TOT_freq_AgeInMonths <- IT_49_Daycare %>% group_by(AgeInMonths) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeInMonths
IT_49_Daycare_TOT_desc_AgeInMonths <-
  IT_49_Daycare %>% group_by(AgeInMonths) %>% arrange(AgeInMonths) %>% summarise(n = n(),
                                                                         median = round(median(TOT_raw), 2),
                                                                         mean = round(mean(TOT_raw), 2),
                                                                         sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:6))

AgeInMonths <- IT_49_Daycare_TOT_desc_AgeInMonths %>% pull(AgeInMonths)

# Plot TOT_raw means, SDs by AgeInMonths
mean_plot <- ggplot(data = IT_49_Daycare_TOT_desc_AgeInMonths, aes(group, mean)) +
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

IT_49_Daycare_dup_IDnumber <- IT_49_Daycare %>% count(IDNumber) %>% filter(n > 1)
write_csv(IT_49_Daycare_dup_IDnumber, here("INPUT-FILES/DATA_CLEANUP_FILES/IT_49_Daycare_dup_IDnumber.csv"))

IT_49_Daycare_missing_AgeInMonths <- IT_49_Daycare %>% filter(is.na(AgeInMonths)) %>% select(IDNumber)
write_csv(IT_49_Daycare_missing_AgeInMonths, here("INPUT-FILES/DATA_CLEANUP_FILES/IT_49_Daycare_missing_AgeInMonths.csv"))

# Regroup into two AgeGroups (AG2): 4-6 mo, 7-9 mo, evaluate these as norm strata.

# Create frequency tables for TOT_raw by AG2
IT_49_Daycare_TOT_freq_AG2 <- IT_49_Daycare %>% mutate(AG2 = case_when(
  AgeInMonths <= 6 ~ "4-6 mo",
  AgeInMonths >= 7 ~ "7-9 mo",
  TRUE ~ NA_character_
)) %>% group_by(AG2) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AG2
IT_49_Daycare_TOT_desc_AG2 <-
  IT_49_Daycare %>% mutate(AG2 = case_when(
    AgeInMonths <= 6 ~ "4-6 mo",
    AgeInMonths >= 7 ~ "7-9 mo",
    TRUE ~ NA_character_
  )) %>% group_by(AG2) %>% arrange(AG2) %>% summarise(n = n(),
                                                      median = round(median(TOT_raw), 2),
                                                      mean = round(mean(TOT_raw), 2),
                                                      sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:2))


