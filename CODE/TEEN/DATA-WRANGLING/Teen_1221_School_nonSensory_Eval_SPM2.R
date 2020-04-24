suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

# Scale vectors with item names

SOC_items_Teen_1221_School <- c("q0012", "q0013", "q0014", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023", "q0024")

SOC_rev_items_Teen_1221_School <- c("q0012", "q0013", "q0014", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

PLA_items_Teen_1221_School <- c("q0103", "q0104", "q0105", "q0106", "q0107", "q0108", "q0112", "q0113", "q0115", "q0116")

# Read data, recode item vars, calculate SOC, PLA raw scores.
Teen_1221_School <-
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/TEEN/SPM-2 Teen ages 1221 School Report Questionnaire.csv')
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    Ethnicity,
    Region,
    SOC_items_Teen_1221_School,
    PLA_items_Teen_1221_School
  ) %>%
  # drop out of age range for form
  drop_na(AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    vars(SOC_items_Teen_1221_School,
         PLA_items_Teen_1221_School),
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
    SOC_rev_items_Teen_1221_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(vars(SOC_items_Teen_1221_School,
                 PLA_items_Teen_1221_School),
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_Teen_1221_School])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(SOC_raw = rowSums(.[SOC_items_Teen_1221_School]),
         PLA_raw = rowSums(.[PLA_items_Teen_1221_School])
         )

# Create frequency tables for SOC_raw, PLA_raw by AgeGroup
Teen_1221_School_SOC_freq_AgeGroup <- Teen_1221_School %>% group_by(AgeGroup) %>% count(SOC_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(SOC_raw), lag_cum_per = lag(cum_per))
Teen_1221_School_PLA_freq_AgeGroup <- Teen_1221_School %>% group_by(AgeGroup) %>% count(PLA_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(PLA_raw), lag_cum_per = lag(cum_per))

# Plot histograms for SOC_raw, PLA_raw frequencies by AgeGroup
School_hist_SOC_age <- ggplot(data = Teen_1221_School, aes(SOC_raw)) +
  geom_histogram(
    binwidth = .2,
    col = "red",
    fill = "blue",
    alpha = .2
  ) +
  labs(title = "Frequency Distribution") + 
  theme(panel.grid.minor=element_blank()) +
  facet_wrap(~AgeGroup)
print(School_hist_SOC_age)
ggsave(here('OUTPUT-FILES/TEEN/PLOTS/School_hist_SOC_age.png'), School_hist_SOC_age)
School_hist_PLA_age <- ggplot(data = Teen_1221_School, aes(PLA_raw)) +
  geom_histogram(
    binwidth = .2,
    col = "red",
    fill = "blue",
    alpha = .2
  ) +
  labs(title = "Frequency Distribution") + 
  theme(panel.grid.minor=element_blank()) +
  facet_wrap(~AgeGroup)
print(School_hist_PLA_age)
ggsave(here('OUTPUT-FILES/TEEN/PLOTS/School_hist_PLA_age.png'), School_hist_PLA_age)

# Compute descriptive statistics, effect sizes for SOC_raw, PLA_raw by AgeGroup
Teen_1221_School_SOC_desc_AgeGroup <-
  Teen_1221_School %>% group_by(AgeGroup) %>% arrange(AgeGroup) %>% summarise(n = n(),
                                                                            median = round(median(SOC_raw), 2),
                                                                            mean = round(mean(SOC_raw), 2),
                                                                            sd = round(sd(SOC_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:4))
write_csv(Teen_1221_School_SOC_desc_AgeGroup, here('OUTPUT-FILES/TEEN/DESCRIPTIVES/School_SOC_desc_AgeGroup.csv'))
Teen_1221_School_PLA_desc_AgeGroup <-
  Teen_1221_School %>% group_by(AgeGroup) %>% arrange(AgeGroup) %>% summarise(n = n(),
                                                                            median = round(median(PLA_raw), 2),
                                                                            mean = round(mean(PLA_raw), 2),
                                                                            sd = round(sd(PLA_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:4))
write_csv(Teen_1221_School_PLA_desc_AgeGroup, here('OUTPUT-FILES/TEEN/DESCRIPTIVES/School_PLA_desc_AgeGroup.csv'))

AgeGroup <- Teen_1221_School_PLA_desc_AgeGroup %>% pull(AgeGroup)

# Plot TOT_raw means, SDs by AgeGroup
School_mean_plot_SOC <- ggplot(data = Teen_1221_School_SOC_desc_AgeGroup, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 4, 1), labels = AgeGroup) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "SOC") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(School_mean_plot_SOC)
ggsave(here('OUTPUT-FILES/TEEN/PLOTS/School_mean_plot_SOC.png'), School_mean_plot_SOC)
School_mean_plot_PLA <- ggplot(data = Teen_1221_School_PLA_desc_AgeGroup, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 4, 1), labels = AgeGroup) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "PLA") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(School_mean_plot_PLA)
ggsave(here('OUTPUT-FILES/TEEN/PLOTS/School_mean_plot_PLA.png'), School_mean_plot_PLA)

# Check for duplicate IDnumber, missing on AgeGroup.

# Teen_1221_School_dup_IDnumber <- Teen_1221_School %>% count(IDNumber) %>% filter(n > 1)
# write_csv(Teen_1221_School_dup_IDnumber, here("DATA/DATA_CLEANUP_FILES/Teen_1221_School_dup_IDnumber.csv"))
# 
# Teen_1221_School_missing_AgeGroup <- Teen_1221_School %>% filter(is.na(AgeGroup)) %>% select(IDNumber)
# write_csv(Teen_1221_School_missing_AgeGroup, here("DATA/DATA_CLEANUP_FILES/Teen_1221_School_missing_AgeGroup.csv"))

