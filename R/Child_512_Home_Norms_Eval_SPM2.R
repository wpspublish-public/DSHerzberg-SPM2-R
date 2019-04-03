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

TOT_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023", 
                              "q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039", 
                              "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050", 
                              "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065", 
                              "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079", 
                              "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092", 
                              "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108", 
                              "q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

SOC_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

SOC_rev_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

VIS_items_Child_512_Home <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039")

HEA_items_Child_512_Home <- c("q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050")

TOU_items_Child_512_Home <- c("q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065")

TS_items_Child_512_Home <- c("q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079")

BOD_items_Child_512_Home <- c("q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092")

BAL_items_Child_512_Home <- c("q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

PLA_items_Child_512_Home <- c("q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

# Read data, recode item vars, calculate TOT.
Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here("DATA/SPM-2 Child ages 512 Home Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    TOT_items_Child_512_Home
  ) %>% rename (AG = AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    TOT_items_Child_512_Home,
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
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(TOT_items_Child_512_Home,
            ~ as.integer(.x)) %>%
  # Recode AgeGroup so it sorts properly.
  mutate(AgeGroup = case_when(
    AG == "5.00 to 6.99 years" ~ "05.00 to 6.99 years",
    AG == "7.00 to 8.99 years" ~ "07.00 to 8.99 years",
    AG == "9.00 to 10.99 years" ~ "09.00 to 10.99 years",
    AG == "11.00 to 12.99 years" ~ "11.00 to 12.99 years",
    TRUE ~ NA_character_)
  ) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(TOT_raw = rowSums(.[TOT_items_Child_512_Home])) %>% #print()
  # Exclude outliers on TOT_raw
  filter(TOT_raw < 200) %>% print()


# Create frequency tables for TOT_raw by AgeGroup
Child_512_Home_TOT_freq_AgeGroup <- Child_512_Home %>% group_by(AgeGroup) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeGroup
Child_512_Home_TOT_desc_AgeGroup <-
  Child_512_Home %>% group_by(AgeGroup) %>% arrange(AgeGroup) %>% summarise(n = n(),
                                                                            median = round(median(TOT_raw), 2),
                                                                            mean = round(mean(TOT_raw), 2),
                                                                            sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:5))

AgeGroup <- Child_512_Home_TOT_desc_AgeGroup %>% pull(AgeGroup)

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Child_512_Home_TOT_desc_AgeGroup, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 5, 1), labels = AgeGroup) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(mean_plot)


# Check for duplicate IDnumber, missing on AgeGroup.

Child_512_Home_dup_IDnumber <- Child_512_Home %>% count(IDNumber) %>% filter(n > 1)
write_csv(Child_512_Home_dup_IDnumber, here("DATA/DATA_CLEANUP_FILES/Child_512_Home_dup_IDnumber.csv"))

Child_512_Home_missing_AgeGroup <- Child_512_Home %>% filter(is.na(AgeGroup)) %>% select(IDNumber)
write_csv(Child_512_Home_missing_AgeGroup, here("DATA/DATA_CLEANUP_FILES/Child_512_Home_missing_AgeGroup.csv"))

# Regroup into two AgeGroups (AG2): 5-8 yr, 9-12 yr, evaluate these as norm strata.

# Create frequency tables for TOT_raw by AG2
Child_512_Home_TOT_freq_AG2 <- Child_512_Home %>% mutate(AG2 = case_when(
  Age <= 8 ~ "5-8 yr",
  Age >= 9 ~ "9-12 yr",
  TRUE ~ NA_character_
)) %>% group_by(AG2) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AG2
Child_512_Home_TOT_desc_AG2 <-
  Child_512_Home %>% mutate(AG2 = case_when(
    Age <= 8 ~ "5-8 yr",
    Age >= 9 ~ "9-12 yr",
    TRUE ~ NA_character_
  )) %>% group_by(AG2) %>% arrange(AG2) %>% summarise(n = n(),
                                                      median = round(median(TOT_raw), 2),
                                                      mean = round(mean(TOT_raw), 2),
                                                      sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:2))


