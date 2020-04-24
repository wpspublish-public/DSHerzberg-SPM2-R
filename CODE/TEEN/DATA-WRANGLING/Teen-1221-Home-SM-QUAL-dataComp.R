suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Teen_1221_Home <- c("q0015", "q0016", "q0017", "q0018", "q0019", "q0021", "q0022", "q0024", "q0025", "q0026", 
                              "q0028", "q0029", "q0030", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", "q0040", 
                              "q0041", "q0042", "q0043", "q0044", "q0047", "q0048", "q0049", "q0050", "q0052", "q0053", 
                              "q0055", "q0056", "q0057", "q0058", "q0059", "q0061", "q0062", "q0063", "q0064", "q0065", 
                              "q0067", "q0068", "q0069", "q0071", "q0072", "q0074", "q0076", "q0077", "q0078", "q0079", 
                              "q0080", "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0091", "q0092", 
                              "q0094", "q0095", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102", "q0103", "q0104",
                              "q0106", "q0107", "q0108", "q0109", "q0111", "q0112", "q0113", "q0114", "q0115", "q0117")

TOT_items_Teen_1221_Home <- c("q0028", "q0029", "q0030", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", "q0040", 
                              "q0041", "q0042", "q0043", "q0044", "q0047", "q0048", "q0049", "q0050", "q0052", "q0053", 
                              "q0055", "q0056", "q0057", "q0058", "q0059", "q0061", "q0062", "q0063", "q0064", "q0065", 
                              "q0067", "q0068", "q0069", "q0071", "q0072", "q0074", "q0076", "q0077", "q0078", "q0079", 
                              "q0080", "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0091", "q0092", 
                              "q0094", "q0095", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102", "q0103", "q0104")

SOC_items_Teen_1221_Home <- c("q0015", "q0016", "q0017", "q0018", "q0019", "q0021", "q0022", "q0024", "q0025", "q0026")

SOC_rev_items_Teen_1221_Home <- c("q0015", "q0016", "q0017", "q0021", "q0022")

VIS_items_Teen_1221_Home <- c("q0028", "q0029", "q0030", "q0031", "q0033", "q0034", "q0035", "q0036", "q0037", "q0040")

HEA_items_Teen_1221_Home <- c("q0041", "q0042", "q0043", "q0044", "q0047", "q0048", "q0049", "q0050", "q0052", "q0053")

TOU_items_Teen_1221_Home <- c("q0055", "q0056", "q0057", "q0058", "q0059", "q0061", "q0062", "q0063", "q0064", "q0065")

TS_items_Teen_1221_Home <- c("q0067", "q0068", "q0069", "q0071", "q0072", "q0074", "q0076", "q0077", "q0078", "q0079")

BOD_items_Teen_1221_Home <- c("q0080", "q0082", "q0083", "q0084", "q0085", "q0086", "q0087", "q0089", "q0091", "q0092")

BAL_items_Teen_1221_Home <- c("q0094", "q0095", "q0096", "q0097", "q0098", "q0099", "q0100", "q0102", "q0103", "q0104")

PLA_items_Teen_1221_Home <- c("q0106", "q0107", "q0108", "q0109", "q0111", "q0112", "q0113", "q0114", "q0115", "q0117")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Teen_1221_Home_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/SPM-2 SM Qual COMBO Teen ages 1221 Home Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Teen_1221_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Teen_1221_Home,
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
    SOC_rev_items_Teen_1221_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Teen_1221_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years")
  ) %>% 
  # select(-AgeGroup) %>% 
# Compute raw scores. Note use of `rowSums(.[TOT_items_Teen_1221_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_Home]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_Home]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_Home]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_Home]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_Home]),
    TS_raw = rowSums(.[TS_items_Teen_1221_Home]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_Home]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_Home]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_Home])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = case_when(
    IDNumber >= 700000 ~ 'Qual',
    TRUE ~ 'SM'
  )) %>% 
  select(IDNumber, data, everything()) %>% 
# Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) %>% 
  filter(!(TOT_raw >= 155 & age_range == '18 to 21 years' & data == 'SM')) %>%
  write_csv(here('INPUT-FILES/TEEN/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Home-combo-norms-input.csv'),
            na = "")

# clean up environment
rm(list = ls(pattern='.*items_Teen_1221_Home'))

Teen_1221_Home <- Teen_1221_Home_items %>% 
  select(
    -(q0015:q0117)
  )   

# EXAMINE DATA TO MAKE AGESTRAT DECISIONS ---------------------------------

# ### THE NEXT SECTION OF CODE FACILITATES EXAMINATION OF TOT_raw to make
# decisions about whether norms need to be stratified by age. Once the decision
# about age-stratification has been made and implemented, the 'examination' code
# can be commented off.


# Create frequency tables for TOT_raw by AgeGroup
Teen_1221_Home_TOT_freq_AgeGroup <- Teen_1221_Home %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(Teen_1221_Home_TOT_freq_AgeGroup, here('OUTPUT-FILES/TEEN/FREQUENCIES/Teen-1221-Home-TOT-freq-AgeGroup.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by AgeGroup
Teen_1221_Home_TOT_desc_AgeGroup_data <-
  Teen_1221_Home %>% group_by(data, age_range) %>% arrange(data, age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:4)
  )

Teen_1221_Home_TOT_desc_AgeGroup <-
  Teen_1221_Home %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                                          median = round(median(TOT_raw), 2),
                                                                                          mean = round(mean(TOT_raw), 2),
                                                                                          sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:4)
  )

write_csv(Teen_1221_Home_TOT_desc_AgeGroup_data, here('OUTPUT-FILES/TEEN/DESCRIPTIVES/Teen-1221-Home-TOT-desc-dataSource.csv'))
write_csv(Teen_1221_Home_TOT_desc_AgeGroup, here('OUTPUT-FILES/TEEN/DESCRIPTIVES/Teen-1221-Home-TOT-desc.csv'))


# Generate single table comparing descriptives from SM and Qual sources.
Teen_1221_Home_TOT_desc_SM <- Teen_1221_Home_TOT_desc_AgeGroup_data %>% 
  filter(data == 'SM') %>% 
  setNames(., str_c(names(.), 'SM', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))
Teen_1221_Home_TOT_desc_Qual <- Teen_1221_Home_TOT_desc_AgeGroup_data %>% 
  filter(data == 'Qual') %>% 
  setNames(., str_c(names(.), 'Qual', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))

Teen_1221_Home_TOT_desc_comp <- Teen_1221_Home_TOT_desc_SM %>% 
  bind_cols(Teen_1221_Home_TOT_desc_Qual) %>% 
  select(-age_range_Qual) %>% 
  rename(age_range = age_range_SM) %>% 
  mutate(
    diff = round(mean_SM - mean_Qual, 2),
    ES_diff = round((mean_SM - mean_Qual) / ((sd_SM + sd_Qual) / 2), 2)
  )

write_csv(Teen_1221_Home_TOT_desc_comp, here('OUTPUT-FILES/TEEN/DESCRIPTIVES/Teen-1221-Home-TOT-desc-comp.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Teen_1221_Home_TOT_desc_AgeGroup_data, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 4, 1), labels = unique(Teen_1221_Home_TOT_desc_AgeGroup_data$age_range)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


