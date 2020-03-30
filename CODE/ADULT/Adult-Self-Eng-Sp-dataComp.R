suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Adult_Self <- c("q0013", "q0014", "q0016", "q0018", "q0020", "q0021", "q0023", 
                          "q0024", "q0026", "q0027", "q0029", "q0030", "q0031", "q0032", 
                          "q0033", "q0034", "q0036", "q0037", "q0040", "q0041", "q0043", 
                          "q0044", "q0046", "q0047", "q0049", "q0050", "q0051", "q0053", 
                          "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", 
                          "q0063", "q0064", "q0067", "q0068", "q0069", "q0073", "q0075", 
                          "q0076", "q0077", "q0081", "q0082", "q0083", "q0084", "q0086", 
                          "q0087", "q0089", "q0090", "q0091", "q0092", "q0093", "q0096", 
                          "q0098", "q0099", "q0100", "q0101", "q0103", "q0104", "q0105", 
                          "q0106", "q0108", "q0111", "q0113", "q0114", "q0115", "q0117", 
                          "q0118", "q0119", "q0122", "q0126", "q0127", "q0128", "q0129", 
                          "q0130", "q0131", "q0132")

TOT_items_Adult_Self <- c("q0029", "q0030", "q0031", "q0032", 
                          "q0033", "q0034", "q0036", "q0037", "q0040", "q0041", "q0043", 
                          "q0044", "q0046", "q0047", "q0049", "q0050", "q0051", "q0053", 
                          "q0054", "q0057", "q0058", "q0059", "q0060", "q0061", "q0062", 
                          "q0063", "q0064", "q0067", "q0068", "q0069", "q0073", "q0075", 
                          "q0076", "q0077", "q0081", "q0082", "q0083", "q0084", "q0086", 
                          "q0087", "q0089", "q0090", "q0091", "q0092", "q0093", "q0096", 
                          "q0098", "q0099", "q0100", "q0101", "q0103", "q0104", "q0105", 
                          "q0106", "q0108", "q0111", "q0113", "q0114", "q0115", "q0117")

SOC_items_Adult_Self <- c("q0013", "q0014", "q0016", "q0018", "q0020", "q0021", "q0023", 
                          "q0024", "q0026", "q0027")

SOC_rev_items_Adult_Self <- c("q0013", "q0014", "q0018", "q0020", "q0027")

VIS_items_Adult_Self <- c("q0029", "q0030", "q0031", "q0032", "q0033", "q0034", "q0036", 
                          "q0037", "q0040", "q0041")

HEA_items_Adult_Self <- c("q0043", "q0044", "q0046", "q0047", "q0049", "q0050", "q0051", 
                          "q0053", "q0054", "q0057")

TOU_items_Adult_Self <- c("q0058", "q0059", "q0060", "q0061", "q0062", "q0063", "q0064", 
                          "q0067", "q0068", "q0069")

TS_items_Adult_Self <- c("q0073", "q0075", "q0076", "q0077", "q0081", "q0082", "q0083", 
                         "q0084", "q0086", "q0087")

BOD_items_Adult_Self <- c("q0089", "q0090", "q0091", "q0092", "q0093", "q0096", "q0098", 
                          "q0099", "q0100", "q0101")

BAL_items_Adult_Self <- c("q0103", "q0104", "q0105", "q0106", "q0108", "q0111", "q0113", 
                          "q0114", "q0115", "q0117")

PLA_items_Adult_Self <- c("q0118", "q0119", "q0122", "q0126", "q0127", "q0128", "q0129", 
                          "q0130", "q0131", "q0132")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SPM-2 Adult ages 1690 Self-Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Self
  ) %>%
  # filter out youngest age group
  filter(AgeGroup != "15.75 to 20.99 years") %>% 
  rename(age_range = AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Adult_Self,
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
    SOC_rev_items_Adult_Self,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Adult_Self,
            ~ as.integer(.x)) %>% 
# Compute raw scores. Note use of `rowSums(.[TOT_items_Adult_Self])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Self]),
    SOC_raw = rowSums(.[SOC_items_Adult_Self]),
    VIS_raw = rowSums(.[VIS_items_Adult_Self]),
    HEA_raw = rowSums(.[HEA_items_Adult_Self]),
    TOU_raw = rowSums(.[TOU_items_Adult_Self]),
    TS_raw = rowSums(.[TS_items_Adult_Self]),
    BOD_raw = rowSums(.[BOD_items_Adult_Self]),
    BAL_raw = rowSums(.[BAL_items_Adult_Self]),
    PLA_raw = rowSums(.[PLA_items_Adult_Self])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = "Eng") %>% 
  select(IDNumber, data, everything()) %>% 
# Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200)

# Prep file to comp
Adult_Self_scores <- Adult_Self %>% 
  select(
    -(All_items_Adult_Self), -(Gender:Region)
  )   


# READ IN SPANISH DATA

Adult_Self_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SPM-2 Adult ages 1690 Self-Report Questionnaire-Sp.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Self
  ) %>% 
  # filter out youngest age group
  filter(AgeGroup != "15.75 to 20.99 years") %>% 
  rename(age_range = AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Adult_Self,
    ~ case_when(
      .x == "Nunca" ~ 1,
      .x == "Ocasionalmente" ~ 2,
      .x == "Frecuentemente" ~ 3,
      .x == "Siempre" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # recode reverse-scored items
  mutate_at(
    SOC_rev_items_Adult_Self,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Adult_Self,
            ~ as.integer(.x)) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Adult_Self])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Self]),
    SOC_raw = rowSums(.[SOC_items_Adult_Self]),
    VIS_raw = rowSums(.[VIS_items_Adult_Self]),
    HEA_raw = rowSums(.[HEA_items_Adult_Self]),
    TOU_raw = rowSums(.[TOU_items_Adult_Self]),
    TS_raw = rowSums(.[TS_items_Adult_Self]),
    BOD_raw = rowSums(.[BOD_items_Adult_Self]),
    BAL_raw = rowSums(.[BAL_items_Adult_Self]),
    PLA_raw = rowSums(.[PLA_items_Adult_Self])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = "Sp") %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200)

# Prep file to comp
Adult_Self_Sp_scores <- Adult_Self_Sp %>% 
  select(
    -(All_items_Adult_Self), -(Gender:Region)
  )   

# clean up environment
rm(list = ls(pattern='.*items_Adult_Self'))

Adult_Self_comp <- bind_rows(Adult_Self_scores, Adult_Self_Sp_scores) 

# EXAMINE DATA---------------------------------

# Create frequency tables for TOT_raw by data source
Adult_Self_comp_TOT_freq_data <- Adult_Self_comp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(Adult_Self_comp_TOT_freq_data, here('OUTPUT-FILES/ADULT/FREQUENCIES/Adult-Self-Eng-Sp-TOT-freq-data.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by data source
Adult_Self_comp_TOT_desc_data <-
  Adult_Self_comp %>% group_by(data) %>% arrange(data) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:2)
  )

write_csv(Adult_Self_comp_TOT_desc_data, here('OUTPUT-FILES/ADULT/DESCRIPTIVES/Adult-Self-Eng-Sp-TOT-desc-dataSource.csv'))

# Generate single table comparing descriptives from Eng and Sp sources.
Adult_Self_comp_TOT_desc_Eng <- Adult_Self_comp_TOT_desc_data %>% 
  filter(data == 'Eng') %>% 
  setNames(., str_c(names(.), 'Eng', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))
Adult_Self_comp_TOT_desc_Sp <- Adult_Self_comp_TOT_desc_data %>% 
  filter(data == 'Sp') %>% 
  setNames(., str_c(names(.), 'Sp', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))

Adult_Self_comp_TOT_desc_comp <- Adult_Self_comp_TOT_desc_Eng %>% 
  bind_cols(Adult_Self_comp_TOT_desc_Sp) %>% 
  # select(-age_range_Sp) %>% 
  # rename(age_range = age_range_Eng) %>% 
  mutate(
    diff = round(mean_Eng - mean_Sp, 2),
    ES_diff = round((mean_Eng - mean_Sp) / ((sd_Eng + sd_Sp) / 2), 2)
  )

write_csv(Adult_Self_comp_TOT_desc_comp, here('OUTPUT-FILES/ADULT/DESCRIPTIVES/Adult-Self-Eng-Sp-TOT-desc-comp.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Adult_Self_comp_TOT_desc_data, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(Adult_Self_comp_TOT_desc_data$data)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "Data Sourcce", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


