suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Adult_Other <- c("q0014", "q0015", "q0016", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027", "q0028",
                          "q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042", 
                           "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057", 
                           "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072", 
                           "q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087", 
                           "q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", 
                           "q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114",
                          "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125", "q0129", "q0131")

TOT_items_Adult_Other <- c("q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042", 
                           "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057", 
                           "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072", 
                           "q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087", 
                           "q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", 
                           "q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114")

SOC_items_Adult_Other <- c("q0014", "q0015", "q0016", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027", "q0028")

SOC_rev_items_Adult_Other <- c("q0014", "q0015", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027")

VIS_items_Adult_Other <- c("q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042")

HEA_items_Adult_Other <- c("q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057")

TOU_items_Adult_Other <- c("q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072")

TS_items_Adult_Other <- c("q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087")

BOD_items_Adult_Other <- c("q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100")

BAL_items_Adult_Other <- c("q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114")

PLA_items_Adult_Other <- c("q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125", "q0129", "q0131")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Adult_Other <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SPM-2 Adult ages 1690 Other Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Other
  ) %>%
  # filter out youngest age group
  filter(AgeGroup != "15.75 to 20.99 years") %>% 
  rename(age_range = AgeGroup) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Adult_Other,
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
    SOC_rev_items_Adult_Other,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Adult_Other,
            ~ as.integer(.x)) %>% 
# Compute raw scores. Note use of `rowSums(.[TOT_items_Adult_Other])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Other]),
    SOC_raw = rowSums(.[SOC_items_Adult_Other]),
    VIS_raw = rowSums(.[VIS_items_Adult_Other]),
    HEA_raw = rowSums(.[HEA_items_Adult_Other]),
    TOU_raw = rowSums(.[TOU_items_Adult_Other]),
    TS_raw = rowSums(.[TS_items_Adult_Other]),
    BOD_raw = rowSums(.[BOD_items_Adult_Other]),
    BAL_raw = rowSums(.[BAL_items_Adult_Other]),
    PLA_raw = rowSums(.[PLA_items_Adult_Other])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = "Eng") %>% 
  select(IDNumber, data, everything()) %>% 
# Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200)

# clean up environment
# rm(list = ls(pattern='.*items_Adult_Other'))


# NEXT: READ IN SPANISH DATA

Adult_Other_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SPM-2 Adult ages 1690 Other Report Questionnaire-Sp.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Other
  ) 





Adult_Other <- Adult_Other_items %>% 
  select(
    -(q0010:q0119)
  )   

# Prep file to comp with daycare data
Adult_Other_scores <- Adult_Other %>% 
  select(
    -(Gender:Region)
  )   

IT_49_Daycare_scores <- read_csv(here('INPUT-FILES/IT/IT-49-Daycare-scores.csv'))

IT_49_comp <- bind_rows(Adult_Other_scores, IT_49_Daycare_scores) %>% 
  mutate(data = case_when(
    data == "Qual" ~ "Home",
    data == "SM" ~ "Home",
    TRUE ~ data
  ))

# EXAMINE DATA TO MAKE AGESTRAT DECISIONS ---------------------------------

# ### THE NEXT SECTION OF CODE FACILITATES EXAMINATION OF TOT_raw to make
# decisions about whether norms need to be stratified by age. Once the decision
# about age-stratification has been made and implemented, the 'examination' code
# can be commented off.


# Create frequency tables for TOT_raw by AgeGroup
IT_49_comp_TOT_freq_AgeGroup <- IT_49_comp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(IT_49_comp_TOT_freq_AgeGroup, here('OUTPUT-FILES/IT/FREQUENCIES/IT-49-Home-Daycare-TOT-freq-AgeGroup.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by AgeGroup
IT_49_comp_TOT_desc_AgeGroup_data <-
  IT_49_comp %>% group_by(data, age_range) %>% arrange(data, age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:2)
  )

IT_49_comp_TOT_desc_AgeGroup <-
  IT_49_comp %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                                          median = round(median(TOT_raw), 2),
                                                                                          mean = round(mean(TOT_raw), 2),
                                                                                          sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:2)
  )

write_csv(IT_49_comp_TOT_desc_AgeGroup_data, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-49-Home-Daycare-TOT-desc-dataSource.csv'))
write_csv(IT_49_comp_TOT_desc_AgeGroup, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-49-Home-Daycare-TOT-desc.csv'))


# Generate single table comparing descriptives from SM and Qual sources.
IT_49_comp_TOT_desc_home <- IT_49_comp_TOT_desc_AgeGroup_data %>% 
  filter(data == 'Home') %>% 
  setNames(., str_c(names(.), 'Home', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))
IT_49_comp_TOT_desc_daycare <- IT_49_comp_TOT_desc_AgeGroup_data %>% 
  filter(data == 'Daycare') %>% 
  setNames(., str_c(names(.), 'Daycare', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))

IT_49_comp_TOT_desc_comp <- IT_49_comp_TOT_desc_home %>% 
  bind_cols(IT_49_comp_TOT_desc_daycare) %>% 
  select(-age_range_Daycare) %>% 
  rename(age_range = age_range_Home) %>% 
  mutate(
    diff = round(mean_Home - mean_Daycare, 2),
    ES_diff = round((mean_Home - mean_Daycare) / ((sd_Home + sd_Daycare) / 2), 2)
  )

write_csv(IT_49_comp_TOT_desc_comp, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-49-Home-Daycare-TOT-desc-comp.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = IT_49_comp_TOT_desc_AgeGroup_data, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(IT_49_comp_TOT_desc_AgeGroup_data$age_range)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


