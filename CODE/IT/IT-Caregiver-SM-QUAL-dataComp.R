suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_IT_Caregiver <- c("QT1", "QT2", "QT4", "QT5", "QT6", "QT7", "QT10", "QT11", "QT12", "QT13", "QT14", "QT15", 
                                "QT17", "QT18", "QT19", "QT20", "QT21", "QT23", "QT25", "QT26", "QT27", "QT28", "QT30", 
                                "QT31", "QT32", "QT33", "QT34", "QT35", "QT37", "QT39", "QT41", "QT43", "QT45", "QT46", 
                                "QT47", "QT48", "QT49", "QT50", "QT51", "QT54", "QT55", "QT56", "QT57", "QT60", "QT62", 
                                "QT65", "QT66", "QT67", "QT68", "QT69", "QT71", "QT72", "QT73", "QT74", "QT75", "QT76", 
                                "QT77", "QT78", "QT79", "QT81", "QT83", "QT85", "QT86", "QT87", "QT89", "QT91", "QT92", 
                                "QT93", "QT94", "QT95", "QT96", "QT97", "QT98", "QT100", "QT101", "QT103", "QT105", 
                            "QT106", "QT107", "QT108")


TOT_items_IT_Caregiver <- c("QT14", "QT15", 
                                "QT17", "QT18", "QT19", "QT20", "QT21", "QT23", "QT25", "QT26", "QT27", "QT28", "QT30", 
                                "QT31", "QT32", "QT33", "QT34", "QT35", "QT37", "QT39", "QT41", "QT43", "QT45", "QT46", 
                                "QT47", "QT48", "QT49", "QT50", "QT51", "QT54", "QT55", "QT56", "QT57", "QT60", "QT62", 
                                "QT65", "QT66", "QT67", "QT68", "QT69", "QT71", "QT72", "QT73", "QT74", "QT75", "QT76", 
                                "QT77", "QT78", "QT79", "QT81", "QT83", "QT85", "QT86", "QT87", "QT89", "QT91", "QT92", 
                                "QT93", "QT94", "QT95")

SOC_items_IT_Caregiver <- c("QT1", "QT2", "QT4", "QT5", "QT6", "QT7", "QT10", "QT11", "QT12", "QT13")

SOC_rev_items_IT_Caregiver <- c("QT1", "QT2", "QT4", "QT5", "QT6", "QT7")

VIS_items_IT_Caregiver <- c("QT14", "QT15", "QT17", "QT18", "QT19", "QT20", "QT21", "QT23", "QT25", "QT26")

HEA_items_IT_Caregiver <- c("QT27", "QT28", "QT30", "QT31", "QT32", "QT33", "QT34", "QT35", "QT37", "QT39")

TOU_items_IT_Caregiver <- c("QT41", "QT43", "QT45", "QT46", "QT47", "QT48", "QT49", "QT50", "QT51", "QT54")

TS_items_IT_Caregiver <- c("QT55", "QT56", "QT57", "QT60", "QT62", "QT65", "QT66", "QT67", "QT68", "QT69")

BOD_items_IT_Caregiver <- c("QT71", "QT72", "QT73", "QT74", "QT75", "QT76", "QT77", "QT78", "QT79", "QT81")

BAL_items_IT_Caregiver <- c("QT83", "QT85", "QT86", "QT87", "QT89", "QT91", "QT92", "QT93", "QT94", "QT95")

PLA_items_IT_Caregiver <- c("QT96", "QT97", "QT98", "QT100", "QT101", "QT103", "QT105", "QT106", "QT107", "QT108")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

IT_Caregiver_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SPM-2 SM Qual COMBO InfantToddler Caregiver Combo.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_Caregiver
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_Caregiver,
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
    SOC_rev_items_IT_Caregiver,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_IT_Caregiver,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 10 ~ "03.5 to 10 mo",
    TRUE ~ "11 to 31.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
# Compute raw scores. Note use of `rowSums(.[TOT_items_IT_Caregiver])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_Caregiver]),
    SOC_raw = rowSums(.[SOC_items_IT_Caregiver]),
    VIS_raw = rowSums(.[VIS_items_IT_Caregiver]),
    HEA_raw = rowSums(.[HEA_items_IT_Caregiver]),
    TOU_raw = rowSums(.[TOU_items_IT_Caregiver]),
    TS_raw = rowSums(.[TS_items_IT_Caregiver]),
    BOD_raw = rowSums(.[BOD_items_IT_Caregiver]),
    BAL_raw = rowSums(.[BAL_items_IT_Caregiver]),
    PLA_raw = rowSums(.[PLA_items_IT_Caregiver])
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
  # filter(!(TOT_raw >= 120 & age_range == '21 to 31.5 mo' & data == 'Qual')) %>%
  filter(!(TOT_raw >= 150 & data == 'Qual')) %>%
  write_csv(here('INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-Caregiver-combo-norms-input.csv'),
            na = "")

# clean up environment
rm(list = ls(pattern='.*items_IT_Caregiver'))

IT_Caregiver <- IT_Caregiver_items %>% 
  select(
    -(QT1:QT108)
  )   

# EXAMINE DATA TO MAKE AGESTRAT DECISIONS ---------------------------------

# ### THE NEXT SECTION OF CODE FACILITATES EXAMINATION OF TOT_raw to make
# decisions about whether norms need to be stratified by age. Once the decision
# about age-stratification has been made and implemented, the 'examination' code
# can be commented off.


# Create frequency tables for TOT_raw by AgeGroup
IT_Caregiver_TOT_freq_AgeGroup <- IT_Caregiver %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(IT_Caregiver_TOT_freq_AgeGroup, here('OUTPUT-FILES/IT/FREQUENCIES/IT-Caregiver-TOT-freq-AgeGroup.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by AgeGroup
IT_Caregiver_TOT_desc_AgeGroup_data <-
  IT_Caregiver %>% group_by(data, age_range) %>% arrange(data, age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:2)
  )

IT_Caregiver_TOT_desc_AgeGroup <-
  IT_Caregiver %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                                          median = round(median(TOT_raw), 2),
                                                                                          mean = round(mean(TOT_raw), 2),
                                                                                          sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:2)
  )

write_csv(IT_Caregiver_TOT_desc_AgeGroup_data, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-Caregiver-TOT-desc-dataSource.csv'))
write_csv(IT_Caregiver_TOT_desc_AgeGroup, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-Caregiver-TOT-desc.csv'))


# Generate single table comparing descriptives from SM and Qual sources.
IT_Caregiver_TOT_desc_SM <- IT_Caregiver_TOT_desc_AgeGroup_data %>% 
  filter(data == 'SM') %>% 
  setNames(., str_c(names(.), 'SM', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))
IT_Caregiver_TOT_desc_Qual <- IT_Caregiver_TOT_desc_AgeGroup_data %>% 
  filter(data == 'Qual') %>% 
  setNames(., str_c(names(.), 'Qual', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))

IT_Caregiver_TOT_desc_comp <- IT_Caregiver_TOT_desc_SM %>% 
  bind_cols(IT_Caregiver_TOT_desc_Qual) %>% 
  select(-age_range_Qual) %>% 
  rename(age_range = age_range_SM) %>% 
  mutate(
    diff = round(mean_SM - mean_Qual, 2),
    ES_diff = round((mean_SM - mean_Qual) / ((sd_SM + sd_Qual) / 2), 2)
  )

write_csv(IT_Caregiver_TOT_desc_comp, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-Caregiver-TOT-desc-comp.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = IT_Caregiver_TOT_desc_AgeGroup_data, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(IT_Caregiver_TOT_desc_AgeGroup_data$age_range)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


