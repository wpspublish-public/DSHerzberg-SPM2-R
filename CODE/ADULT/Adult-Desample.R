suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# Desampling is applied first to the Self form sample, the excluded cases are
# then also removed from the Other form sample

Adult_Self_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SM-QUAL-COMBO-NORMS-INPUT/Adult-Self-combo-norms-input.csv")
  ))) 
Adult_Self_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SP-NORMS-INPUT/Adult-Self-Sp-norms-input.csv")
  ))) 
Adult_Self_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/INHOUSE-NORMS-INPUT/Adult-Self-inHouse-norms-input.csv")
  ))) 
Adult_Self <- bind_rows(Adult_Self_Eng, Adult_Self_Sp, Adult_Self_inHouse) %>% 
  arrange(IDNumber)

# STAGE 1 DESAMPLE --------------------------------------------------------

# Subsample: 100 females with 4-yr college
set.seed(123)
Adult_Self_F_BA <- Adult_Self %>% 
  filter(Gender == "Female" & HighestEducation == "Bachelor's degree or higher" ) %>% 
  sample_n(100)

# Remove all females with BA from main sample
Adult_Self_not_F_BA <- Adult_Self %>% 
  filter(!(Gender == "Female" & HighestEducation == "Bachelor's degree or higher" ))

# combine subsamples to create desampled data set

Adult_Self_desamp1 <- bind_rows(Adult_Self_F_BA, Adult_Self_not_F_BA) %>% 
  arrange(IDNumber)

# STAGE 2 DESAMPLE --------------------------------------------------------

# Subsample: 14 Blacks with BA+
set.seed(123)
Adult_Self_Black_BAplus <- Adult_Self_desamp1 %>% 
  filter(Ethnicity == "Black" & 
           HighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(14)

# Subsample: 30 Hisp with BA+
set.seed(123)
Adult_Self_Hisp_BAplus <- Adult_Self_desamp1 %>% 
  filter(Ethnicity == "Hispanic" & 
           HighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(30)

# Subsample: 8 MultiRacial with BA+
set.seed(123)
Adult_Self_Multi_BAplus <- Adult_Self_desamp1 %>% 
  filter(Ethnicity == "MultiRacial" & 
           HighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(8)

# Subsample: 6 Asian with BA+
set.seed(123)
Adult_Self_Asian_BAplus <- Adult_Self_desamp1 %>% 
  filter(Ethnicity == "Asian" & 
           HighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(6)

# Combine 4 subsamples

Adult_Self_stage2 <- bind_rows(
  Adult_Self_Black_BAplus,
  Adult_Self_Hisp_BAplus,
  Adult_Self_Multi_BAplus,
  Adult_Self_Asian_BAplus
) %>% 
  arrange(IDNumber)

Adult_Self_desamp <- Adult_Self_desamp1 %>% anti_join(Adult_Self_stage2, by = 'IDNumber')

# WRITE FINAL DESAMPLE FILE -----------------------------------------------


write_csv(Adult_Self_desamp, here('INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Self-allData-desamp.csv'))


# extract ID numbers of cases excluded in Self desamp process

Adult_Self_desamp_excludedID <- Adult_Self %>% anti_join(
  Adult_Self_desamp, 
  by = 'IDNumber'
) %>% 
  select(IDNumber) %>% 
  arrange(IDNumber)

# Apply Self desampling to Other data set

Adult_Other_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SM-ONLY-NORMS-INPUT/Adult-Other-SM-only-norms-input.csv")
  ))) 
Adult_Other_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/INHOUSE-NORMS-INPUT/Adult-Other-inHouse-norms-input.csv")
  ))) 
Adult_Other <- bind_rows(Adult_Other_Eng, Adult_Other_inHouse) %>% 
  arrange(IDNumber)

Adult_Other_desamp <- Adult_Other %>% anti_join(
  Adult_Self_desamp_excludedID, 
  by = 'IDNumber'
) %>% 
  arrange(IDNumber)

write_csv(Adult_Other_desamp, here('INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Other-allData-desamp.csv'))


# EXAMINE DATA---------------------------------

# Create frequency tables for TOT_raw by data source
Adult_Self_desamp_TOT_freq <- Adult_Self_desamp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(Adult_Self_desamp_TOT_freq, here('OUTPUT-FILES/ADULT/FREQUENCIES/Adult-Self-Eng-Sp-TOT-freq-data.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by age range
Adult_Self_desamp_TOT_desc_age <-
  Adult_Self_desamp %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2)#,
         # group = c(1:2)
  )

write_csv(Adult_Self_desamp_TOT_desc_age, here('OUTPUT-FILES/ADULT/DESCRIPTIVES/Adult-Self-desamp-TOT-desc-age.csv'))

# # Generate single table comparing descriptives from Eng and Sp sources.
# Adult_Self_desamp_TOT_desc_Eng <- Adult_Self_desamp_TOT_desc %>% 
#   filter(data == 'Eng') %>% 
#   setNames(., str_c(names(.), 'Eng', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# Adult_Self_desamp_TOT_desc_Sp <- Adult_Self_desamp_TOT_desc %>% 
#   filter(data == 'Sp') %>% 
#   setNames(., str_c(names(.), 'Sp', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# 
# Adult_Self_desamp_TOT_desc_desamp <- Adult_Self_desamp_TOT_desc_Eng %>% 
#   bind_cols(Adult_Self_desamp_TOT_desc_Sp) %>% 
#   # select(-age_range_Sp) %>% 
#   # rename(age_range = age_range_Eng) %>% 
#   mutate(
#     diff = round(mean_Eng - mean_Sp, 2),
#     ES_diff = round((mean_Eng - mean_Sp) / ((sd_Eng + sd_Sp) / 2), 2)
#   )
# 
# write_csv(Adult_Self_desamp_TOT_desc_desamp, here('OUTPUT-FILES/ADULT/DESCRIPTIVES/Adult-Self-desamp-TOT-desc.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Adult_Self_desamp_TOT_desc, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(Adult_Self_desamp_TOT_desc$data)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "Data Sourcce", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


