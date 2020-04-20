suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# Desampling is applied first to the Home form sample, the excluded cases are
# then also removed from the other form samples

Preschool_25_Home_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/SM-QUAL-COMBO-NORMS-INPUT/Preschool-25-Home-combo-norms-input.csv")
  ))) 
Preschool_25_Home_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/SP-NORMS-INPUT/Preschool-25-Home-Sp-norms-input.csv")
  ))) 
Preschool_25_Home_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/INHOUSE-NORMS-INPUT/Preschool-25-Home-inHouse-norms-input.csv")
  ))) 
Preschool_25_Home <- bind_rows(Preschool_25_Home_Eng, Preschool_25_Home_Sp, Preschool_25_Home_inHouse) %>% 
  arrange(IDNumber)

# STAGE 1 DESAMPLE --------------------------------------------------------

# Subsample: 189 Whites with four year college
set.seed(123)
Preschool_25_Home_White_BAplus <- Preschool_25_Home %>% 
  filter(Ethnicity == "White" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(189)

# Remove all Whites with four year college from main sample
Preschool_25_Home_not_White_BAplus <- Preschool_25_Home %>% 
  filter(!(Ethnicity == "White" & 
             ParentHighestEducation == "Bachelor's degree or higher"))
  
# combine subsamples to create desampled data set

Preschool_25_Home_desamp1 <- bind_rows(Preschool_25_Home_White_BA, Preschool_25_Home_not_White_BA) %>% 
  arrange(IDNumber)

# STAGE 2 DESAMPLE --------------------------------------------------------

# Subsample: 9 Blacks with BA+
set.seed(123)
Preschool_25_Home_Black_BAplus <- Preschool_25_Home_desamp1 %>% 
  filter(Ethnicity == "Black" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(9)

# Subsample: 10 Hisp with BA+
set.seed(123)
Preschool_25_Home_Hisp_BAplus <- Preschool_25_Home_desamp1 %>% 
  filter(Ethnicity == "Hispanic" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(10)

# Subsample: 20 MultiRacial with BA+
set.seed(123)
Preschool_25_Home_Multi_BAplus <- Preschool_25_Home_desamp1 %>% 
  filter(Ethnicity == "MultiRacial" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(20)

# Subsample: 41 White with BA+
set.seed(123)
Preschool_25_Home_White_BAplus <- Preschool_25_Home_desamp1 %>% 
  filter(Ethnicity == "White" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(41)

# Combine 4 subsamples

Preschool_25_Home_stage2 <- bind_rows(
  Preschool_25_Home_Black_BAplus,
  Preschool_25_Home_Hisp_BAplus,
  Preschool_25_Home_Multi_BAplus,
  Preschool_25_Home_White_BAplus
) %>% 
  arrange(IDNumber)

rm(list = ls(pattern = 'BAplus'))

Preschool_25_Home_desamp <- Preschool_25_Home_desamp1 %>% anti_join(Preschool_25_Home_stage2, by = 'IDNumber')

# WRITE FINAL DESAMPLE FILE -----------------------------------------------

write_csv(Preschool_25_Home_desamp, here('INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-Home-allData-desamp.csv'))

# extract ID numbers of cases excluded in Home desamp process

Preschool_25_Home_desamp_excludedID <- Preschool_25_Home %>% anti_join(
  Preschool_25_Home_desamp, 
  by = 'IDNumber'
) %>% 
  select(IDNumber) %>% 
  arrange(IDNumber)

# write all school data for norming, without applying any desampling.

Preschool_25_School_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/SM-ONLY-NORMS-INPUT/Preschool-25-School-SM-only-norms-input.csv")
  ))) 
Preschool_25_School_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/INHOUSE-NORMS-INPUT/Preschool-25-School-inHouse-norms-input.csv")
  ))) 
Preschool_25_School <- bind_rows(Preschool_25_School_Eng, Preschool_25_School_inHouse) %>% 
  arrange(IDNumber)

write_csv(
  Preschool_25_School, 
  here(
    'INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-School-allData-desamp.csv'
    ),
  na = ""
  )

# EXAMINE DATA---------------------------------

# Create frequency tables for TOT_raw by data source
Preschool_25_Home_desamp_TOT_freq <- Preschool_25_Home_desamp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(Preschool_25_Home_desamp_TOT_freq, here('OUTPUT-FILES/PRESCHOOL/FREQUENCIES/Preschool-25-Home-Eng-Sp-TOT-freq-data.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by age range
Preschool_25_Home_desamp_TOT_desc_age <-
  Preschool_25_Home_desamp %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2)#,
         # group = c(1:2)
  )

write_csv(Preschool_25_Home_desamp_TOT_desc_age, here('OUTPUT-FILES/PRESCHOOL/DESCRIPTIVES/Preschool-25-Home-desamp-TOT-desc-age.csv'))

# # Generate single table comparing descriptives from Eng and Sp sources.
# Preschool_25_Home_desamp_TOT_desc_Eng <- Preschool_25_Home_desamp_TOT_desc %>% 
#   filter(data == 'Eng') %>% 
#   setNames(., str_c(names(.), 'Eng', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# Preschool_25_Home_desamp_TOT_desc_Sp <- Preschool_25_Home_desamp_TOT_desc %>% 
#   filter(data == 'Sp') %>% 
#   setNames(., str_c(names(.), 'Sp', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# 
# Preschool_25_Home_desamp_TOT_desc_desamp <- Preschool_25_Home_desamp_TOT_desc_Eng %>% 
#   bind_cols(Preschool_25_Home_desamp_TOT_desc_Sp) %>% 
#   # select(-age_range_Sp) %>% 
#   # rename(age_range = age_range_Eng) %>% 
#   mutate(
#     diff = round(mean_Eng - mean_Sp, 2),
#     ES_diff = round((mean_Eng - mean_Sp) / ((sd_Eng + sd_Sp) / 2), 2)
#   )
# 
# write_csv(Preschool_25_Home_desamp_TOT_desc_desamp, here('OUTPUT-FILES/PRESCHOOL/DESCRIPTIVES/Preschool-25-Home-desamp-TOT-desc.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Preschool_25_Home_desamp_TOT_desc, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(Preschool_25_Home_desamp_TOT_desc$data)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "Data Sourcce", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


