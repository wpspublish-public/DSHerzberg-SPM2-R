suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# Desampling is applied first to the Home form sample, the excluded cases are
# then also removed from the other form samples

Child_512_Home_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv")
  ))) 
Child_512_Home_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/SP-NORMS-INPUT/Child-512-Home-Sp-norms-input.csv")
  ))) 
Child_512_Home_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/INHOUSE-NORMS-INPUT/Child-512-Home-inHouse-norms-input.csv")
  ))) 
Child_512_Home <- bind_rows(Child_512_Home_Eng, Child_512_Home_Sp, Child_512_Home_inHouse) %>% 
  arrange(IDNumber)


# STAGE 1 DESAMPLE --------------------------------------------------------

# Subsample: 100 Blacks with HS deg or some college
set.seed(123)
Child_512_Home_Black_HSsomeColl <- Child_512_Home %>% 
  filter(Ethnicity == "Black" & 
           (ParentHighestEducation == "High school graduate (including GED)" | 
              ParentHighestEducation == "Some college or associate degree" )) %>% 
  sample_n(100)

# Remove all Blacks with HS deg or some college from main sample
Child_512_Home_not_Black_HSsomeColl <- Child_512_Home %>% 
  filter(!(Ethnicity == "Black" & 
           (ParentHighestEducation == "High school graduate (including GED)" | 
              ParentHighestEducation == "Some college or associate degree" )))
  
# combine subsamples to create desampled data set

Child_512_Home_desamp1 <- bind_rows(Child_512_Home_Black_HSsomeColl, Child_512_Home_not_Black_HSsomeColl) %>% 
  arrange(IDNumber)

# 50% subsample
set.seed(123)
Child_512_Home_Black_HSsomeColl_50pct <- Child_512_Home %>% 
  filter(Ethnicity == "Black" & 
           (ParentHighestEducation == "High school graduate (including GED)" | 
              ParentHighestEducation == "Some college or associate degree" )) %>% 
  sample_n(154)
Child_512_Home_desamp1_50pct <- bind_rows(Child_512_Home_Black_HSsomeColl_50pct, Child_512_Home_not_Black_HSsomeColl) %>% 
  arrange(IDNumber)


# STAGE 2 DESAMPLE --------------------------------------------------------

# Subsample: 15 Blacks with BA+
set.seed(123)
Child_512_Home_Black_BAplus <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "Black" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(15)
# 50% subsample
Child_512_Home_Black_BAplus_50pct <- Child_512_Home_Black_BAplus %>% sample_frac(.5)

# Subsample: 15 Blacks with Some College
set.seed(123)
Child_512_Home_Black_SomeColl <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "Black" & 
           ParentHighestEducation == "Some college or associate degree") %>% 
  sample_n(15)
# 50% subsample
Child_512_Home_Black_SomeColl_50pct <- Child_512_Home_Black_SomeColl %>% sample_frac(.5)

# Subsample: 17 Hisp with BA+
set.seed(123)
Child_512_Home_Hisp_BAplus <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "Hispanic" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(17)
# 50% subsample
Child_512_Home_Hisp_BAplus_50pct <- Child_512_Home_Hisp_BAplus %>% sample_frac(.5)

# Subsample: 18 Hisp with Some College
set.seed(123)
Child_512_Home_Hisp_SomeColl <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "Hispanic" & 
           ParentHighestEducation == "Some college or associate degree") %>% 
  sample_n(18)
# 50% subsample
Child_512_Home_Hisp_SomeColl_50pct <- Child_512_Home_Hisp_SomeColl %>% sample_frac(.5)

# Subsample: 10 MultiRacial with BA+
set.seed(123)
Child_512_Home_Multi_BAplus <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "MultiRacial" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(10)
# 50% subsample
Child_512_Home_Multi_BAplus_50pct <- Child_512_Home_Multi_BAplus %>% sample_frac(.5)

# Subsample: 10 MultiRacial with Some College
set.seed(123)
Child_512_Home_Multi_SomeColl <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "MultiRacial" & 
           ParentHighestEducation == "Some college or associate degree") %>% 
  sample_n(10)
# 50% subsample
Child_512_Home_Multi_SomeColl_50pct <- Child_512_Home_Multi_SomeColl %>% sample_frac(.5)

# Subsample: 58 White with BA+
set.seed(123)
Child_512_Home_White_BAplus <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "White" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(58)
# 50% subsample
Child_512_Home_White_BAplus_50pct <- Child_512_Home_White_BAplus %>% sample_frac(.5)

# Subsample: 57 White with Some College
set.seed(123)
Child_512_Home_White_SomeColl <- Child_512_Home_desamp1 %>% 
  filter(Ethnicity == "White" & 
           ParentHighestEducation == "Some college or associate degree") %>% 
  sample_n(57)
# 50% subsample
Child_512_Home_White_SomeColl_50pct <- Child_512_Home_White_SomeColl %>% sample_frac(.5)

# Combine 8 subsamples

Child_512_Home_stage2 <- bind_rows(
  Child_512_Home_Black_BAplus,
  Child_512_Home_Black_SomeColl,
  Child_512_Home_Hisp_BAplus,
  Child_512_Home_Hisp_SomeColl,
  Child_512_Home_Multi_BAplus,
  Child_512_Home_Multi_SomeColl,
  Child_512_Home_White_BAplus,
  Child_512_Home_White_SomeColl
) %>% 
  arrange(IDNumber)

Child_512_Home_desamp <- Child_512_Home_desamp1 %>% 
  anti_join(
    Child_512_Home_stage2, 
    by = 'IDNumber'
    )

# Combine 8 50% subsamples

Child_512_Home_stage2_50pct <- bind_rows(
  Child_512_Home_Black_BAplus_50pct,
  Child_512_Home_Black_SomeColl_50pct,
  Child_512_Home_Hisp_BAplus_50pct,
  Child_512_Home_Hisp_SomeColl_50pct,
  Child_512_Home_Multi_BAplus_50pct,
  Child_512_Home_Multi_SomeColl_50pct,
  Child_512_Home_White_BAplus_50pct,
  Child_512_Home_White_SomeColl_50pct
) %>% 
  arrange(IDNumber)

rm(list = ls(pattern = 'BAplus'))
rm(list = ls(pattern ='SomeColl'))
rm(list = ls(pattern ='Black'))

Child_512_Home_desamp_50pct <- Child_512_Home_desamp1_50pct %>% 
  anti_join(
    Child_512_Home_stage2_50pct, 
    by = 'IDNumber'
    )

# WRITE FINAL DESAMPLE FILE -----------------------------------------------

write_csv(Child_512_Home_desamp, here('INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-Home-allData-desamp.csv'))

# extract ID numbers of cases excluded in Home desamp process

Child_512_Home_desamp_excludedID <- Child_512_Home %>% anti_join(
  Child_512_Home_desamp, 
  by = 'IDNumber'
) %>% 
  select(IDNumber) %>% 
  arrange(IDNumber)

Child_512_Home_desamp_excludedID_50pct <- Child_512_Home %>% anti_join(
  Child_512_Home_desamp_50pct, 
  by = 'IDNumber'
) %>% 
  select(IDNumber) %>% 
  arrange(IDNumber)

# Apply Home desamp to School data set

Child_512_School_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/SM-ONLY-NORMS-INPUT/Child-512-School-SM-only-norms-input.csv")
  ))) 
Child_512_School_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/INHOUSE-NORMS-INPUT/Child-512-School-inHouse-norms-input.csv")
  ))) 
Child_512_School <- bind_rows(Child_512_School_Eng, Child_512_School_inHouse) %>% 
  arrange(IDNumber)

Child_512_School_desamp <- Child_512_School %>% anti_join(
  Child_512_Home_desamp_excludedID_50pct,
  by = 'IDNumber'
) %>%
  arrange(IDNumber)

write_csv(Child_512_School_desamp, here('INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/Child-512-School-allData-desamp.csv'))

# EXAMINE DATA---------------------------------

# Create frequency tables for TOT_raw by data source
Child_512_Home_desamp_TOT_freq <- Child_512_Home_desamp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(Child_512_Home_desamp_TOT_freq, here('OUTPUT-FILES/CHILD/FREQUENCIES/CHILD-512-Home-Eng-Sp-TOT-freq-data.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by age range
Child_512_Home_desamp_TOT_desc_age <-
  Child_512_Home_desamp %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2)#,
         # group = c(1:2)
  )

write_csv(Child_512_Home_desamp_TOT_desc_age, here('OUTPUT-FILES/CHILD/DESCRIPTIVES/Child-512-Home-desamp-TOT-desc-age.csv'))

# # Generate single table comparing descriptives from Eng and Sp sources.
# Child_512_Home_desamp_TOT_desc_Eng <- Child_512_Home_desamp_TOT_desc %>% 
#   filter(data == 'Eng') %>% 
#   setNames(., str_c(names(.), 'Eng', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# Child_512_Home_desamp_TOT_desc_Sp <- Child_512_Home_desamp_TOT_desc %>% 
#   filter(data == 'Sp') %>% 
#   setNames(., str_c(names(.), 'Sp', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# 
# Child_512_Home_desamp_TOT_desc_desamp <- Child_512_Home_desamp_TOT_desc_Eng %>% 
#   bind_cols(Child_512_Home_desamp_TOT_desc_Sp) %>% 
#   # select(-age_range_Sp) %>% 
#   # rename(age_range = age_range_Eng) %>% 
#   mutate(
#     diff = round(mean_Eng - mean_Sp, 2),
#     ES_diff = round((mean_Eng - mean_Sp) / ((sd_Eng + sd_Sp) / 2), 2)
#   )
# 
# write_csv(Child_512_Home_desamp_TOT_desc_desamp, here('OUTPUT-FILES/CHILD/DESCRIPTIVES/CHILD-512-Home-desamp-TOT-desc.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Child_512_Home_desamp_TOT_desc, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(Child_512_Home_desamp_TOT_desc$data)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "Data Sourcce", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


