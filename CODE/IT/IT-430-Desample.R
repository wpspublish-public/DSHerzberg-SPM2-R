suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# Desampling is applied first to the Home form sample, the excluded cases are
# then also removed from the other form samples

IT_49_Home_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv")
  ))) %>% 
  select(-(contains("q0")))
IT_1030_Home_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv")
  ))) %>% 
  select(-(contains("q0")))
IT_430_Home_Eng <- bind_rows(IT_49_Home_Eng, IT_1030_Home_Eng)


IT_49_Home_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SP-NORMS-INPUT/IT-49-Home-Sp-norms-input.csv")
  ))) %>% 
  select(-(contains("q0")))
IT_1030_Home_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SP-NORMS-INPUT/IT-1030-Home-Sp-norms-input.csv")
  ))) %>% 
  select(-(contains("q0")))
IT_430_Home_Sp <- bind_rows(IT_49_Home_Sp, IT_1030_Home_Sp)

IT_49_Home_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/INHOUSE-NORMS-INPUT/IT-49-Home-inHouse-norms-input.csv")
  ))) %>% 
  select(-(contains("q0")))
IT_1030_Home_inHouse <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/INHOUSE-NORMS-INPUT/IT-1030-Home-inHouse-norms-input.csv")
  ))) %>% 
  select(-(contains("q0")))
IT_430_Home_inHouse <- bind_rows(IT_49_Home_inHouse, IT_1030_Home_inHouse)

IT_430_Home <- bind_rows(IT_430_Home_Eng, IT_430_Home_Sp, IT_430_Home_inHouse) %>% 
  arrange(IDNumber)

# STAGE 1 DESAMPLE --------------------------------------------------------

# Subsample: 212 Whites with four year college
set.seed(123)
IT_430_Home_White_BA <- IT_430_Home %>% 
  filter(Ethnicity == "White" & 
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(212)

# Remove all Whites with four year college from main sample
IT_430_Home_not_White_BA <- IT_430_Home %>% 
  filter(!(Ethnicity == "White" & 
             ParentHighestEducation == "Bachelor's degree or higher"))
  
# combine subsamples to create desampled data set

IT_430_Home_desamp1 <- bind_rows(IT_430_Home_White_BA, IT_430_Home_not_White_BA) %>% 
  arrange(IDNumber)

# STAGE 2 DESAMPLE --------------------------------------------------------

# Subsample: 11 Hisp South with BA+
set.seed(123)
IT_430_Home_Hisp_South_BAplus <- IT_430_Home_desamp1 %>% 
  filter(Ethnicity == "Hispanic" & 
           Region == "south" &
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(11)

# Subsample: 14 MultiRacial South with BA+
set.seed(123)
IT_430_Home_Multi_South_BAplus <- IT_430_Home_desamp1 %>% 
  filter(Ethnicity == "MultiRacial" & 
           Region == "south" &
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(14)

# Subsample: 75 White South with BA+
set.seed(123)
IT_430_Home_White_South_BAplus <- IT_430_Home_desamp1 %>% 
  filter(Ethnicity == "White" & 
           Region == "south" &
           ParentHighestEducation == "Bachelor's degree or higher") %>% 
  sample_n(75)

# Combine 3 subsamples

IT_430_Home_stage2 <- bind_rows(
  IT_430_Home_Hisp_South_BAplus,
  IT_430_Home_Multi_South_BAplus,
  IT_430_Home_White_South_BAplus
) %>% 
  arrange(IDNumber)

IT_430_Home_desamp <- IT_430_Home_desamp1 %>% anti_join(IT_430_Home_stage2, by = 'IDNumber')

# WRITE FINAL DESAMPLE FILE -----------------------------------------------
write_csv(IT_430_Home_desamp, here('INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-430-Home-allData-desamp.csv'))

# EXAMINE DATA---------------------------------

# Create frequency tables for TOT_raw by data source
IT_430_Home_desamp_TOT_freq <- IT_430_Home_desamp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(IT_430_Home_desamp_TOT_freq, here('OUTPUT-FILES/IT/FREQUENCIES/IT-430-Home-Eng-Sp-TOT-freq-data.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by age range
IT_430_Home_desamp_TOT_desc_age <-
  IT_430_Home_desamp %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2)#,
         # group = c(1:2)
  )

write_csv(IT_430_Home_desamp_TOT_desc_age, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-430-Home-desamp-TOT-desc-age.csv'))

# # Generate single table comparing descriptives from Eng and Sp sources.
# IT_430_Home_desamp_TOT_desc_Eng <- IT_430_Home_desamp_TOT_desc %>% 
#   filter(data == 'Eng') %>% 
#   setNames(., str_c(names(.), 'Eng', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# IT_430_Home_desamp_TOT_desc_Sp <- IT_430_Home_desamp_TOT_desc %>% 
#   filter(data == 'Sp') %>% 
#   setNames(., str_c(names(.), 'Sp', sep = '_')) %>% 
#   ungroup() %>% 
#   select(-matches('data|median|group'))
# 
# IT_430_Home_desamp_TOT_desc_desamp <- IT_430_Home_desamp_TOT_desc_Eng %>% 
#   bind_cols(IT_430_Home_desamp_TOT_desc_Sp) %>% 
#   # select(-age_range_Sp) %>% 
#   # rename(age_range = age_range_Eng) %>% 
#   mutate(
#     diff = round(mean_Eng - mean_Sp, 2),
#     ES_diff = round((mean_Eng - mean_Sp) / ((sd_Eng + sd_Sp) / 2), 2)
#   )
# 
# write_csv(IT_430_Home_desamp_TOT_desc_desamp, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-430-Home-desamp-TOT-desc.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = IT_430_Home_desamp_TOT_desc, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(IT_430_Home_desamp_TOT_desc$data)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "Data Sourcce", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


