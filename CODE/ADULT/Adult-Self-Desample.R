suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS


Adult_Self_Eng <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SM-QUAL-COMBO-NORMS-INPUT/Adult-Self-combo-norms-input.csv")
  ))) 
Adult_Self_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/SP-NORMS-INPUT/Adult-Self-Sp-norms-input.csv")
  ))) 
Adult_Self <- bind_rows(Adult_Self_Eng, Adult_Self_Sp) %>% 
  arrange(IDNumber)


# Subsample: 100 females with 4-yr college
set.seed(123)
Adult_Self_F_BA <- Adult_Self %>% 
  filter(Gender == "Female" & HighestEducation == "Bachelor's degree or higher" ) %>% 
  sample_n(100)

# Remove all females with BA from main sample
Adult_Self_not_F_BA <- Adult_Self %>% 
  filter(!(Gender == "Female" & HighestEducation == "Bachelor's degree or higher" ))

# combine subsamples to create desampled data set

Adult_Self_desamp <- bind_rows(Adult_Self_F_BA, Adult_Self_not_F_BA) %>% 
  arrange(IDNumber)

write_csv(Adult_Self_desamp, here('INPUT-FILES/ADULT/ALLDATA-DESAMP-NORMS-INPUT/Adult-Self-allData-desamp.csv'))

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


