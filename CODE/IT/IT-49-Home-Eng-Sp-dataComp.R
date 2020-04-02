suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023", 
                          "q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                          "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                          "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                          "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                          "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                          "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105", 
                          "q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")


TOT_items_IT_49_Home <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038", 
                          "q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050", 
                          "q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066", 
                          "q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078", 
                          "q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091", 
                          "q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105")

SOC_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023")

SOC_rev_items_IT_49_Home <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017", "q0019", "q0023")

VIS_items_IT_49_Home <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035", "q0037", "q0038")

HEA_items_IT_49_Home <- c("q0039", "q0040", "q0042", "q0044", "q0045", "q0046", "q0047", "q0048", "q0049", "q0050")

TOU_items_IT_49_Home <- c("q0052", "q0053", "q0054", "q0055", "q0057", "q0058", "q0061", "q0063", "q0065", "q0066")

TS_items_IT_49_Home <- c("q0067", "q0068", "q0070", "q0072", "q0073", "q0074", "q0075", "q0076", "q0077", "q0078")

BOD_items_IT_49_Home <- c("q0080", "q0081", "q0083", "q0084", "q0085", "q0087", "q0088", "q0089", "q0090", "q0091")

BAL_items_IT_49_Home <- c("q0092", "q0093", "q0095", "q0097", "q0098", "q0100", "q0101", "q0102", "q0104", "q0105")

PLA_items_IT_49_Home <- c("q0109", "q0110", "q0111", "q0113", "q0114", "q0115", "q0116", "q0117", "q0118", "q0119")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

IT_49_Home_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_49_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  # mutate_at(
  #   All_items_IT_49_Home,
  #   ~ case_when(
  #     .x == "Never" ~ 1,
  #     .x == "Occasionally" ~ 2,
  #     .x == "Frequently" ~ 3,
  #     .x == "Always" ~ 4,
  #     TRUE ~ NA_real_
  #   )
  # ) %>%
  # # recode reverse-scored items
  # mutate_at(
  #   SOC_rev_items_IT_49_Home,
  #   ~ case_when(.x == 4 ~ 1,
  #               .x == 3 ~ 2,
  #               .x == 2 ~ 3,
  #               .x == 1 ~ 4,
  #               TRUE ~ NA_real_)
  # ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_IT_49_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 6 ~ "3.5 to 6 mo",
    TRUE ~ "7 to 10.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_IT_49_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_49_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_49_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_49_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_49_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_49_Home]),
    TS_raw = rowSums(.[TS_items_IT_49_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_49_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_49_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_49_Home])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = "Eng") %>% 
  select(IDNumber, data, everything()) %>% 
# Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200)

# Prep file to comp
IT_49_Home_scores <- IT_49_Home_items %>% 
  select(
    -(All_items_IT_49_Home), -(Gender:Region)
  )   


# READ IN SPANISH DATA

IT_49_Home_Sp <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/SPM-2 InfantToddler 49 Months-Sp.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_49_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_IT_49_Home,
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
    SOC_rev_items_IT_49_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # recode gender and educ
  mutate(
    Gender = case_when(
      Gender == "Masculino" ~ "Male",
      Gender == "Femenino" ~ "Female",
      TRUE ~ NA_character_
    ),
    ParentHighestEducation = case_when(
      ParentHighestEducation == "No terminé la escuela secundaria (no obtuve el diploma)" ~ "Did not complete high school (no diploma)",
      ParentHighestEducation == "Graduado/a de secundaria (incluye diploma de educación general o GED)" ~ "High school graduate (including GED)",
      ParentHighestEducation == "Alguna educación superior o grado asociado (associate degree)" ~ "Some college or associate degree",
      ParentHighestEducation == "Licenciatura o grado más alto" ~ "Bachelor's degree or higher",
      TRUE ~ NA_character_
    )
  ) %>% 
  # Convert scored item vars to integers
  mutate_at(All_items_IT_49_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 6 ~ "3.5 to 6 mo",
    TRUE ~ "7 to 10.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_IT_49_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_49_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_49_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_49_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_49_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_49_Home]),
    TS_raw = rowSums(.[TS_items_IT_49_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_49_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_49_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_49_Home])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = "Sp") %>% 
  select(IDNumber, data, everything()) %>% 
  # Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  # filter(TOT_raw < 200)
  filter(TOT_raw < 120)
# Save trimmed file 
write_csv(IT_49_Home_Sp, here(
  'INPUT-FILES/IT/SP-NORMS-INPUT/IT-49-Home-Sp-norms-input.csv'
))

# Prep file to comp
IT_49_Home_Sp_scores <- IT_49_Home_Sp %>% 
  select(
    -(All_items_IT_49_Home), -(Gender:Region)
  )   

# clean up environment
rm(list = ls(pattern='.*items_IT_49_Home'))

IT_49_Home_comp <- bind_rows(IT_49_Home_scores, IT_49_Home_Sp_scores) 

# EXAMINE DATA---------------------------------

# Create frequency tables for TOT_raw by data source
IT_49_Home_comp_TOT_freq_data <- IT_49_Home_comp %>% group_by(data, age_range) %>% count(TOT_raw) %>%
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# write_csv(IT_49_Home_comp_TOT_freq_data, here('OUTPUT-FILES/IT/FREQUENCIES/IT-49-Home-Eng-Sp-TOT-freq-data.csv'))


# Compute descriptive statistics, effect sizes for TOT_raw by data source
IT_49_Home_comp_TOT_desc_data <-
  IT_49_Home_comp %>% group_by(data) %>% arrange(data) %>% summarise(n = n(),
                                                                        median = round(median(TOT_raw), 2),
                                                                        mean = round(mean(TOT_raw), 2),
                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2),
         group = c(1:2)
  )

write_csv(IT_49_Home_comp_TOT_desc_data, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-49-Home-Eng-Sp-TOT-desc-dataSource.csv'))

# Generate single table comparing descriptives from Eng and Sp sources.
IT_49_Home_comp_TOT_desc_Eng <- IT_49_Home_comp_TOT_desc_data %>% 
  filter(data == 'Eng') %>% 
  setNames(., str_c(names(.), 'Eng', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))
IT_49_Home_comp_TOT_desc_Sp <- IT_49_Home_comp_TOT_desc_data %>% 
  filter(data == 'Sp') %>% 
  setNames(., str_c(names(.), 'Sp', sep = '_')) %>% 
  ungroup() %>% 
  select(-matches('data|median|group'))

IT_49_Home_comp_TOT_desc_comp <- IT_49_Home_comp_TOT_desc_Eng %>% 
  bind_cols(IT_49_Home_comp_TOT_desc_Sp) %>% 
  # select(-age_range_Sp) %>% 
  # rename(age_range = age_range_Eng) %>% 
  mutate(
    diff = round(mean_Eng - mean_Sp, 2),
    ES_diff = round((mean_Eng - mean_Sp) / ((sd_Eng + sd_Sp) / 2), 2)
  )

write_csv(IT_49_Home_comp_TOT_desc_comp, here('OUTPUT-FILES/IT/DESCRIPTIVES/IT-49-Home-Eng-Sp-TOT-desc-comp.csv'))

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = IT_49_Home_comp_TOT_desc_data, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 2, 1), labels = unique(IT_49_Home_comp_TOT_desc_data$data)) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "Data Sourcce", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)


