###### LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(MatchIt))
suppressMessages(library(tableone))
suppressMessages(library(knitr))
### HOME DATA ---------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Home_Stand_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:Region, clin_status)

Teen_1221_Home_Clin_preMatch <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:Region, clin_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures clin
# vs typ status
Teen_1221_Home_Clin_Stand_preMatch <- bind_rows(
  Teen_1221_Home_Clin_preMatch,
  Teen_1221_Home_Stand_preMatch 
) %>% 
  mutate(Group = case_when(
    clin_status == 'clin' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Teen_1221_Home_Clin_Stand_preMatch))
Teen_1221_Home_Clin_Stand_preMatch <- filter_all(
  Teen_1221_Home_Clin_Stand_preMatch, 
  all_vars(!is.na(.))
  )

# print table comparing distributions of two samples
preMatch_dist <-
  CreateTableOne(
    vars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    data = Teen_1221_Home_Clin_Stand_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'clin_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(123)
match <- matchit(
  Group ~ Age + Gender + ParentHighestEducation + Ethnicity, 
  data = Teen_1221_Home_Clin_Stand_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by clin_status
Teen_1221_Home_Clin_Stand_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) #%>% 
  # mutate(age_range = case_when(
  #   Age <= 10 ~"4 to 10 mo",
  #   Age >= 21 ~"21 to 30 mo",
  #   T ~ "11 to 20 mo"
  # ))
Teen_1221_Home_Stand_match <- Teen_1221_Home_Clin_Stand_match %>% 
  filter(clin_status == 'typ')
Teen_1221_Home_Clin_match <- Teen_1221_Home_Clin_Stand_match %>% 
  filter(clin_status == 'clin')

# examine distributions of demographic vars
var_order <- c("data", "age_range", "Age", "AgeInMonths", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")
cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt",
  # AgeInMonths
  NA, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
  '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
  # age_range
  NA, "3.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "7 to 10.5 mo", "09.5 to 20 mo",  
  "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
  "21 to 31.5 mo", "5 to 8 years", "9 to 12 years", "12 to 13 years", "14 to 15 years", 
  "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")

match_dist_Stand <- Teen_1221_Home_Stand_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Matched typical sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Clin <- Teen_1221_Home_Clin_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Clinical sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched typical, clinical demo counts.
match_dist_Home <- bind_rows(
  match_dist_Clin,
  match_dist_Stand
) %>% 
  mutate(form = case_when(
    group == 'Clinical sample' ~ 'Home',
    T ~ NA_character_
  )) %>% 
  select(form, everything())

rm(list=setdiff(ls(), c("Teen_1221_Home_Stand_match", "match_dist_Home")))

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Home_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  )))

Teen_1221_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv")
  )))

# COMPARE MATCHED SAMPLES ON T-SCORES -------------------------------------

# Extract match cases from stand sample
Teen_1221_Home_matchStand <- Teen_1221_Home_Stand %>% 
  semi_join(Teen_1221_Home_Stand_match, by ='IDNumber')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# Write matched typical t-score descriptives
Teen_1221_Home_matchStand_t_desc <-
  Teen_1221_Home_matchStand %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    scale =="SOC" ~ "Matched Typical",
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd)

# Write clinical t-score descriptives
Teen_1221_Home_clin_t_desc <-
  Teen_1221_Home_Clin %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    scale =="SOC" ~ "Mixed Clinical",
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_clin = n,
         mean_clin = mean,
         sd_clin = sd)

# Combine stand, clin columns, add ES column

Teen_1221_Home_match_t_desc <- bind_cols(Teen_1221_Home_matchStand_t_desc,
                                      Teen_1221_Home_clin_t_desc) %>%
  mutate(ES = abs((mean_typ - mean_clin) / ((sd_typ + sd_clin / 2))),
         form = case_when(
           scale == "SOC" ~ "Home",
           T ~ NA_character_
         )
  ) %>%
  mutate_at(vars(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
              (round(., 2))) %>%
  select(form, everything(), -sample, -sample1)

rm(list = setdiff(ls(), c('match_dist_Home', 'Teen_1221_Home_match_t_desc')))

### SCHOOL DATA -------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_School_Stand_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:Region, clin_status)

Teen_1221_School_Clin_preMatch <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:Region, clin_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures clin
# vs typ status
Teen_1221_School_Clin_Stand_preMatch <- bind_rows(
  Teen_1221_School_Clin_preMatch,
  Teen_1221_School_Stand_preMatch 
) %>% 
  mutate(Group = case_when(
    clin_status == 'clin' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Teen_1221_School_Clin_Stand_preMatch))
Teen_1221_School_Clin_Stand_preMatch <- filter_all(
  Teen_1221_School_Clin_Stand_preMatch, 
  all_vars(!is.na(.))
)

# print table comparing distributions of two samples
preMatch_dist <-
  CreateTableOne(
    vars = c(
      'Age',
      'Gender',
      'Ethnicity'
    ),
    data = Teen_1221_School_Clin_Stand_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'Ethnicity'
    ),
    strata = 'clin_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(123)
match <- matchit(
  Group ~ Age + Gender + Ethnicity, 
  data = Teen_1221_School_Clin_Stand_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by clin_status
Teen_1221_School_Clin_Stand_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) #%>% 
# mutate(age_range = case_when(
#   Age <= 10 ~"4 to 10 mo",
#   Age >= 21 ~"21 to 30 mo",
#   T ~ "11 to 20 mo"
# ))
Teen_1221_School_Stand_match <- Teen_1221_School_Clin_Stand_match %>% 
  filter(clin_status == 'typ')
Teen_1221_School_Clin_match <- Teen_1221_School_Clin_Stand_match %>% 
  filter(clin_status == 'clin')

# examine distributions of demographic vars
var_order <- c("data", "age_range", "Age", "AgeInMonths", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")
cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt",
  # AgeInMonths
  NA, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
  '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
  # age_range
  NA, "3.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "7 to 10.5 mo", "09.5 to 20 mo",  
  "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
  "21 to 31.5 mo", "5 to 8 years", "9 to 12 years", "12 to 13 years", "14 to 15 years", 
  "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")

match_dist_Stand <- Teen_1221_School_Stand_match %>% 
  select(age_range, Gender, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    # lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Matched typical sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Clin <- Teen_1221_School_Clin_match %>% 
  select(age_range, Gender, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    # lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Clinical sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched typical, clinical demo counts.
match_dist_School <- bind_rows(
  match_dist_Clin,
  match_dist_Stand
) %>% 
  mutate(form = case_when(
    group == 'Clinical sample' ~ 'School',
    T ~ NA_character_
  )) %>% 
  select(form, everything())

rm(list = setdiff(ls(), 
                  c("Teen_1221_School_Stand_match", "match_dist_School", 
                    "match_dist_Home", "Teen_1221_Home_match_t_desc")))

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_School_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
  )))

Teen_1221_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv")
  )))

# COMPARE MATCHED SAMPLES ON T-SCORES -------------------------------------

# Extract match cases from stand sample
Teen_1221_School_matchStand <- Teen_1221_School_Stand %>% 
  semi_join(Teen_1221_School_Stand_match, by ='IDNumber')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# Write matched typical t-score descriptives
Teen_1221_School_matchStand_t_desc <-
  Teen_1221_School_matchStand %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    scale =="SOC" ~ "Matched Typical",
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd)

# Write clinical t-score descriptives
Teen_1221_School_clin_t_desc <-
  Teen_1221_School_Clin %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    scale =="SOC" ~ "Mixed Clinical",
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_clin = n,
         mean_clin = mean,
         sd_clin = sd)

# Combine stand, clin columns, add ES column

Teen_1221_School_match_t_desc <- bind_cols(Teen_1221_School_matchStand_t_desc,
                                              Teen_1221_School_clin_t_desc) %>%
  mutate(ES = abs((mean_typ - mean_clin) / ((sd_typ + sd_clin / 2))),
         form = case_when(
          scale == "SOC" ~ "School",
          T ~ NA_character_
         )
  ) %>%
  mutate_at(vars(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
              (round(., 2))) %>%
  select(form, everything(), -sample, -sample1)

rm(list = setdiff(ls(), c(
  'match_dist_Home', 'Teen_1221_Home_match_t_desc',
  'match_dist_School', 'Teen_1221_School_match_t_desc'
)))

### SELF DATA ---------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Self_Stand_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:Region, clin_status)

Teen_1221_Self_Clin_preMatch <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:Region, clin_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures clin
# vs typ status
Teen_1221_Self_Clin_Stand_preMatch <- bind_rows(
  Teen_1221_Self_Clin_preMatch,
  Teen_1221_Self_Stand_preMatch 
) %>% 
  mutate(Group = case_when(
    clin_status == 'clin' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Teen_1221_Self_Clin_Stand_preMatch))
Teen_1221_Self_Clin_Stand_preMatch <- filter_all(
  Teen_1221_Self_Clin_Stand_preMatch, 
  all_vars(!is.na(.))
)

# print table comparing distributions of two samples
preMatch_dist <-
  CreateTableOne(
    vars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    data = Teen_1221_Self_Clin_Stand_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'clin_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(123)
match <- matchit(
  Group ~ Age + Gender + ParentHighestEducation + Ethnicity, 
  data = Teen_1221_Self_Clin_Stand_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by clin_status
Teen_1221_Self_Clin_Stand_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) #%>% 
# mutate(age_range = case_when(
#   Age <= 10 ~"4 to 10 mo",
#   Age >= 21 ~"21 to 30 mo",
#   T ~ "11 to 20 mo"
# ))
Teen_1221_Self_Stand_match <- Teen_1221_Self_Clin_Stand_match %>% 
  filter(clin_status == 'typ')
Teen_1221_Self_Clin_match <- Teen_1221_Self_Clin_Stand_match %>% 
  filter(clin_status == 'clin')

# examine distributions of demographic vars
var_order <- c("data", "age_range", "Age", "AgeInMonths", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")
cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt",
  # AgeInMonths
  NA, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
  '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
  # age_range
  NA, "3.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "7 to 10.5 mo", "09.5 to 20 mo",  
  "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
  "21 to 31.5 mo", "5 to 8 years", "9 to 12 years", "12 to 13 years", "14 to 15 years", 
  "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")

match_dist_Stand <- Teen_1221_Self_Stand_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Matched typical sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Clin <- Teen_1221_Self_Clin_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Clinical sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched typical, clinical demo counts.
match_dist_Self <- bind_rows(
  match_dist_Clin,
  match_dist_Stand
) %>% 
  mutate(form = case_when(
    group == 'Clinical sample' ~ 'Self',
    T ~ NA_character_
  )) %>% 
  select(form, everything())

rm(list = setdiff(ls(), c(
  'match_dist_Home', 'Teen_1221_Home_match_t_desc',
  'match_dist_School', 'Teen_1221_School_match_t_desc',
  'match_dist_Self','Teen_1221_Self_Stand_match'
)))

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Self_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  )))

Teen_1221_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv")
  )))

# COMPARE MATCHED SAMPLES ON T-SCORES -------------------------------------

# Extract match cases from stand sample
Teen_1221_Self_matchStand <- Teen_1221_Self_Stand %>% 
  semi_join(Teen_1221_Self_Stand_match, by ='IDNumber')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# Write matched typical t-score descriptives
Teen_1221_Self_matchStand_t_desc <-
  Teen_1221_Self_matchStand %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    scale =="SOC" ~ "Matched Typical",
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd)

# Write clinical t-score descriptives
Teen_1221_Self_clin_t_desc <-
  Teen_1221_Self_Clin %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    scale =="SOC" ~ "Mixed Clinical",
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_clin = n,
         mean_clin = mean,
         sd_clin = sd)

# Combine stand, clin columns, add ES column

Teen_1221_Self_match_t_desc <- bind_cols(Teen_1221_Self_matchStand_t_desc,
                                         Teen_1221_Self_clin_t_desc) %>%
  mutate(ES = abs((mean_typ - mean_clin) / ((sd_typ + sd_clin / 2))),
         form = case_when(
           scale == "SOC" ~ "Self",
           T ~ NA_character_
         )
  ) %>%
  mutate_at(vars(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
              (round(., 2))) %>%
  select(form, everything(), -sample, -sample1)

# WRITE MANUAL TABLE OUTPUT -----------------------------------------------------

match_dist <- bind_rows(
  match_dist_Home,
  match_dist_School,
  match_dist_Self
)

write_csv(match_dist,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t529d-Teen-1221-mixed-clin-matchDemos-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

Teen_1221_match_t_desc <- bind_rows(
  Teen_1221_Home_match_t_desc,
  Teen_1221_School_match_t_desc,
  Teen_1221_Self_match_t_desc
)

# write table comping t-score descriptives with ES
write_csv(Teen_1221_match_t_desc,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t529d-Teen-1221-mixed-clin-ES-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
