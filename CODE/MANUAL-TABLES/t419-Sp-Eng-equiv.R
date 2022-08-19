# LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(MatchIt))
suppressMessages(library(tableone))
suppressMessages(library(knitr))
# ######### IT 430 HOME DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_49_Home_Eng_preMatch <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  mutate(lang_status = "Eng") %>% 
  select(IDNumber:Region, lang_status)

IT_1030_Home_Eng_preMatch <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  mutate(lang_status = "Eng") %>% 
  select(IDNumber:Region, lang_status)

IT_49_Home_Sp_preMatch <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-Sp-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-Sp-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)

IT_1030_Home_Sp_preMatch <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-Sp-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-Sp-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)

IT_430_Home_Eng_preMatch <- bind_rows(
  IT_49_Home_Eng_preMatch,
  IT_1030_Home_Eng_preMatch
) %>% arrange(IDNumber)

IT_430_Home_Sp_preMatch <- bind_rows(
  IT_49_Home_Sp_preMatch,
  IT_1030_Home_Sp_preMatch
) %>% arrange(IDNumber)

rm(list = ls(pattern = '49'))
rm(list = ls(pattern = '1030'))

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
IT_430_Home_Sp_Eng_preMatch <- bind_rows(
  IT_430_Home_Sp_preMatch,
  IT_430_Home_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(IT_430_Home_Sp_Eng_preMatch))
IT_430_Home_Sp_Eng_preMatch <- filter_all(
  IT_430_Home_Sp_Eng_preMatch, 
  all_vars(!is.na(.))
  )

# print table comparing distributions of two samples
preMatch_dist <-
  CreateTableOne(
    vars = c(
      'AgeInMonths',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    data = IT_430_Home_Sp_Eng_preMatch,
    factorVars = c(
      'AgeInMonths',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(777)
match <- matchit(
  Group ~ AgeInMonths + Gender + ParentHighestEducation + Ethnicity, 
  data = IT_430_Home_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
IT_430_Home_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    AgeInMonths <= 10 ~"4 to 10 mo",
    AgeInMonths >= 21 ~"21 to 30 mo",
    T ~ "11 to 20 mo"
  ))
IT_430_Home_Eng_match <- IT_430_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
IT_430_Home_Sp_match <- IT_430_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- IT_430_Home_Eng_match %>% 
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
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- IT_430_Home_Sp_match %>% 
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
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
IT_430_Home_match_dist <- bind_rows(match_dist_Sp,
                                    match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'IT 430 Home',
                           T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_49_Home_Eng <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv")
  )))
) %>% 
  select(-(q0010:q0119))

IT_1030_Home_Eng <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv")
  )))
) %>% 
  select(-(q0011:q0125))

IT_49_Home_Sp <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-Sp-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-Sp-T-Scores-per-case.csv")
  )))
) %>% 
  select(-(q0010:q0119))

IT_1030_Home_Sp <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-Sp-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-Sp-T-Scores-per-case.csv")
  )))
) %>% 
  select(-(q0011:q0125))

IT_430_Home_Eng <- bind_rows(
  IT_49_Home_Eng,
  IT_1030_Home_Eng
) %>% arrange(IDNumber)

IT_430_Home_Sp <- bind_rows(
  IT_49_Home_Sp,
  IT_1030_Home_Sp
) %>% arrange(IDNumber)

rm(list = ls(pattern = '49'))
rm(list = ls(pattern = '1030'))

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
IT_430_Home_matchEng <- IT_430_Home_Eng %>% 
  semi_join(IT_430_Home_Eng_match, by ='IDNumber') %>% 
  arrange(AgeInMonths, Gender, ParentHighestEducation, Ethnicity)

# Sort Sp cases on matching vars
IT_430_Home_Sp <- IT_430_Home_Sp %>% 
  arrange(AgeInMonths, Gender, ParentHighestEducation, Ethnicity)

cor_cols <- bind_cols(
  IT_430_Home_matchEng,
  IT_430_Home_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

IT_430_Home_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
         ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'IT 430 Home', 
    T ~ NA_character_)
    ) %>% 
  select(form, scale, r, p)
 
rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table'
  )))

# ######### IT CAREGIVER HOME DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_Caregiver_Eng_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Eng") %>% 
  select(IDNumber:Region, lang_status)

IT_Caregiver_Sp_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-Sp-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)


# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
IT_Caregiver_Sp_Eng_preMatch <- bind_rows(
  IT_Caregiver_Sp_preMatch,
  IT_Caregiver_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(IT_Caregiver_Sp_Eng_preMatch))
IT_Caregiver_Sp_Eng_preMatch <- filter_all(
  IT_Caregiver_Sp_Eng_preMatch, 
  all_vars(!is.na(.))
)

# print table comparing distributions of two samples
preMatch_dist <-
  CreateTableOne(
    vars = c(
      'AgeInMonths',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    data = IT_Caregiver_Sp_Eng_preMatch,
    factorVars = c(
      'AgeInMonths',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(777)
match <- matchit(
  Group ~ AgeInMonths + Gender + ParentHighestEducation + Ethnicity, 
  data = IT_Caregiver_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
IT_Caregiver_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    AgeInMonths <= 10 ~"4 to 10 mo",
    AgeInMonths >= 21 ~"21 to 30 mo",
    T ~ "11 to 20 mo"
  ))
IT_Caregiver_Eng_match <- IT_Caregiver_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
IT_Caregiver_Sp_match <- IT_Caregiver_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- IT_Caregiver_Eng_match %>% 
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
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- IT_Caregiver_Sp_match %>% 
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
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
IT_Caregiver_match_dist <- bind_rows(match_dist_Sp,
                                     match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'IT Caregiver',
                          T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_Caregiver_Eng <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case.csv")
  )))

IT_Caregiver_Sp <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-Sp-T-Scores-per-case.csv")
  )))

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
IT_Caregiver_matchEng <- IT_Caregiver_Eng %>% 
  semi_join(IT_Caregiver_Eng_match, by ='IDNumber') %>% 
  arrange(AgeInMonths, Gender, ParentHighestEducation, Ethnicity)

# Sort Sp cases on matching vars
IT_Caregiver_Sp <- IT_Caregiver_Sp %>% 
  arrange(AgeInMonths, Gender, ParentHighestEducation, Ethnicity)

cor_cols <- bind_cols(
  IT_Caregiver_matchEng,
  IT_Caregiver_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

IT_Caregiver_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'IT Caregiver', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table',
  'IT_Caregiver_match_dist', 'IT_Caregiver_cor_table'
)))


# ######### PRESCHOOL 25 HOME DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Preschool_25_Home_Eng_preMatch <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  mutate(lang_status = "Eng") %>% 
  select(IDNumber:Region, lang_status)

Preschool_25_Home_Sp_preMatch <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-Sp-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-Sp-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
Preschool_25_Home_Sp_Eng_preMatch <- bind_rows(
  Preschool_25_Home_Sp_preMatch,
  Preschool_25_Home_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Preschool_25_Home_Sp_Eng_preMatch))
Preschool_25_Home_Sp_Eng_preMatch <- filter_all(
  Preschool_25_Home_Sp_Eng_preMatch, 
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
    data = Preschool_25_Home_Sp_Eng_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(1111111)
match <- matchit(
  Group ~ Age + Gender + ParentHighestEducation + Ethnicity, 
  data = Preschool_25_Home_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
Preschool_25_Home_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    Age <= 4 ~ "2 to 4 years",
    TRUE ~ "5 years"))
Preschool_25_Home_Eng_match <- Preschool_25_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
Preschool_25_Home_Sp_match <- Preschool_25_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- Preschool_25_Home_Eng_match %>% 
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
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- Preschool_25_Home_Sp_match %>% 
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
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
Preschool_25_Home_match_dist <- bind_rows(match_dist_Sp,
                                          match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'Preschool 25 Home',
                          T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Preschool_25_Home_Eng <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

Preschool_25_Home_Sp<- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-Sp-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-Sp-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
Preschool_25_Home_matchEng <- Preschool_25_Home_Eng %>% 
  semi_join(Preschool_25_Home_Eng_match, by ='IDNumber') %>% 
  # arrange(Age, Gender, ParentHighestEducation, Ethnicity)
  arrange(ParentHighestEducation, Age, Gender, Ethnicity)

# Sort Sp cases on matching vars
Preschool_25_Home_Sp <- Preschool_25_Home_Sp %>% 
  # arrange(Age, Gender, ParentHighestEducation, Ethnicity)
  arrange(ParentHighestEducation, Age, Gender, Ethnicity)

cor_cols <- bind_cols(
  Preschool_25_Home_matchEng,
  Preschool_25_Home_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

Preschool_25_Home_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Preschool 25 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table',
  'IT_Caregiver_match_dist', 'IT_Caregiver_cor_table',
  'Preschool_25_Home_match_dist', 'Preschool_25_Home_cor_table'
)))

# ######### CHILD 512 HOME DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Child_512_Home_Eng_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Eng",
         dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(IDNumber:Region, lang_status, -dup)

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Child_512_Home_Eng_preMatch$IDNumber)

Child_512_Home_Sp_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-Sp-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
Child_512_Home_Sp_Eng_preMatch <- bind_rows(
  Child_512_Home_Sp_preMatch,
  Child_512_Home_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Child_512_Home_Sp_Eng_preMatch))
Child_512_Home_Sp_Eng_preMatch <- filter_all(
  Child_512_Home_Sp_Eng_preMatch, 
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
    data = Child_512_Home_Sp_Eng_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(980980909)
match <- matchit(
  Group ~ Age + Gender + ParentHighestEducation + Ethnicity, 
  data = Child_512_Home_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
Child_512_Home_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years"))
Child_512_Home_Eng_match <- Child_512_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
Child_512_Home_Sp_match <- Child_512_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- Child_512_Home_Eng_match %>% 
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
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- Child_512_Home_Sp_match %>% 
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
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
Child_512_Home_match_dist <- bind_rows(match_dist_Sp,
                                       match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'Child 512 Home',
                          T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Child_512_Home_Eng <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case.csv")
  ))) %>% 
  mutate(dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(-dup)

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Child_512_Home_Eng_preMatch$IDNumber)

Child_512_Home_Sp <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-Sp-T-Scores-per-case.csv")
  )))

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
Child_512_Home_matchEng <- Child_512_Home_Eng %>% 
  semi_join(Child_512_Home_Eng_match, by ='IDNumber') %>% 
  # arrange(Age, Gender, ParentHighestEducation, Ethnicity)
  arrange(ParentHighestEducation, Age, Gender, Ethnicity)

# Sort Sp cases on matching vars
Child_512_Home_Sp <- Child_512_Home_Sp %>% 
  # arrange(Age, Gender, ParentHighestEducation, Ethnicity)
  arrange(ParentHighestEducation, Age, Gender, Ethnicity)

cor_cols <- bind_cols(
  Child_512_Home_matchEng,
  Child_512_Home_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

Child_512_Home_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Child 512 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table',
  'IT_Caregiver_match_dist', 'IT_Caregiver_cor_table',
  'Preschool_25_Home_match_dist', 'Preschool_25_Home_cor_table',
  'Child_512_Home_match_dist', 'Child_512_Home_cor_table'
)))

# ######### TEEN 1221 HOME DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Home_Eng_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Eng",
         dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(IDNumber:Region, lang_status, -dup)

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Teen_1221_Home_Eng_preMatch$IDNumber)

Teen_1221_Home_Sp_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-Sp-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
Teen_1221_Home_Sp_Eng_preMatch <- bind_rows(
  Teen_1221_Home_Sp_preMatch,
  Teen_1221_Home_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Teen_1221_Home_Sp_Eng_preMatch))
Teen_1221_Home_Sp_Eng_preMatch <- filter_all(
  Teen_1221_Home_Sp_Eng_preMatch, 
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
    data = Teen_1221_Home_Sp_Eng_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(99999)
match <- matchit(
  Group ~ Age + Gender + ParentHighestEducation + Ethnicity, 
  data = Teen_1221_Home_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
Teen_1221_Home_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years"))
Teen_1221_Home_Eng_match <- Teen_1221_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
Teen_1221_Home_Sp_match <- Teen_1221_Home_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- Teen_1221_Home_Eng_match %>% 
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
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- Teen_1221_Home_Sp_match %>% 
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
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
Teen_1221_Home_match_dist <- bind_rows(match_dist_Sp,
                                       match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'Teen 1221 Home',
                          T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Home_Eng <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  ))) %>% 
  mutate(dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(-dup)

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Teen_1221_Home_Eng_preMatch$IDNumber)

Teen_1221_Home_Sp <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-Sp-T-Scores-per-case.csv")
  )))

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
Teen_1221_Home_matchEng <- Teen_1221_Home_Eng %>% 
  semi_join(Teen_1221_Home_Eng_match, by ='IDNumber') %>% 
  arrange(Age, Gender, ParentHighestEducation, Ethnicity)
# arrange(ParentHighestEducation, Age, Gender, Ethnicity)

# Sort Sp cases on matching vars
Teen_1221_Home_Sp <- Teen_1221_Home_Sp %>% 
  arrange(Age, Gender, ParentHighestEducation, Ethnicity)
# arrange(ParentHighestEducation, Age, Gender, Ethnicity)

cor_cols <- bind_cols(
  Teen_1221_Home_matchEng,
  Teen_1221_Home_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

Teen_1221_Home_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Teen 1221 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table',
  'IT_Caregiver_match_dist', 'IT_Caregiver_cor_table',
  'Preschool_25_Home_match_dist', 'Preschool_25_Home_cor_table',
  'Child_512_Home_match_dist', 'Child_512_Home_cor_table',
  'Teen_1221_Home_match_dist', 'Teen_1221_Home_cor_table'
)))

# ######### TEEN 1221 SELF DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Self_Eng_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Eng",
         dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(IDNumber:Region, lang_status, -dup)

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Teen_1221_Self_Eng_preMatch$IDNumber)

Teen_1221_Self_Sp_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-Sp-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
Teen_1221_Self_Sp_Eng_preMatch <- bind_rows(
  Teen_1221_Self_Sp_preMatch,
  Teen_1221_Self_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Teen_1221_Self_Sp_Eng_preMatch))
Teen_1221_Self_Sp_Eng_preMatch <- filter_all(
  Teen_1221_Self_Sp_Eng_preMatch, 
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
    data = Teen_1221_Self_Sp_Eng_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'ParentHighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(49)
match <- matchit(
  Group ~ Age + Gender + ParentHighestEducation + Ethnicity, 
  data = Teen_1221_Self_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
Teen_1221_Self_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years"))
Teen_1221_Self_Eng_match <- Teen_1221_Self_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
Teen_1221_Self_Sp_match <- Teen_1221_Self_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- Teen_1221_Self_Eng_match %>% 
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
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- Teen_1221_Self_Sp_match %>% 
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
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
Teen_1221_Self_match_dist <- bind_rows(match_dist_Sp,
                                       match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'Teen 1221 Self',
                          T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Self_Eng <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  ))) %>% 
  mutate(dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(-dup)

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Teen_1221_Self_Eng_preMatch$IDNumber)

Teen_1221_Self_Sp <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-Sp-T-Scores-per-case.csv")
  )))

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
Teen_1221_Self_matchEng <- Teen_1221_Self_Eng %>% 
  semi_join(Teen_1221_Self_Eng_match, by ='IDNumber') %>% 
  arrange(Age, Gender, ParentHighestEducation, Ethnicity)
# arrange(ParentHighestEducation, Age, Gender, Ethnicity)

# Sort Sp cases on matching vars
Teen_1221_Self_Sp <- Teen_1221_Self_Sp %>% 
  arrange(Age, Gender, ParentHighestEducation, Ethnicity)
# arrange(ParentHighestEducation, Age, Gender, Ethnicity)

cor_cols <- bind_cols(
  Teen_1221_Self_matchEng,
  Teen_1221_Self_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

Teen_1221_Self_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Teen 1221 Self', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table',
  'IT_Caregiver_match_dist', 'IT_Caregiver_cor_table',
  'Preschool_25_Home_match_dist', 'Preschool_25_Home_cor_table',
  'Child_512_Home_match_dist', 'Child_512_Home_cor_table',
  'Teen_1221_Home_match_dist', 'Teen_1221_Home_cor_table',
  'Teen_1221_Self_match_dist', 'Teen_1221_Self_cor_table'
)))

# ######### ADULT SELF DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Adult_Self_Eng_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Eng",
         dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(IDNumber:Region, lang_status, -dup) %>% 
  filter_all(
    all_vars(!is.na(.))
  )

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Adult_Self_Eng_preMatch$IDNumber)

Adult_Self_Sp_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-Sp-T-Scores-per-case.csv")
  ))) %>% 
  mutate(lang_status = "Sp") %>% 
  select(IDNumber:Region, lang_status) %>% 
  filter_all(
    all_vars(!is.na(.))
  )

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Adult_Self_Sp_preMatch$IDNumber)

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures Eng
# vs Sp status
Adult_Self_Sp_Eng_preMatch <- bind_rows(
  Adult_Self_Sp_preMatch,
  Adult_Self_Eng_preMatch 
) %>% 
  mutate(Group = case_when(
    lang_status == 'Sp' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, remove rows with NA using next filter_all()
# line (but be cognizant of how many cases you are dropping and how this might
# affect the resulting matched sample)
sum(is.na(Adult_Self_Sp_Eng_preMatch))
Adult_Self_Sp_Eng_preMatch <- filter_all(
  Adult_Self_Sp_Eng_preMatch, 
  all_vars(!is.na(.))
)

# print table comparing distributions of two samples
preMatch_dist <-
  CreateTableOne(
    vars = c(
      'Age',
      'Gender',
      'HighestEducation',
      'Ethnicity'
    ),
    data = Adult_Self_Sp_Eng_preMatch,
    factorVars = c(
      'Age',
      'Gender',
      'HighestEducation',
      'Ethnicity'
    ),
    strata = 'lang_status'
  )

table <- print(preMatch_dist,
               printToggle = FALSE,
               noSpaces = TRUE)

kable(table[, 1:3],
      align = 'c',
      caption = 'Table 1: Comparison of pre-matched samples')

# run matchit to get 1:1 matching
set.seed(1234)
match <- matchit(
  Group ~ Age + Gender + HighestEducation + Ethnicity, 
  data = Adult_Self_Sp_Eng_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# print table showing that sample sizes are identical
kable(match_summ$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Matched sample sizes')

# print table showing distribution of demo variables in matching samples
kable(match_summ$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df; split by lang_status
Adult_Self_Sp_Eng_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) %>% 
  mutate(age_range = case_when(
    Age <= 30 ~ "21.00 to 30.99 years",
    between(Age, 31, 40) ~ "31.00 to 40.99 years",
    between(Age, 41, 50) ~ "41.00 to 50.99 years",
    between(Age, 51, 64) ~ "51.00 to 64.99 years",
    TRUE ~ "65.00 to 99.99 years"))
Adult_Self_Eng_match <- Adult_Self_Sp_Eng_match %>% 
  filter(lang_status == 'Eng')
Adult_Self_Sp_match <- Adult_Self_Sp_Eng_match %>% 
  filter(lang_status == 'Sp')

source(here('CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R'))

match_dist_Eng <- Adult_Self_Eng_match %>% 
  select(age_range, Gender, HighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "HighestEducation" & Variable == "HighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Matched Eng sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Sp <- Adult_Self_Sp_match %>% 
  select(age_range, Gender, HighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "HighestEducation" & Variable == "HighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    # lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    Variable == 'age_range' ~ 'Sp sample',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched English, Spanish demo counts.
Adult_Self_match_dist <- bind_rows(match_dist_Sp,
                                   match_dist_Eng) %>%
  mutate(form = case_when(group == 'Sp sample' ~ 'Adult Self',
                          T ~ NA_character_)) %>%
  select(form, everything())

# RE-ASSEMBLE AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Adult_Self_Eng <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv")
  ))) %>% 
  mutate(dup = duplicated(IDNumber)
  ) %>% 
  filter(dup != TRUE) %>% 
  select(-dup) %>% 
  filter_at(
    vars(IDNumber:Region),
    ~ !is.na(.)
  )

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID
# encountered, returns 0 if no dups)
anyDuplicated(Adult_Self_Eng_preMatch$IDNumber)

Adult_Self_Sp <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-Sp-T-Scores-per-case.csv")
  ))) %>% 
  filter_at(
    vars(IDNumber:Region),
    ~ !is.na(.)
  )

# CORRELATE SP ENG T-SCORES -------------------------------------

# Extract match cases from Eng sample, sort on matching vars
Adult_Self_matchEng <- Adult_Self_Eng %>% 
  semi_join(Adult_Self_Eng_match, by ='IDNumber') %>% 
  # arrange(Age, Gender, HighestEducation, Ethnicity)
  arrange(HighestEducation, Age, Gender, Ethnicity)

# Sort Sp cases on matching vars
Adult_Self_Sp <- Adult_Self_Sp %>% 
  # arrange(Age, Gender, HighestEducation, Ethnicity)
  arrange(HighestEducation, Age, Gender, Ethnicity)

cor_cols <- bind_cols(
  Adult_Self_matchEng,
  Adult_Self_Sp
) %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT-TOT_NT1', 'SOC_NT-SOC_NT1', 'VIS_NT-VIS_NT1', 
             'HEA_NT-HEA_NT1', 'TOU_NT-TOU_NT1', 'TS_NT-TS_NT1', 
             'BOD_NT-BOD_NT1', 'BAL_NT-BAL_NT1', 'PLA_NT-PLA_NT1')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

Adult_Self_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>%
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Adult Self', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

rm(list = setdiff(ls(), c(
  'IT_430_Home_match_dist', 'IT_430_Home_cor_table',
  'IT_Caregiver_match_dist', 'IT_Caregiver_cor_table',
  'Preschool_25_Home_match_dist', 'Preschool_25_Home_cor_table',
  'Child_512_Home_match_dist', 'Child_512_Home_cor_table',
  'Teen_1221_Home_match_dist', 'Teen_1221_Home_cor_table',
  'Teen_1221_Self_match_dist', 'Teen_1221_Self_cor_table',
  'Adult_Self_match_dist', 'Adult_Self_cor_table'
)))


# WRITE MANUAL TABLES -----------------------------------------------------

# demo counts
match_dist <- bind_rows(
  IT_430_Home_match_dist, 
  IT_Caregiver_match_dist, 
  Preschool_25_Home_match_dist,
  Child_512_Home_match_dist, 
  Teen_1221_Home_match_dist, 
  Teen_1221_Self_match_dist, 
  Adult_Self_match_dist
)

write_csv(match_dist,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t419-Sp-Eng-matchDemos-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

# Sp-Eng correlations
cor_table <- bind_rows(
  IT_430_Home_cor_table, 
  IT_Caregiver_cor_table, 
  Preschool_25_Home_cor_table,
  Child_512_Home_cor_table, 
  Teen_1221_Home_cor_table, 
  Teen_1221_Self_cor_table, 
  Adult_Self_cor_table
)

write_csv(cor_table,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t419-Sp-Eng-equiv-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
