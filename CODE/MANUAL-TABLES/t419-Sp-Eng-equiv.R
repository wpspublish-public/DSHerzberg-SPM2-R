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


# WRITE MANUAL TABLES -----------------------------------------------------

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")


write_csv(match_dist,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t419-Sp-Eng-matchDemos-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')



# write table comping t-score descriptives with ES
write_csv(IT_430_Home_match_t_desc,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t529a-IT-430-mixed-clin-ES-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
