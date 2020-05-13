###### LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(MatchIt))
### HOME DATA ---------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Home_SPD_Stand_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Stand-All-T-scores-per-case.csv")
  )))


Home_SPD_Clin_preMatch <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Clin-All-T-scores-per-case.csv")
  ))) %>% 
  filter(clin_dx == "SPD")

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures clin
# vs typ status
Home_SPD_Clin_Stand_preMatch <- bind_rows(
  Home_SPD_Clin_preMatch,
  Home_SPD_Stand_preMatch 
) %>% 
  mutate(Group = case_when(
    clin_status == 'clin' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, recode all NA to 999
sum(is.na(Home_SPD_Clin_Stand_preMatch))

# identify cols with NA
na_cols <- Home_SPD_Clin_Stand_preMatch %>% select_if(~ any(is.na(.)))

# in NA cols, replace NA with 999
Home_SPD_Clin_Stand_preMatch <- Home_SPD_Clin_Stand_preMatch %>%
  replace_na(
    list(
      IDNumber = 999,
      AgeInMonths = 999,
      Age = 999,
      clin_dx = "999",
      ParentHighestEducation = "999",
      HighestEducation = "999",
      Region = "999"
    ))

# run matchit to get 1:1 matching
set.seed(39485703)
match <- matchit(
  Group ~ age_range + Gender + ParentHighestEducation + Ethnicity, 
  data = Home_SPD_Clin_Stand_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# save matched samples into new df; split by clin_status
Home_SPD_Clin_Stand_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) #%>% 
Home_SPD_Stand_match <- Home_SPD_Clin_Stand_match %>% 
  filter(clin_status == 'typ')
Home_SPD_Clin_match <- Home_SPD_Clin_Stand_match %>% 
  filter(clin_status == 'clin')

# demo counts
source(here("CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R"))

match_dist_Stand <- Home_SPD_Stand_match %>% 
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
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Clin <- Home_SPD_Clin_match %>% 
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
    rownames(.) == "1" ~ 'SPD',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched typical, clinical demo counts.
match_dist_Home <- bind_rows(
  match_dist_Clin,
  match_dist_Stand
) %>% 
  mutate(form_dx = case_when(
    rownames(.) == "1" ~ 'Home-SPD',
    T ~ NA_character_
  )) %>% 
  select(form_dx, everything())

rm(list=setdiff(ls(), c("Home_SPD_Stand_match", "match_dist_Home")))

# RE-READ FINALIZED SAMPLES --------------------------------------------------

Home_SPD_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Stand-All-T-scores-per-case.csv")
  )))

Home_SPD_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Clin-All-T-scores-per-case.csv")
  ))) %>% 
  filter(clin_dx == "SPD")

# COMPARE MATCHED SAMPLES ON T-SCORES -------------------------------------

# Extract match cases from stand sample
Home_SPD_matchStand <- Home_SPD_Stand %>% 
  semi_join(Home_SPD_Stand_match, by ='IDNumber')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# Write matched typical t-score descriptives
Home_SPD_matchStand_t_desc <-
  Home_SPD_matchStand %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd)

# Write clinical t-score descriptives
Home_SPD_clin_t_desc <-
  Home_SPD_Clin %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'SPD',
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_clin = n,
         mean_clin = mean,
         sd_clin = sd)

# Combine stand, clin columns, add ES column

Home_SPD_match_t_desc <- bind_cols(Home_SPD_matchStand_t_desc,
                                   Home_SPD_clin_t_desc) %>%
  mutate(ES = abs((mean_typ - mean_clin) / ((sd_typ + sd_clin / 2))),
         form_dx = case_when(
           rownames(.) == "1" ~ 'Home-SPD',
           T ~ NA_character_
         )
  ) %>%
  mutate_at(vars(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
              (round(., 2))) %>%
  select(form_dx, everything(), -sample, -sample1)

rm(list = setdiff(ls(), c('match_dist_Home', 'Home_SPD_match_t_desc')))

### SCHOOL DATA ---------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

School_SPD_Stand_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Stand-All-T-scores-per-case.csv")
  )))


School_SPD_Clin_preMatch <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Clin-All-T-scores-per-case.csv")
  ))) %>% 
  filter(clin_dx == "SPD")

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures clin
# vs typ status
School_SPD_Clin_Stand_preMatch <- bind_rows(
  School_SPD_Clin_preMatch,
  School_SPD_Stand_preMatch 
) %>% 
  mutate(Group = case_when(
    clin_status == 'clin' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, recode all NA to 999
sum(is.na(School_SPD_Clin_Stand_preMatch))

# identify cols with NA
na_cols <- School_SPD_Clin_Stand_preMatch %>% select_if(~ any(is.na(.)))

# in NA cols, replace NA with 999
School_SPD_Clin_Stand_preMatch <- School_SPD_Clin_Stand_preMatch %>%
  replace_na(
    list(
      IDNumber = 999,
      AgeInMonths = 999,
      Age = 999,
      clin_dx = "999",
      ParentHighestEducation = "999",
      HighestEducation = "999",
      Region = "999"
    ))

# run matchit to get 1:1 matching
set.seed(4)
match <- matchit(
  Group ~ age_range + Gender + Ethnicity, 
  data = School_SPD_Clin_Stand_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# save matched samples into new df; split by clin_status
School_SPD_Clin_Stand_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) #%>% 
School_SPD_Stand_match <- School_SPD_Clin_Stand_match %>% 
  filter(clin_status == 'typ')
School_SPD_Clin_match <- School_SPD_Clin_Stand_match %>% 
  filter(clin_status == 'clin')

# demo counts
source(here("CODE/VAR-CAT-ORDER/var-cat-order-demo-tables.R"))

match_dist_Stand <- School_SPD_Stand_match %>% 
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
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_Clin <- School_SPD_Clin_match %>% 
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
    rownames(.) == "1" ~ 'SPD',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched typical, clinical demo counts.
match_dist_School <- bind_rows(
  match_dist_Clin,
  match_dist_Stand
) %>% 
  mutate(form_dx = case_when(
    rownames(.) == "1" ~ 'School-SPD',
    T ~ NA_character_
  )) %>% 
  select(form_dx, everything())

# RE-READ FINALIZED SAMPLES --------------------------------------------------

School_SPD_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Stand-All-T-scores-per-case.csv")
  )))

School_SPD_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Clin-All-T-scores-per-case.csv")
  ))) %>% 
  filter(clin_dx == "SPD")

# COMPARE MATCHED SAMPLES ON T-SCORES -------------------------------------

# Extract match cases from stand sample
School_SPD_matchStand <- School_SPD_Stand %>% 
  semi_join(School_SPD_Stand_match, by ='IDNumber')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# Write matched typical t-score descriptives
School_SPD_matchStand_t_desc <-
  School_SPD_matchStand %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd)

# Write clinical t-score descriptives
School_SPD_clin_t_desc <-
  School_SPD_Clin %>% 
  select(contains('_NT')) %>% 
  rename_all(~ str_sub(., 1, -4)) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'SPD',
    T ~ NA_character_
  )) %>% 
  select(sample, scale, n, mean, sd) %>% 
  rename(n_clin = n,
         mean_clin = mean,
         sd_clin = sd)

# Combine stand, clin columns, add ES column

School_SPD_match_t_desc <- bind_cols(School_SPD_matchStand_t_desc,
                                     School_SPD_clin_t_desc) %>%
  mutate(ES = abs((mean_typ - mean_clin) / ((sd_typ + sd_clin / 2))),
         form_dx = case_when(
           rownames(.) == "1" ~ 'School-SPD',
           T ~ NA_character_
         )
  ) %>%
  mutate_at(vars(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
              (round(., 2))) %>%
  select(form_dx, everything(), -sample, -sample1)

rm(list = setdiff(ls(), c(
  'match_dist_Home', 'Home_SPD_match_t_desc',
  'match_dist_School', 'School_SPD_match_t_desc'
)))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------------

match_dist <- bind_rows(
  match_dist_Home,
  match_dist_School,
)

write_csv(match_dist,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t526a-SPD-matchDemos-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

SPD_match_t_desc <- bind_rows(
  Home_SPD_match_t_desc,
  School_SPD_match_t_desc
)

# write table comping t-score descriptives with ES
write_csv(SPD_match_t_desc,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t526a-SPD-ES-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
