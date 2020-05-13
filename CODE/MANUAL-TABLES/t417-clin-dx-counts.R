###### LOAD PACKAGES -----------------------------------------------------------
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))

#### IT 430 HOME DATA -----------------------------------------------------
# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_49_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

IT_1030_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

IT_430_Home_Clin <- bind_rows(
  IT_49_Home_Clin,
  IT_1030_Home_Clin
) %>% 
  drop_na(clin_dx) %>% 
  arrange(IDNumber)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

IT_430_clin_dx_counts <- IT_430_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'IT 430',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(IT_430_Home_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

IT_430_clin_dx_cases <- IT_430_Home_Clin %>%
  mutate(HighestEducation = NA,
         form = "IT-430-Home",
         ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -AgeInMonths)

rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))

#### IT CAREGIVER DATA -----------------------------------------------------
# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_Caregiver_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

IT_Caregiver_clin_dx_counts <- IT_Caregiver_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'IT Caregiver',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(IT_Caregiver_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

IT_Caregiver_clin_dx_cases <- IT_Caregiver_Clin %>%
  mutate(HighestEducation = NA,
         form = "IT-Caregiver",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -AgeInMonths)

rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))

#### PRESCHOOL 25  DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Preschool_24_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-clin-T-Scores-per-case.csv")
  )))

Preschool_5_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-clin-T-Scores-per-case.csv")
  )))

Preschool_25_Home_Clin <- bind_rows(
  Preschool_24_Home_Clin,
  Preschool_5_Home_Clin
) %>% 
  drop_na(clin_dx) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

Preschool_24_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-clin-T-Scores-per-case.csv")
  )))

Preschool_5_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-clin-T-Scores-per-case.csv")
  )))

Preschool_25_School_Clin <- bind_rows(
  Preschool_24_School_Clin,
  Preschool_5_School_Clin
) %>% 
  drop_na(clin_dx) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Preschool_25_Home_clin_dx <- Preschool_25_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Preschool 25 Home',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Preschool_25_Home_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Preschool_25_School_clin_dx <- Preschool_25_School_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Preschool 25 School',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Preschool_25_School_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Preschool_25_clin_dx_counts <- bind_rows(
  Preschool_25_Home_clin_dx,
  Preschool_25_School_clin_dx
)

Preschool_25_Home_clin_dx_cases <- Preschool_25_Home_Clin %>%
  mutate(HighestEducation = NA,
         form = "Preschool-25-Home",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Preschool_25_School_clin_dx_cases <- Preschool_25_School_Clin %>%
  mutate(ParentHighestEducation = NA,
         HighestEducation = NA,
         form = "Preschool-25-School",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Preschool_25_clin_dx_cases <- bind_rows(
  Preschool_25_Home_clin_dx_cases,
  Preschool_25_School_clin_dx_cases
)

rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))

#### CHILD 512  DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Child_512_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

Child_512_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Child_512_Home_clin_dx <- Child_512_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Child 512 Home',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Child_512_Home_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Child_512_School_clin_dx <- Child_512_School_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Child 512 School',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Child_512_School_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Child_512_clin_dx_counts <- bind_rows(
  Child_512_Home_clin_dx,
  Child_512_School_clin_dx
)

Child_512_Home_clin_dx_cases <- Child_512_Home_Clin %>%
  mutate(HighestEducation = NA,
         form = "Child-512-Home",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Child_512_School_clin_dx_cases <- Child_512_School_Clin %>%
  mutate(ParentHighestEducation = NA,
         HighestEducation = NA,
         form = "Child-512-School",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Child_512_clin_dx_cases <- bind_rows(
  Child_512_Home_clin_dx_cases,
  Child_512_School_clin_dx_cases
)

rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))

#### TEEN 1221  DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

Teen_1221_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

Teen_1221_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Teen_1221_Home_clin_dx <- Teen_1221_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Teen 1221 Home',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_Home_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Teen_1221_School_clin_dx <- Teen_1221_School_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Teen 1221 School',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_School_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Teen_1221_Self_clin_dx <- Teen_1221_Self_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Teen 1221 Self',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_Self_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Teen_1221_clin_dx_counts <- bind_rows(
  Teen_1221_Home_clin_dx,
  Teen_1221_School_clin_dx,
  Teen_1221_Self_clin_dx
)

Teen_1221_Home_clin_dx_cases <- Teen_1221_Home_Clin %>%
  mutate(HighestEducation = NA,
         form = "Teen-1221-Home",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Teen_1221_School_clin_dx_cases <- Teen_1221_School_Clin %>%
  mutate(ParentHighestEducation = NA,
         HighestEducation = NA,
         form = "Teen-1221-School",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Teen_1221_Self_clin_dx_cases <- Teen_1221_Self_Clin %>%
  mutate(HighestEducation = NA,
         form = "Teen-1221-Self",
  ) %>% 
  select(IDNumber, form, age_range:ParentHighestEducation, HighestEducation, everything(), -Age)

Teen_1221_clin_dx_cases <- bind_rows(
  Teen_1221_Home_clin_dx_cases,
  Teen_1221_School_clin_dx_cases,
  Teen_1221_Self_clin_dx_cases
)

rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))

#### ADULT DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Adult_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

Adult_Other_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Adult_Self_clin_dx <- Adult_Self_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Adult Self',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Adult_Self_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Adult_Other_clin_dx <- Adult_Other_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Adult Other',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Adult_Other_Clin)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)

Adult_clin_dx_counts <- bind_rows(
  Adult_Self_clin_dx,
  Adult_Other_clin_dx
)

Adult_Self_clin_dx_cases <- Adult_Self_Clin %>%
  mutate(ParentHighestEducation = NA,
         form = "Adult-Self",
  ) %>% 
  select(IDNumber, form, age_range:Gender, ParentHighestEducation, HighestEducation, everything(), -Age)

Adult_Other_clin_dx_cases <- Adult_Other_Clin %>%
  mutate(ParentHighestEducation = NA,
         form = "Adult-Other",
  ) %>% 
  select(IDNumber, form, age_range:Gender, ParentHighestEducation, HighestEducation, everything(), -Age)

Adult_clin_dx_cases <- bind_rows(
  Adult_Self_clin_dx_cases,
  Adult_Other_clin_dx_cases,
)

rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))

#### ALL AGES - UNIQUE CASES -----------------------------------------------------

# READ ALL CLINICAL CASES --------------------------------------------------

source(here("CODE/READ-T-SCORES-PER-CASE/read-Clin-all-forms-ages.R"))

# extract cases with Dup ID numbers
all_clin_dupIDs <- Clin_all_forms_ages %>%
  mutate(dup = duplicated(IDNumber)) %>%
  filter(dup == TRUE) %>%
  select(IDNumber) %>% 
  arrange(IDNumber)

# remove dupID cases
# Clin_all_forms_ages_noDups <- Clin_all_forms_ages %>% 
#   semi_join(all_clin_dupIDs, by = "IDNumber") %>% 
#   arrange(IDNumber)

# among all clinical cases, keep only first instance of IDNumber
Clin_all_forms_ages_noDups <- Clin_all_forms_ages %>%
  arrange(IDNumber) %>% 
  filter((IDNumber != lag(IDNumber) | is.na(lag(IDNumber))))

# Write unique clin cases for analysis
write_csv(
  Clin_all_forms_ages_noDups,
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Clin-all-uniqueIDs-T-scores-per-case.csv"
  ),
  na = ""
)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

all_clin_uniqueIDs_dx_counts <- Clin_all_forms_ages_noDups %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ 'All Unique Clinical Cases',
                     T ~ NA_character_),
    pct_samp = round(((n / nrow(Clin_all_forms_ages_noDups)) * 100), 1)
  ) %>%
  select(form, dx, n, pct_samp)


rm(list = setdiff(ls(), ls(pattern = 'counts|cases')))



###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

# write table of combined matched typical, clinical demo counts.
clin_dx_counts <- bind_rows(
  IT_430_clin_dx_counts,
  IT_Caregiver_clin_dx_counts,
  Preschool_25_clin_dx_counts,
  Child_512_clin_dx_counts,
  Teen_1221_clin_dx_counts,
  Adult_clin_dx_counts
  )

write_csv(clin_dx_counts,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t417-clin-dx-counts-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


write_csv(all_clin_uniqueIDs_dx_counts,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t417-clin-dx-counts-all-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


