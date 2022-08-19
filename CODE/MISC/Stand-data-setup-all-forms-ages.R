###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

### READ STAND T-SCORES PER CASE FOR ALL FORMS, AGES ------------------------------

# HOME
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))

# SCHOOL
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))

# SELF
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Self-Stand.R"))

# OTHER
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Stand.R"))

rm(list = ls(pattern = "items"))

### PARSE COLUMNS -----------------------------------------------------------

NT_order <- scale_order %>% str_c(., "_NT")

# HOME

IT_49_Home_Stand_common <- IT_49_Home_Stand %>% 
  mutate(Age = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

IT_1030_Home_Stand_common <- IT_1030_Home_Stand %>% 
  mutate(Age = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

Preschool_25_Home_Stand_common <- Preschool_25_Home_Stand %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

Child_512_Home_Stand_common <- Child_512_Home_Stand %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

Teen_1221_Home_Stand_common <- Teen_1221_Home_Stand %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

# SCHOOL

Preschool_25_School_Stand_common <- Preschool_25_School_Stand %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

Child_512_School_Stand_common <- Child_512_School_Stand %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

Teen_1221_School_Stand_common <- Teen_1221_School_Stand %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

# SELF

Teen_1221_Self_Stand_common <- Teen_1221_Self_Stand %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

Adult_Self_Stand_common <- Adult_Self_Stand %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

# OTHER

Adult_Other_Stand_common <- Adult_Other_Stand %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, age_range, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, all_of(NT_order))

rm(list = setdiff(ls(), ls(pattern = "common")))

### ASSEMBLE AND WRITE COMBINED T-SCORES-PER-CASE FILES ---------------------

write_csv(
  bind_rows(
    IT_49_Home_Stand_common,
    IT_1030_Home_Stand_common,
    Preschool_25_Home_Stand_common,
    Child_512_Home_Stand_common,
    Teen_1221_Home_Stand_common
  ),
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Stand-All-T-scores-per-case.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    Preschool_25_School_Stand_common,
    Child_512_School_Stand_common,
    Teen_1221_School_Stand_common
  ),
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Stand-All-T-scores-per-case.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    Teen_1221_Self_Stand_common,
    Adult_Self_Stand_common
  ),
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Self-Stand-All-T-scores-per-case.csv"
  ),
  na = ""
)

write_csv(
    Adult_Other_Stand_common,
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Other-Stand-All-T-scores-per-case.csv"
  ),
  na = ""
)

