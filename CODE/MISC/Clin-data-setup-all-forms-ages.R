###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

### READ CLIN T-SCORES PER CASE FOR ALL FORMS, AGES ------------------------------

# HOME
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Clin.R"))

# SCHOOL
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Clin.R"))

# SELF
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Self-Clin.R"))

# OTHER
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Clin.R"))

rm(list = ls(pattern = "items"))

### PARSE COLUMNS -----------------------------------------------------------

NT_order <- scale_order %>% str_c(., "_NT")

# HOME

IT_49_Home_Clin_common <- IT_49_Home_Clin %>% 
  mutate(Age = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

IT_1030_Home_Clin_common <- IT_1030_Home_Clin %>% 
  mutate(Age = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

Preschool_25_Home_Clin_common <- Preschool_25_Home_Clin %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

Child_512_Home_Clin_common <- Child_512_Home_Clin %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

Teen_1221_Home_Clin_common <- Teen_1221_Home_Clin %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

# SCHOOL

Preschool_25_School_Clin_common <- Preschool_25_School_Clin %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

Child_512_School_Clin_common <- Child_512_School_Clin %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

Teen_1221_School_Clin_common <- Teen_1221_School_Clin %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

# SELF

Teen_1221_Self_Clin_common <- Teen_1221_Self_Clin %>% 
  mutate(AgeInMonths = NA_real_, HighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

Adult_Self_Clin_common <- Adult_Self_Clin %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

# OTHER

Adult_Other_Clin_common <- Adult_Other_Clin %>% 
  mutate(AgeInMonths = NA_real_, ParentHighestEducation = NA_character_) %>% 
  select(IDNumber, clin_status, clin_dx, AgeInMonths, Age, Gender, ParentHighestEducation, HighestEducation, 
         Ethnicity, Region, NT_order)

rm(list = setdiff(ls(), ls(pattern = "common")))

### ASSEMBLE AND WRITE COMBINED T-SCORES-PER-CASE FILES ---------------------

write_csv(
  bind_rows(
    IT_49_Home_Clin_common,
    IT_1030_Home_Clin_common,
    Preschool_25_Home_Clin_common,
    Child_512_Home_Clin_common,
    Teen_1221_Home_Clin_common
  ),
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Clin-All-T-scores-per-case.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    Preschool_25_School_Clin_common,
    Child_512_School_Clin_common,
    Teen_1221_School_Clin_common
  ),
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Clin-All-T-scores-per-case.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    Teen_1221_Self_Clin_common,
    Adult_Self_Clin_common
  ),
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Self-Clin-All-T-scores-per-case.csv"
  ),
  na = ""
)

write_csv(
    Adult_Other_Clin_common,
  here(
    "OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Other-Clin-All-T-scores-per-case.csv"
  ),
  na = ""
)
