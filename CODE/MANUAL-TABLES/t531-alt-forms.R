###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
## IT-1030-Home DATA -------------------------------------------------------

# Read, score form1 data
source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

IT_1030_Home_alt_form1_IT_1030_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALT/SPM-2 InfantToddler 1030 Months Alt-form1.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_IT_1030_Home)
  ) %>%
  mutate_at(
    All_items_IT_1030_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_IT_1030_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_IT_1030_Home,
            ~ as.integer(.x)) %>% 
  mutate(age_range = case_when(
    AgeInMonths <= 20 ~ "09.5 to 20 mo",
    TRUE ~ "21 to 31.5 mo")
  ) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_1030_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_1030_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_1030_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_1030_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_1030_Home]),
    TS_raw = rowSums(.[TS_items_IT_1030_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_1030_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_1030_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_1030_Home])
  ) %>% 
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# Read, score form2 data
source(here('CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R'))

IT_1030_Home_alt_form2_IT_49_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALT/SPM-2 InfantToddler 1030 Months Alt-form2.csv")
  ))) %>% 
  select(IDNumber, StandForm, AgeInMonths, all_of(All_items_IT_49_Home)) %>%
  mutate_at(
    All_items_IT_49_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_IT_49_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_IT_49_Home,
            ~ as.integer(.x)) %>% 
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
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

IT_1030_Home_alt_form12_raw <- IT_1030_Home_alt_form1_IT_1030_Home %>% 
  left_join(IT_1030_Home_alt_form2_IT_49_Home, by = "IDNumber") %>% 
  mutate(form = "IT-1030-Home-with-IT-49-Home",
         Educ = NA_character_
  ) %>% 
  select(IDNumber, form, age_range:Gender, Educ, everything())

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Preschool-25-Home DATA --------------------------------------------------

# Read, score form1 data
source(here('CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R'))

Preschool_25_Home_alt_form1_Preschool_25_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/PRESCHOOL/ALT/SPM-2 Preschooler ages 25 Home Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Preschool_25_Home)
  ) %>%
  mutate_at(
    All_items_Preschool_25_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Preschool_25_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Preschool_25_Home,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(Age <= 4 ~ "2 to 4 years",
                               TRUE ~ "5 years")) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_Home]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_Home]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_Home]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_Home]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_Home]),
    TS_raw = rowSums(.[TS_items_Preschool_25_Home]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_Home]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_Home]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_Home])
  ) %>%
  mutate(data = 'alt',
         clin_status = 'typ',
         clin_dx = NA) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))


# Read, score form2 data
source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

Preschool_25_Home_alt_form2_IT_1030_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/PRESCHOOL/ALT/SPM-2 Preschooler ages 25 Home Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_IT_1030_Home)
  ) %>%
  mutate_at(
    All_items_IT_1030_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_IT_1030_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_IT_1030_Home,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_IT_1030_Home]),
    SOC_raw = rowSums(.[SOC_items_IT_1030_Home]),
    VIS_raw = rowSums(.[VIS_items_IT_1030_Home]),
    HEA_raw = rowSums(.[HEA_items_IT_1030_Home]),
    TOU_raw = rowSums(.[TOU_items_IT_1030_Home]),
    TS_raw = rowSums(.[TS_items_IT_1030_Home]),
    BOD_raw = rowSums(.[BOD_items_IT_1030_Home]),
    BAL_raw = rowSums(.[BAL_items_IT_1030_Home]),
    PLA_raw = rowSums(.[PLA_items_IT_1030_Home])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Preschool_25_Home_alt_form12_raw <- Preschool_25_Home_alt_form1_Preschool_25_Home %>% 
  left_join(Preschool_25_Home_alt_form2_IT_1030_Home, by = "IDNumber") %>% 
  mutate(form = "Preschool-25-Home-with-IT-1030-Home") %>% 
  select(IDNumber, form, everything()) %>% 
  rename(Educ = ParentHighestEducation)

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Child-512-Home DATA -----------------------------------------------------

# read, score form1 data
source(here('CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R'))

Child_512_Home_alt_form1_Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 Home Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Child_512_Home)
  ) %>%
  mutate_at(
    All_items_Child_512_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Child_512_Home,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years")
  ) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# read, score form2 data
source(here('CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R'))

Child_512_Home_alt_form2_Preschool_25_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 Home Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_Preschool_25_Home)
  ) %>%
  mutate_at(
    All_items_Preschool_25_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Preschool_25_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Preschool_25_Home,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_Home]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_Home]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_Home]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_Home]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_Home]),
    TS_raw = rowSums(.[TS_items_Preschool_25_Home]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_Home]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_Home]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_Home])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Child_512_Home_alt_form12_raw <- Child_512_Home_alt_form1_Child_512_Home %>% 
  left_join(Child_512_Home_alt_form2_Preschool_25_Home, by = "IDNumber") %>% 
  mutate(form = "Child-512-Home-with-Preschool-25-Home") %>% 
  select(IDNumber, form, everything()) %>% 
  rename(Educ = ParentHighestEducation)

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Child-512-School DATA -----------------------------------------------------

# read, score form1 data
source(here('CODE/ITEM-VECTORS/Child-512-School-item-vectors.R'))

Child_512_School_alt_form1_Child_512_School <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 School Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Child_512_School)
  ) %>%
  mutate_at(
    All_items_Child_512_School,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Child_512_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Child_512_School,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years")
  ) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_School]),
    SOC_raw = rowSums(.[SOC_items_Child_512_School]),
    VIS_raw = rowSums(.[VIS_items_Child_512_School]),
    HEA_raw = rowSums(.[HEA_items_Child_512_School]),
    TOU_raw = rowSums(.[TOU_items_Child_512_School]),
    TS_raw = rowSums(.[TS_items_Child_512_School]),
    BOD_raw = rowSums(.[BOD_items_Child_512_School]),
    BAL_raw = rowSums(.[BAL_items_Child_512_School]),
    PLA_raw = rowSums(.[PLA_items_Child_512_School])
  ) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# read, score form2 data
source(here('CODE/ITEM-VECTORS/Preschool-25-School-item-vectors.R'))

Child_512_School_alt_form2_Preschool_25_School <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 School Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_Preschool_25_School)
  ) %>%
  mutate_at(
    All_items_Preschool_25_School,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Preschool_25_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Preschool_25_School,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Preschool_25_School]),
    SOC_raw = rowSums(.[SOC_items_Preschool_25_School]),
    VIS_raw = rowSums(.[VIS_items_Preschool_25_School]),
    HEA_raw = rowSums(.[HEA_items_Preschool_25_School]),
    TOU_raw = rowSums(.[TOU_items_Preschool_25_School]),
    TS_raw = rowSums(.[TS_items_Preschool_25_School]),
    BOD_raw = rowSums(.[BOD_items_Preschool_25_School]),
    BAL_raw = rowSums(.[BAL_items_Preschool_25_School]),
    PLA_raw = rowSums(.[PLA_items_Preschool_25_School])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Child_512_School_alt_form12_raw <- Child_512_School_alt_form1_Child_512_School %>% 
  left_join(Child_512_School_alt_form2_Preschool_25_School, by = "IDNumber") %>% 
  mutate(form = "Child-512-School-with-Preschool-25-School",
         Educ = NA_character_
         ) %>% 
  select(IDNumber, form, age_range:Gender, Educ, everything())

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Teen-1221-Home DATA -----------------------------------------------------

# read, score form1 data
source(here('CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R'))

Teen_1221_Home_alt_form1_Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 Home Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Teen_1221_Home)
  ) %>%
  mutate_at(
    All_items_Teen_1221_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Teen_1221_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Teen_1221_Home,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years")
  ) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_Home]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_Home]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_Home]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_Home]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_Home]),
    TS_raw = rowSums(.[TS_items_Teen_1221_Home]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_Home]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_Home]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_Home])
  ) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# read, score form2 data
source(here('CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R'))

Teen_1221_Home_alt_form2_Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 Home Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_Child_512_Home)
  ) %>%
  mutate_at(
    All_items_Child_512_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Child_512_Home,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Teen_1221_Home_alt_form12_raw <- Teen_1221_Home_alt_form1_Teen_1221_Home %>% 
  left_join(Teen_1221_Home_alt_form2_Child_512_Home, by = "IDNumber") %>% 
  mutate(form = "Teen-1221-Home-with-Child-512-Home") %>% 
  select(IDNumber, form, everything()) %>% 
  rename(Educ = ParentHighestEducation)

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Teen-1221-School DATA -----------------------------------------------------

# read, score form1 data
source(here('CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R'))

Teen_1221_School_alt_form1_Teen_1221_School <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 School Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Teen_1221_School)
  ) %>%
  mutate_at(
    All_items_Teen_1221_School,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Teen_1221_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Teen_1221_School,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(
    Age <= 13 ~ "12 to 13 years",
    between(Age, 14, 15) ~ "14 to 15 years",
    between(Age, 16, 17) ~ "16 to 17 years",
    TRUE ~ "18 to 21 years")
  ) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_School]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_School]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_School]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_School]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_School]),
    TS_raw = rowSums(.[TS_items_Teen_1221_School]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_School]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_School]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_School])
  ) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# read, score form2 data
source(here('CODE/ITEM-VECTORS/Child-512-School-item-vectors.R'))

Teen_1221_School_alt_form2_Child_512_School <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 School Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_Child_512_School)
  ) %>%
  mutate_at(
    All_items_Child_512_School,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Child_512_School,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Child_512_School,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_School]),
    SOC_raw = rowSums(.[SOC_items_Child_512_School]),
    VIS_raw = rowSums(.[VIS_items_Child_512_School]),
    HEA_raw = rowSums(.[HEA_items_Child_512_School]),
    TOU_raw = rowSums(.[TOU_items_Child_512_School]),
    TS_raw = rowSums(.[TS_items_Child_512_School]),
    BOD_raw = rowSums(.[BOD_items_Child_512_School]),
    BAL_raw = rowSums(.[BAL_items_Child_512_School]),
    PLA_raw = rowSums(.[PLA_items_Child_512_School])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Teen_1221_School_alt_form12_raw <- Teen_1221_School_alt_form1_Teen_1221_School %>% 
  left_join(Teen_1221_School_alt_form2_Child_512_School, by = "IDNumber") %>% 
  mutate(form = "Teen-1221-School-with-Child-512-School",
         Educ = NA_character_
  ) %>% 
  select(IDNumber, form, age_range:Gender, Educ, everything())

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Adult-Self DATA -----------------------------------------------------

# read, score form1 data
source(here('CODE/ITEM-VECTORS/Adult-Self-item-vectors.R'))

Adult_Self_alt_form1_Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Self-Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Adult_Self)
  ) %>%
  mutate_at(
    All_items_Adult_Self,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Adult_Self,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Adult_Self,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(
    Age <= 30 ~ "21.00 to 30.99 years",
    between(Age, 31, 40) ~ "31.00 to 40.99 years",
    between(Age, 41, 50) ~ "41.00 to 50.99 years",
    between(Age, 51, 64) ~ "51.00 to 64.99 years",
    TRUE ~ "65.00 to 99.99 years")) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Self]),
    SOC_raw = rowSums(.[SOC_items_Adult_Self]),
    VIS_raw = rowSums(.[VIS_items_Adult_Self]),
    HEA_raw = rowSums(.[HEA_items_Adult_Self]),
    TOU_raw = rowSums(.[TOU_items_Adult_Self]),
    TS_raw = rowSums(.[TS_items_Adult_Self]),
    BOD_raw = rowSums(.[BOD_items_Adult_Self]),
    BAL_raw = rowSums(.[BAL_items_Adult_Self]),
    PLA_raw = rowSums(.[PLA_items_Adult_Self])
  ) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# read, score form2 data
source(here('CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R'))

Adult_Self_alt_form2_Teen_1221_Self <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Self-Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_Teen_1221_Self)
  ) %>%
  mutate_at(
    All_items_Teen_1221_Self,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Teen_1221_Self,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Teen_1221_Self,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_Self]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_Self]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_Self]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_Self]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_Self]),
    TS_raw = rowSums(.[TS_items_Teen_1221_Self]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_Self]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_Self]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_Self])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Adult_Self_alt_form12_raw <- Adult_Self_alt_form1_Adult_Self %>% 
  left_join(Adult_Self_alt_form2_Teen_1221_Self, by = "IDNumber") %>% 
  mutate(form = "Adult-Self-with-Teen-1221-Self") %>% 
  select(IDNumber, form, everything()) %>% 
  rename(Educ = HighestEducation)

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

## Adult-Other DATA -----------------------------------------------------

# read, score form1 data
source(here('CODE/ITEM-VECTORS/Adult-Other-item-vectors.R'))

Adult_Other_alt_form1_Adult_Other <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Other Report Questionnaire Alt-form1.csv"
    )
  ))) %>%
  select(
    IDNumber,
    Age,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    all_of(All_items_Adult_Other)
  ) %>%
  mutate_at(
    All_items_Adult_Other,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Adult_Other,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Adult_Other,
            ~ as.integer(.x)) %>%
  mutate(age_range = case_when(
    Age <= 30 ~ "21.00 to 30.99 years",
    between(Age, 31, 40) ~ "31.00 to 40.99 years",
    between(Age, 41, 50) ~ "41.00 to 50.99 years",
    between(Age, 51, 64) ~ "51.00 to 64.99 years",
    TRUE ~ "65.00 to 99.99 years")) %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Other]),
    SOC_raw = rowSums(.[SOC_items_Adult_Other]),
    VIS_raw = rowSums(.[VIS_items_Adult_Other]),
    HEA_raw = rowSums(.[HEA_items_Adult_Other]),
    TOU_raw = rowSums(.[TOU_items_Adult_Other]),
    TS_raw = rowSums(.[TS_items_Adult_Other]),
    BOD_raw = rowSums(.[BOD_items_Adult_Other]),
    BAL_raw = rowSums(.[BAL_items_Adult_Other]),
    PLA_raw = rowSums(.[PLA_items_Adult_Other])
  ) %>%
  select(IDNumber, age_range, Gender:Region, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt1"))

# read, score form2 data
source(here('CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R'))

Adult_Other_alt_form2_Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Other Report Questionnaire Alt-form2.csv"
    )
  ))) %>%
  select(
    IDNumber,
    all_of(All_items_Teen_1221_Home)
  ) %>%
  mutate_at(
    All_items_Teen_1221_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate_at(
    SOC_rev_items_Teen_1221_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate_at(All_items_Teen_1221_Home,
            ~ as.integer(.x)) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Teen_1221_Home]),
    SOC_raw = rowSums(.[SOC_items_Teen_1221_Home]),
    VIS_raw = rowSums(.[VIS_items_Teen_1221_Home]),
    HEA_raw = rowSums(.[HEA_items_Teen_1221_Home]),
    TOU_raw = rowSums(.[TOU_items_Teen_1221_Home]),
    TS_raw = rowSums(.[TS_items_Teen_1221_Home]),
    BOD_raw = rowSums(.[BOD_items_Teen_1221_Home]),
    BAL_raw = rowSums(.[BAL_items_Teen_1221_Home]),
    PLA_raw = rowSums(.[PLA_items_Teen_1221_Home])
  ) %>%
  select(IDNumber, contains("_raw")) %>%
  rename_at(vars(contains("_raw")), ~ str_c(., "_alt2"))

Adult_Other_alt_form12_raw <- Adult_Other_alt_form1_Adult_Other %>% 
  left_join(Adult_Other_alt_form2_Teen_1221_Home, by = "IDNumber") %>% 
  mutate(form = "Adult-Other-with-Teen-1221-Home") %>% 
  select(IDNumber, form, everything()) %>% 
  rename(Educ = HighestEducation)

rm(list = setdiff(ls(), ls(pattern = "_raw")))  

#### COMBINE ALL DATA, WRITE CORR TABLE --------------------------------------

all_alt_raw <- bind_rows(
  IT_1030_Home_alt_form12_raw,
  Preschool_25_Home_alt_form12_raw,
  Child_512_Home_alt_form12_raw,
  Child_512_School_alt_form12_raw,
  Teen_1221_Home_alt_form12_raw,
  Teen_1221_School_alt_form12_raw,
  Adult_Self_alt_form12_raw,
  Adult_Other_alt_form12_raw
)

cor_cols <- all_alt_raw %>% 
  select(contains("_raw"))

all_alt_raw_cor_n <-
  corr.test(cor_cols)[['n']] 

scale_order <- c("SOC_raw_alt1", "VIS_raw_alt1", "HEA_raw_alt1", "TOU_raw_alt1", 
                 "TS_raw_alt1", "BOD_raw_alt1", "BAL_raw_alt1", "PLA_raw_alt1", "TOT_raw_alt1")

all_alt_raw_output <- data.frame(cor(cor_cols)) %>% 
  rownames_to_column() %>% 
  rename(scale = rowname) %>% 
  filter(str_detect(scale, "alt1")) %>% 
  select(scale, contains("alt2")) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, str_replace(scale_order, "1", "2")) %>%
  mutate(n = case_when(
    rownames(.) == "1" ~ all_alt_raw_cor_n,
    T ~ NA_real_
  )) %>% 
  select(n, scale, str_replace(scale_order, "1", "2"))


###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
write_csv(all_alt_raw_output,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t531-alt-forms-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


