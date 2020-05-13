###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
## IT-1030-Home DATA -------------------------------------------------------

# Read alt data
source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

IT_1030_Home_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALT/SPM-2 InfantToddler 1030 Months Alt-form1.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    # ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_1030_Home
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
  mutate(data = 'alt',
         clin_status = 'typ',
         clin_dx = NA
  ) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

############## START HERE

IT_1030_Home_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALT/SPM-2 InfantToddler 1030 Months Alt-form2.csv")
  ))) %>% 
  select(IDNumber, AgeInMonths, All_items_IT_1030_Home) %>% 
  rename_at(vars(contains("_1")), ~ str_replace(., "_1", "")) %>% 
  select(IDNumber, StandForm, AgeInMonths, All_items_IT_1030_Home)

  
  
  select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_1030_Home
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
  mutate(data = 'alt',
         clin_status = 'typ',
         clin_dx = NA
  ) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())




# split sample by age_range

IT_1020_Home_alt <- IT_1030_Home_alt %>% filter(age_range == "09.5 to 20 mo")
IT_2130_Home_alt <- IT_1030_Home_alt %>% filter(age_range == "21 to 31.5 mo")

# read raw-to-t lookup tables, create lookup cols by scale
IT_2130_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/RAW-T-LOOKUP-TABLES/IT-2130-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_2130_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_2130_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
IT_2130_Home_alt_T <- map_dfc(score_names,
                       ~
                         IT_2130_Home_alt %>% left_join(eval(as.name(
                           str_c(.x, '_2130_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_2130_Home_alt, .) %>% 
  arrange(IDNumber) %>% 
  mutate(form = "IT_1030_Home_alt") %>% 
  select(IDNumber, form, contains("_NT"))%>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_alt"))

rm(list = setdiff(ls(), ls(pattern = "alt_T")))

## Preschool-25-Home DATA --------------------------------------------------
source(here('CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R'))

Preschool_25_Home_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALT/SPM-2 Preschooler ages 25 Home Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Preschool_25_Home)

Preschool_25_Home_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALT/SPM-2 Preschooler ages 25 Home Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, All_items_Preschool_25_Home)

## Child-512-Home DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R'))

Child_512_Home_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 Home Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Child_512_Home)

Child_512_Home_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 Home Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Child_512_Home)

## Child-512-School DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Child-512-School-item-vectors.R'))

Child_512_School_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 School Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Child_512_School)

Child_512_School_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALT/SPM-2 Child ages 512 School Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Child_512_School)

## Teen-1221-Home DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R'))

Teen_1221_Home_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 Home Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Teen_1221_Home)

Teen_1221_Home_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 Home Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Teen_1221_Home)

## Teen-1221-School DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R'))

Teen_1221_School_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 School Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Teen_1221_School)

Teen_1221_School_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/TEEN/ALT/SPM-2 Teen ages 1221 School Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Teen_1221_School)

## Adult-Self DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Adult-Self-item-vectors.R'))

Adult_Self_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Self-Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Adult_Self)

Adult_Self_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Self-Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Adult_Self)

## Adult-Other DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Adult-Other-item-vectors.R'))

Adult_Other_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Other Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Adult_Other)

Adult_Other_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Other Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Adult_Other)



