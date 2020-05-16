###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
#### IT-49-Home v IT-Caregiver -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-Caregiver-Stand.R"))

IT_49_Home_Stand_DIF_comp <- IT_49_Home_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_49_Home = TOT_NT)

IT_Caregiver_Stand_DIF_comp <- IT_Caregiver_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_Caregiver = TOT_NT)

# create comp file, calc DIF score
IT_49_Home_v_IT_Caregiver_Stand_DIF <- IT_49_Home_Stand_DIF_comp %>%
  inner_join(IT_Caregiver_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_IT_49_Home - TOT_NT_IT_Caregiver,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
IT_49_Home_v_IT_Caregiver_Stand_DIF_output <- IT_49_Home_v_IT_Caregiver_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'IT-49-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'IT-Caregiver',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(IT_49_Home_v_IT_Caregiver_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-Caregiver-Clin.R"))

IT_49_Home_Clin_DIF_comp <- IT_49_Home_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_49_Home = TOT_NT)

IT_Caregiver_Clin_DIF_comp <- IT_Caregiver_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_Caregiver = TOT_NT)

# create comp file, calc DIF score
IT_49_Home_v_IT_Caregiver_Clin_DIF <- IT_49_Home_Clin_DIF_comp %>%
  inner_join(IT_Caregiver_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_IT_49_Home - TOT_NT_IT_Caregiver,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
IT_49_Home_v_IT_Caregiver_Clin_DIF_output <- IT_49_Home_v_IT_Caregiver_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'IT-49-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'IT-Caregiver',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(IT_49_Home_v_IT_Caregiver_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


#### IT-1030-Home v IT-Caregiver -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-Caregiver-Stand.R"))

IT_1030_Home_Stand_DIF_comp <- IT_1030_Home_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_1030_Home = TOT_NT)

IT_Caregiver_Stand_DIF_comp <- IT_Caregiver_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_Caregiver = TOT_NT)

# create comp file, calc DIF score
IT_1030_Home_v_IT_Caregiver_Stand_DIF <- IT_1030_Home_Stand_DIF_comp %>%
  inner_join(IT_Caregiver_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_IT_1030_Home - TOT_NT_IT_Caregiver,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
IT_1030_Home_v_IT_Caregiver_Stand_DIF_output <- IT_1030_Home_v_IT_Caregiver_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'IT-1030-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'IT-Caregiver',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(IT_1030_Home_v_IT_Caregiver_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-Caregiver-Clin.R"))

IT_1030_Home_Clin_DIF_comp <- IT_1030_Home_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_1030_Home = TOT_NT)

IT_Caregiver_Clin_DIF_comp <- IT_Caregiver_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_IT_Caregiver = TOT_NT)

# create comp file, calc DIF score
IT_1030_Home_v_IT_Caregiver_Clin_DIF <- IT_1030_Home_Clin_DIF_comp %>%
  inner_join(IT_Caregiver_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_IT_1030_Home - TOT_NT_IT_Caregiver,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
IT_1030_Home_v_IT_Caregiver_Clin_DIF_output <- IT_1030_Home_v_IT_Caregiver_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'IT-1030-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'IT-Caregiver',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(IT_1030_Home_v_IT_Caregiver_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


#### Preschool-25-Home v Preschool-25-School -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))

Preschool_25_Home_Stand_DIF_comp <- Preschool_25_Home_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Preschool_25_Home = TOT_NT)

Preschool_25_School_Stand_DIF_comp <- Preschool_25_School_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Preschool_25_School = TOT_NT)

# create comp file, calc DIF score
Preschool_25_Home_v_Preschool_25_School_Stand_DIF <- Preschool_25_Home_Stand_DIF_comp %>%
  inner_join(Preschool_25_School_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Preschool_25_Home - TOT_NT_Preschool_25_School,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Preschool_25_Home_v_Preschool_25_School_Stand_DIF_output <- Preschool_25_Home_v_Preschool_25_School_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Preschool-25-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Preschool-25-School',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Preschool_25_Home_v_Preschool_25_School_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Clin.R"))

Preschool_25_Home_Clin_DIF_comp <- Preschool_25_Home_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Preschool_25_Home = TOT_NT)

Preschool_25_School_Clin_DIF_comp <- Preschool_25_School_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Preschool_25_School = TOT_NT)

# create comp file, calc DIF score
Preschool_25_Home_v_Preschool_25_School_Clin_DIF <- Preschool_25_Home_Clin_DIF_comp %>%
  inner_join(Preschool_25_School_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Preschool_25_Home - TOT_NT_Preschool_25_School,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Preschool_25_Home_v_Preschool_25_School_Clin_DIF_output <- Preschool_25_Home_v_Preschool_25_School_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Preschool-25-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Preschool-25-School',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Preschool_25_Home_v_Preschool_25_School_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


#### Child-512-Home v Child-512-School -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))

Child_512_Home_Stand_DIF_comp <- Child_512_Home_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Child_512_Home = TOT_NT)

Child_512_School_Stand_DIF_comp <- Child_512_School_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Child_512_School = TOT_NT)

# create comp file, calc DIF score
Child_512_Home_v_Child_512_School_Stand_DIF <- Child_512_Home_Stand_DIF_comp %>%
  inner_join(Child_512_School_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Child_512_Home - TOT_NT_Child_512_School,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Child_512_Home_v_Child_512_School_Stand_DIF_output <- Child_512_Home_v_Child_512_School_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Child-512-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Child-512-School',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Child_512_Home_v_Child_512_School_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Clin.R"))

Child_512_Home_Clin_DIF_comp <- Child_512_Home_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Child_512_Home = TOT_NT)

Child_512_School_Clin_DIF_comp <- Child_512_School_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Child_512_School = TOT_NT)

# create comp file, calc DIF score
Child_512_Home_v_Child_512_School_Clin_DIF <- Child_512_Home_Clin_DIF_comp %>%
  inner_join(Child_512_School_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Child_512_Home - TOT_NT_Child_512_School,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Child_512_Home_v_Child_512_School_Clin_DIF_output <- Child_512_Home_v_Child_512_School_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Child-512-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Child-512-School',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Child_512_Home_v_Child_512_School_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


#### Teen-1221-Home v Teen-1221-School -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))

Teen_1221_Home_Stand_DIF_comp <- Teen_1221_Home_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Home = TOT_NT)

Teen_1221_School_Stand_DIF_comp <- Teen_1221_School_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_School = TOT_NT)

# create comp file, calc DIF score
Teen_1221_Home_v_Teen_1221_School_Stand_DIF <- Teen_1221_Home_Stand_DIF_comp %>%
  inner_join(Teen_1221_School_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_Home - TOT_NT_Teen_1221_School,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_Home_v_Teen_1221_School_Stand_DIF_output <- Teen_1221_Home_v_Teen_1221_School_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_Home_v_Teen_1221_School_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Clin.R"))

Teen_1221_Home_Clin_DIF_comp <- Teen_1221_Home_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Home = TOT_NT)

Teen_1221_School_Clin_DIF_comp <- Teen_1221_School_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_School = TOT_NT)

# create comp file, calc DIF score
Teen_1221_Home_v_Teen_1221_School_Clin_DIF <- Teen_1221_Home_Clin_DIF_comp %>%
  inner_join(Teen_1221_School_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_Home - TOT_NT_Teen_1221_School,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_Home_v_Teen_1221_School_Clin_DIF_output <- Teen_1221_Home_v_Teen_1221_School_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_Home_v_Teen_1221_School_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


#### Teen-1221-Home v Teen-1221-Self -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))

Teen_1221_Home_Stand_DIF_comp <- Teen_1221_Home_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Home = TOT_NT)

Teen_1221_Self_Stand_DIF_comp <- Teen_1221_Self_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Self = TOT_NT)

# create comp file, calc DIF score
Teen_1221_Home_v_Teen_1221_Self_Stand_DIF <- Teen_1221_Home_Stand_DIF_comp %>%
  inner_join(Teen_1221_Self_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_Home - TOT_NT_Teen_1221_Self,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_Home_v_Teen_1221_Self_Stand_DIF_output <- Teen_1221_Home_v_Teen_1221_Self_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-Self',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_Home_v_Teen_1221_Self_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Clin.R"))

Teen_1221_Home_Clin_DIF_comp <- Teen_1221_Home_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Home = TOT_NT)

Teen_1221_Self_Clin_DIF_comp <- Teen_1221_Self_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Self = TOT_NT)

# create comp file, calc DIF score
Teen_1221_Home_v_Teen_1221_Self_Clin_DIF <- Teen_1221_Home_Clin_DIF_comp %>%
  inner_join(Teen_1221_Self_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_Home - TOT_NT_Teen_1221_Self,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_Home_v_Teen_1221_Self_Clin_DIF_output <- Teen_1221_Home_v_Teen_1221_Self_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-Home',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-Self',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_Home_v_Teen_1221_Self_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


#### Teen-1221-School v Teen-1221-Self -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))

Teen_1221_School_Stand_DIF_comp <- Teen_1221_School_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_School = TOT_NT)

Teen_1221_Self_Stand_DIF_comp <- Teen_1221_Self_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Self = TOT_NT)

# create comp file, calc DIF score
Teen_1221_School_v_Teen_1221_Self_Stand_DIF <- Teen_1221_School_Stand_DIF_comp %>%
  inner_join(Teen_1221_Self_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_School - TOT_NT_Teen_1221_Self,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_School_v_Teen_1221_Self_Stand_DIF_output <- Teen_1221_School_v_Teen_1221_Self_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-Self',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_School_v_Teen_1221_Self_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Clin.R"))

Teen_1221_School_Clin_DIF_comp <- Teen_1221_School_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_School = TOT_NT)

Teen_1221_Self_Clin_DIF_comp <- Teen_1221_Self_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Self = TOT_NT)

# create comp file, calc DIF score
Teen_1221_School_v_Teen_1221_Self_Clin_DIF <- Teen_1221_School_Clin_DIF_comp %>%
  inner_join(Teen_1221_Self_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_School - TOT_NT_Teen_1221_Self,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_School_v_Teen_1221_Self_Clin_DIF_output <- Teen_1221_School_v_Teen_1221_Self_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-Self',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_School_v_Teen_1221_Self_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))


###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
DIF_counts_ouput <- bind_rows(
  IT_49_Home_v_IT_Caregiver_Stand_DIF_output,
  IT_49_Home_v_IT_Caregiver_Clin_DIF_output,
  IT_1030_Home_v_IT_Caregiver_Stand_DIF_output,
  IT_1030_Home_v_IT_Caregiver_Clin_DIF_output,
  Preschool_25_Home_v_Preschool_25_School_Stand_DIF_output,
  Preschool_25_Home_v_Preschool_25_School_Clin_DIF_output,
  Child_512_Home_v_Child_512_School_Stand_DIF_output,
  Child_512_Home_v_Child_512_School_Clin_DIF_output,
  Teen_1221_Home_v_Teen_1221_School_Stand_DIF_output,
  Teen_1221_Home_v_Teen_1221_School_Clin_DIF_output,
  Teen_1221_Home_v_Teen_1221_Self_Stand_DIF_output,
  Teen_1221_Home_v_Teen_1221_Self_Clin_DIF_output,
  Teen_1221_School_v_Teen_1221_Self_Stand_DIF_output,
  Teen_1221_School_v_Teen_1221_Self_Clin_DIF_output,
  Adult_Self_v_Adult_Other_Stand_DIF_output,
  Adult_Self_v_Adult_Other_Clin_DIF_output
)

write_csv(DIF_counts_ouput,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t528-DIF-counts-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
