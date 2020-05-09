###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
## IT-1030-Home DATA -------------------------------------------------------

# Read alt data
source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

IT_1030_Home_alt <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALT/SPM-2 InfantToddler 1030 Months ALT.csv")
  ))) %>% select(
    IDNumber,
    AgeInMonths,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_IT_1030_Home
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
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
  # recode reverse-scored items
  mutate_at(
    SOC_rev_items_IT_1030_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_IT_1030_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    AgeInMonths <= 20 ~ "09.5 to 20 mo",
    TRUE ~ "21 to 31.5 mo")
  ) %>% 
  # select(-AgeGroup) %>% 
  # Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
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
  # Create data var 
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

# read Preschool-25-Home data

source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Stand.R"))

test <- IT_2130_Home_alt_T %>% 
  inner_join(Preschool_25_School_Stand, by = "IDNumber")

alt_ID <- IT_2130_Home_alt_T$IDNumber

test1 <- filter(Preschool_25_Home_Stand, IDNumber == alt_ID)
