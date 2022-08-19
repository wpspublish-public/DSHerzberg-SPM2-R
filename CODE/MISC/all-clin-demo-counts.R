suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# READ COMBINED CLIN SAMPLE ---------------------------------------------

all_clin_uniqueIDs <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Clin-all-uniqueIDs-T-scores-per-case.csv")
  ))) %>% 
  mutate(educ = case_when(
    is.na(ParentHighestEducation) ~ HighestEducation,
    is.na(HighestEducation) ~ ParentHighestEducation,
    T ~ NA_character_), 
    age_level = case_when(
      age_range %in% c("03.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "07 to 10.5 mo", "09.5 to 20 mo",  
                       "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
                       "21 to 31.5 mo") ~ "Infant-Toddler",
      age_range %in% c("2 to 4 years", "5 years") ~ "Preschool",
      age_range %in% c("5 to 8 years", "9 to 12 years") ~ "Child",
      age_range %in% c("12 to 13 years", 
                       "14 to 15 years", "16 to 17 years", "18 to 21 years") ~ "Adolescent",
      age_range %in% c("21.00 to 30.99 years", "31.00 to 40.99 years", 
                       "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years") ~ "Adult",
      T ~ NA_character_
    )
  )

# write table of demographic counts -----------------------------------------

var_order <- c("Gender", "age_level", "Ethnicity", "educ", "Region")

cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt",
  # AgeInMonths
  NA, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
  '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
  # age_range
  NA, "03.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "07 to 10.5 mo", "09.5 to 20 mo",  
  "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
  "21 to 31.5 mo", "2 to 4 years", "5 years", "5 to 8 years", "9 to 12 years", "12 to 13 years", 
  "14 to 15 years", "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # age_level
  "Infant-Toddler", "Preschool", "Child", "Adolescent", "Adult",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation & educ
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")


all_clin_uniqueIDs_demo_counts <- all_clin_uniqueIDs %>% 
  select(Gender, age_level, Ethnicity, educ, Region) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "Category") %>% 
  group_by(Variable) %>%
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "age_level" & Variable == "age_level" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "educ" & Variable == "educ" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable), 
    sample_pct = round((n/nrow(all_clin_uniqueIDs)) * 100, 1)
    )

write_csv(all_clin_uniqueIDs_demo_counts, here(
  str_c(
    'OUTPUT-FILES/MISC/all-clin-uniqueIDs-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
),
na = '(missing)')

