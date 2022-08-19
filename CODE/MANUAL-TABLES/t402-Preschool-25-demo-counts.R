suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# READ FINALIZED STAND SAMPLE ---------------------------------------------

Preschool_25_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/PRESCHOOL/ALLDATA-DESAMP-NORMS-INPUT/Preschool-25-Home-allData-desamp.csv")
  )))

# write table of demographic counts -----------------------------------------

var_order <- c("data", "age_range", "Age", "AgeInMonths", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")

cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt", 
  # age_range
  NA, "03.5 to 6 mo", "03.5 to 10 mo", "07 to 10.5 mo", "09.5 to 20 mo",  "11 to 31.5 mo", 
  "21 to 31.5 mo", "5 to 8 years", "9 to 12 years", "12 to 13 years", "14 to 15 years", 
  "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")


Preschool_25_Home_demo_counts <- Preschool_25_Home %>% 
  select(Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    # lag(Variable) == "data" & Variable == "data" ~ "",
    # lag(Variable) == "AgeInMonths" & Variable == "AgeInMonths" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(Preschool_25_Home_demo_counts, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t402-Preschool-25-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

