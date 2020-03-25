# CREATE DEMO COUNT TABLES FOR SM-QUAL COMBO FILES

suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))

# Adult Self

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Adult-Self-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "HighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "21.00 to 30.99 years", "31.00 to 40.99 years", "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "NativeHawPacIsl", "MultiRacial", "Other",
               NA, "northeast", "midwest", "south", "west")                                   


Adult_Self_demo_counts <- Adult_Self %>% 
  select(data, age_range, Gender, HighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "HighestEducation" & Variable == "HighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(Adult_Self_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/Adult-Self-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())

# Child 512 home

Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "5 to 8 years", "9 to 12 years",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "MultiRacial", "Other",
               "northeast", "midwest", "south", "west")                                   


Child_512_Home_demo_counts <- Child_512_Home %>% 
  select(data, age_range, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(Child_512_Home_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/Child-512-Home-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())


# IT 49 home

IT_49_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "3.5 to 6 mo", "7 to 10.5 mo",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "MultiRacial", "Other",
               "northeast", "midwest", "south", "west")                                   


IT_49_Home_demo_counts <- IT_49_Home %>% 
  select(data, age_range, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(IT_49_Home_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/IT-49-Home-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())


# IT 1030 home

IT_1030_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "09.5 to 20 mo", "21 to 31.5 mo",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "NativeHawPacIsl", "MultiRacial", "Other",
               NA, "northeast", "midwest", "south", "west")                                   


IT_1030_Home_demo_counts <- IT_1030_Home %>% 
  select(data, age_range, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(IT_1030_Home_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/IT-1030-Home-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())


# IT Caregiver home

IT_Caregiver <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/IT-Caregiver-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "03.5 to 10 mo", "11 to 31.5 mo",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "NativeHawPacIsl", "MultiRacial", "Other",
               NA, "northeast", "midwest", "south", "west")                                   


IT_Caregiver_demo_counts <- IT_Caregiver %>% 
  select(data, age_range, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(IT_Caregiver_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/IT-Caregiver-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())


# Preschool 25 home

Preschool_25_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Preschool-25-Home-combo-norms-input.csv")
  )))

var_order <- c("data", "Age", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "2", "3", "4", "5",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "NativeHawPacIsl", "MultiRacial", "Other",
               "northeast", "midwest", "south", "west")                                   


Preschool_25_Home_demo_counts <- Preschool_25_Home %>% 
  select(data, Age, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "Age" & Variable == "Age" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(Preschool_25_Home_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/Preschool-25-Home-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())

# Teen 1221 home

Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Home-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "12 to 13 years", "14 to 15 years", "16 to 17 years", "18 to 21 years",
               "Male", "Female",
               "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "NativeHawPacIsl", "MultiRacial", "Other",
               "northeast", "midwest", "south", "west")

Teen_1221_Home_demo_counts <- Teen_1221_Home %>% 
  select(data, age_range, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(Teen_1221_Home_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/Teen-1221-Home-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())

# Teen 1221 self

Teen_1221_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Self-combo-norms-input.csv")
  )))

var_order <- c("data", "age_range", "Gender", "ParentHighestEducation", "Ethnicity", "Region")

cat_order <- c("SM", "Qual",
               "12 to 13 years", "14 to 15 years", "16 to 17 years", "18 to 21 years",
               "Male", "Female",
               NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", "Some college or associate degree", "Bachelor's degree or higher",
               "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", "NativeHawPacIsl", "MultiRacial", "Other",
               "northeast", "midwest", "south", "west")

Teen_1221_Self_demo_counts <- Teen_1221_Self %>% 
  select(data, age_range, Gender, ParentHighestEducation, Ethnicity, Region) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "data" & Variable == "data" ~ "",
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    lag(Variable) == "Region" & Variable == "Region" ~ "",
    TRUE ~ Variable
  ))

write_csv(Teen_1221_Self_demo_counts, here(
  paste0(
    'INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/DEMO-COUNTS/Teen-1221-Self-combo-demo-counts-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = '(missing)'
)

rm(list = ls())



