# CREATE DEMO COUNT TABLES FOR SM-QUAL COMBO FILES

suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))

# Next two vecs provide the "master" sort orders for all possible demo variables, and all
# possible values (categories) within each demo var

var_order <- c("data", "age_range", "Age", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")

cat_order <- c(
  # data
  NA, "SM", "Qual",
  # age_range
  NA, "3.5 to 6 mo", "03.5 to 10 mo", "7 to 10.5 mo", "09.5 to 20 mo",  "11 to 31.5 mo", 
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

# Adult Self

Adult_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Adult-Self-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)

# Child 512 home

Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)

# IT 49 home

IT_49_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/IT-49-Home-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)

# IT 1030 home

IT_1030_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/IT-1030-Home-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)

# IT Caregiver home

IT_Caregiver <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/IT-Caregiver-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)

# Preschool 25 home

Preschool_25_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Preschool-25-Home-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)

# Teen 1221 home

Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Home-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)


# Teen 1221 self

Teen_1221_Self <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/SM-QUAL-COMBO-NORMS-INPUT/Teen-1221-Self-combo-norms-input.csv")
  )))

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

gdata::keep(var_order, cat_order, sure = T)
