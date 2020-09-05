suppressMessages(library(here))
suppressMessages(library(tidyverse))

form <- c(
  "IT-home", "IT-caregiver", 
  "preschool-home", "preschool-school", 
  "child-home", "child-school",
  "teen-home", "teen-school", "teen-self", 
  "adult-self", "adult-other"
  )

scale <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "TOT", "PLA", "SOC")

form_IDs <- tribble(
  ~file, ~form, ~environ, ~age_group,
  "IT-46-Home", "IT", "Home", "4-6 months", 
  "IT-79-Home", "IT", "Home", "7-9 months", 
  "IT-1020-Home", "IT", "Home", "10-20 months", 
  "IT-2130-Home", "IT", "Home", "21-30 months", 
  "IT-Caregiver", "IT", "Caregiver", NA_character_, 
  "Preschool-24-Home", "Preschool", "Home", "2-4 years", 
  "Preschool-5-Home", "Preschool", "Home", "5 years", 
  "Preschool-24-School", "Preschool", "School", "2-4 years", 
  "Preschool-5-School", "Preschool", "School", "5 years", 
  "Child-512-Home", "Child", "Home", "5-12 years", 
  "Child-512-School","Child", "School", "5-12 years", 
  "Teen-1221-Home", "Teen", "Home", "12-21 years", 
  "Teen-1221-School", "Teen", "School", "12-21 years", 
  "Teen-1221-Self", "Teen", "Self", "12-21 years", 
  "Adult-Self", "Adult", "Self", "Adult", 
  "Adult-Other", "Adult", "Other", "Adult"
)

file_name <- map_chr(form_IDs$file, ~ str_c(.x, "-raw-T-lookup-4080T.csv"))

lookup <- file_name %>% map(
  ~ suppressMessages(read_csv(
    here(
      str_c("INPUT-FILES/OES-TABLES/", .x)
    )
  ))
) %>% 
  set_names(form_IDs$file) %>% 
  bind_rows(.id = "file")





