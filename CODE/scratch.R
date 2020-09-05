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

file_prefix <- c(
  "IT-46-Home", "IT-79-Home", "IT-1020-Home", "IT-2130-Home", "IT-Caregiver", 
  "Preschool-24-Home", "Preschool-5-Home", "Preschool-24-School", "Preschool-5-School",  
  "Child-512-Home", "Child-512-School",
  "Teen-1221-Home", "Teen-1221-School", "Teen-1221-Self",  
  "Adult-Self", "Adult-Other"
  )

file_name <- map_chr(file_prefix, ~ str_c(.x, "-raw-T-lookup-4080T.csv"))

lookup <- file_name %>% map(
  ~ suppressMessages(read_csv(
    here(
      str_c("INPUT-FILES/OES-TABLES/", .x)
    )
  ))
) %>% 
  set_names(file_prefix) %>% 
  bind_rows(.id = "file")

