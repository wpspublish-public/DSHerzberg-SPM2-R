suppressMessages(library(here))
suppressMessages(library(tidyverse))

form <- c(
  "IT-home", "IT-caregiver", 
  "preschool-home", "preschool-school", 
  "child-home", "child-school",
  "teen-home", "teen-school", "teen-self", 
  "adult-self", "adult-other"
  )

form_order <- c("IT", "Preschool", "Child", "Teen", "Adult")
environ_order <- c("Home", "Caregiver", "School", "Self", "Other")
age_group_order <- c("4-6 months", "7-9 months", "10-20 months", 
                     "21-30 months", NA_character_, "2-4 years", "5 years", 
                     "5-12 years", "12-21 years", "Adult")
scale_order <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "TOT", "PLA", "SOC")

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
  bind_rows(.id = "file") %>% 
  left_join(x = form_IDs, by = "file") %>% 
  select(-file) %>%
  rename_with(
    ~ str_replace_all( 
      ., "_NT", ""
    ),
    VIS_NT:SOC_NT
  ) %>% 
  pivot_longer(
    VIS:SOC,
    names_to = "scale",
    values_to = "t_score"
  ) %>% 
  relocate(scale, .after = "age_group") %>% 
  arrange(
    match(form, all_of(form_order)), 
    match(environ, all_of(environ_order)), 
    match(age_group, all_of(age_group_order)), 
    match(scale, all_of(scale_order)),
    desc(raw)
    ) %>% 
  drop_na(t_score)









