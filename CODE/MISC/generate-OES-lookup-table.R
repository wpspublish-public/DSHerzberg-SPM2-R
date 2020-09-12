suppressMessages(library(here))
suppressMessages(library(tidyverse))

form_order <- c("Infant", "Toddler", "Caregiver", "Preschool", 
                "Child", "Adolescent", "Adult")
environ_order <- c("Home", "Caregiver", "School", "Self", "Other")
age_group_order <- c("4-6 months", "7-9 months", "10-20 months", 
                     "21-30 months", "Not Specified", "2-4 years", "5 years", 
                     "5-12 years", "12-21 years", "21-87 years")
scale_order <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "TOT", "PLA", "SOC")

min_max <- tibble(
  age_group = all_of(age_group_order), 
  min_age = c(4, 7, 10, 21, 4, 24, 60, 60, 144, 252), 
  max_age = c(6, 9, 20, 30, 1055, 59, 71, 155, 263, 1055)
)

form_IDs <- tribble(
  ~file, ~form, ~environ, ~age_group,
  "IT-46-Home", "Infant", "Home", "4-6 months", 
  "IT-79-Home", "Infant", "Home", "7-9 months", 
  "IT-1020-Home", "Toddler", "Home", "10-20 months", 
  "IT-2130-Home", "Toddler", "Home", "21-30 months", 
  "IT-Caregiver", "Caregiver", "Caregiver", "Not Specified", 
  "Preschool-24-Home", "Preschool", "Home", "2-4 years", 
  "Preschool-5-Home", "Preschool", "Home", "5 years", 
  "Preschool-24-School", "Preschool", "School", "2-4 years", 
  "Preschool-5-School", "Preschool", "School", "5 years", 
  "Child-512-Home", "Child", "Home", "5-12 years", 
  "Child-512-School","Child", "School", "5-12 years", 
  "Teen-1221-Home", "Adolescent", "Home", "12-21 years", 
  "Teen-1221-School", "Adolescent", "School", "12-21 years", 
  "Teen-1221-Self", "Adolescent", "Self", "12-21 years", 
  "Adult-Self", "Adult", "Self", "21-87 years", 
  "Adult-Other", "Adult", "Other", "21-87 years"
)

form_IDs_school_environ <- tribble(
  ~file, ~form, ~environ, ~age_group,
  "ART", "School-Environ", "Art Teacher", "5-12 years", 
  "BUS", "School-Environ", "Bus Driver", "5-12 years", 
  "CAF", "School-Environ", "Cafeteria Worker", "5-12 years", 
  "MUS", "School-Environ", "Music Teacher", "5-12 years", 
  "PHY", "School-Environ", "Phys Ed Teacher", "5-12 years", 
  "REC", "School-Environ", "Recess Supervisor", "5-12 years", 
)

form_IDs_driving <- tribble(
  ~file, ~form, ~environ, ~age_group,
  "teen-driving-home", "Driving", "Home", "12-21 years", 
  "teen-driving-self", "Driving", "Self", "12-21 years", 
  "adult-driving-self", "Driving", "Self", "21-87 years", 
  "adult-driving-other", "Driving", "Other", "21-87 years"
)

percentile <- suppressMessages(read_csv(
  here(
    str_c("INPUT-FILES/OES-TABLES/t-percentile-lookup-4080T.csv")
  )
))

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
  drop_na(t_score) %>% 
  mutate(across(
    scale,
    ~ case_when(
      .x == "TS" ~ "T&S",
      .x == "TOT" ~ "ST",
      .x == "PLA" ~ "PLN",
      T ~ .x
    )
  )) %>% 
  left_join(percentile, by = "t_score") %>% 
  left_join(min_max, by = "age_group") %>% 
  mutate(interp_range = case_when(
    t_score <=59 ~ "Typical", 
    between(t_score, 60, 69) ~ "Moderate Difficulties",
    t_score >= 70 ~ "Severe Difficulties"
  ))

file_name_school_environ <- map_chr(form_IDs_school_environ$file, ~ str_c(.x, "-cutoff-summary.csv"))

lookup_school_environ <- file_name_school_environ %>% map(
  ~ suppressMessages(read_csv(
    here(
      str_c("OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/", .x)
    )
  ))
) %>% 
  set_names(form_IDs_school_environ$file) %>% 
  bind_rows(.id = "file") %>% 
  select(file, TOT_raw, cutoff) %>% 
  group_by(file) %>% 
  mutate(
    across(cutoff, 
    ~ runner::fill_run(.)
    )
  ) %>% 
  left_join(x = form_IDs_school_environ, by = "file") %>% 
  rename(
    raw = TOT_raw,
    scale = file
    ) %>% 
  left_join(min_max, by = "age_group") %>% 
  mutate(
    t_score = NA_real_,
    percentile = NA_real_,
    interp_range = case_when(
    cutoff == "cutoff" ~ "above cutoff", 
    T ~ "below cutoff"
  )) %>%
  select(-cutoff) %>% 
  relocate(scale, .after = "age_group") %>% 
  relocate(c(t_score, percentile), .after = "raw")
  
  


 
