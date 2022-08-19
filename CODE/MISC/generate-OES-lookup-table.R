suppressMessages(library(here))
suppressMessages(library(tidyverse))

scale_order <-
  c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "TOT", "PLA", "SOC")

# create form_IDs dataframes that lay out rows and columns structure for final
# output, in desired final sequence
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

# Decompose age group into min/max age in months. This dataframe has a lookup
# structure that can be used with left_join(). Note use of unique() to pull
# age_group values from the forms_IDs dataframe.
min_max <- tibble(
  age_group = unique(form_IDs$age_group), 
  min_age = c(4, 7, 10, 21, 4, 24, 60, 60, 144, 252), 
  max_age = c(6, 9, 20, 30, 1055, 59, 71, 155, 263, 1055)
)

percentile <- suppressMessages(read_csv(here(
  str_c("INPUT-FILES/OES-TABLES/t-percentile-lookup-4080T.csv")
)))

# create vector of file names of lookup tables to be transformed and bound
# together into a single output file for OES upload
file_name <- map_chr(form_IDs$file, ~ str_c(.x, "-raw-T-lookup-4080T.csv"))

# map read_csv() over file_name vector to get lookup tables into a list,
# set_names() using column from form_IDS, bind_rows() can take a list of
# dataframes as 1st argument and return single dataframe, with file column to
# holding names of origin files.
lookup <- file_name %>% map(~ suppressMessages(read_csv(here(
  str_c("INPUT-FILES/OES-TABLES/", .x)
)))) %>%
  set_names(form_IDs$file) %>%
  bind_rows(.id = "file") %>%
# here we use left_join to bring in the cols from form_IDs, designating the
# latter as input x to get those columns on the left side
  left_join(x = form_IDs, by = "file") %>%
  select(-file) %>%
# rename_with() applies a renaming function (1st arg) to range of cols (2nd arg)
    rename_with(~ str_replace_all(., "_NT", ""),
              VIS_NT:SOC_NT) %>%
  # The named score columns hold the t-scores corresponding to raw scores for
  # the SPM-2 scales. We now pivot these cols into long format, such that raw-T
  # lookup pairs are nested within scale names.
  pivot_longer(VIS:SOC,
               names_to = "scale",
               values_to = "t_score") %>%
  relocate(scale, .after = "age_group") %>%
  # We sort the data hiearchically to match the desired row sequence, matching
  # to sort orders contained in vectors pulled from form_IDs
  arrange(
    match(form, unique(form_IDs$form)),
    match(environ, unique(form_IDs$environ)),
    match(age_group, unique(form_IDs$age_group)),
    match(scale, all_of(scale_order)),
    desc(raw)
  ) %>%
  # Because the subscales and total score have different raw score ranges, there
  # are many raw score rows with no associated t-score pair (i.e., high raw
  # scores are "off the scale" of the subscale scores). A simple way to drop
  # these rows is drop_na() for the t-score col
  drop_na(t_score) %>%
  # here we substitue desired scale acronyms using mutate() and across()
  mutate(across(
    scale,
    ~ case_when(.x == "TS" ~ "T&S",
                .x == "TOT" ~ "ST",
                .x == "PLA" ~ "PLN",
                T ~ .x)
  )) %>%
  # we complete the table, adding percentile, min age, and max age cols using
  # left_join() look ups, and an interp_range col using mutate() and case_when()
  left_join(percentile, by = "t_score") %>%
  left_join(min_max, by = "age_group") %>%
  mutate(
    interp_range = case_when(
      t_score <= 59 ~ "Typical",
      between(t_score, 60, 69) ~ "Moderate Difficulties",
      t_score >= 70 ~ "Severe Difficulties"
    )
  )

file_name_school_environ <- map_chr(form_IDs_school_environ$file, ~ str_c(.x, "-cutoff-summary.csv"))

lookup_school_environ <- file_name_school_environ %>% map(~ suppressMessages(read_csv(here(
  str_c("OUTPUT-FILES/SCHOOL-ENVIRON-DRIVING/", .x)
)))) %>%
  set_names(form_IDs_school_environ$file) %>%
  bind_rows(.id = "file") %>%
  select(file, TOT_raw, cutoff) %>%
  # At this point the data object doesn't have all possible values for TOT_raw,
  # because of the nature of the input files. complete() is used to fill in the
  # missing rows, so that all possible values of TOT_raw, from 15 thru 60, are
  # represented in rows.
  complete(file, TOT_raw = 15:60) %>%
  # At this point, the cutoff column has a value only in the row corresponding
  # to the actual cutoff raw score. To give all rows above and below the cutoff
  # score a value on the interp_range variable, we first group the object by
  # file, so that subsequent operations run "within" row groups defined by their
  # value on file. We then use mutate(), across(), and fill_run(), to extend the
  # existing value of the cutoff col to all raw score values above the existing
  # value. Thus, within each file group, rows are now either "cutoff" or NA in
  # the cutoff column, which represents those raw scores being either above or
  # below the cutoff value.
  group_by(file) %>%
  mutate(across(cutoff,
                ~ runner::fill_run(.))) %>%
  left_join(x = form_IDs_school_environ, by = "file") %>%
  rename(raw = TOT_raw,
         scale = file) %>%
  left_join(min_max, by = "age_group") %>%
  mutate(
    t_score = NA_real_,
    percentile = NA_real_,
    interp_range = case_when(cutoff == "cutoff" ~ "above cutoff",
                             T ~ "below cutoff")
  ) %>%
  select(-cutoff) %>%
  relocate(scale, .after = "age_group") %>%
  relocate(c(t_score, percentile), .after = "raw") %>% 
  arrange(
    match(environ, form_IDs_school_environ$environ),
    desc(raw)
  )
  
file_name_driving <- map_chr(form_IDs_driving$file, ~ str_c(.x, "-cutoff-summary.csv"))

lookup_driving <- file_name_driving %>% map(~ suppressMessages(read_csv(here(
  str_c("OUTPUT-FILES/SCHOOL-ENVIRON-DRIVING/", .x)
)))) %>%
  set_names(form_IDs_driving$file) %>%
  bind_rows(.id = "file") %>%
  select(file, TOT_raw, cutoff) %>%
  complete(file, TOT_raw = 18:72) %>%
  group_by(file) %>%
  mutate(across(cutoff,
                ~ runner::fill_run(.))) %>%
  left_join(x = form_IDs_driving, by = "file") %>%
  rename(raw = TOT_raw,
         scale = file) %>%
  left_join(min_max, by = "age_group") %>%
  mutate(
    t_score = NA_real_,
    percentile = NA_real_,
    interp_range = case_when(cutoff == "cutoff" ~ "above cutoff",
                             T ~ "below cutoff")
  ) %>%
  select(-cutoff) %>%
  relocate(scale, .after = "age_group") %>%
  relocate(c(t_score, percentile), .after = "raw") %>% 
  arrange(
    match(age_group, unique(form_IDs$age_group)),
    match(environ, unique(form_IDs$environ)),
    desc(raw)
  )
  
lookup_all <- bind_rows(lookup,
                        lookup_school_environ,
                        lookup_driving)

 write_csv(
   lookup_all,
   here(
     "OUTPUT-FILES/OES-LOOKUP/SPM2-OES-norms-lookup.csv"
   ),
   na = ""
 )
 
