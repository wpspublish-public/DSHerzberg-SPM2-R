ageXgender_full <- Child_512_Home_desamp %>%
  group_by(Age, Gender) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Gender, values_from = n)

ageXpeduc_full <- Child_512_Home_desamp %>%
  group_by(Age, ParentHighestEducation) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = ParentHighestEducation, values_from = n)

ageXethnic_full <- Child_512_Home_desamp %>%
  group_by(Age, Ethnicity) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Ethnicity, values_from = n)

ageXregion_full <- Child_512_Home_desamp %>%
  group_by(Age, Region) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Region, values_from = n)

demos_full <- list(ageXgender_full,
                   ageXpeduc_full,
                   ageXethnic_full,
                   ageXregion_full) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(Child_512_Home_desamp),
      TRUE ~ NA_integer_
    )
  ) %>%
  relocate(c(sample, n), .before = "Age")



ageXgender_60_perc <- sample_60perc %>%
  group_by(Age, Gender) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Gender, values_from = n)

ageXpeduc_60_perc <- sample_60perc %>%
  group_by(Age, ParentHighestEducation) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = ParentHighestEducation, values_from = n)

ageXethnic_60_perc <- sample_60perc %>%
  group_by(Age, Ethnicity) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Ethnicity, values_from = n)

ageXregion_60_perc <- sample_60perc %>%
  group_by(Age, Region) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = Region, values_from = n)

demos_60_perc <- list(ageXgender_60_perc,
                   ageXpeduc_60_perc,
                   ageXethnic_60_perc,
                   ageXregion_60_perc) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "60_perc",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(sample_60perc),
      TRUE ~ NA_integer_
    )
  ) %>%
  relocate(c(sample, n), .before = "Age")

demos_comp <- bind_rows(demos_full,
                        demos_60_perc) %>%
  mutate(across(sample,
                ~ case_when(
                  lag(sample) == sample ~ NA_character_, 
                  TRUE ~ .x
                )))

# write .csv of demos comp
write_csv(
  demos_comp,
  here(
    "OUTPUT-FILES/CHILD/COMP-60PERC-SAMPLE/Child-512-Home-demos-full-60perc-comp.csv"
  ),
  na = ""
)
