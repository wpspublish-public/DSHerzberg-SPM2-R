demos_60_perc <- map(
    c("Gender", "ParentHighestEducation", "Ethnicity", "Region"),
  ~
    sample_60perc %>%
    group_by(Age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
  ) %>%
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

