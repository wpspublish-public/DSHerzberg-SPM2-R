suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))

# ######### IT 430 HOME DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

IT_49_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

IT_1030_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_dx)

IT_430_Home_Clin <- bind_rows(
  IT_49_Home_Clin,
  IT_1030_Home_Clin
) %>% 
  drop_na(clin_dx) %>% 
  arrange(IDNumber)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

IT_430_clin_dx_counts <- IT_430_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'IT 430',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(IT_430_Home_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = 'clin_dx_counts')))

# ######### PRESCHOOL 25  DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Preschool_24_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-clin-T-Scores-per-case.csv")
  )))

Preschool_5_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-clin-T-Scores-per-case.csv")
  )))

Preschool_25_Home_Clin <- bind_rows(
  Preschool_24_Home_Clin,
  Preschool_5_Home_Clin
) %>% 
  drop_na(clin_dx) %>% 
  arrange(IDNumber)

Preschool_24_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-clin-T-Scores-per-case.csv")
  )))

Preschool_5_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-clin-T-Scores-per-case.csv")
  )))

Preschool_25_School_Clin <- bind_rows(
  Preschool_24_School_Clin,
  Preschool_5_School_Clin
) %>% 
  drop_na(clin_dx) %>% 
  arrange(IDNumber)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Preschool_25_Home_clin_dx <- Preschool_25_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Preschool 25 Home',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Preschool_25_Home_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Preschool_25_School_clin_dx <- Preschool_25_School_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Preschool 25 School',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Preschool_25_School_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Preschool_25_clin_dx_counts <- bind_rows(
  Preschool_25_Home_clin_dx,
  Preschool_25_School_clin_dx
)

rm(list = setdiff(ls(), ls(pattern = 'clin_dx_counts')))

# ######### CHILD 512  DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Child_512_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

Child_512_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Child_512_Home_clin_dx <- Child_512_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Child 512 Home',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Child_512_Home_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Child_512_School_clin_dx <- Child_512_School_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Child 512 School',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Child_512_School_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Child_512_clin_dx_counts <- bind_rows(
  Child_512_Home_clin_dx,
  Child_512_School_clin_dx
)

rm(list = setdiff(ls(), ls(pattern = 'clin_dx_counts')))

# ######### TEEN 1221  DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Teen_1221_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

Teen_1221_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

Teen_1221_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Teen_1221_Home_clin_dx <- Teen_1221_Home_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Teen 1221 Home',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Teen_1221_Home_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Teen_1221_School_clin_dx <- Teen_1221_School_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Teen 1221 School',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Teen_1221_School_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Teen_1221_Self_clin_dx <- Teen_1221_Self_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Teen 1221 Self',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Teen_1221_Self_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Teen_1221_clin_dx_counts <- bind_rows(
  Teen_1221_Home_clin_dx,
  Teen_1221_School_clin_dx,
  Teen_1221_Self_clin_dx
)

rm(list = setdiff(ls(), ls(pattern = 'clin_dx_counts')))

# ######### ADULT DATA -----------------------------------------------------

# READ AND COMBINE FINALIZED SAMPLES --------------------------------------------------

Adult_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

Adult_Other_Clin <-
  suppressMessages(as_tibble(read_csv(
    here(
      "OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-clin-T-Scores-per-case.csv"
    )
  ))) %>%
  drop_na(clin_dx) %>%
  arrange(IDNumber)

# GENERATE TABLE OF DISORDER COUNTS ------------------------------------------

Adult_Self_clin_dx <- Adult_Self_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Adult Self',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Adult_Self_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Adult_Other_clin_dx <- Adult_Other_Clin %>%
  select(clin_dx) %>%
  gather("Variable", "dx") %>%
  group_by(Variable, dx) %>%
  count(Variable, dx) %>%
  ungroup() %>%
  mutate(
    form = case_when(is.na(lag(dx)) ~ 'Adult Other',
                     T ~ NA_character_),
    pct_samp = round(n / nrow(Adult_Other_Clin), 3)
  ) %>%
  select(form, dx, n, pct_samp)

Adult_clin_dx_counts <- bind_rows(
  Adult_Self_clin_dx,
  Adult_Other_clin_dx
)

rm(list = setdiff(ls(), ls(pattern = 'clin_dx_counts')))

# WRITE MANUAL TABLES -----------------------------------------------------

# write table of combined matched typical, clinical demo counts.
clin_dx_counts <- bind_rows(
  IT_430_clin_dx_counts,
  Preschool_25_clin_dx_counts,
  Child_512_clin_dx_counts,
  Teen_1221_clin_dx_counts,
  Adult_clin_dx_counts
  )

write_csv(clin_dx_counts,
          here(
            str_c(
              'OUTPUT-FILES/MANUAL-TABLES/t417-clin-dx-counts-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

