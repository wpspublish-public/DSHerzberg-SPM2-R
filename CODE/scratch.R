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

