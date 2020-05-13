Clin_all_forms_ages <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Home-Clin-All-T-scores-per-case.csv")
  ))), 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/School-Clin-All-T-scores-per-case.csv")
  ))), 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Self-Clin-All-T-scores-per-case.csv")
  ))), 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/COMBINED-T-SCORES-PER-CASE/Other-Clin-All-T-scores-per-case.csv")
  ))) 
) %>% 
  arrange(IDNumber)
