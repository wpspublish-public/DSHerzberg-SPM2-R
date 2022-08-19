source(here("CODE/ITEM-VECTORS/IT-Caregiver-item-vectors.R"))

IT_Caregiver_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-clin-T-Scores-per-case.csv")
  )))

