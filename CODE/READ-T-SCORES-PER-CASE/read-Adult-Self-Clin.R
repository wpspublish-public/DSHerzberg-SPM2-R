source(here("CODE/ITEM-VECTORS/Adult-Self-item-vectors.R"))

Adult_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-clin-T-Scores-per-case.csv")
  )))

