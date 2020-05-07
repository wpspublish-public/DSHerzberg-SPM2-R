source(here("CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R"))

Teen_1221_Self <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  )))

