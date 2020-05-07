source(here("CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R"))

Teen_1221_Home <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  )))

