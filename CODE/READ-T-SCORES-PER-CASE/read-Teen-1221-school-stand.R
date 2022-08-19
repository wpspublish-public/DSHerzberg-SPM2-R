source(here("CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R"))

Teen_1221_School_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
  )))

