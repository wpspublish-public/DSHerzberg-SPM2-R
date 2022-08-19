source(here("CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R"))

Teen_1221_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv")
  )))

