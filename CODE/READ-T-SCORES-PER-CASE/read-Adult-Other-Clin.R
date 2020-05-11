source(here("CODE/ITEM-VECTORS/Adult-Other-item-vectors.R"))

Adult_Other_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-clin-T-Scores-per-case.csv")
  )))

