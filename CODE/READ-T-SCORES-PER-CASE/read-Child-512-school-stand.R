source(here("CODE/ITEM-VECTORS/Child-512-School-item-vectors.R"))

Child_512_School_Stand <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-T-Scores-per-case.csv")
  )))

