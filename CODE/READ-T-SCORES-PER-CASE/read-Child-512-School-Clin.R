source(here("CODE/ITEM-VECTORS/Child-512-School-item-vectors.R"))

Child_512_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-clin-T-Scores-per-case.csv")
  )))

