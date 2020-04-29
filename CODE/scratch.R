suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

typ <- suppressMessages(as_tibble(read_csv(
  here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case.csv")
)))

clin <- suppressMessages(as_tibble(read_csv(
  here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-clin-T-Scores-per-case.csv")
)))

identical(names(typ), names(clin))
