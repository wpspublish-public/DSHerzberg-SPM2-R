source(here("CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R"))

IT_1030_Home_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber)
