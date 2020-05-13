## Adult-Other DATA -----------------------------------------------------

source(here('CODE/ITEM-VECTORS/Adult-Other-item-vectors.R'))

Adult_Other_alt_form1 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Other Report Questionnaire Alt-form1.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Adult_Other)

Adult_Other_alt_form2 <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/ALT/SPM-2 Adult ages 1690 Other Report Questionnaire Alt-form2.csv")
  ))) %>% 
  select(IDNumber, Age, All_items_Adult_Other)
