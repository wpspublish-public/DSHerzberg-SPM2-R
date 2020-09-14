suppressMessages(library(here))
suppressMessages(library(tidyverse))

  
  
 
test_LJ <- tibble(TOT_raw = 15:60) %>% 
  left_join(lookup_school_environ[[1]], by = "TOT_raw")

test_complete <- lookup_school_environ[[1]] %>%
  mutate(file = "ART") %>% 
  # relocate(file, .before = "TOT_raw") %>% 
  complete(file, TOT_raw = 15:60)


form1 <- unique(form_IDs$form)
environ1 <- unique(form_IDs$environ)
age_group1 <- unique(form_IDs$age_group)
