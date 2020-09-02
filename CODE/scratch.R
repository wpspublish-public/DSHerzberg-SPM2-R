# READ FINALIZED STAND SAMPLE ---------------------------------------------

IT_46_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-49-Home-allData-desamp.csv")
  ))) %>% 
  filter(AgeInMonths <=6) %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

# score_names_old <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")
score_names <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "TOT", "PLA", "SOC")
subscale_names <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA", "SOC")




# Repeat above for subscale raw-to-T columns.
subscale_lookup <- map(
  subscale_names, 
  

  
  
  # join TOT and subscale columns
  all_lookup <- full_join(TOT_lookup, subscale_lookup, by = 'raw') %>% 
    relocate(TOT_NT, .after = "BAL_NT")
  
