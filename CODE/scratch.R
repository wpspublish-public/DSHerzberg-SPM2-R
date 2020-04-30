# SCHOOL DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLE

Preschool_25_School_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

scale_order <- c("SOC_raw", "VIS_raw", "HEA_raw", "TOU_raw", 
                 "TS_raw", "BOD_raw", "BAL_raw", "PLA_raw", "TOT_raw")

# WRITE RAW SCORE DESCRIPTIVES

Preschool_25_School_Stand_raw_desc <-
  Preschool_25_School_Stand %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "School Form Stand",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Stand = n,
         mean_Stand = mean,
         sd_Stand = sd)

########## CLIN

# READ FINALIZED SAMPLE

Preschool_25_School_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

# WRITE RAW SCORE DESCRIPTIVES

Preschool_25_School_Clin_raw_desc <-
  Preschool_25_School_Clin %>% 
  select(contains('raw')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale =="SOC_raw" ~ "School Form Clin",
    T ~ NA_character_
  )) %>% 
  select(form, scale, n, mean, sd) %>% 
  rename(n_Clin = n,
         mean_Clin = mean,
         sd_Clin = sd)

# Combine stand, clin columns, add ES column

Preschool_25_School_raw_desc <- bind_cols(Preschool_25_School_Stand_raw_desc,
                                        Preschool_25_School_Clin_raw_desc) %>%
  select(-scale1) %>%
  mutate(ES = abs((mean_Stand - mean_Clin) / ((sd_Stand + sd_Clin / 2)))) %>%
  mutate_at(vars(mean_Stand, sd_Stand, mean_Clin, sd_Clin, ES), ~
              (round(., 2)))

rm(list = ls(pattern = 'Clin'))
rm(list = ls(pattern = 'Stand'))

