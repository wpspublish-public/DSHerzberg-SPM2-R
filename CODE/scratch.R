#### Teen-1221-School v Teen-1221-Self -----------------------------------------------
## STAND -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))

Teen_1221_School_Stand_DIF_comp <- Teen_1221_School_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_School = TOT_NT)

Teen_1221_Self_Stand_DIF_comp <- Teen_1221_Self_Stand %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Self = TOT_NT)

# create comp file, calc DIF score
Teen_1221_School_v_Teen_1221_Self_Stand_DIF <- Teen_1221_School_Stand_DIF_comp %>%
  inner_join(Teen_1221_Self_Stand_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_School - TOT_NT_Teen_1221_Self,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_School_v_Teen_1221_Self_Stand_DIF_output <- Teen_1221_School_v_Teen_1221_Self_Stand_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-Self',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Stand',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_School_v_Teen_1221_Self_Stand_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

## CLIN -------------------------------------------------------------------
# read data, parse columns
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Clin.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Clin.R"))

Teen_1221_School_Clin_DIF_comp <- Teen_1221_School_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_School = TOT_NT)

Teen_1221_Self_Clin_DIF_comp <- Teen_1221_Self_Clin %>% 
  select(IDNumber, TOT_NT) %>% 
  rename(TOT_NT_Teen_1221_Self = TOT_NT)

# create comp file, calc DIF score
Teen_1221_School_v_Teen_1221_Self_Clin_DIF <- Teen_1221_School_Clin_DIF_comp %>%
  inner_join(Teen_1221_Self_Clin_DIF_comp, by = "IDNumber") %>%
  mutate(
    DIF = TOT_NT_Teen_1221_School - TOT_NT_Teen_1221_Self,
    DIF_range = case_when(
      DIF <= -15 ~ "<=-15 (Definite Difference)",
      between(DIF,-14,-10) ~ "-10 to -14 (Probable Difference)",
      between(DIF,-9, 9) ~ "-9 to 9 (No Difference)",
      between(DIF, 10, 14) ~ "10 to 14 (Probable Difference)",
      DIF >= 15 ~ ">=15 (Definite Difference)"
    )
  )

DIF_range_order <- c("<=-15 (Definite Difference)", "-10 to -14 (Probable Difference)", 
                     "-9 to 9 (No Difference)", "10 to 14 (Probable Difference)", 
                     ">=15 (Definite Difference)")

# get DIF_range freq counts
Teen_1221_School_v_Teen_1221_Self_Clin_DIF_output <- Teen_1221_School_v_Teen_1221_Self_Clin_DIF %>%
  select(DIF_range) %>%
  gather("DIF_range") %>%
  group_by(DIF_range) %>%
  count(DIF_range) %>%
  ungroup() %>%
  arrange(match(DIF_range, DIF_range_order)) %>% 
  mutate(
    form1 = case_when(rownames(.) == "1" ~ 'Teen-1221-School',
                      T ~ NA_character_),
    form2 = case_when(rownames(.) == "1" ~ 'Teen-1221-Self',
                      T ~ NA_character_),
    sample = case_when(rownames(.) == "1" ~ 'Clin',
                       T ~ NA_character_),
    pct_samp = round(((n / nrow(Teen_1221_School_v_Teen_1221_Self_Clin_DIF)) * 100), 1)
  ) %>%
  select(form1, form2, sample, DIF_range, n, pct_samp)

rm(list = setdiff(ls(), ls(pattern = "output")))

