## Child-512-Home DATA ---------------------------------------------------------
# READ PAPER FORMS, OBTAIN T-SCORES ---------------------------------------
source(here('CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R'))

# Read paper forms
Child_512_Home_paper <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/PAPER-FORMS/SPM-2 Child Home 5-12.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  mutate_at(
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>% 
  select(IDNumber, contains("_raw"))

# Extract IDNumbers
Child_512_Home_paper_ID <- Child_512_Home_paper %>% 
  select(IDNumber)

# find shared cases with Child-512-Home-Stand
orig_data <- 
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/CHILD/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv')
  )))

paper_AgeInMonths <- Child_512_Home_paper_ID %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths)

# Add AgeInMonths to paper data
Child_512_Home_paper <- Child_512_Home_paper %>% 
  full_join(paper_AgeInMonths, by = "IDNumber") %>% 
  select(IDNumber, AgeInMonths, everything()) %>% 
  drop_na()

# read raw-to-t lookup tables, create lookup cols by scale
Child_512_Home_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Child_512_Home_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Child_512_Home_paper_T <- map_dfc(score_names,
                                ~
                                  Child_512_Home_paper %>% left_join(eval(as.name(
                                    str_c(.x, '_512_lookup_col')
                                  )),
                                  by = str_c(.x, '_raw')) %>%
                                  select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_Home_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))


# READ DIGITAL FORMS, OBTAIN T-SCORES ---------------------------------------
# join with digital data, obtain digital raw scores
Child_512_Home_paper_T_dig_raw <- Child_512_Home_paper_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>% 
  select(IDNumber, AgeInMonths, contains("_NT"),contains("_raw"))

# create lookup cols by scale
map_df(
  score_names,
  ~
    Child_512_Home_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Child_512_Home_paper_dig_T <- map_dfc(score_names,
                                    ~
                                      Child_512_Home_paper_T_dig_raw %>% left_join(eval(as.name(
                                        str_c(.x, '_512_lookup_col')
                                      )),
                                      by = str_c(.x, '_raw')) %>%
                                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_Home_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))

rm(list = ls(pattern = "col"))

# GENERATE DIG-PAPER EQUIV CORR TABLE -------------------------------------
cor_cols <- Child_512_Home_paper_dig_T %>% 
  select(contains('_NT'))

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Child_512_Home_paper_dig_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Child-512-Home',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


