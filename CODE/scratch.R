## Adult-Other DATA ---------------------------------------------------------
# READ PAPER FORMS, OBTAIN T-SCORES ---------------------------------------
source(here('CODE/ITEM-VECTORS/Adult-Other-item-vectors.R'))

# Read paper forms
Adult_Other_paper <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/ADULT/PAPER-FORMS/SPM-2 Adult Other 16-90.csv")
  ))) %>% 
  rename(IDNumber = ID) %>% 
  drop_na(IDNumber) %>% 
  arrange(IDNumber) %>% 
  mutate_at(
    SOC_rev_items_Adult_Other,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Other]),
    SOC_raw = rowSums(.[SOC_items_Adult_Other]),
    VIS_raw = rowSums(.[VIS_items_Adult_Other]),
    HEA_raw = rowSums(.[HEA_items_Adult_Other]),
    TOU_raw = rowSums(.[TOU_items_Adult_Other]),
    TS_raw = rowSums(.[TS_items_Adult_Other]),
    BOD_raw = rowSums(.[BOD_items_Adult_Other]),
    BAL_raw = rowSums(.[BAL_items_Adult_Other]),
    PLA_raw = rowSums(.[PLA_items_Adult_Other])
  ) %>% 
  select(IDNumber, contains("_raw"))

# Extract IDNumbers
Adult_Other_paper_ID <- Adult_Other_paper %>% 
  select(IDNumber)

# find shared cases with Adult-Other-Stand
orig_data <- 
  suppressMessages(as_tibble(read_csv(
    here('INPUT-FILES/ADULT/SM-ONLY-NORMS-INPUT/Adult-Other-SM-only-norms-input.csv')
  )))

paper_Age <- Adult_Other_paper_ID %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  select(IDNumber, Age)

# Add Age to paper data
Adult_Other_paper <- Adult_Other_paper %>% 
  full_join(paper_Age, by = "IDNumber") %>% 
  select(IDNumber, Age, everything()) %>% 
  drop_na()

# read raw-to-t lookup tables, create lookup cols by scale
Adult_Other_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/RAW-T-LOOKUP-TABLES/Adult-Other-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Adult_Other_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Other_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Adult_Other_paper_T <- map_dfc(score_names,
                                ~
                                  Adult_Other_paper %>% left_join(eval(as.name(
                                    str_c(.x, '_Other_lookup_col')
                                  )),
                                  by = str_c(.x, '_raw')) %>%
                                  select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Adult_Other_paper, .) %>% 
  select(IDNumber, contains("_NT")) %>% 
  rename_at(vars(contains("_NT")), ~ str_c(., "_p"))

rm(list = ls(pattern = "col"))


# READ DIGITAL FORMS, OBTAIN T-SCORES ---------------------------------------
# join with digital data, obtain digital raw scores
Adult_Other_paper_T_dig_raw <- Adult_Other_paper_T %>% 
  inner_join(orig_data, by = "IDNumber") %>% 
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Other]),
    SOC_raw = rowSums(.[SOC_items_Adult_Other]),
    VIS_raw = rowSums(.[VIS_items_Adult_Other]),
    HEA_raw = rowSums(.[HEA_items_Adult_Other]),
    TOU_raw = rowSums(.[TOU_items_Adult_Other]),
    TS_raw = rowSums(.[TS_items_Adult_Other]),
    BOD_raw = rowSums(.[BOD_items_Adult_Other]),
    BAL_raw = rowSums(.[BAL_items_Adult_Other]),
    PLA_raw = rowSums(.[PLA_items_Adult_Other])
  ) %>% 
  select(IDNumber, Age, contains("_NT"),contains("_raw"))

# create lookup cols by scale
map_df(
  score_names,
  ~
    Adult_Other_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Other_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
Adult_Other_paper_dig_T <- map_dfc(score_names,
                                    ~
                                      Adult_Other_paper_T_dig_raw %>% left_join(eval(as.name(
                                        str_c(.x, '_Other_lookup_col')
                                      )),
                                      by = str_c(.x, '_raw')) %>%
                                      select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Adult_Other_paper_T_dig_raw, .) %>% 
  select(IDNumber, contains("_NT"))%>% 
  mutate(TOT_NT_dif = abs(TOT_NT - TOT_NT_p)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

rm(list = ls(pattern = "col"))

# GENERATE DIG-PAPER EQUIV CORR TABLE -------------------------------------
cor_cols <- Adult_Other_paper_dig_T %>% 
  select(contains('_NT'), -TOT_NT_dif)

cor_row <- c('TOT_NT_-TOT_NT', 'SOC_NT_-SOC_NT', 'VIS_NT_-VIS_NT',
             'HEA_NT_-HEA_NT', 'TOU_NT_-TOU_NT', 'TS_NT_-TS_NT',
             'BOD_NT_-BOD_NT', 'BAL_NT_-BAL_NT', 'PLA_NT_-PLA_NT')

Adult_Other_paper_dig_cor_table <-
  corr.test(cor_cols)[['ci']] %>%
  rownames_to_column(var = 'pair') %>%
  filter(pair %in% cor_row) %>%
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')) %>%
  arrange(match(scale, scale_order)) %>%
  mutate(
    form = case_when(scale == 'SOC' ~ 'Adult-Other',
                     T ~ NA_character_),
    n = case_when(scale == 'SOC' ~ corr.test(cor_cols)[['n']][1],
                  T ~ NA_real_)
  ) %>%
  select(form, scale, n, r, p) %>%
  mutate_if(is.numeric, ~ round(., 3))

rm(list = setdiff(ls(), ls(pattern = 'table')))


