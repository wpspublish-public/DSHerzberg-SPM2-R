###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
###### ALPHA SECTION ----------------------------------------------------
#### HOME 49 DATA -------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------
IT_49_Home <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv')
  )))
) %>% arrange(IDNumber)

source(here('CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R'))

map_df(scale_order,
       ~
         IT_49_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_IT_49_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_49'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    IT_49_Home %>%
    filter(age_range == "03.5 to 6 mo") %>%
    select(eval(as.name(
      str_c(.x, '_items_IT_49_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_46'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    IT_49_Home %>%
    filter(age_range == "07 to 10.5 mo") %>%
    select(eval(as.name(
      str_c(.x, '_items_IT_49_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_79'), ., envir = .GlobalEnv)
)

n_49 <- nrow(TOT_item_scores_49)
n_46 <- nrow(TOT_item_scores_46)
n_79 <- nrow(TOT_item_scores_79)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_49 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_49')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_49 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_49'), ., envir = .GlobalEnv)
)
alpha_46 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_46')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_46 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_46'), ., envir = .GlobalEnv)
)
alpha_79 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_79')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_79 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_79'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_49, alpha_46, alpha_79)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
IT_49_Home_T_sd_49 <-
  IT_49_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_49 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_49)

output_49 <- alpha %>% left_join(IT_49_Home_T_sd_49,
                                 by = 'scale') %>%
  mutate(
    SEM_49 = sd_49 * (sqrt(1 - alpha_49)),
    CV_90 = 1.6449 * SEM_49,
    CV_95 = 1.96 * SEM_49
  ) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  mutate(
    form = case_when(rownames(.) == "1" ~ "Infant Form",
                     T ~ NA_character_),
    n_49 = case_when(rownames(.) == "1" ~ n_49,
                     T ~ NA_integer_),
    n_46 = case_when(rownames(.) == "1" ~ n_46,
                     T ~ NA_integer_),
    n_79 = case_when(rownames(.) == "1" ~ n_79,
                     T ~ NA_integer_),
  ) %>%
  select(form:n_79, everything(),-sd_49)

rm(list = setdiff(ls(), c("output_49")))

#### HOME 1030 DATA -------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------
IT_1030_Home <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv')
  )))
) %>% arrange(IDNumber)

source(here('CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R'))

map_df(scale_order,
       ~
         IT_1030_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_IT_1030_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_1030'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    IT_1030_Home %>%
    filter(age_range == "09.5 to 20 mo") %>%
    select(eval(as.name(
      str_c(.x, '_items_IT_1030_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_1020'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    IT_1030_Home %>%
    filter(age_range == "21 to 31.5 mo") %>%
    select(eval(as.name(
      str_c(.x, '_items_IT_1030_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_2130'), ., envir = .GlobalEnv)
)

n_1030 <- nrow(TOT_item_scores_1030)
n_1020 <- nrow(TOT_item_scores_1020)
n_2130 <- nrow(TOT_item_scores_2130)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_1030 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1030')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1030 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1030'), ., envir = .GlobalEnv)
)
alpha_1020 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1020')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1020 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1020'), ., envir = .GlobalEnv)
)
alpha_2130 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_2130')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_2130 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_2130'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_1030, alpha_1020, alpha_2130)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
IT_1030_Home_T_sd_1030 <-
  IT_1030_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_1030 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_1030)

output_1030 <- alpha %>% left_join(
  IT_1030_Home_T_sd_1030, 
  by = 'scale'
) %>% 
  mutate(SEM_1030 = sd_1030*(sqrt(1-alpha_1030)),
         CV_90 = 1.6449*SEM_1030,
         CV_95 = 1.96*SEM_1030) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(
    form = case_when(rownames(.) == "1" ~ "Toddler Form",
                     T ~ NA_character_),
    n_1030 = case_when(rownames(.) == "1" ~ n_1030,
                     T ~ NA_integer_),
    n_1020 = case_when(rownames(.) == "1" ~ n_1020,
                     T ~ NA_integer_),
    n_2130 = case_when(rownames(.) == "1" ~ n_2130,
                     T ~ NA_integer_),
  ) %>%
  select(form:n_2130, everything(),-sd_1030)

rm(list = setdiff(ls(), ls(pattern = "output")))

#### CAREGIVER DATA -------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------
IT_Caregiver <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case.csv')
  )))

source(here('CODE/ITEM-VECTORS/IT-Caregiver-item-vectors.R'))

map_df(scale_order,
       ~
         IT_Caregiver %>%
         select(eval(as.name(
           str_c(.x, '_items_IT_Caregiver')
         ))) %>%
         assign(str_c(.x, '_item_scores_430'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    IT_Caregiver %>%
    filter(age_range == "03.5 to 10 mo") %>%
    select(eval(as.name(
      str_c(.x, '_items_IT_Caregiver')
    ))) %>%
    assign(str_c(.x, '_item_scores_410'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    IT_Caregiver %>%
    filter(age_range == "11 to 31.5 mo") %>%
    select(eval(as.name(
      str_c(.x, '_items_IT_Caregiver')
    ))) %>%
    assign(str_c(.x, '_item_scores_1130'), ., envir = .GlobalEnv)
)

n_430 <- nrow(TOT_item_scores_430)
n_410 <- nrow(TOT_item_scores_410)
n_1130 <- nrow(TOT_item_scores_1130)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_430 <- map_df(scale_order, ~
                      alpha(
                        cor(
                          eval(as.name(str_c(.x, '_item_scores_430')))
                        )
                      )[["total"]] %>%
                      mutate(scale = .x) %>% 
                      select(scale, raw_alpha) %>% 
                      rename(alpha_430 = raw_alpha) %>% 
                      assign(str_c(.x, '_alpha_430'), ., envir = .GlobalEnv)
)
alpha_410 <- map_df(scale_order, ~
                      alpha(
                        cor(
                          eval(as.name(str_c(.x, '_item_scores_410')))
                        ),
                        check.keys = TRUE
                      )[["total"]] %>%
                      mutate(scale = .x) %>% 
                      select(scale, raw_alpha) %>% 
                      rename(alpha_410 = raw_alpha) %>% 
                      assign(str_c(.x, '_alpha_410'), ., envir = .GlobalEnv)
)
alpha_1130 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1130')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1130 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1130'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_430, alpha_410, alpha_1130)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
IT_Caregiver_T_sd_430 <-
  IT_Caregiver %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_430 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_430)

output_Caregiver <- alpha %>% left_join(
  IT_Caregiver_T_sd_430, 
  by = 'scale'
) %>% 
  mutate(SEM_430 = sd_430*(sqrt(1-alpha_430)),
         CV_90 = 1.6449*SEM_430,
         CV_95 = 1.96*SEM_430) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(
    form = case_when(rownames(.) == "1" ~ "Caregiver Form",
                     T ~ NA_character_),
    n_430 = case_when(rownames(.) == "1" ~ n_430,
                     T ~ NA_integer_),
    n_410 = case_when(rownames(.) == "1" ~ n_410,
                     T ~ NA_integer_),
    n_1130 = case_when(rownames(.) == "1" ~ n_1130,
                     T ~ NA_integer_),
  ) %>%
  select(form:n_1130, everything(),-sd_430)

rm(list = setdiff(ls(), ls(pattern = "output")))

#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

forms <- c('49', '1030', 'Caregiver')

map(forms, ~
      write_csv(eval(as.name(str_c('output_', .x))), here(
        paste0(
          'OUTPUT-FILES/MANUAL-TABLES/t501-IT-',
          .x,
          '-alpha-',
          format(Sys.Date(), "%Y-%m-%d"),
          '.csv'
        )
      ),
      na = ''))

###### TRT SECTION ----------------------------------------------------

#### HOME 49 DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
IT_49_Home_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
IT_49_Home_time2 <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-time2-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-time2-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
IT_49_Home_TRT <- IT_49_Home_Stand %>% 
  inner_join(
    IT_49_Home_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- IT_49_Home_TRT %>% 
  select(contains('_NT'))

# This names the rows of interest in the overall correlation matrix (e.g., TOT
# at t1 correlated with TOT at t2)
cor_row <- c('TOT_NT_1-TOT_NT_2', 'SOC_NT_1-SOC_NT_2', 'VIS_NT_1-VIS_NT_2',
             'HEA_NT_1-HEA_NT_2', 'TOU_NT_1-TOU_NT_2', 'TS_NT_1-TS_NT_2',
             'BOD_NT_1-BOD_NT_2', 'BAL_NT_1-BAL_NT_2', 'PLA_NT_1-PLA_NT_2')

# scale order for final output table
scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")


# GENERATE TRT CORRS AND DESC ---------------------------------------------

# using psych::corr.test(), write table containing the test-retest corrs for
# each scale, along with p values. `corr.test` returns a list, and the
# expression `corr.test(cor_cols)[['ci']]` plucks the correlation matrix (as a
# df) from the list.
IT_49_Home_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'IT 49 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
IT_49_Home_t1_T_desc <-
  IT_49_Home_TRT %>% 
  select(contains('_NT_t1')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, n, mean, sd) %>% 
  rename(n = n,
         mean_t1 = mean,
         sd_t1 = sd)

# t2 descriptives
IT_49_Home_t2_T_desc <-
  IT_49_Home_TRT %>% 
  select(contains('_NT_t2')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, mean, sd) %>% 
  rename(mean_t2 = mean,
         sd_t2 = sd)


# JOIN CORRS AND DESC INTO OUTPUT FORMAT ----------------------------------

# Join the descriptive tables and calculate ES for the t1-t2 T-score diff
IT_49_Home_TRT_T_desc <- IT_49_Home_t1_T_desc %>% 
  full_join(
    IT_49_Home_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
IT_49_Home_TRT_T <- IT_49_Home_TRT_cor_table %>%
  full_join(
    IT_49_Home_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), 'IT_49_Home_TRT_T'))



#### HOME 1030 DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
IT_1030_Home_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
IT_1030_Home_time2 <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-time2-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-time2-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
IT_1030_Home_TRT <- IT_1030_Home_Stand %>% 
  inner_join(
    IT_1030_Home_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- IT_1030_Home_TRT %>% 
  select(contains('_NT'))

# This names the rows of interest in the overall correlation matrix (e.g., TOT
# at t1 correlated with TOT at t2)
cor_row <- c('TOT_NT_1-TOT_NT_2', 'SOC_NT_1-SOC_NT_2', 'VIS_NT_1-VIS_NT_2',
             'HEA_NT_1-HEA_NT_2', 'TOU_NT_1-TOU_NT_2', 'TS_NT_1-TS_NT_2',
             'BOD_NT_1-BOD_NT_2', 'BAL_NT_1-BAL_NT_2', 'PLA_NT_1-PLA_NT_2')

# scale order for final output table
scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")


# GENERATE TRT CORRS AND DESC ---------------------------------------------

# using psych::corr.test(), write table containing the test-retest corrs for
# each scale, along with p values. `corr.test` returns a list, and the
# expression `corr.test(cor_cols)[['ci']]` plucks the correlation matrix (as a
# df) from the list.
IT_1030_Home_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'IT 1030 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
IT_1030_Home_t1_T_desc <-
  IT_1030_Home_TRT %>% 
  select(contains('_NT_t1')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, n, mean, sd) %>% 
  rename(n = n,
         mean_t1 = mean,
         sd_t1 = sd)

# t2 descriptives
IT_1030_Home_t2_T_desc <-
  IT_1030_Home_TRT %>% 
  select(contains('_NT_t2')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, mean, sd) %>% 
  rename(mean_t2 = mean,
         sd_t2 = sd)


# JOIN CORRS AND DESC INTO OUTPUT FORMAT ----------------------------------

# Join the descriptive tables and calculate ES for the t1-t2 T-score diff
IT_1030_Home_TRT_T_desc <- IT_1030_Home_t1_T_desc %>% 
  full_join(
    IT_1030_Home_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
IT_1030_Home_TRT_T <- IT_1030_Home_TRT_cor_table %>%
  full_join(
    IT_1030_Home_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), c('IT_49_Home_TRT_T',
                          'IT_1030_Home_TRT_T'
)))



#### CAREGIVER DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
IT_Caregiver_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case.csv')
  ))
  ) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
IT_Caregiver_time2 <-
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-time2-T-Scores-per-case.csv')
  ))
  ) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
IT_Caregiver_TRT <- IT_Caregiver_Stand %>% 
  inner_join(
    IT_Caregiver_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- IT_Caregiver_TRT %>% 
  select(contains('_NT'))

# This names the rows of interest in the overall correlation matrix (e.g., TOT
# at t1 correlated with TOT at t2)
cor_row <- c('TOT_NT_1-TOT_NT_2', 'SOC_NT_1-SOC_NT_2', 'VIS_NT_1-VIS_NT_2',
             'HEA_NT_1-HEA_NT_2', 'TOU_NT_1-TOU_NT_2', 'TS_NT_1-TS_NT_2',
             'BOD_NT_1-BOD_NT_2', 'BAL_NT_1-BAL_NT_2', 'PLA_NT_1-PLA_NT_2')

# scale order for final output table
scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")


# GENERATE TRT CORRS AND DESC ---------------------------------------------

# using psych::corr.test(), write table containing the test-retest corrs for
# each scale, along with p values. `corr.test` returns a list, and the
# expression `corr.test(cor_cols)[['ci']]` plucks the correlation matrix (as a
# df) from the list.
IT_Caregiver_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'IT Caregiver', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
IT_Caregiver_t1_T_desc <-
  IT_Caregiver_TRT %>% 
  select(contains('_NT_t1')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, n, mean, sd) %>% 
  rename(n = n,
         mean_t1 = mean,
         sd_t1 = sd)

# t2 descriptives
IT_Caregiver_t2_T_desc <-
  IT_Caregiver_TRT %>% 
  select(contains('_NT_t2')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  mutate(scale1 = str_sub(scale, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, mean, sd) %>% 
  rename(mean_t2 = mean,
         sd_t2 = sd)


# JOIN CORRS AND DESC INTO OUTPUT FORMAT ----------------------------------

# Join the descriptive tables and calculate ES for the t1-t2 T-score diff
IT_Caregiver_TRT_T_desc <- IT_Caregiver_t1_T_desc %>% 
  full_join(
    IT_Caregiver_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
IT_Caregiver_TRT_T <- IT_Caregiver_TRT_cor_table %>%
  full_join(
    IT_Caregiver_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), c('IT_49_Home_TRT_T',
                          'IT_1030_Home_TRT_T',
                          'IT_Caregiver_TRT_T'
)))



#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

forms <- c('49_Home', '1030_Home', 'Caregiver')

map(forms, ~
      write_csv(eval(as.name(str_c('IT_', .x, '_TRT_T'))), here(
        paste0(
          'OUTPUT-FILES/MANUAL-TABLES/t501-IT-',
          .x,
          '-TRT-',
          format(Sys.Date(), "%Y-%m-%d"),
          '.csv'
        )
      ),
      na = ''))

