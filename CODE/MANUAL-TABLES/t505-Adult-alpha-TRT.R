###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
###### ALPHA SECTION ----------------------------------------------------

#### ADULT SELF DATA -----------------------------------------------------------------

# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ------------------------

Adult_Self <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv')
  )))

source(here('CODE/ITEM-VECTORS/Adult-Self-item-vectors.R'))

map_df(scale_order,
       ~
         Adult_Self %>%
         select(eval(as.name(
           str_c(.x, '_items_Adult_Self')
         ))) %>%
         assign(str_c(.x, '_item_scores_2199'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Adult_Self %>%
    filter(age_range == "21.00 to 30.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_2130'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Self %>%
    filter(age_range == "31.00 to 40.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_3140'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Self %>%
    filter(age_range == "41.00 to 50.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_4150'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Self %>%
    filter(age_range == "51.00 to 64.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_5164'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Self %>%
    filter(age_range == "65.00 to 99.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_6599'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------

alpha_2199 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_2199')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_2199 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_2199'), ., envir = .GlobalEnv)
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
alpha_3140 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_3140')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_3140 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_3140'), ., envir = .GlobalEnv)
)
alpha_4150 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_4150')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_4150 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_4150'), ., envir = .GlobalEnv)
)
alpha_5164 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_5164')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_5164 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_5164'), ., envir = .GlobalEnv)
)
alpha_6599 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_6599')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_6599 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_6599'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_2199, alpha_2130, alpha_3140, alpha_4150, alpha_5164, alpha_6599)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------

Adult_Self_T_sd_2199 <-
  Adult_Self %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_2199 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_2199)

output_Self <- alpha %>% left_join(
  Adult_Self_T_sd_2199, 
  by = 'scale'
) %>% 
  mutate(SEM_2199 = sd_2199*(sqrt(1-alpha_2199)),
         CV_90 = 1.6449*SEM_2199,
         CV_95 = 1.96*SEM_2199) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Self Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_2199) 

rm(list = setdiff(ls(), ls(pattern = "output")))

#### ADULT OTHER DATA -----------------------------------------------------------------

# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------
Adult_Other <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-T-Scores-per-case.csv')
  )))

source(here('CODE/ITEM-VECTORS/Adult-Other-item-vectors.R'))

map_df(scale_order,
       ~
         Adult_Other %>%
         select(eval(as.name(
           str_c(.x, '_items_Adult_Other')
         ))) %>%
         assign(str_c(.x, '_item_scores_2199'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Adult_Other %>%
    filter(age_range == "21.00 to 30.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Other')
    ))) %>%
    assign(str_c(.x, '_item_scores_2130'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Other %>%
    filter(age_range == "31.00 to 40.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Other')
    ))) %>%
    assign(str_c(.x, '_item_scores_3140'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Other %>%
    filter(age_range == "41.00 to 50.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Other')
    ))) %>%
    assign(str_c(.x, '_item_scores_4150'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Other %>%
    filter(age_range == "51.00 to 64.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Other')
    ))) %>%
    assign(str_c(.x, '_item_scores_5164'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Adult_Other %>%
    filter(age_range == "65.00 to 99.99 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Adult_Other')
    ))) %>%
    assign(str_c(.x, '_item_scores_6599'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)


# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_2199 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_2199')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_2199 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_2199'), ., envir = .GlobalEnv)
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
alpha_3140 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_3140')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_3140 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_3140'), ., envir = .GlobalEnv)
)
alpha_4150 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_4150')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_4150 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_4150'), ., envir = .GlobalEnv)
)
alpha_5164 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_5164')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_5164 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_5164'), ., envir = .GlobalEnv)
)
alpha_6599 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_6599')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_6599 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_6599'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_2199, alpha_2130, alpha_3140, alpha_4150, alpha_5164, alpha_6599)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))


# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
Adult_Other_T_sd_2199 <-
  Adult_Other %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_2199 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_2199)

output_Other <- alpha %>% left_join(
  Adult_Other_T_sd_2199, 
  by = 'scale'
) %>% 
  mutate(SEM_2199 = sd_2199*(sqrt(1-alpha_2199)),
         CV_90 = 1.6449*SEM_2199,
         CV_95 = 1.96*SEM_2199) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Rater Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_2199) 

rm(list = setdiff(ls(), ls(pattern = "output")))

#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Self,
                    output_Other),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t505-Adult-alpha-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')
rm(list = ls())

###### TRT SECTION ----------------------------------------------------
#### ADULT SELF DATA ---------------------------------------------

# READ & PARSE DATA -------------------------------------------------------

# read stand
Adult_Self_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Adult_Self_time2 <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-time2-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Adult_Self_TRT <- Adult_Self_Stand %>% 
  inner_join(
    Adult_Self_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>% 
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Adult_Self_TRT %>% 
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
Adult_Self_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Adult Self', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Adult_Self_t1_T_desc <-
  Adult_Self_TRT %>% 
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
Adult_Self_t2_T_desc <-
  Adult_Self_TRT %>% 
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
Adult_Self_TRT_T_desc <- Adult_Self_t1_T_desc %>% 
  full_join(
    Adult_Self_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Adult_Self_TRT_T <- Adult_Self_TRT_cor_table %>%
  full_join(
    Adult_Self_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
         ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), 'Adult_Self_TRT_T'))
 

#### ADULT OTHER DATA ---------------------------------------------

# READ & PARSE DATA -------------------------------------------------------

# read stand
Adult_Other_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Adult_Other_time2 <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-time2-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Adult_Other_TRT <- Adult_Other_Stand %>% 
  inner_join(
    Adult_Other_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>% 
  arrange(TOT_NT_dif) %>%
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Adult_Other_TRT %>% 
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
Adult_Other_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Adult Other', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Adult_Other_t1_T_desc <-
  Adult_Other_TRT %>% 
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
Adult_Other_t2_T_desc <-
  Adult_Other_TRT %>% 
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
Adult_Other_TRT_T_desc <- Adult_Other_t1_T_desc %>% 
  full_join(
    Adult_Other_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Adult_Other_TRT_T <- Adult_Other_TRT_cor_table %>%
  full_join(
    Adult_Other_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), c('Adult_Self_TRT_T', 'Adult_Other_TRT_T')))


#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(Adult_Self_TRT_T,
                    Adult_Other_TRT_T),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t505-Adult-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

