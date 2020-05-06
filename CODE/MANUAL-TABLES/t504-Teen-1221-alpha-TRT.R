###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
###### ALPHA SECTION ----------------------------------------------------
#### TEEN HOME DATA -----------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE -------------------------------------
Teen_1221_Home <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv')
  )))

source(here('CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R'))

map_df(scale_order,
       ~
         Teen_1221_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_Teen_1221_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_1221'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Teen_1221_Home %>%
    filter(age_range == "12 to 13 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_1213'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_Home %>%
    filter(age_range == "14 to 15 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_1415'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_Home %>%
    filter(age_range == "16 to 17 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_1617'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_Home %>%
    filter(age_range == "18 to 21 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_1821'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_1221 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1221')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1221 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1221'), ., envir = .GlobalEnv)
)
alpha_1213 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1213')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1213 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1213'), ., envir = .GlobalEnv)
)
alpha_1415 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1415')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1415 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1415'), ., envir = .GlobalEnv)
)
alpha_1617 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1617')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1617 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1617'), ., envir = .GlobalEnv)
)
alpha_1821 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1821')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1821 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1821'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_1221, alpha_1213, alpha_1415, alpha_1617, alpha_1821)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))


# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
Teen_1221_Home_T_sd_1221 <-
  Teen_1221_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_1221 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_1221)

output_Home <- alpha %>% left_join(
  Teen_1221_Home_T_sd_1221, 
  by = 'scale'
) %>% 
  mutate(SEM_1221 = sd_1221*(sqrt(1-alpha_1221)),
         CV_90 = 1.6449*SEM_1221,
         CV_95 = 1.96*SEM_1221) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Home Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_1221) 

rm(list = setdiff(ls(), ls(pattern = "output")))

#### TEEN SCHOOL DATA -----------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------------------
Teen_1221_School <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv')
  )))

source(here('CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R'))

map_df(scale_order,
       ~
         Teen_1221_School %>%
         select(eval(as.name(
           str_c(.x, '_items_Teen_1221_School')
         ))) %>%
         assign(str_c(.x, '_item_scores_1221'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Teen_1221_School %>%
    filter(age_range == "12 to 13 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_1213'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_School %>%
    filter(age_range == "14 to 15 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_1415'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_School %>%
    filter(age_range == "16 to 17 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_1617'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_School %>%
    filter(age_range == "18 to 21 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_1821'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)


# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_1221 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1221')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1221 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1221'), ., envir = .GlobalEnv)
)
alpha_1213 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1213')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1213 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1213'), ., envir = .GlobalEnv)
)
alpha_1415 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1415')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1415 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1415'), ., envir = .GlobalEnv)
)
alpha_1617 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1617')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1617 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1617'), ., envir = .GlobalEnv)
)
alpha_1821 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1821')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1821 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1821'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_1221, alpha_1213, alpha_1415, alpha_1617, alpha_1821)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))


# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
Teen_1221_School_T_sd_1221 <-
  Teen_1221_School %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_1221 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_1221)

output_School <- alpha %>% left_join(
  Teen_1221_School_T_sd_1221, 
  by = 'scale'
) %>% 
  mutate(SEM_1221 = sd_1221*(sqrt(1-alpha_1221)),
         CV_90 = 1.6449*SEM_1221,
         CV_95 = 1.96*SEM_1221) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "School Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_1221) 

rm(list = setdiff(ls(), ls(pattern = "output")))

#### TEEN SELF DATA -----------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE -------------------------------------
Teen_1221_Self <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv')
  )))

source(here('CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R'))

map_df(scale_order,
       ~
         Teen_1221_Self %>%
         select(eval(as.name(
           str_c(.x, '_items_Teen_1221_Self')
         ))) %>%
         assign(str_c(.x, '_item_scores_1221'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Teen_1221_Self %>%
    filter(age_range == "12 to 13 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_1213'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_Self %>%
    filter(age_range == "14 to 15 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_1415'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_Self %>%
    filter(age_range == "16 to 17 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_1617'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Teen_1221_Self %>%
    filter(age_range == "18 to 21 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Teen_1221_Self')
    ))) %>%
    assign(str_c(.x, '_item_scores_1821'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_1221 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1221')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1221 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1221'), ., envir = .GlobalEnv)
)
alpha_1213 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1213')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1213 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1213'), ., envir = .GlobalEnv)
)
alpha_1415 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1415')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1415 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1415'), ., envir = .GlobalEnv)
)
alpha_1617 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1617')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1617 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1617'), ., envir = .GlobalEnv)
)
alpha_1821 <- map_df(scale_order, ~
                       alpha(
                         cor(
                           eval(as.name(str_c(.x, '_item_scores_1821')))
                         )
                       )[["total"]] %>%
                       mutate(scale = .x) %>% 
                       select(scale, raw_alpha) %>% 
                       rename(alpha_1821 = raw_alpha) %>% 
                       assign(str_c(.x, '_alpha_1821'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_1221, alpha_1213, alpha_1415, alpha_1617, alpha_1821)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
Teen_1221_Self_T_sd_1221 <-
  Teen_1221_Self %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_1221 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_1221)

output_Self <- alpha %>% left_join(
  Teen_1221_Self_T_sd_1221, 
  by = 'scale'
) %>% 
  mutate(SEM_1221 = sd_1221*(sqrt(1-alpha_1221)),
         CV_90 = 1.6449*SEM_1221,
         CV_95 = 1.96*SEM_1221) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Self Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_1221) 

rm(list = setdiff(ls(), ls(pattern = "output")))

#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Home,
                    output_School,
                    output_Self),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t504-Teen-1221-alpha-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

rm(list = ls())



###### TRT SECTION ----------------------------------------------------
#### TEEN HOME DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
Teen_1221_Home_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Teen_1221_Home_time2 <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-time2-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Teen_1221_Home_TRT <- Teen_1221_Home_Stand %>% 
  inner_join(
    Teen_1221_Home_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Teen_1221_Home_TRT %>% 
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
Teen_1221_Home_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Teen 1221 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Teen_1221_Home_t1_T_desc <-
  Teen_1221_Home_TRT %>% 
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
Teen_1221_Home_t2_T_desc <-
  Teen_1221_Home_TRT %>% 
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
Teen_1221_Home_TRT_T_desc <- Teen_1221_Home_t1_T_desc %>% 
  full_join(
    Teen_1221_Home_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Teen_1221_Home_TRT_T <- Teen_1221_Home_TRT_cor_table %>%
  full_join(
    Teen_1221_Home_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), 'Teen_1221_Home_TRT_T'))



#### TEEN SCHOOL DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
Teen_1221_School_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Teen_1221_School_time2 <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-time2-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Teen_1221_School_TRT <- Teen_1221_School_Stand %>% 
  inner_join(
    Teen_1221_School_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Teen_1221_School_TRT %>% 
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
Teen_1221_School_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Teen 1221 School', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Teen_1221_School_t1_T_desc <-
  Teen_1221_School_TRT %>% 
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
Teen_1221_School_t2_T_desc <-
  Teen_1221_School_TRT %>% 
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
Teen_1221_School_TRT_T_desc <- Teen_1221_School_t1_T_desc %>% 
  full_join(
    Teen_1221_School_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Teen_1221_School_TRT_T <- Teen_1221_School_TRT_cor_table %>%
  full_join(
    Teen_1221_School_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), c('Teen_1221_Home_TRT_T', 'Teen_1221_School_TRT_T')))

#### TEEN SELF DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
Teen_1221_Self_Stand <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Teen_1221_Self_time2 <- 
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-time2-T-Scores-per-case.csv")
  ))) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Teen_1221_Self_TRT <- Teen_1221_Self_Stand %>% 
  inner_join(
    Teen_1221_Self_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Teen_1221_Self_TRT %>% 
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
Teen_1221_Self_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Teen 1221 Self', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Teen_1221_Self_t1_T_desc <-
  Teen_1221_Self_TRT %>% 
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
Teen_1221_Self_t2_T_desc <-
  Teen_1221_Self_TRT %>% 
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
Teen_1221_Self_TRT_T_desc <- Teen_1221_Self_t1_T_desc %>% 
  full_join(
    Teen_1221_Self_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Teen_1221_Self_TRT_T <- Teen_1221_Self_TRT_cor_table %>%
  full_join(
    Teen_1221_Self_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), c('Teen_1221_Home_TRT_T', 
                          'Teen_1221_School_TRT_T', 
                          'Teen_1221_Self_TRT_T'
                          )))

#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(Teen_1221_Home_TRT_T,
                    Teen_1221_School_TRT_T,
                    Teen_1221_Self_TRT_T),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t504-Teen-1221-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

