###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))
###### ALPHA SECTION ----------------------------------------------------
#### CHILD HOME DATA -------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------
Preschool_25_Home <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-T-Scores-per-case.csv')
  )))
) %>% arrange(IDNumber)

source(here('CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R'))

map_df(scale_order,
       ~
         Preschool_25_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_Preschool_25_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_25'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Preschool_25_Home %>%
    filter(age_range == "2 to 4 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Preschool_25_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_24'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Preschool_25_Home %>%
    filter(age_range == "5 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Preschool_25_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_5'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_25 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_25')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_25 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_25'), ., envir = .GlobalEnv)
)
alpha_24 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_24')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_24 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_24'), ., envir = .GlobalEnv)
)
alpha_5 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_5')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_5 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_5'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_25, alpha_24, alpha_5)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
Preschool_25_Home_T_sd_25 <-
  Preschool_25_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_25 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_25)

output_Home <- alpha %>% left_join(
  Preschool_25_Home_T_sd_25, 
  by = 'scale'
) %>% 
  mutate(SEM_25 = sd_25*(sqrt(1-alpha_25)),
         CV_90 = 1.6449*SEM_25,
         CV_95 = 1.96*SEM_25) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Home Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_25) 

rm(list=setdiff(ls(), c("output_Home")))

#### CHILD SCHOOL DATA -----------------------------------------------------------------
# READ DATA & STRATIFY ITEM SCORES BY AGE RANGE ---------------------------
Preschool_25_School <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-T-Scores-per-case.csv')
  )))
) %>% arrange(IDNumber)

source(here('CODE/ITEM-VECTORS/Preschool-25-School-item-vectors.R'))

map_df(scale_order,
       ~
         Preschool_25_School %>%
         select(eval(as.name(
           str_c(.x, '_items_Preschool_25_School')
         ))) %>%
         assign(str_c(.x, '_item_scores_25'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Preschool_25_School %>%
    filter(age_range == "2 to 4 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Preschool_25_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_24'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Preschool_25_School %>%
    filter(age_range == "5 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Preschool_25_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_5'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

# COMPUTE SCALE ALPHAS FOR EACH AGE RANGE ---------------------------------
alpha_25 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_25')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_25 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_25'), ., envir = .GlobalEnv)
)
alpha_24 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_24')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_24 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_24'), ., envir = .GlobalEnv)
)
alpha_5 <- map_df(scale_order, ~
                    alpha(
                      cor(
                        eval(as.name(str_c(.x, '_item_scores_5')))
                      )
                    )[["total"]] %>%
                    mutate(scale = .x) %>% 
                    select(scale, raw_alpha) %>% 
                    rename(alpha_5 = raw_alpha) %>% 
                    assign(str_c(.x, '_alpha_5'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_25, alpha_24, alpha_5)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

# GENERATE OUTPUT WITH CV COLUMNS BASED ON TOTAL SAMPLE ALPHAS ------------
Preschool_25_School_T_sd_25 <-
  Preschool_25_School %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_25 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_25)

output_School <- alpha %>% left_join(
  Preschool_25_School_T_sd_25, 
  by = 'scale'
) %>% 
  mutate(SEM_25 = sd_25*(sqrt(1-alpha_25)),
         CV_90 = 1.6449*SEM_25,
         CV_95 = 1.96*SEM_25) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "School Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_25) 

rm(list = setdiff(ls(), ls(pattern = "output")))

#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Home,
                    output_School),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t502-Preschool-25-alpha-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


###### TRT SECTION ----------------------------------------------------
#### PRESCHOOL HOME DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
Preschool_25_Home_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Preschool_25_Home_time2 <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-time2-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-time2-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Preschool_25_Home_TRT <- Preschool_25_Home_Stand %>% 
  inner_join(
    Preschool_25_Home_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Preschool_25_Home_TRT %>% 
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
Preschool_25_Home_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Preschool 25 Home', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Preschool_25_Home_t1_T_desc <-
  Preschool_25_Home_TRT %>% 
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
Preschool_25_Home_t2_T_desc <-
  Preschool_25_Home_TRT %>% 
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
Preschool_25_Home_TRT_T_desc <- Preschool_25_Home_t1_T_desc %>% 
  full_join(
    Preschool_25_Home_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Preschool_25_Home_TRT_T <- Preschool_25_Home_TRT_cor_table %>%
  full_join(
    Preschool_25_Home_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p, -SEM) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), 'Preschool_25_Home_TRT_T'))




#### PRESCHOOL SCHOOL DATA ---------------------------------------------
# READ & PARSE DATA -------------------------------------------------------
# read stand
Preschool_25_School_Stand <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:clin_dx, contains('_NT'))

# read time 2
Preschool_25_School_time2 <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-time2-T-Scores-per-case.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-time2-T-Scores-per-case.csv')
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber, contains('_NT'))

# Create a new df of only cases that have t1 and t2 data, with only demos and T
# scores. Create T score diff variable to allow filtering of outliers.
Preschool_25_School_TRT <- Preschool_25_School_Stand %>% 
  inner_join(
    Preschool_25_School_time2,
    by = 'IDNumber',
    suffix = c('_t1', '_t2')
  ) %>% 
  mutate(TOT_NT_dif = abs(TOT_NT_t1 - TOT_NT_t2)) %>% 
  select(TOT_NT_dif, everything()) %>%  
  arrange(TOT_NT_dif) %>% 
  filter(TOT_NT_dif < 10)

# This selects only the T-score columns that will be correlated
cor_cols <- Preschool_25_School_TRT %>% 
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
Preschool_25_School_TRT_cor_table <- corr.test(cor_cols)[['ci']] %>% 
  rownames_to_column(var = 'pair') %>% 
  filter(pair %in% cor_row) %>% 
  mutate(scale1 = str_sub(pair, 1, 3),
         scale = str_replace(scale1, '_', '')
  ) %>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(form = case_when(
    scale == 'SOC' ~ 'Preschool 25 School', 
    T ~ NA_character_)
  ) %>% 
  select(form, scale, r, p)

# use psych::describe() to get a table of t1 means, sds
Preschool_25_School_t1_T_desc <-
  Preschool_25_School_TRT %>% 
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
Preschool_25_School_t2_T_desc <-
  Preschool_25_School_TRT %>% 
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
Preschool_25_School_TRT_T_desc <- Preschool_25_School_t1_T_desc %>% 
  full_join(
    Preschool_25_School_t2_T_desc,
    by = 'scale'
  )  %>%
  mutate(ES = abs((mean_t1 - mean_t2) / ((sd_t1 + sd_t2 / 2))))

# join corr and desc tables in final output format, calculate corrected-r
# (corrected for SD of stand sample), CV_90, CV_95
Preschool_25_School_TRT_T <- Preschool_25_School_TRT_cor_table %>%
  full_join(
    Preschool_25_School_TRT_T_desc,
    by = 'scale'
  ) %>%
  mutate(correct_r = (r * (10 / sd_t1)) / sqrt(1 + r^2 * ((10^2 / sd_t1^2) - 1)),
         SEM = sd_t1*(sqrt(1-correct_r)),
         CV_90 = 1.6449*SEM,
         CV_95 = 1.96*SEM
  ) %>% 
  select(form, scale, n, correct_r, everything(), -r, -p, -SEM) %>% 
  mutate_if(is.numeric, ~ round(., 2))

rm(list = setdiff(ls(), c('Preschool_25_Home_TRT_T', 'Preschool_25_School_TRT_T')))



#### WRITE MANUAL TABLE OUTPUT -----------------------------------------------
write_csv(bind_rows(Preschool_25_Home_TRT_T,
                    Preschool_25_School_TRT_T),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t502-Preschool-25-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

