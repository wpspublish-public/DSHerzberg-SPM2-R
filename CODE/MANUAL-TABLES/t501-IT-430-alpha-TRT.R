suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

# 49 DATA -----------------------------------------------------------------

IT_49_Home <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case-2020-04-21.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case-2020-04-21.csv')
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

rm(list = item_vectors)

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

IT_49_Home_T_sd_49 <-
  IT_49_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_49 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_49)

output_49 <- alpha %>% left_join(
  IT_49_Home_T_sd_49, 
  by = 'scale'
) %>% 
  mutate(SEM_49 = sd_49*(sqrt(1-alpha_49)),
         CV_90 = 1.6449*SEM_49,
         CV_95 = 1.96*SEM_49) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Infant Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_49) 

rm(list=setdiff(ls(), c("output_49")))

# 1030 DATA -----------------------------------------------------------------

IT_1030_Home <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-T-Scores-per-case-2020-04-27.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-T-Scores-per-case-2020-04-27.csv')
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

rm(list = item_vectors)

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
  mutate(form = case_when(
    scale =="SOC" ~ "Toddler Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_1030) 

rm(list = setdiff(ls(), ls(pattern = "output")))

# CAREGIVER DATA -----------------------------------------------------------------

IT_Caregiver <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-T-Scores-per-case-2020-04-21.csv')
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

rm(list = item_vectors)

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
  mutate(form = case_when(
    scale =="SOC" ~ "Caregiver Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_430) 

rm(list = setdiff(ls(), ls(pattern = "output")))

# WRITE MANUAL TABLE OUTPUT -----------------------------------------------

forms <- c('49', '1030', 'Caregiver')

map(forms, ~
      write_csv(eval(as.name(str_c('output_', .x))), here(
        paste0(
          'OUTPUT-FILES/MANUAL-TABLES/t501-IT-',
          .x,
          '-alpha-TRT-',
          format(Sys.Date(), "%Y-%m-%d"),
          '.csv'
        )
      ),
      na = ''))


