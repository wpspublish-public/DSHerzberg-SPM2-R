suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

# HOME DATA -----------------------------------------------------------------

Preschool_25_Home <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-T-Scores-per-case-2020-04-18.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-T-Scores-per-case-2020-04-18.csv')
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

# SCHOOL DATA -----------------------------------------------------------------

Preschool_25_School <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-T-Scores-per-case-2020-04-19.csv')
  ))),
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-T-Scores-per-case-2020-04-19.csv')
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

# WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Home,
                    output_School),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t502-Preschool-alpha-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

