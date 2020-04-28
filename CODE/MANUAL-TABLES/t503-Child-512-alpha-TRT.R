suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

# HOME DATA -----------------------------------------------------------------

Child_512_Home <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case-2020-04-23.csv')
  )))

source(here('CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R'))

map_df(scale_order,
       ~
         Child_512_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_512'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Child_512_Home %>%
    filter(age_range == "5 to 8 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Child_512_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_58'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Child_512_Home %>%
    filter(age_range == "9 to 12 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Child_512_Home')
    ))) %>%
    assign(str_c(.x, '_item_scores_912'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

alpha_512 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_512')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_512 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_512'), ., envir = .GlobalEnv)
)
alpha_58 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_58')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_58 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_58'), ., envir = .GlobalEnv)
)
alpha_912 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_912')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_912 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_912'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_512, alpha_58, alpha_912)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

Child_512_Home_T_sd_512 <-
  Child_512_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_512 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_512)

output_Home <- alpha %>% left_join(
  Child_512_Home_T_sd_512, 
  by = 'scale'
) %>% 
  mutate(SEM_512 = sd_512*(sqrt(1-alpha_512)),
         CV_90 = 1.6449*SEM_512,
         CV_95 = 1.96*SEM_512) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "Home Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_512) 

rm(list=setdiff(ls(), c("output_Home")))

# SCHOOL DATA -----------------------------------------------------------------

Child_512_School <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-T-Scores-per-case-2020-04-18.csv')
  )))

source(here('CODE/ITEM-VECTORS/Child-512-School-item-vectors.R'))

map_df(scale_order,
       ~
         Child_512_School %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_School')
         ))) %>%
         assign(str_c(.x, '_item_scores_512'), ., envir = .GlobalEnv))
map_df(
  scale_order,
  ~
    Child_512_School %>%
    filter(age_range == "5 to 8 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Child_512_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_58'), ., envir = .GlobalEnv)
)
map_df(
  scale_order,
  ~
    Child_512_School %>%
    filter(age_range == "9 to 12 years") %>%
    select(eval(as.name(
      str_c(.x, '_items_Child_512_School')
    ))) %>%
    assign(str_c(.x, '_item_scores_912'), ., envir = .GlobalEnv)
)

rm(list = item_vectors)

alpha_512 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_512')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_512 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_512'), ., envir = .GlobalEnv)
)
alpha_58 <- map_df(scale_order, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores_58')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x) %>% 
                     select(scale, raw_alpha) %>% 
                     rename(alpha_58 = raw_alpha) %>% 
                     assign(str_c(.x, '_alpha_58'), ., envir = .GlobalEnv)
)
alpha_912 <- map_df(scale_order, ~
                    alpha(
                      cor(
                        eval(as.name(str_c(.x, '_item_scores_912')))
                      )
                    )[["total"]] %>%
                    mutate(scale = .x) %>% 
                    select(scale, raw_alpha) %>% 
                    rename(alpha_912 = raw_alpha) %>% 
                    assign(str_c(.x, '_alpha_912'), ., envir = .GlobalEnv)
)

rm(list = ls(pattern = 'item_scores'))
rm(list = ls(pattern = '_alpha'))

list <- list(alpha_512, alpha_58, alpha_912)

alpha <- list %>% reduce(left_join, by = "scale")

rm(list)
rm(list = ls(pattern = 'alpha_'))

Child_512_School_T_sd_512 <-
  Child_512_School %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_512 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_512)

output_School <- alpha %>% left_join(
  Child_512_School_T_sd_512, 
  by = 'scale'
) %>% 
  mutate(SEM_512 = sd_512*(sqrt(1-alpha_512)),
         CV_90 = 1.6449*SEM_512,
         CV_95 = 1.96*SEM_512) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  mutate(form = case_when(
    scale =="SOC" ~ "School Form",
    T ~ NA_character_
  )) %>% 
  select(form, everything(), -sd_512) 

rm(list = setdiff(ls(), ls(pattern = "output")))

# WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Home,
                    output_School),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t503-Child-alpha-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

