suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

IT_49_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-49-Home-allData-desamp.csv")
  ))) 

source(here('CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R'))

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

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

list <- list(alpha_49, alpha_46, alpha_79)

alpha <- list %>% reduce(left_join, by = "scale")
