suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

# SELF DATA -----------------------------------------------------------------

Adult_Self <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-T-Scores-per-case-2020-04-15.csv')
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

# OTHER DATA -----------------------------------------------------------------

Adult_Other <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-T-Scores-per-case-2020-04-15.csv')
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

# WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Self,
                    output_Other),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t505-Adult-alpha-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


