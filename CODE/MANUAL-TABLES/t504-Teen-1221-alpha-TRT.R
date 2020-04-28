suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

# HOME DATA -----------------------------------------------------------------

Teen_1221_Home <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-Home-T-Scores-per-case-2020-04-16.csv')
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

# SCHOOL DATA -----------------------------------------------------------------

Teen_1221_School <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-School-T-Scores-per-case-2020-04-16.csv')
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

# SELF DATA -----------------------------------------------------------------

Teen_1221_Self <- 
  suppressMessages(as_tibble(read_csv(
    here('OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-Self-T-Scores-per-case-2020-04-16.csv')
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

# WRITE MANUAL TABLE OUTPUT -----------------------------------------------

write_csv(bind_rows(output_Home,
                    output_School,
                    output_Self),
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t504-Teen-alpha-TRT-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


