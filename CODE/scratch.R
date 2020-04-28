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
                         )
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



