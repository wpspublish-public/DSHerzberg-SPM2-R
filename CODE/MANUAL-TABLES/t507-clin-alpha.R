###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
#### IT 49 HOME CLIN--------------------------------------------------------------

# IT49 has only 11 clinical cases, this code section reads IT 49 data and
# selects subset of items that are equivalent to IT 1030 items. Code the reads
# IT 1030 data, selects subset of equivalent items, and renames those items to
# the IT 49 names, so the two data sets can be combined. Alpha is then
# calculated on this shared set of items over the two data sets, providing a
# large enough sample that we can get an alpha on data that includes IT 49 data.

source(here("CODE/MISC/IT-430-names-shared-item-cols.R"))

IT_49_Home_Clin_shared_items <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IT_49_names_item_cols_shared_with_IT_1030)

IT_1030_Home_Clin_shared_items <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IT_1030_names_item_cols_shared_with_IT_49)

rename_IT_1030_col_in <- IT_1030_Home_Clin_shared_items

source(here("CODE/MISC/rename-IT-1030-shared-item-cols-to-IT-49-names.R"))

IT_430_Home_Clin_shared_items <- bind_rows(
  IT_49_Home_Clin_shared_items,
  rename_IT_1030_col_out
)

alpha_IT_430_Home_shared_items <-
  alpha(cor(IT_430_Home_Clin_shared_items))[["total"]] %>%
  select(raw_alpha) %>%
  rename(alpha_IT_430_Home_shared_items = raw_alpha)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))


#### IT 1030 HOME CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/IT-1030-Home-item-vectors.R"))

IT_1030_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-1020-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-2130-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

map_df(scale_order,
       ~
         IT_1030_Home_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_IT_1030_Home')
         ))) %>%
         assign(str_c(.x, '_item_cols_IT_1030_Home'), ., envir = .GlobalEnv))

alpha_IT_1030_Home <- map_df(scale_order, ~
                               alpha(
                                 cor(
                                   eval(as.name(str_c(.x, '_item_cols_IT_1030_Home')))
                                 ),
                                 check.keys = TRUE
                               )[["total"]] %>%
                               mutate(scale = .x) %>% 
                               select(scale, raw_alpha) %>% 
                               rename(alpha_IT_1030_Home = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### IT CAREGIVER CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/IT-Caregiver-item-vectors.R"))

IT_Caregiver_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-Caregiver-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         IT_Caregiver_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_IT_Caregiver')
         ))) %>%
         assign(str_c(.x, '_item_cols_IT_Caregiver'), ., envir = .GlobalEnv))

alpha_IT_Caregiver <- map_df(scale_order, ~
                               alpha(
                                 cor(
                                   eval(as.name(str_c(.x, '_item_cols_IT_Caregiver')))
                                 ),
                                 check.keys = TRUE
                               )[["total"]] %>%
                               mutate(scale = .x) %>% 
                               select(scale, raw_alpha) %>% 
                               rename(alpha_IT_Caregiver = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### PRESCHOOL 25 HOME CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Preschool-25-Home-item-vectors.R"))

Preschool_25_Home_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

map_df(scale_order,
       ~
         Preschool_25_Home_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Preschool_25_Home')
         ))) %>%
         assign(str_c(.x, '_item_cols_Preschool_25_Home'), ., envir = .GlobalEnv))

alpha_Preschool_25_Home <- map_df(scale_order, ~
                                    alpha(
                                      cor(
                                        eval(as.name(str_c(.x, '_item_cols_Preschool_25_Home')))
                                      )
                                    )[["total"]] %>%
                                    mutate(scale = .x) %>% 
                                    select(scale, raw_alpha) %>% 
                                    rename(alpha_Preschool_25_Home = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### PRESCHOOL 25 SCHOOL CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Preschool-25-School-item-vectors.R"))

Preschool_25_School_Clin <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-School-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-School-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber)

map_df(scale_order,
       ~
         Preschool_25_School_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Preschool_25_School')
         ))) %>%
         assign(str_c(.x, '_item_cols_Preschool_25_School'), ., envir = .GlobalEnv))

alpha_Preschool_25_School <- map_df(scale_order, ~
                                      alpha(
                                        cor(
                                          eval(as.name(str_c(.x, '_item_cols_Preschool_25_School')))
                                        )
                                      )[["total"]] %>%
                                      mutate(scale = .x) %>% 
                                      select(scale, raw_alpha) %>% 
                                      rename(alpha_Preschool_25_School = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### CHILD 512 HOME CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Child-512-Home-item-vectors.R"))

Child_512_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Child_512_Home_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_Home')
         ))) %>%
         assign(str_c(.x, '_item_cols_Child_512_Home'), ., envir = .GlobalEnv))

alpha_Child_512_Home <- map_df(scale_order, ~
                                 alpha(
                                   cor(
                                     eval(as.name(str_c(.x, '_item_cols_Child_512_Home')))
                                   )
                                 )[["total"]] %>%
                                 mutate(scale = .x) %>% 
                                 select(scale, raw_alpha) %>% 
                                 rename(alpha_Child_512_Home = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### CHILD 512 SCHOOL CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Child-512-School-item-vectors.R"))

Child_512_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-School-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Child_512_School_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_School')
         ))) %>%
         assign(str_c(.x, '_item_cols_Child_512_School'), ., envir = .GlobalEnv))

alpha_Child_512_School <- map_df(scale_order, ~
                                   alpha(
                                     cor(
                                       eval(as.name(str_c(.x, '_item_cols_Child_512_School')))
                                     )
                                   )[["total"]] %>%
                                   mutate(scale = .x) %>% 
                                   select(scale, raw_alpha) %>% 
                                   rename(alpha_Child_512_School = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### TEEN 1221 HOME CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-Home-item-vectors.R"))

Teen_1221_Home_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Teen_1221_Home_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Teen_1221_Home')
         ))) %>%
         assign(str_c(.x, '_item_cols_Teen_1221_Home'), ., envir = .GlobalEnv))

alpha_Teen_1221_Home <- map_df(scale_order, ~
                                 alpha(
                                   cor(
                                     eval(as.name(str_c(.x, '_item_cols_Teen_1221_Home')))
                                   )
                                 )[["total"]] %>%
                                 mutate(scale = .x) %>% 
                                 select(scale, raw_alpha) %>% 
                                 rename(alpha_Teen_1221_Home = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))
#### TEEN 1221 SCHOOL CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-School-item-vectors.R"))

Teen_1221_School_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-School-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Teen_1221_School_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Teen_1221_School')
         ))) %>%
         assign(str_c(.x, '_item_cols_Teen_1221_School'), ., envir = .GlobalEnv))

alpha_Teen_1221_School <- map_df(scale_order, ~
                                   alpha(
                                     cor(
                                       eval(as.name(str_c(.x, '_item_cols_Teen_1221_School')))
                                     )
                                   )[["total"]] %>%
                                   mutate(scale = .x) %>% 
                                   select(scale, raw_alpha) %>% 
                                   rename(alpha_Teen_1221_School = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### TEEN 1221 SELF CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Teen-1221-Self-item-vectors.R"))

Teen_1221_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Teen_1221_Self_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Teen_1221_Self')
         ))) %>%
         assign(str_c(.x, '_item_cols_Teen_1221_Self'), ., envir = .GlobalEnv))

alpha_Teen_1221_Self <- map_df(scale_order, ~
                                 alpha(
                                   cor(
                                     eval(as.name(str_c(.x, '_item_cols_Teen_1221_Self')))
                                   )
                                 )[["total"]] %>%
                                 mutate(scale = .x) %>% 
                                 select(scale, raw_alpha) %>% 
                                 rename(alpha_Teen_1221_Self = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### ADULT SELF CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Adult-Self-item-vectors.R"))

Adult_Self_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Adult_Self_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Adult_Self')
         ))) %>%
         assign(str_c(.x, '_item_cols_Adult_Self'), ., envir = .GlobalEnv))

alpha_Adult_Self <- map_df(scale_order, ~
                             alpha(
                               cor(
                                 eval(as.name(str_c(.x, '_item_cols_Adult_Self')))
                               )
                             )[["total"]] %>%
                             mutate(scale = .x) %>% 
                             select(scale, raw_alpha) %>% 
                             rename(alpha_Adult_Self = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

#### ADULT OTHER CLIN----------------------------------------------------------
source(here("CODE/ITEM-VECTORS/Adult-Other-item-vectors.R"))

Adult_Other_Clin <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Other-clin-T-Scores-per-case.csv")
  )))

map_df(scale_order,
       ~
         Adult_Other_Clin %>%
         select(eval(as.name(
           str_c(.x, '_items_Adult_Other')
         ))) %>%
         assign(str_c(.x, '_item_cols_Adult_Other'), ., envir = .GlobalEnv))

alpha_Adult_Other <- map_df(scale_order, ~
                              alpha(
                                cor(
                                  eval(as.name(str_c(.x, '_item_cols_Adult_Other')))
                                )
                              )[["total"]] %>%
                              mutate(scale = .x) %>% 
                              select(scale, raw_alpha) %>% 
                              rename(alpha_Adult_Other = raw_alpha)
)

rm(list = setdiff(ls(), ls(pattern = 'alpha')))

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------

list <- mget(ls(pattern = "alpha"))

alpha_clin <- list %>% reduce(left_join, by = "scale") %>% 
  select(scale, alpha_IT_1030_Home, alpha_IT_Caregiver, alpha_Preschool_25_Home,
         alpha_Preschool_25_School, alpha_Child_512_Home, alpha_Child_512_School,
         alpha_Teen_1221_Home, alpha_Teen_1221_School, alpha_Teen_1221_Self,
         alpha_Adult_Self, alpha_Adult_Other) %>% 
  mutate_if(is.numeric, ~round(., 3))
  
write_csv(alpha_clin,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t507-clin-alpha-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')

