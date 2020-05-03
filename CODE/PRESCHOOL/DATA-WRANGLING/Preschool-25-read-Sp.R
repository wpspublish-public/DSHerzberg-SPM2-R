suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# ######### PRESCHOOL 25 HOME DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

Preschool_25_Home_Sp <- bind_rows(suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/PRESCHOOL/SP-NORMS-INPUT/Preschool-25-Home-Sp-norms-input.csv"
  )
))),
suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/PRESCHOOL/INHOUSE-NORMS-INPUT/Preschool-25-Home-inHouse-norms-input.csv"
  )
)))) %>%
  filter(!(data %in% c("In-house-Eng", "In-house-Alt")))

Preschool_24_Home_Sp <- Preschool_25_Home_Sp %>% filter(age_range == "2 to 4 years")
Preschool_5_Home_Sp <- Preschool_25_Home_Sp %>% filter(age_range == "5 years")

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# 46 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_24_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-24-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Preschool_24_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_46_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_46 <- map_dfc(score_names,
                     ~
                       Preschool_24_Home_Sp %>% left_join(eval(as.name(
                         str_c(.x, '_46_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_46,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

# 79 DATA

# read raw-to-t lookup tables, create lookup cols by scale
Preschool_5_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/Preschool-5-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Preschool_5_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_79_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_79 <- map_dfc(score_names,
                     ~
                       Preschool_5_Home_Sp %>% left_join(eval(as.name(
                         str_c(.x, '_79_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_79,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

# ######### 1030 HOME DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

IT_1030_Home_Sp <- bind_rows(suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/PRESCHOOL/SP-NORMS-INPUT/IT-1030-Home-Sp-norms-input.csv"
  )
))),
suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/PRESCHOOL/INHOUSE-NORMS-INPUT/IT-1030-Home-inHouse-norms-input.csv"
  )
)))) %>%
  filter(data != "In-house-Eng")

IT_1020_Home_Sp <- IT_1030_Home_Sp %>% filter(age_range == "09.5 to 20 mo")
IT_2130_Home_Sp <- IT_1030_Home_Sp %>% filter(age_range == "21 to 31.5 mo")

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# 1020 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_1020_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/IT-1020-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_1020_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1020_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_1020 <- map_dfc(score_names,
                       ~
                         IT_1020_Home_Sp %>% left_join(eval(as.name(
                           str_c(.x, '_1020_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_1020_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_1020,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/IT-1020-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

# 2130 DATA

# read raw-to-t lookup tables, create lookup cols by scale
IT_2130_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/IT-2130-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_2130_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_2130_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_2130 <- map_dfc(score_names,
                       ~
                         IT_2130_Home_Sp %>% left_join(eval(as.name(
                           str_c(.x, '_2130_lookup_col')
                         )),
                         by = str_c(.x, '_raw')) %>%
                         select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_2130_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_2130,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/IT-2130-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))



# ######### CAREGIVER DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

IT_Caregiver_Sp <-
  suppressMessages(as_tibble(read_csv(
    here(
      "INPUT-FILES/PRESCHOOL/SP-NORMS-INPUT/IT-Caregiver-Sp-norms-input.csv"
    )
  )))

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# read raw-to-t lookup tables, create lookup cols by scale
IT_Caregiver_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/PRESCHOOL/RAW-T-LOOKUP-TABLES/IT-Caregiver-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    IT_Caregiver_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_Caregiver_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_Caregiver <- map_dfc(score_names,
                            ~
                              IT_Caregiver_Sp %>% left_join(eval(as.name(
                                str_c(.x, '_Caregiver_lookup_col')
                              )),
                              by = str_c(.x, '_raw')) %>%
                              select(!!str_c(.x, '_NT'))) %>%
  bind_cols(IT_Caregiver_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, AgeInMonths, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_Caregiver,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/IT-Caregiver-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))




