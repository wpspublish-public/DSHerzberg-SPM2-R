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

# 24 DATA

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
    assign(str_c(.x, '_24_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_24 <- map_dfc(score_names,
                     ~
                       Preschool_24_Home_Sp %>% left_join(eval(as.name(
                         str_c(.x, '_24_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_24_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_24,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-24-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

# 5 DATA

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
    assign(str_c(.x, '_5_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_5 <- map_dfc(score_names,
                     ~
                       Preschool_5_Home_Sp %>% left_join(eval(as.name(
                         str_c(.x, '_5_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Preschool_5_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_5,
  here(
    'OUTPUT-FILES/PRESCHOOL/T-SCORES-PER-CASE/Preschool-5-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

