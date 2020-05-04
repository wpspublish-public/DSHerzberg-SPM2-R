suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# ######### TEEN 1221 HOME DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

Teen_1221_Home_Sp <- bind_rows(suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/TEEN/SP-NORMS-INPUT/Teen-1221-Home-Sp-norms-input.csv"
  )
))),
suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/TEEN/INHOUSE-NORMS-INPUT/Teen-1221-Home-inHouse-norms-input.csv"
  )
)))) %>%
  filter(!(data %in% c("In-house-Eng", "In-house-Alt")))

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# read raw-to-t lookup tables, create lookup cols by scale
Teen_1221_Home_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/RAW-T-LOOKUP-TABLES/Teen-1221-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Teen_1221_Home_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1221_Home_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_1221_Home <- map_dfc(score_names,
                     ~
                       Teen_1221_Home_Sp %>% left_join(eval(as.name(
                         str_c(.x, '_1221_Home_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Teen_1221_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_1221_Home,
  here(
    'OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))


# ######### TEEN 1221 SELF DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

Teen_1221_Self_Sp <- bind_rows(suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/TEEN/SP-NORMS-INPUT/Teen-1221-Self-Sp-norms-input.csv"
  )
))),
suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/TEEN/INHOUSE-NORMS-INPUT/Teen-1221-Self-inHouse-norms-input.csv"
  )
)))) %>%
  filter(!(data %in% c("In-house-Eng", "In-house-Alt")))

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# read raw-to-t lookup tables, create lookup cols by scale
Teen_1221_Self_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/TEEN/RAW-T-LOOKUP-TABLES/Teen-1221-Self-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Teen_1221_Self_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1221_Self_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_1221_Self <- map_dfc(score_names,
                            ~
                              Teen_1221_Self_Sp %>% left_join(eval(as.name(
                                str_c(.x, '_1221_Self_lookup_col')
                              )),
                              by = str_c(.x, '_raw')) %>%
                              select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Teen_1221_Self_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_1221_Self,
  here(
    'OUTPUT-FILES/TEEN/T-SCORES-PER-CASE/Teen-1221-Self-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

