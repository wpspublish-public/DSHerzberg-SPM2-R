suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# ######### CHILD 512 HOME DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

Child_512_Home_Sp <- bind_rows(suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/CHILD/SP-NORMS-INPUT/Child-512-Home-Sp-norms-input.csv"
  )
))),
suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/CHILD/INHOUSE-NORMS-INPUT/Child-512-Home-inHouse-norms-input.csv"
  )
)))) %>%
  filter(!(data %in% c("In-house-Eng", "In-house-Alt")))

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# read raw-to-t lookup tables, create lookup cols by scale
Child_512_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-Home-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Child_512_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_512_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_512 <- map_dfc(score_names,
                     ~
                       Child_512_Home_Sp %>% left_join(eval(as.name(
                         str_c(.x, '_512_lookup_col')
                       )),
                       by = str_c(.x, '_raw')) %>%
                       select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Child_512_Home_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_512,
  here(
    'OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

