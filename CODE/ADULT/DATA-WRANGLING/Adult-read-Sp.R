suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# ######### ADULT SELF DATA --------------------------------------------------

# READ DATA ---------------------------------------------------------------

Adult_Self_Sp <- bind_rows(suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/ADULT/SP-NORMS-INPUT/Adult-Self-Sp-norms-input.csv"
  )
))),
suppressMessages(as_tibble(read_csv(
  here(
    "INPUT-FILES/ADULT/INHOUSE-NORMS-INPUT/Adult-Self-inHouse-norms-input.csv"
  )
)))) %>%
  filter(!(data %in% c("In-house-Eng", "In-house-Alt")))

# LOOK UP T-SCORES, WRITE OUTPUT --------------------------------------------------------

# read raw-to-t lookup tables, create lookup cols by scale
Adult_Self_rawToT <-
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/ADULT/RAW-T-LOOKUP-TABLES/Adult-Self-raw-T-lookup.csv")
  )))

map_df(
  score_names,
  ~
    Adult_Self_rawToT %>%
    select(raw,!!str_c(.x, '_NT')) %>%
    rename(!!str_c(.x, "_raw") := raw) %>%
    assign(str_c(.x, '_1221_Self_lookup_col'), ., envir = .GlobalEnv)
)

# look up T scores with left_join, bind T score columns to main data set
output_1221_Self <- map_dfc(score_names,
                            ~
                              Adult_Self_Sp %>% left_join(eval(as.name(
                                str_c(.x, '_1221_Self_lookup_col')
                              )),
                              by = str_c(.x, '_raw')) %>%
                              select(!!str_c(.x, '_NT'))) %>%
  bind_cols(Adult_Self_Sp, .) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write outuput for analysis

write_csv(
  output_1221_Self,
  here(
    'OUTPUT-FILES/ADULT/T-SCORES-PER-CASE/Adult-Self-Sp-T-Scores-per-case.csv'
  ),
  na = ''
)

rm(list = ls(pattern = 'col'))

