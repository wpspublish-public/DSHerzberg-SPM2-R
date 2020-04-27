suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# SCHOOL ENVIRON SCORE

scales <- c('ART', 'MUS', 'PHY', 'REC', 'CAF', 'BUS')

map_dfr(scales, ~
  suppressMessages(read_csv(
  here(
    str_c('OUTPUT-FILES/CHILD/SCHOOL-ENVIRON/', .x, '-cutoff-summary.csv')
  ))) %>% 
  filter(!is.na(cutoff) | !is.na(total_n)) %>% 
  mutate(cutoff = lead(TOT_raw),
         pct_above = round(100 - lead(cum_per), 2),
         form = .x ) %>% 
  filter(!is.na(cutoff)) %>% 
  select(
    form,
    total_n,
    mean_TOT_raw,
    sd_TOT_raw,
    cutoff,
    pct_above
  ) %>% 
  rename(
    n = total_n,
    mean = mean_TOT_raw,
    sd = sd_TOT_raw
  ) %>% 
  assign(str_c(.x, '_row'), ., envir = .GlobalEnv)
)

school_environ_table <- 
  bind_rows(
    ART_row,
    MUS_row,
    PHY_row,
    REC_row,
    CAF_row,
    BUS_row
  )

# TEEN DRIVING

form_teen <- c('self', 'home')

map_dfr(form_teen, ~
          suppressMessages(read_csv(
            here(
              str_c('OUTPUT-FILES/TEEN/MISC/teen-driving-', .x, '-cutoff-summary.csv')
            ))) %>% 
          filter(!is.na(cutoff) | !is.na(total_n)) %>% 
          mutate(cutoff = lead(TOT_raw),
                 pct_above = round(100 - lead(cum_per), 2),
                 form = str_c('teen_driving_', .x) ) %>% 
          filter(!is.na(cutoff)) %>% 
          select(
            form,
            total_n,
            mean_TOT_raw,
            sd_TOT_raw,
            cutoff,
            pct_above
          ) %>% 
          rename(
            n = total_n,
            mean = mean_TOT_raw,
            sd = sd_TOT_raw
          ) %>% 
          assign(str_c(.x, '_row'), ., envir = .GlobalEnv)
)

teen_driving_table <- 
  bind_rows(
    self_row,
    home_row
  )

# ADULT DRIVING

form_adult <- c('self', 'other')

map_dfr(form_adult, ~
          suppressMessages(read_csv(
            here(
              str_c('OUTPUT-FILES/ADULT/MISC/adult-driving-', .x, '-cutoff-summary.csv')
            ))) %>% 
          filter(!is.na(cutoff) | !is.na(total_n)) %>% 
          mutate(cutoff = lead(TOT_raw),
                 pct_above = round(100 - lead(cum_per), 2),
                 form = str_c('adult_driving_', .x) ) %>% 
          filter(!is.na(cutoff)) %>% 
          select(
            form,
            total_n,
            mean_TOT_raw,
            sd_TOT_raw,
            cutoff,
            pct_above
          ) %>% 
          rename(
            n = total_n,
            mean = mean_TOT_raw,
            sd = sd_TOT_raw
          ) %>% 
          assign(str_c(.x, '_row'), ., envir = .GlobalEnv)
)

adult_driving_table <- 
  bind_rows(
    self_row,
    other_row
  )

# CONSOLIDATED TABLE

t411 <- bind_rows(
  school_environ_table,
  teen_driving_table,
  adult_driving_table
)

write_csv(t411, here(
  paste0(
    'OUTPUT-FILES/MANUAL-TABLES/t411-school-environ-driving-cutoffs-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
), 
na = ''
)


