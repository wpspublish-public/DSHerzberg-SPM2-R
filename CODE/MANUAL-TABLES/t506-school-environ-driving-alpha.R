###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
# READ DATA ---------------------------------------------------------------------

file_names <- c("ART", "BUS", "CAF", "MUS", "PHY", "REC", 
                "teen-driving-home", "teen-driving-self")

file_names %>%
  map(~
        suppressMessages(as_tibble(read_csv(here(
          str_c(
            "INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/",
            .x,
            "-data.csv"
          )
        )))) %>% 
        select(contains('q0')) %>% 
        assign(str_c(.x, '_item_scores'), ., envir = .GlobalEnv)
      )

# COMPUTE ALPHAS -------------------------------------------------------------
alpha <- map_df(file_names, ~
                     alpha(
                       cor(
                         eval(as.name(str_c(.x, '_item_scores')))
                       )
                     )[["total"]] %>%
                     mutate(scale = .x,
                            n = nrow(eval(as.name(str_c(.x, '_item_scores')))),
                            n_items = ncol(eval(as.name(str_c(.x, '_item_scores'))))) %>% 
                     select(scale, n_items, n, raw_alpha) %>% 
                     rename(alpha = raw_alpha) %>% 
                  mutate_at(vars(alpha), ~ round(., 3))
)


# WRITE MANUAL TABLE -----------------------------------------------------

write_csv(alpha,
          here(
            paste0(
              'OUTPUT-FILES/MANUAL-TABLES/t506-school-environ-driving-alpha-',
              format(Sys.Date(), "%Y-%m-%d"),
              '.csv'
            )
          ),
          na = '')


