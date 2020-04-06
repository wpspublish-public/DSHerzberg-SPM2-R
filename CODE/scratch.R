suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

test <-
  Preschool_25_Home %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
                                                                                        median = round(median(TOT_raw), 2),
                                                                                        mean = round(mean(TOT_raw), 2),
                                                                                        sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2)#,
         # group = c(1:2)
  )
