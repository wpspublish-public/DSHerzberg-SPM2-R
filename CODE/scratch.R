suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

dcp_oldnum1 <- c("q0010", "q0011", "q0012", "q0013", "q0014", "q0015", "q0016", "q0017")
dcp_oldnum2 <- c("q0026", "q0027", "q0028", "q0029", "q0030", "q0031", "q0032", "q0035")
# tibble::lst creates a named list, using the names of the input elements to name the list elements
dcp_oldlist <- lst(dcp_oldnum1, dcp_oldnum2)

dcp_newlist <- map(
  dcp_oldlist,
  ~
    str_sub(.x, 3, 5) %>%
    as.integer() %>%
    `+`(1) %>%
    str_pad(4, pad = '0') %>%
    str_c('q', .)
)
list2env(dcp_newlist, envir = .GlobalEnv)

# now go backwards and rename existing items to original numbers.

test1 <- enframe(dcp_oldnum1)
test2 <- enframe(dcp_oldnum2)
test12 <- c(dcp_oldnum1, dcp_oldnum2)
test3 <- bind_rows(test1, test2) %>% select(value, name) %>% 
  data.table::transpose() %>% 
  data.table::setnames(test12) %>% 
  filter(q0010 == '1') %>% 
  mutate(id = '25')

# names(test3) %<>% 
#   str_sub(3, 5) %>%
#   as.integer() %>%
#   `-`(1) %>%
#   str_pad(4, pad = '0') %>%
#   str_c('q', .)

rename_if(test3, str_detect(names(test3), "q"), ~
              str_sub(., 3, 5) %>%
              as.integer() %>%
              `-`(1) %>%
              str_pad(4, pad = '0') %>%
              str_c('q', .)
)





