# COMBINED REPORT

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

df <- tibble(x = 1:2, y = 3:4, z = 5:6) %>% 
  rowwise() %>% 
  mutate(mean_es = mean(c_across(x:z)))

temp1 <- splice(raw_desc_ESonly_by_form, raw_desc_reports_by_form_list)

raw_desc_reports_by_form_list<- 
  map(
    file_names,
    ~
      suppressMessages(read_csv(here(str_c(
        output_file_path_raw_desc, .x, "-allData-desamp-raw-desc-full-60perc-comp.csv"
      ))))
  ) %>% 
  splice(raw_desc_ESonly_by_form, .) %>% 
  set_names(c("mean_ES", file_names))


