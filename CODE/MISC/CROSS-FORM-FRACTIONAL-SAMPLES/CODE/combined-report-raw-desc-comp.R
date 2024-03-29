# COMBINED REPORT

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(writexl))

output_file_path_raw_desc <- "CODE/MISC/CROSS-FORM-FRACTIONAL-SAMPLES/OUTPUT-FILES/RAW-DESC-COMP/"

file_names <- c(
  "IT-49-Home",
  "IT-1030-Home",
  "Preschool-25-Home",
  "Preschool-25-School",
  "Child-512-Home",
  "Child-512-School",
  "Teen-1221-Home",
  "Teen-1221-School",
  "Teen-1221-Self",
  "Adult-Self",
  "Adult-Other"
)

raw_desc_ESonly_by_form <-
  map(file_names,
      ~
        suppressMessages(read_csv(here(
          str_c(
            output_file_path_raw_desc,
            .x,
            "-allData-desamp-raw-desc-full-60perc-comp.csv"
          )
        ))) %>%
        select(scale, ES)) %>%
  set_names(file_names) %>%
  reduce(left_join,
         by = "scale") %>% 
  rowwise() %>% 
  mutate(mean_ES = round(mean(c_across(ES.x:ES)),3)) %>%
  select(scale, mean_ES)

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

write_xlsx(
  raw_desc_reports_by_form_list,
  here(str_c(
    output_file_path_raw_desc, "all-forms-raw-desc-full-60perc-comp.xlsx"
  ))
)
