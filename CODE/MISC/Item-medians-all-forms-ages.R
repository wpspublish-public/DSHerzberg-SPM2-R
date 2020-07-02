###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

### READ STAND T-SCORES PER CASE FOR ALL FORMS, AGES ------------------------------

# HOME
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Home-Stand.R"))

# SCHOOL
source(here("CODE/READ-T-SCORES-PER-CASE/read-Preschool-25-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Child-512-School-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-School-Stand.R"))

# SELF
source(here("CODE/READ-T-SCORES-PER-CASE/read-Teen-1221-Self-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Self-Stand.R"))

# OTHER
source(here("CODE/READ-T-SCORES-PER-CASE/read-Adult-Other-Stand.R"))

rm(list = ls(pattern = "items"))

### BUILD DATA FRAME WITH FORM, ITEM, MEDIAN COLS ------------------------------

data <- c(ls(pattern = "Stand"))

forms <- str_replace(str_sub(data, 1, -7), "_", "-")

item_medians_stand <- list(
  mget(data),
  forms
) %>% 
  pmap_df(
    ~ ..1 %>%
      select(contains("q0")) %>% 
      pivot_longer(everything(), names_to = "item") %>% 
      group_by(item) %>% 
      summarize(median = round(median(value))) %>% 
      mutate(form = case_when(
        rownames(.) == "1" ~ ..2,
        T ~ NA_character_
      )) %>%
      relocate(form, .before = item)
  )

###### WRITE OUTPUT -----------------------------------------------------------

write_csv(
  item_medians_stand,
  here(
    "OUTPUT-FILES/ITEM-MEDIANS/SPM2-item-medians-stand.csv"
  ),
  na = ""
)
