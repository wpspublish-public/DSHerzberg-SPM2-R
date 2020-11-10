###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

### READ STAND ITEM RESPONSES PER CASE FOR ALL FORMS, AGES ------------------------------

# HOME
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-49-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-1030-Home-Stand.R"))
source(here("CODE/READ-T-SCORES-PER-CASE/read-IT-Caregiver-Stand.R"))
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

# SCHOOL ENVIRONMENTS
ART_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/ART-data.csv'
  )
))

BUS_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/BUS-data.csv'
  )
))

CAF_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/CAF-data.csv'
  )
))

MUS_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/MUS-data.csv'
  )
))

PHY_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/PHY-data.csv'
  )
))

REC_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/REC-data.csv'
  )
))

# TEEN DRIVING
teen_home_driving_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/teen-driving-home-data.csv'
  )
))

teen_self_driving_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/teen-driving-self-data.csv'
  )
))

rm(list = ls(pattern = "items"))

# ADULT DRIVING
adult_other_driving_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/adult-driving-other-data.csv'
  )
))

adult_self_driving_Stand <- suppressMessages(read_csv(
  here(
    'INPUT-FILES/CHILD/SCHOOL-ENVIRON-DRIVING-DATA/adult-driving-self-data.csv'
  )
))

rm(list = ls(pattern = "items"))

### BUILD DATA FRAME WITH FORM, ITEM, MEDIAN COLS ------------------------------

data <- c(ls(pattern = "Stand"))

forms <- str_replace_all(str_sub(data, 1, -7), "_", "-")

form_order <- c("IT-49-Home", "IT-1030-Home", "IT-Caregiver", "Preschool-25-Home", "Preschool-25-School", 
                "Child-512-Home", "Child-512-School", "Teen-1221-Home", "Teen-1221-School", 
                "Teen-1221-Self", "Adult-Other", "Adult-Self", "ART", "BUS", "CAF", "MUS", "PHY", "REC", 
                "teen-home-driving", "teen-self-driving", "adult-other-driving", "adult-self-driving") 

item_medians_stand <- list(mget(data),
                           forms) %>%
  pmap_df(
    ~ ..1 %>%
      select(contains(c("q0", "QT"))) %>%
      pivot_longer(everything(), names_to = "item") %>%
      group_by(item) %>%
      summarize(median = round(median(value))) %>%
      mutate(form = ..2) %>%
      relocate(form, .before = item) 
  ) %>%
  arrange(match(form, form_order)) %>%
  mutate(across(
    c(form),
    ~ case_when((lag(form) != form | is.na(lag(form))) ~ form,
                T ~ NA_character_)
  ))

###### WRITE OUTPUT -----------------------------------------------------------

write_csv(
  item_medians_stand,
  here(
    "OUTPUT-FILES/ITEM-MEDIANS/SPM2-item-medians-stand.csv"
  ),
  na = ""
)

