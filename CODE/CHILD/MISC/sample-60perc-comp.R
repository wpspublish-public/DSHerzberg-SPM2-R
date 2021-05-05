suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(ggrepel) # ggplot2 EXTENSIONS

# read in final normative sample for child-home form.
Child_512_Home_desamp <- read_csv(
  here(
    "INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp.csv"
    ))

# use dplyr::sample_frac() to get a 60% sample. By grouping on all demo vars, we
# roughly preserve demographic proportions in smaller sample.
set.seed(1234)
sample_60perc <- Child_512_Home_desamp %>% 
  group_by(Age, Gender, ParentHighestEducation, Ethnicity, Region) %>%
  sample_frac(0.6)

# quick check on counts/distribution for each demo variables, across full sample
# and 60% sample.
table(Child_512_Home_desamp$Age)
table(sample_60perc$Age)

table(Child_512_Home_desamp$Gender)
table(sample_60perc$Gender)

table(Child_512_Home_desamp$ParentHighestEducation)
table(sample_60perc$ParentHighestEducation)

table(Child_512_Home_desamp$Ethnicity)
table(sample_60perc$Ethnicity)

table(Child_512_Home_desamp$Region)
table(sample_60perc$Region)

# write .csv of 60% sample for use in other procedures
write_csv(sample_60perc, here(
  "INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp-60perc.csv"
))

