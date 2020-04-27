suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

IT_49_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/IT/ALLDATA-DESAMP-NORMS-INPUT/IT-49-Home-allData-desamp.csv")
  ))) 

source(here('CODE/ITEM-VECTORS/IT-49-Home-item-vectors.R'))

TOT_item_scores <- IT_49_Home %>% 
  select(TOT_items_IT_49_Home)

TOT_alpha <- alpha(cor(TOT_item_scores))[["total"]] %>% 
  select(raw_alpha)
