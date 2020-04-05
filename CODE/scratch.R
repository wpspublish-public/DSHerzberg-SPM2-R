suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# demo code showing that dplyr::arrange() can match to a pre-set sort order,
# even when the data doesn't contain all of the values in the sort-order vecs.

# var_order <- c("age_range", "gender", "educ", "ethnic", "region", "data")
# 
# cat_order <- c(NA, "typ", "clin",
#                NA, "2-4", "5-7", "8-10", "11-13", "14-16",
#                NA, "male", "female",
#                NA, "hispanic", "asian", "black", "white", "multi", "other")
# 
# df <- tibble(var = c(rep("data", 2), rep("age_range", 4), rep("gender", 2), rep("ethnic", 4)),
#              cat = c("clin", "typ", "11-13", "5-7", "8-10", "2-4", "female", "male", "white", "black", "hispanic", "asian"),
#              n = c(30, 70, 12, 18, 47, 23, 52, 48, 56, 20, 23, 1))
# 
# df_sort <- df %>% arrange(match(var, var_order), match(cat, cat_order))

             
test <- temp %>% 
  select(
    -(q0014:q0119)
  ) %>% 
  filter(Ethnicity == "NativeHawPacIsl" | Ethnicity == "AmericanIndAlaskanNat")

temp <- read_csv(
  here('INPUT-FILES/PRESCHOOL/SM-QUAL-COMBO-NORMS-INPUT/Preschool-25-Home-combo-norms-input.csv')
)

