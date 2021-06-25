# SPM-2: derived normalized T-scores for each case in standardization sample;
# create raw-to-T lookup tables.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
suppressMessages(suppressWarnings(library(tidyverse)))
library(bestNormalize) # NORMALIZATION METHODS
suppressMessages(library(psych)) # DESCRIPTIVE TABLES
suppressMessages(library(data.table))

# READ FINALIZED STAND SAMPLE ---------------------------------------------

Child_512_Home <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/ALLDATA-DESAMP-NORMS-INPUT/CHILD-512-Home-allData-desamp-60perc.csv")
  )))  %>% 
  relocate(c(VIS_raw, HEA_raw, TOU_raw, TS_raw, BOD_raw, BAL_raw, 
             TOT_raw, PLA_raw, SOC_raw), .after = "age_range")

# score_names_old <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")
score_names <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "TOT", "PLA", "SOC")
subscale_names <- c("VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA", "SOC")

# Check for any dupIDs (anyDuplicated() returns row number of FIRST dup ID encountered)
anyDuplicated(Child_512_Home$IDNumber)

# Check for any NAs on IDNumber, returns TRUE if NA exist
any(is.na(Child_512_Home$IDNumber))

# extract cases with Dup ID numbers or NA on IDNumber, write out for investigation
Child_512_Home_dupMissIDs <- Child_512_Home %>% 
  mutate(dup = duplicated(IDNumber)) %>% 
  filter(dup == TRUE | is.na(IDNumber)) %>% 
  select(-dup) %>% 
  write_csv(
    here(
      paste0(
        'OUTPUT-FILES/CHILD/DUP-IDS/Child-512-Home-dupIDs-missingIDs-',
        format(Sys.Date(), "%Y-%m-%d"),
        '.csv'
      )
    ), 
    na = 'missing'
  )

# DETERMINE BEST NORMALIZATION MODEL --------------------------------------

# (NOTE: THIS SECTION SHOULD BE TOGGLED OFF AFTER SELECTION OF NORMALIZATION
# MODEL)

# # # create a bestNormalize object to lock down the normalizing function that will be used on repeated runs of the norms.
# TOT_nz_obj <- bestNormalize(Child_512_Home$TOT_raw)
# 
# # print transformation
# TOT_nz_obj$chosen_transform
# 
# # Extract transformation type
# chosen_transform <- class(TOT_nz_obj$chosen_transform)[1]
# 
# # apply the chosen method to create normalized z-scores for each case.
# TOT_nz_transform <- eval(as.name(chosen_transform))(Child_512_Home$TOT_raw)


# APPLY SELECTED NORMALIZATION MODEL TO CREATE NORMALIZED Z-SCORES --------

# Apply a static, repeatable transformation to create normalized z-scores for
# each case.

# create char vec with names for the nine score transformations
nz_transform_names <- c(paste0(score_names, '_nz_transform'))

# pull nine raw score columns into a list
raw_score_cols_list <- map(score_names, ~ Child_512_Home %>% 
                             pull(
                               !!as.name(paste0(.x, '_raw'))
                             )
)

# create the nine named objects that contain the normalization for each score
# distribution. In this call of `purrr::walk2()`, the .f calls assign(), because
# the central purpose of this code is to use assign to create a series of named
# objects in the global environment. The `walk` functions are used when the
# output of interest is a side effect. The names for these objects are contained
# in the .x argument (a char vec). The data to be normalized is in the list of
# nine raw score columns `raw_score_cols_list`, which as assigned to the .y
# argument of walk2(), using the dot . shorthand. Within assign(), the value
# argument allows the selected normalization transformation to be applied to the
# .y data. 

# NOTE: MUST SUBSITUTE NAMED TRANSFORMATION FROM PREVIOUS STEP IN THIS LINE:
# value = orderNorm(.y), e.g., value = [SELECTED TRANSFORMATION](.y), 

raw_score_cols_list %>%
  walk2(
    .x = c(nz_transform_names),         # names to assign
    .y = .,                # object to be assigned
    .f = ~ assign(x = .x, 
                  value = orderNorm(.y), 
                  envir = .GlobalEnv)
  )

# Each of the named objects (normalization for each score) created in the
# previous smippet is a list. Use base::mget to put these named objects into a
# 'list of lists'. Here, mget takes a single argument, a char vec holding the
# names of the lists that are to be put into the new list `nz_transform_list`

nz_transform_list <- mget(nz_transform_names)

# Create a char vec containing the names of the output objects for the next
# step. These output objects are single-column named dfs containing the
# normalized z scores corresponding to the raw score for each case. The objects and the single
# columns within them have the same names
nz_names <- c(paste0(score_names, '_nz'))

# Create nine single-column named dataframes, each containing the normalized z
# scores for each case. The input is the list `nz_transform_list` containing the
# nine normalization objects (each itself a list). That input is assigned to the
# .y argument of `walk2()`, while the names of the output objects `nz_names` are
# assigned to the .x argument. Within assign(), the value argument has as its
# innermost function `purrr::pluck()`, which extracts an element of a list in
# the .y input. In this case, what's being extracted is the `x.t`, the vector of
# normalized z scores. That vector is wrapped in `data.frame`, to coerce it into
# a data frame, which is then wrapped in `setNames`, which names the column of
# the resulting data frame using the variable names contained in the .x
# argument.
nz_transform_list %>%
  walk2(
    .x = c(nz_names),
    .y = .,
    .f = ~ assign(x = .x, 
                  value = setNames(data.frame(pluck(.y, 'x.t')), c(.x)),
                  envir = .GlobalEnv)
  )

# remove the normalization objects, which are no longer needed
rm(list = ls(nz_transform_list))


# DERIVE NORMALIZED T-SCORES FOR EACH CASE --------------------------------

# put the nine single column normalized z-score data frames into a list
nz_col_list <- mget(nz_names)

# Next snippet replaces the normalized z-score with a normalized T-score (and
# truncates the T-score distribution). 

# map2_dfc takes a list of data frames as input, and outputs a single data
# frame, binding the transformed output columns together. In map2_dfc, the input
# list is assigned to the .x argument, and the vector of score_names is assigned
# to the .y argument. Note the use of unquoting `!!`, `as.name`, and the
# specialized equals sign `:=` for NSE (non-standard evavluation)
NT_cols <- map2_dfc(nz_col_list, score_names, ~
                      .x %>% mutate(
                        !!as.name(paste0(.y, '_NT')) := round((!!as.name(paste0(.y, '_nz'))*10)+50)
                      ) %>% mutate_at(
                        vars(paste0(.y, '_NT')), ~ case_when(
                          .x < 40 ~ 40,
                          .x > 80 ~ 80,
                          TRUE ~ .x
                        )
                      ) %>%
                      select(
                        paste0(.y, '_NT')
                      )
) %>% 
  mutate_if(is.numeric, as.integer)

# Bind the normalized T-score columns to the table containing raw scores for
# each case.
Child_512_Home <- Child_512_Home %>% bind_cols(NT_cols) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())

# write T-scores per case table to .csv
write_csv(Child_512_Home, here(
  "OUTPUT-FILES/NORMS-OUTPUT-4080T/Child-512-Home-T-Scores-per-case-4080T-60perc.csv"
  # paste0(
  #   'OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case-',
  #   format(Sys.Date(), "%Y-%m-%d"),
  #   '.csv'
  # )
), 
na = ''
)

