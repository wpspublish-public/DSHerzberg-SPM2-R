# SPM-2: derived normalized T-scores for each case in standardization sample;
# create raw-to-T lookup tables.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS
library(bestNormalize) # NORMALIZATION METHODS

# SCALE VECTORS WITH ITEM NAMES -------------------------------------------

All_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023", "q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039", 
                              "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050", 
                              "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065", 
                              "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079", 
                              "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092", 
                              "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108", "q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

TOT_items_Child_512_Home <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039", 
                              "q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050", 
                              "q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065", 
                              "q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079", 
                              "q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092", 
                              "q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

SOC_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

SOC_rev_items_Child_512_Home <- c("q0014", "q0015", "q0016", "q0017", "q0018", "q0019", "q0020", "q0021", "q0022", "q0023")

VIS_items_Child_512_Home <- c("q0027", "q0028", "q0029", "q0030", "q0031", "q0035", "q0036", "q0037", "q0038", "q0039")

HEA_items_Child_512_Home <- c("q0040", "q0041", "q0042", "q0043", "q0044", "q0045", "q0046", "q0047", "q0048", "q0050")

TOU_items_Child_512_Home <- c("q0053", "q0055", "q0057", "q0058", "q0059", "q0060", "q0061", "q0063", "q0064", "q0065")

TS_items_Child_512_Home <- c("q0068", "q0070", "q0071", "q0072", "q0073", "q0075", "q0076", "q0077", "q0078", "q0079")

BOD_items_Child_512_Home <- c("q0081", "q0082", "q0083", "q0084", "q0086", "q0088", "q0089", "q0090", "q0091", "q0092")

BAL_items_Child_512_Home <- c("q0095", "q0096", "q0097", "q0098", "q0102", "q0103", "q0105", "q0106", "q0107", "q0108")

PLA_items_Child_512_Home <- c("q0111", "q0112", "q0113", "q0114", "q0115", "q0116", "q0117", "q0120", "q0121", "q0122")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")


# READ DATA, RECODE ITEMS, CALC RAW SCORES --------------------------------

Child_512_Home_items <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/CHILD/SPM-2 SM Qual COMBO Child ages 512 Home Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    # AgeGroup,
    Gender,
    ParentHighestEducation,
    Ethnicity,
    Region,
    All_items_Child_512_Home
  ) %>%
  # filter out 4 yo
  # filter(Age >= 5) %>% 
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Child_512_Home,
    ~ case_when(
      .x == "Never" ~ 1,
      .x == "Occasionally" ~ 2,
      .x == "Frequently" ~ 3,
      .x == "Always" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # recode reverse-scored items
  mutate_at(
    SOC_rev_items_Child_512_Home,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Child_512_Home,
            ~ as.integer(.x)) %>% 
  # Add age_range var.
  mutate(age_range = case_when(
    Age <= 8 ~ "5 to 8 years",
    TRUE ~ "9 to 12 years")
  ) %>% 
  # select(-AgeGroup) %>% 
# Compute raw scores. Note use of `rowSums(.[TOT_items_Child_512_Home])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Child_512_Home]),
    SOC_raw = rowSums(.[SOC_items_Child_512_Home]),
    VIS_raw = rowSums(.[VIS_items_Child_512_Home]),
    HEA_raw = rowSums(.[HEA_items_Child_512_Home]),
    TOU_raw = rowSums(.[TOU_items_Child_512_Home]),
    TS_raw = rowSums(.[TS_items_Child_512_Home]),
    BOD_raw = rowSums(.[BOD_items_Child_512_Home]),
    BAL_raw = rowSums(.[BAL_items_Child_512_Home]),
    PLA_raw = rowSums(.[PLA_items_Child_512_Home])
  ) %>% 
  # Create data var to differentiate Survey monkey from Qualtrics case
  mutate(data = case_when(
    IDNumber >= 700000 ~ 'Qual',
    TRUE ~ 'SM'
  )) %>% 
  select(IDNumber, data, everything()) %>% 
# Exclude outliers on TOT_raw (also exlude by data source to equalize samples
  # from diiferent data sources)
  filter(TOT_raw < 200) #%>% 
  # filter(!(TOT_raw >= 138 & data == 'Qual')) %>%
  # write_csv(here('INPUT-FILES/CHILD/SM-QUAL-COMBO-NORMS-INPUT/Child-512-Home-combo-norms-input.csv'),
            # na = "")

# clean up environment
rm(list = ls(pattern='.*items_Child_512_Home'))

Child_512_Home <- Child_512_Home_items %>% 
  select(
    -(q0014:q0122)
  )   

# EXAMINE DATA TO MAKE AGESTRAT DECISIONS ---------------------------------

# ### THE NEXT SECTION OF CODE FACILITATES EXAMINATION OF TOT_raw to make
# decisions about whether norms need to be stratified by age. Once the decision
# about age-stratification has been made and implemented, the 'examination' code
# can be commented off.


# Create frequency tables for TOT_raw by age_range
# Child_512_Home_TOT_freq_AgeGroup <- Child_512_Home %>% group_by(age_range) %>% count(TOT_raw) %>% 
#   mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))


# Compute descriptive statistics, effect sizes for TOT_raw by age_range
# Child_512_Home_TOT_desc_AgeGroup <-
#   Child_512_Home %>% group_by(age_range) %>% arrange(age_range) %>% summarise(n = n(),
#                                                                          median = round(median(TOT_raw), 2),
#                                                                          mean = round(mean(TOT_raw), 2),
#                                                                          sd = round(sd(TOT_raw), 2)) %>%
#   mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:2))
# 
# AgeGroup <- Child_512_Home_TOT_desc_AgeGroup %>% pull(age_range)

# Plot TOT_raw means, SDs by age_range
# mean_plot <- ggplot(data = Child_512_Home_TOT_desc_AgeGroup, aes(group, mean)) +
#   geom_point(
#     col = "blue",
#     fill = "blue",
#     alpha = .5,
#     size = 3,
#     shape = 23
#   ) +
#   geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
#   scale_x_continuous(breaks = seq(1, 2, 1), labels = AgeGroup) +
#   scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
#   labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "TOT") +
#   geom_errorbar(
#     aes(ymin = mean - sd, ymax = mean + sd),
#     col = "red",
#     size = 0.2,
#     width = 0.2
#   ) 
# print(mean_plot)

# Check for duplicate IDnumber.

# Child_512_Home_dup <- Child_512_Home %>% count(IDNumber) %>% filter(n > 1)
# write_csv(Child_512_Home_dup, here("OUTPUT-FILES/CHILD/Child_512_Home_dup.csv"))


# DETERMINE BEST NORMALIZATION MODEL --------------------------------------

# (NOTE: THIS SECTION SHOULD BE TOGGLED OFF AFTER SELECTION OF NORMALIZATION
# MODEL)

# # create a bestNormalize object to lock down the normalizing function that will be used on repeated runs of the norms.
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
                  value = arcsinh_x(.y), 
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
    .x < 25 ~ 25,
    .x > 75 ~ 75,
    TRUE ~ .x
  )
) %>%
  select(
    paste0(.y, '_NT')
  )
)

# Bind the normalized T-score columns to the table containing raw scores for
# each case.
Child_512_Home <- Child_512_Home %>% bind_cols(NT_cols)

# write T-scores per case table to .csv
write_csv(Child_512_Home, here(
  paste0(
    'OUTPUT-FILES/CHILD/T-SCORES-PER-CASE/Child-512-Home-T-Scores-per-case-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))

# clean up environment
rm(list = ls(pattern='.*_nz'))

# histogram to check normality
# MASS::truehist(Child_512_Home$TOT_NT, h = 1)
# hist_plot <- ggplot(data = Child_512_Home, aes(TOT_NT)) +
#   geom_histogram(
#     binwidth = .2,
#     col = "red"
#   ) +
#   scale_y_continuous(breaks = seq(0, 250, 25)) +
#   labs(title = "TOT_NT")
# print(hist_plot)

# GENERATE RAW-TO-T LOOKUP TABLES -----------------------------------------

# Generate raw-to-T lookup columns. Handle TOT and subscale scores separately,
# because each type has different raw score range. Start wtih TOT. Input is
# stand sample with raw scores and normalized T scores for each case. Group
# cases by raw score, relationship between raw and T is many-to-one.
TOT_lookup <- Child_512_Home %>% group_by(
  TOT_raw
) %>% 
  # Because raw-to-T is many to one, all values of T are identical for each raw,
  # and summarizing by the min value of T per raw yields the ONLY value of T per
  # raw. But we need the raw column to contain all possible values of raw, and
  # not all possible values of raw are represented in the stand sample. Thus
  # current data object jumps possible raw values (e.g, raw = 62 and raw = 65
  # might be adjacent rows in this table)
  summarise(
    TOT_NT = min(TOT_NT)
  ) %>% 
  # complete expands the table vertically, filling in missing values of raw
  # within the range given. This leaves NA cells for T for those rows that
  # didn't have raw values in the input object.
  complete(
    TOT_raw = 10:240
  ) %>% 
  # fill replaces NA in T going down the table, with values from the last
  # preceding (lagging) cell that was not NA.
  fill(
    TOT_NT
  ) %>% 
  # A second call of fill is needed to handle inputs where the first cell(s) of
  # T are NA. 2nd fill call is uses direction up to fill those first NA cells
  # with the value from the first subsequent (leading) cell that is not NA.
  fill(
    TOT_NT,
    .direction = "up"
  ) %>% 
  rename(
    raw = TOT_raw
  ) %>% 
  mutate_at(
    vars(TOT_NT), ~ case_when(
      raw < 60 ~ NA_real_,
      TRUE ~ .x
    )
  )

# Repeat above for subscale raw-to-T columns.
subscale_names <- score_names[2:9]

subscale_lookup <- map(
  subscale_names, 
  ~ Child_512_Home %>% group_by(
    !!as.name(paste0(.x, '_raw'))
  ) %>% 
    summarise(
      !!as.name(paste0(.x, '_NT')) := min(!!as.name(paste0(.x, '_NT')))
    ) %>% 
    complete(
      !!as.name(paste0(.x, '_raw')) := 10:240
    ) %>% 
    fill(
      paste0(.x, '_NT')
    ) %>% 
    fill(
      paste0(.x, '_NT'),
      .direction = "up"
    ) %>% 
    rename(
      raw = !!as.name(paste0(.x, '_raw'))
    ) %>% 
    mutate_at(
      vars(!!as.name(paste0(.x, '_NT'))), ~ case_when(
        raw > 40 ~ NA_real_,
        TRUE ~ .x
      )
    )
) %>% 
  reduce(
    left_join, 
    by = 'raw'
  )

# join TOT and subscale columns
all_lookup <- full_join(TOT_lookup, subscale_lookup, by = 'raw')

all_lookup_col_names <- c(paste0(score_names, '_raw'))

# write final raw-to-T lookup table to .csv
write_csv(all_lookup, here(
  paste0(
    'OUTPUT-FILES/CHILD/RAW-T-LOOKUP-TABLES/Child-512-Home-raw-T-lookup-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))


# generate print pub format raw-to-T table
all_lookup_pub <- all_lookup %>% 
  # gather collapses wide table into three-column tall table with key-value
  # pairs: rawscore, scale(key var, many rows for each scale), T(value
  # var, one row for each value of T within each scale)
  gather(scale, T,-raw) %>% 
  group_by(scale) %>%
  # expand the table vertically, adding new rows, so there's a row for every possible T value
  complete(T = 25:75) %>% 
  ungroup() %>%
  # regroup table by two levels
  group_by(scale, T) %>%
  # filter step retains all 1-row groups, and the first and last rows of any
  # multi-row groups. n() == 1 returns 1-row groups; n() > 1 & row_number()
  # %in% c(1, n()) returns rows of multi-row groups with the row number of
  # either 1 (first row), or n() which is the number of rows and also the
  # number of the last row. The first and last rows hold the min and max
  # values of raw for that value of T (the grouping variable)
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  # Summarise creates a table with one row per group (one row per
  # possible value of T). For the 1-row groups, str_c simply passes the
  # value of raw as a string; for the multi-row groups, str_c joins the min
  # and max values of raw with the '--' separator.
  summarise(raw = str_c(raw, collapse = '--')) %>%
  # recode missing values of raw to '-'
  mutate_at(vars(raw), ~ case_when(is.na(.x) ~ '-', TRUE ~ .x)) %>%
  # sort on two levels
  arrange(scale, desc(T)) %>% 
  # spread table back to wide, all values of T (one row for each), scale
  # columns filled with values of rawscore
  spread(scale, raw) %>%
  # sort descending on T
  arrange(desc(T)) %>% 
  # rename with desired final column names
  rename_at(vars(ends_with('_NT')), ~ gsub("_NT", "_raw", .)) %>% 
  # order columns left-to-right
  select(T, all_lookup_col_names)

# write final print format raw-to-T lookup table to .csv
write_csv(all_lookup_pub, here(
  paste0(
    'OUTPUT-FILES/CHILD/PRINT-FORMAT-NORMS-TABLES/Child-512-Home-print-raw-T-lookup-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))


