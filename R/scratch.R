# Examine SPM-2 data to determine need for age-stratified norms.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS
library(bestNormalize) # NORMALIZATION METHODS

# Scale vectors with item names

All_items_Adult_Other <- c("q0014", "q0015", "q0016", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027", "q0028",
                          "q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042", 
                           "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057", 
                           "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072", 
                           "q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087", 
                           "q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", 
                           "q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114",
                          "q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125", "q0129", "q0131")

TOT_items_Adult_Other <- c("q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042", 
                           "q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057", 
                           "q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072", 
                           "q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087", 
                           "q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100", 
                           "q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114")

SOC_items_Adult_Other <- c("q0014", "q0015", "q0016", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027", "q0028")

SOC_rev_items_Adult_Other <- c("q0014", "q0015", "q0022", "q0023", "q0024", "q0025", "q0026", "q0027")

VIS_items_Adult_Other <- c("q0029", "q0031", "q0033", "q0034", "q0036", "q0037", "q0039", "q0040", "q0041", "q0042")

HEA_items_Adult_Other <- c("q0043", "q0045", "q0046", "q0047", "q0048", "q0050", "q0053", "q0055", "q0056", "q0057")

TOU_items_Adult_Other <- c("q0058", "q0060", "q0061", "q0063", "q0064", "q0065", "q0066", "q0069", "q0071", "q0072")

TS_items_Adult_Other <- c("q0073", "q0074", "q0075", "q0076", "q0079", "q0080", "q0081", "q0084", "q0086", "q0087")

BOD_items_Adult_Other <- c("q0088", "q0090", "q0091", "q0092", "q0094", "q0095", "q0096", "q0097", "q0099", "q0100")

BAL_items_Adult_Other <- c("q0103", "q0105", "q0107", "q0108", "q0109", "q0110", "q0111", "q0112", "q0113", "q0114")

PLA_items_Adult_Other <- c("q0118", "q0119", "q0120", "q0121", "q0122", "q0123", "q0124", "q0125", "q0129", "q0131")

score_names <- c("TOT", "SOC", "VIS", "HEA", "TOU", "TS", "BOD", "BAL", "PLA")

# Read data, recode item vars, calculate TOT.
Adult_Other <-
  suppressMessages(as_tibble(read_csv(
    here("DATA/SPM-2 Adult ages 1690 Other Report Questionnaire.csv")
  ))) %>% select(
    IDNumber,
    Age,
    AgeGroup,
    Gender,
    HighestEducation,
    Ethnicity,
    Region,
    All_items_Adult_Other
  ) %>%
  # recode items from char to num (mutate_at applies funs to specific columns)
  mutate_at(
    All_items_Adult_Other,
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
    SOC_rev_items_Adult_Other,
    ~ case_when(.x == 4 ~ 1,
                .x == 3 ~ 2,
                .x == 2 ~ 3,
                .x == 1 ~ 4,
                TRUE ~ NA_real_)
  ) %>%
  # Convert scored item vars to integers
  mutate_at(All_items_Adult_Other,
            ~ as.integer(.x)) %>% 
  # Compute TOT raw score. Note use of `rowSums(.[TOT_items_Adult_Other])`: when used 
  # within a pipe, you can pass a vector of column names to `base::rowSums`, but you
  # must use wrap the column vector in a column-subsetting expression: `.[]`, where the
  # dot is a token for the data in the pipe.
  mutate(
    TOT_raw = rowSums(.[TOT_items_Adult_Other]),
    SOC_raw = rowSums(.[SOC_items_Adult_Other]),
    VIS_raw = rowSums(.[VIS_items_Adult_Other]),
    HEA_raw = rowSums(.[HEA_items_Adult_Other]),
    TOU_raw = rowSums(.[TOU_items_Adult_Other]),
    TS_raw = rowSums(.[TS_items_Adult_Other]),
    BOD_raw = rowSums(.[BOD_items_Adult_Other]),
    BAL_raw = rowSums(.[BAL_items_Adult_Other]),
    PLA_raw = rowSums(.[PLA_items_Adult_Other])
  ) %>% 
  select(
    -(q0014:q0131)
  ) %>% 
  #print()
  # Exclude outliers on TOT_raw
  filter(TOT_raw <200) %>% print()

# Create frequency tables for TOT_raw by AgeGroup
Adult_Other_TOT_freq_AgeGroup <- Adult_Other %>% group_by(AgeGroup) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeGroup
Adult_Other_TOT_desc_AgeGroup <-
  Adult_Other %>% group_by(AgeGroup) %>% arrange(AgeGroup) %>% summarise(n = n(),
                                                                         median = round(median(TOT_raw), 2),
                                                                         mean = round(mean(TOT_raw), 2),
                                                                         sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:6))

AgeGroup <- Adult_Other_TOT_desc_AgeGroup %>% pull(AgeGroup)

# Plot TOT_raw means, SDs by AgeGroup
mean_plot <- ggplot(data = Adult_Other_TOT_desc_AgeGroup, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, 6, 1), labels = AgeGroup) +
  scale_y_continuous(breaks = seq(0, 250, 25), limits = c(0, 250)) +
  labs(title = "Raw Score Means (with SDs)", x = "AgeGroup", y = "TOT") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(mean_plot)

# # generate histograms by agestrat
# Adult_Other_by_AgeGroup <- Adult_Other %>% group_by(AgeGroup)
# 
# hist_plot <- ggplot(data = Adult_Other_by_AgeGroup, aes(TOT_raw)) +
#   geom_histogram(
#     binwidth = .2,
#     col = "red"
#   ) +
#   scale_y_continuous(breaks = seq(0, 250, 25)) +
#   labs(title = "Frequency Distribution") +
#   # stat_function(
#   #   fun = function(x, mean, sd, n){
#   #     n * dnorm(x = x, mean = mean, sd = sd)
#   #   },
#   #   args = with(ANTraw_by_agestrat, c(mean = mean(ANT_total), sd = sd(ANT_total), n
#   #                     = length(ANT_total)))
#   # ) +
#   theme(panel.grid.minor=element_blank()) +
#   facet_wrap(~AgeGroup)
# print(hist_plot)



# Check for duplicate IDnumber.

Adult_Other_dup <- Adult_Other %>% count(IDNumber) %>% filter(n > 1)
write_csv(Adult_Other_dup, here("DATA/Adult_Other_dup.csv"))


# generate normalized t-scores for each case

# create a bestNormalize object to lock down the normalizing function that will be used on repeated runs of the norms.
# TOT_nz_obj <- bestNormalize(Adult_Other$TOT_raw)

# print transformation
# TOT_nz_obj$chosen_transform

# Extract transformation type
# chosen_transform <- class(TOT_nz_obj$chosen_transform)[1]

# apply the chosen method to create normalized z-scores for each case.
# TOT_nz_transform <- eval(as.name(chosen_transform))(Adult_Other$TOT_raw)

# apply a static, repeatable transformation to create normalized z-scores for each case.

# create char vec with names for the nine score transformations
nz_transform_names <- c(paste0(score_names, '_nz_transform'))

# pull nine raw score columns into a list
raw_score_cols_list <- map(score_names, ~ Adult_Other %>% 
              pull(
                !!as.name(paste0(.x, '_raw'))
              )
)

# create the nine named objects that contain the normalization for each score
# distribution. In this call of `purrr::map2()`, the .f calls assign(), because
# the central purpose of this code is to use assign to create a series of named
# objects in the global environment. The names for these objects are contained
# in the .x argument (a char vec). The data to be normalized is in the list of
# nine raw score columns `raw_score_cols_list`, which as assigned to the .y
# argument of walk2(), using the dot . shorthand. Within assign(), the value
# argument allows the boxcox transformation to be applied to the .y data
raw_score_cols_list %>%
  walk2(
    .x = c(nz_transform_names),         # names to assign
    .y = .,                # object to be assigned
    .f = ~ assign(x = .x, 
                  value = boxcox(.y), 
                  envir = .GlobalEnv)
  )

########## START HERE

score_col_names <- c(paste0(score_names, '_nz'))

pluck(TOT_nz_transform, 'x.t')


# extract and append normalized transformed z-scores to input table.
Adult_Other$TOT_nz <- TOT_nz_transform$x.t
Adult_Other$SOC_nz <- SOC_nz_transform$x.t
Adult_Other$VIS_nz <- VIS_nz_transform$x.t
Adult_Other$HEA_nz <- HEA_nz_transform$x.t
Adult_Other$TOU_nz <- TOU_nz_transform$x.t
Adult_Other$TS_nz <- TS_nz_transform$x.t
Adult_Other$BOD_nz <- BOD_nz_transform$x.t
Adult_Other$BAL_nz <- BAL_nz_transform$x.t
Adult_Other$PLA_nz <- PLA_nz_transform$x.t

# caLculate normalized t-scores per case, and truncate the t-score distribution at 25 and 75.
Adult_Other %>% mutate(
  TOT_NT = round((TOT_nz*10)+50)
) %>% mutate_at(
  vars(TOT_NT), ~ case_when(
    .x < 25 ~ 25,
    .x > 75 ~ 75,
    TRUE ~ .x
  )
) %>% 
  assign('test', ., envir = .GlobalEnv)

# histogram
MASS::truehist(Adult_Other$TOT_NT, h = 1)
