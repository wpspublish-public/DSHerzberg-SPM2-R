suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# 49 DATA ---------------------------------------------

########## STAND

# READ FINALIZED SAMPLES

IT_49_Home_Stand_match <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_status)

IT_49_Home_Clin_match <- bind_rows(
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-46-Home-clin-T-Scores-per-case.csv")
  ))),
  suppressMessages(as_tibble(read_csv(
    here("OUTPUT-FILES/IT/T-SCORES-PER-CASE/IT-79-Home-clin-T-Scores-per-case.csv")
  )))
) %>% 
  arrange(IDNumber) %>% 
  select(IDNumber:Region, clin_status)

# matchit example ---------------------------------------------------------

# use pacman to install & load multiple pacckages
pacman::p_load(knitr, wakefield, MatchIt, tableone, captioner)

# create patient dataframe
set.seed(1234)
df.patients <- r_data_frame(n = 250, 
                            age(x = 30:78, 
                                name = 'Age'), 
                            sex(x = c("Male", "Female"), 
                                prob = c(0.70, 0.30), 
                                name = "Sex"))
df.patients$Sample <- as.factor('Patients')

# create population dataframe
set.seed(1234)
df.population <- r_data_frame(n = 1000, 
                              age(x = 18:80, 
                                  name = 'Age'), 
                              sex(x = c("Male", "Female"), 
                                  prob = c(0.50, 0.50), 
                                  name = "Sex"))
df.population$Sample <- as.factor('Population')

# merge patient and population data (bind rows) and create new vars
# group is a logical that's TRUE if sample == patient
# distress is a distress score that's set to be higher on average in females
mydata <- rbind(df.patients, df.population)
mydata$Group <- as.logical(mydata$Sample == 'Patients')
mydata$Distress <- ifelse(mydata$Sex == 'Male', age(nrow(mydata), x = 0:42, name = 'Distress'),
                          age(nrow(mydata), x = 15:42, name = 'Distress'))

# print table comparing distributions of two samples
table1 <- CreateTableOne(vars = c('Age', 'Sex', 'Distress'), 
                         data = mydata, 
                         factorVars = 'Sex', 
                         strata = 'Sample')
table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table1[,1:3],  
      align = 'c', 
      caption = 'Table 1: Comparison of unmatched samples')

# The method command method="nearest" specifies that the nearest neighbors
# method will be used. Other matching methods are exact matching,
# subclassification, optimal matching, genetic matching, and full matching
# (method = c("exact", "subclass", "optimal", ""genetic", "full")). The ratio
# command ratio = 1 indicates a one-to-one matching approach. With regard to our
# example, for each case in the patient sample exactly one case in the
# population sample will be matched. Please also note that the Group variable
# needs to be logic (TRUE vs. FALSE).
set.seed(1234)
match.it <- matchit(Group ~ Age + Sex, data = mydata, method="nearest", ratio=1)
a <- summary(match.it)

# After matching the samples, the size of the population sample was reduced to
# the size of the patient sample (n=250; as shown in this table ).
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')

# The following output shows, that the distributions of the variables Age and
# Sex are nearly identical after matching.
kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df
df.match <- match.data(match.it)[1:ncol(mydata)]
rm(df.patients, df.population)

# now try this on the SPM-2 data
IT_49_Home_Stand_Clin_match <- bind_rows(
  IT_49_Home_Stand_match, 
  IT_49_Home_Clin_match
  )
IT_49_Home_Stand_Clin_match$Group <- as.logical(IT_49_Home_Stand_Clin_match$clin_status == 'clin')

# print table comparing distributions of two samples
table1 <- CreateTableOne(vars = c('AgeInMonths', 'Gender', 'ParentHighestEducation', 'Ethnicity'), 
                         data = IT_49_Home_Stand_Clin_match, 
                         factorVars = c('AgeInMonths', 'Gender', 'ParentHighestEducation', 'Ethnicity'),
                         strata = 'clin_status')
table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table1[,1:3],  
      align = 'c', 
      caption = 'Table 1: Comparison of unmatched samples')

# run matchit to get 1:1 matching
set.seed(1234)
match.it <- matchit(
  Group ~ AgeInMonths + Gender + ParentHighestEducation + Ethnicity, 
  data = IT_49_Home_Stand_Clin_match, 
  method = "nearest", 
  ratio = 1)
a <- summary(match.it)

# print table showing that sample sizes are identical
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')

# print table showing distribution of demo variables in matching samples
kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

# save matched samples into new df
# df.match <- match.data(match.it)[1:ncol(IT_49_Home_Stand_Clin_match)]
df.match <- match.data(match.it) %>% 
  select(-Group, -distance, -weights)

# examine distributions of demographic vars
match_dist <- df.match %>% 
  group_by(clin_status, AgeInMonths, Gender, ParentHighestEducation, Ethnicity) %>% 
  summarise(n = n())

# put matched groups side-by-side
typ_match <- df.match %>% filter(clin_status == 'typ')
clin_match <- df.match %>% filter(clin_status == 'clin')
match_comp <- bind_cols(typ_match, clin_match)

write_csv(match_comp, here('match_comp.csv'))

