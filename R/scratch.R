# generate histograms by agestrat
IT_1030_by_AgeGroup <- IT_1030 %>% mutate(AG2 = case_when(
  AgeInMonths <= 20 ~ "10-20 mo",
  AgeInMonths >= 21 ~ "21-30 mo",
  TRUE ~ NA_character_
)) %>% group_by(AG2)

hist_plot <- ggplot(data = IT_1030_by_AgeGroup, aes(TOT_raw)) +
  geom_histogram(
    binwidth = .2,
    col = "red"
  ) +
  scale_y_continuous(breaks = seq(0, 250, 25)) +
  labs(title = "Frequency Distribution") +
  # stat_function(
  #   fun = function(x, mean, sd, n){
  #     n * dnorm(x = x, mean = mean, sd = sd)
  #   },
  #   args = with(ANTraw_by_agestrat, c(mean = mean(ANT_total), sd = sd(ANT_total), n
  #                     = length(ANT_total)))
  # ) +
  theme(panel.grid.minor=element_blank()) +
  facet_wrap(~AG2)
  print(hist_plot)

LT200_IT_1030 <- IT_1030 %>% filter(TOT_raw <200)

# Create frequency tables for TOT_raw by AgeGroup
LT200_IT_1030_TOT_freq_AgeGroup <- LT200_IT_1030 %>% group_by(AgeGroup) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AgeGroup
LT200_IT_1030_TOT_desc_AgeGroup <-
  LT200_IT_1030 %>% group_by(AgeGroup) %>% arrange(AgeGroup) %>% summarise(n = n(),
                                                                                 median = round(median(TOT_raw), 2),
                                                                                 mean = round(mean(TOT_raw), 2),
                                                                                 sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:5))


# Create frequency tables for TOT_raw by AG2
IT_1030_TOT_freq_AG2 <- LT200_IT_1030 %>% mutate(AG2 = case_when(
  Age <= 4 ~ "2-4 yr",
  Age >= 5 ~ "5 yr",
  TRUE ~ NA_character_
)) %>% group_by(AG2) %>% count(TOT_raw) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(TOT_raw), lag_cum_per = lag(cum_per))

# Compute descriptive statistics, effect sizes for TOT_raw by AG2
IT_1030_TOT_desc_AG2 <-
  LT200_IT_1030 %>% mutate(AG2 = case_when(
    Age <= 4 ~ "2-4 yr",
    Age >= 5 ~ "5 yr",
    TRUE ~ NA_character_
  )) %>% group_by(AG2) %>% arrange(AG2) %>% summarise(n = n(),
                                                      median = round(median(TOT_raw), 2),
                                                      mean = round(mean(TOT_raw), 2),
                                                      sd = round(sd(TOT_raw), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:2))


