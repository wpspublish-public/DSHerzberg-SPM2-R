# generate histograms by agestrat
Preschool_25_School_by_AgeGroup <- Preschool_25_School %>% group_by(AgeGroup)

hist_plot <- ggplot(data = Preschool_25_School_by_AgeGroup, aes(TOT_raw)) +
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
  facet_wrap(~AgeGroup)
  print(hist_plot)
