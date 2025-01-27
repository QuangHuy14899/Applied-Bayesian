library(ggplot2)
# best team model with nonlinear
summary(model_test)
ce1 = conditional_effects(model_test)
mcmc_plot(model_test, type = "trace")
mcmc_plot(model_test, type = "dens")
pp1 = pp_check(model_test)
bayes_R2(model_test)

pp1 + 
  ggtitle("Team Model with non-linear effect") +  # Add a title
  xlab("Goal Contribution") +                                  # Customize x-axis label
  ylab("Density") +                                         # Customize y-axis label
  xlim(0, 10) +                                             # Shorten x-axis limits
  theme_minimal()  

plot_ce11 = plot(ce1, plot = FALSE)[[1]]
plot_ce12 = plot(ce1, plot = FALSE)[[2]]



plot_ce11 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Attempts on Target",           # Relabel axes
    y = "Goal Contribution",
    title = "Team Model with non-linear effect"
  ) +
  theme_minimal()

plot_ce12 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Minutes Played",           # Relabel axes
    y = "Goal Contribution",
    title = "Team Model with non-linear effect"
  ) +
  theme_minimal()


# best team model with linear
summary(model_test_lin)
ce2 = conditional_effects(model_test_lin)
mcmc_plot(model_test_lin, type = "trace")
mcmc_plot(model_test_lin, type = "dens")
pp2 = pp_check(model_test_lin)
bayes_R2(model_test_lin)

pp2 + 
  ggtitle("Team Model without non-linear effect") +  # Add a title
  xlab("Goal Contribution") +                                  # Customize x-axis label
  ylab("Density") +                                         # Customize y-axis label
  xlim(0, 10) +                                             # Shorten x-axis limits
  theme_minimal()  

plot_ce21 = plot(ce2, plot = FALSE)[[1]]
plot_ce22 = plot(ce2, plot = FALSE)[[2]]

plot_ce21 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Attempts on Target",           # Relabel axes
    y = "Goal Contribution",
    title = "Team Model without non-linear effect"
  ) +
  theme_minimal()

plot_ce22 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Minutes Played",           # Relabel axes
    y = "Goal Contribution",
    title = "Team Model without non-linear effect"
  ) +
  theme_minimal()

# model for individual with nonlinear
summary(model)
ce3 =conditional_effects(model)
mcmc_plot(model, type = "trace")
mcmc_plot(model, type = "dens")
pp3 = pp_check(model)
bayes_R2(model)

pp3 + 
  ggtitle("Player Model with non-linear effect") +  # Add a title
  xlab("Goal Contribution") +                                  # Customize x-axis label
  ylab("Density") +                                         # Customize y-axis label
  xlim(0, 10) +                                             # Shorten x-axis limits
  theme_minimal()  

plot_ce31 = plot(ce3, plot = FALSE)[[1]]
plot_ce32 = plot(ce3, plot = FALSE)[[2]]

plot_ce31 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Attempts on Target",           # Relabel axes
    y = "Goal Contribution",
    title = "Player Model with non-linear effect"
  ) +
  theme_minimal()

plot_ce32 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Minutes Played",           # Relabel axes
    y = "Goal Contribution",
    title = "Player Model with non-linear effect"
  ) +
  theme_minimal()


# model for individual with linear
summary(model_lin)
ce4 = conditional_effects(model_lin)
mcmc_plot(model_lin, type = "trace")
mcmc_plot(model_lin, type = "dens")
pp4 = pp_check(model_lin)
bayes_R2(model_lin)

pp4 + 
  ggtitle("Player Model without non-linear effect") +  # Add a title
  xlab("Goal Contribution") +                                  # Customize x-axis label
  ylab("Density") +                                         # Customize y-axis label
  xlim(0, 10) +                                             # Shorten x-axis limits
  theme_minimal()  

plot_ce41 = plot(ce4, plot = FALSE)[[1]]
plot_ce42 = plot(ce4, plot = FALSE)[[2]]

plot_ce41 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Attempts on Target",           # Relabel axes
    y = "Goal Contribution",
    title = "Player Model without non-linear effect"
  ) +
  theme_minimal()

plot_ce42 +
  coord_flip() +                          # Swap x and y axes
  labs(
    x = "Minutes Played",           # Relabel axes
    y = "Goal Contribution",
    title = "Player Model without non-linear effect"
  ) +
  theme_minimal()
