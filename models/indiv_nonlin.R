# model for individual with nonlinear
priors_player_nonlinear = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "sd", group = "field_position", lb=0),  # Prior for σ_field_position
  set_prior("normal(0, 5)", class = "sds", lb=0),
  set_prior("normal(0,5)", class = "shape", lb=0)
)


model_indiv_nonlin <- brm(
  goal_contribution ~ 
    s(attempts_on_target, k=5) +
    s(minutes_played, k=5)+
    (1|field_position),                              # Categorical predictors (gamma coefficients)
  family = negbinomial(),                                  # Poisson likelihood for count data
  data = data1,                                     # Data frame containing the variables
  prior = priors_player_nonlinear,
  control = list(adapt_delta = 0.99),                                 # Warm-up iterations per chain
  save_pars = save_pars(all = TRUE)
)
model_indiv_nonlin <- add_criterion(model_indiv_nonlin, "loo",moment_match = TRUE)

summary(model_indiv_nonlin)

pp_check(model_indiv_nonlin, type = "dens_overlay", ndraws = 100)

library(bayesplot)
library(ggplot2)
pp_check(model_indiv_nonlin, type = "dens_overlay", ndraws = 100) +
  coord_cartesian(xlim = c(0, 10))  # Adjust upper_bound as needed
