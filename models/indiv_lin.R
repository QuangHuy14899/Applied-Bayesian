# model for individual with linear
prior_player_linear = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "sd", group = "field_position", lb=0),
  set_prior("normal(0,5)", class = "shape", lb=0)
)

model_indiv_lin <- brm(
  goal_contribution ~ 
    attempts_on_target +
    minutes_played+
    (1|field_position),                              # Categorical predictors (gamma coefficients)
  family = negbinomial(),                                  # Poisson likelihood for count data
  data = data1,                                     # Data frame containing the variables
  prior = prior_player_linear,
  control = list(adapt_delta = 0.99),                                 # Warm-up iterations per chain
  save_pars = save_pars(all = TRUE)
)
model_indiv_lin <- add_criterion(model_indiv_lin, "loo",moment_match = TRUE)

summary(model_indiv_lin)

# Posterior Predictive Checks
pp_check(model_indiv_lin, type = "dens_overlay", ndraws = 100)

# Trace Plots
plot(model_indiv_lin, variable = c("b_Intercept", "sd_field_position__Intercept", "b_attempts_on_target", "b_minutes_played"))

# Check Autocorrelation
library(bayesplot)
posterior <- as.array(model_indiv_lin)
mcmc_acf(posterior, pars = c("b_Intercept", "sd_field_position__Intercept", "b_attempts_on_target", "b_minutes_played"))

# PPC with x-axis limit
library(bayesplot)
library(ggplot2)
pp_check(model_indiv_lin, type = "dens_overlay", ndraws = 100) +
  coord_cartesian(xlim = c(0, 10))  # Adjust upper_bound as needed
