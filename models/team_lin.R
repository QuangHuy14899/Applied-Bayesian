# best team model with linear
prior_team_linear = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "sd", group = "field_position", lb=0),
  set_prior("normal(0, 5)", class = "sd", group = "team", lb=0),
  set_prior("normal(0,5)", class = "shape", lb=0)
)

model_team_lin <- brm(
  goal_contribution ~ 
    attempts_on_target +
    minutes_played +
    (1|field_position) +
    (1|team),
  data = data1,
  family = negbinomial(),
  prior = prior_team_linear,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
model_team_lin <- add_criterion(model_team_lin, "loo",moment_match = TRUE)

summary(model_team_lin)

pp_check(model_team_lin, type = "dens_overlay", ndraws = 100)

library(bayesplot)
library(ggplot2)
pp_check(model_team_lin, type = "dens_overlay", ndraws = 100) +
  coord_cartesian(xlim = c(0, 10))  # Adjust upper_bound as needed
