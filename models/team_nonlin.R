# best team model with nonlinear
set.seed(1234)

priors_team_nonlinear = c(
  set_prior("normal(0, 10)", class = "Intercept"),                          # Prior for Intercept
  set_prior("normal(0, 5)", class = "sd", group = "field_position", lb=0),  # Prior for std of random effect field_position
  set_prior("normal(0, 5)", class = "sd", group = "team", lb=0),            # Prior for std of random effect team
  set_prior("normal(0, 5)", class = "sds", lb=0),                           # Prior for std of smooth terms
  set_prior("normal(0,5)", class = "shape", lb=0)                           # Prior for overdispersion param
)

model_team_nonlin <- brm(
  goal_contribution ~ 
    s(attempts_on_target,k=5) +
    s(minutes_played,k=5) +
    (1|field_position) +
    (1|team),
  data = data1,
  family = negbinomial(),
  prior = priors_team_nonlinear,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
model_team_nonlin <- add_criterion(model_team_nonlin, "loo",moment_match = TRUE)

summary(model_team_nonlin)

pp_check(model_team_nonlin, type = "dens_overlay", ndraws = 100)

library(bayesplot)
library(ggplot2)
pp_check(model_team_nonlin, type = "dens_overlay", ndraws = 100) +
  coord_cartesian(xlim = c(0, 15))  # Adjust upper_bound as needed
