priors2 = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "sds"),
  set_prior("normal(0, 5)", class = "b", coef = "field_positionForward"),
  set_prior("normal(0, 5)", class = "b", coef = "field_positionMidfielder")
)

model_bayes3 <- brm(
  goal_contribution ~ dribbles + total_attempts + attempts_on_target + fouls_suffered + 
    t2(scale(distance_covered.km.h.), scale(minutes_played), k = 10) + field_position + (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
model_bayes3 <- add_criterion(model_bayes3, "loo")




bayes_R2(model_bayes3)

