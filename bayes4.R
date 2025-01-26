data1$dribbles_per_match <- data1$dribbles / data1$matches_appareance
data1$attempts_on_target_per_match <- data1$attempts_on_target / data1$matches_appareance
data1$attempts_off_target_per_match <- data1$attempts_off_target / data1$matches_appareance
data1$blocked_per_match <- data1$blocked / data1$matches_appareance
data1$fouls_suffered_per_match <- data1$fouls_suffered / data1$matches_appareance
data1$minutes_played_per_match <- data1$minutes_played / data1$matches_appareance

priors2 = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "sds")
  # set_prior("normal(0, 5)", class = "b", coef = "field_positionForward"),
  # set_prior("normal(0, 5)", class = "b", coef = "field_positionMidfielder")
)

model_test <- brm(
  goal_contribution ~ 
    dribbles + 
    blocked +
    # attempts_on_target + 
    attempts_off_target +
    fouls_suffered + 
    t2(attempts_on_target, scale(minutes_played), k = 10) + 
    field_position + 
    matches_appareance +
    (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)



bayes_R2(model_test)


