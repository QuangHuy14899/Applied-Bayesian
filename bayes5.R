data1$dribbles_per_match <- data1$dribbles / data1$matches_appareance
data1$attempts_on_target_per_match <- data1$attempts_on_target / data1$matches_appareance
data1$attempts_off_target_per_match <- data1$attempts_off_target / data1$matches_appareance
data1$blocked_per_match <- data1$blocked / data1$matches_appareance
data1$fouls_suffered_per_match <- data1$fouls_suffered / data1$matches_appareance
data1$minutes_played_per_match <- data1$minutes_played / data1$matches_appareance

priors2 = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b", lb = 0),
  set_prior("normal(0, 5)", class = "sds")
  # set_prior(exponential(1), class = "sigma")  # Prior for residual variance
)

model_test <- brm(
  goal_contribution ~ 
    s(attempts_on_target, k = 5) +
    s(minutes_played, k = 5) +
    (1|field_position) +
    (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)


summary(model_test)


bayes_R2(model_test)


