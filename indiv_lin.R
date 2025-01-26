priors_lin = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b", lb = 0)
  # set_prior("normal(0, 5)", class = "sds")
  # set_prior(exponential(1), class = "sigma")  # Prior for residual variance
)

model_indiv_lin <- brm(
  goal_contribution ~ 
    attempts_on_target +
    minutes_played +
    (1|field_position),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)


summary(model_indiv_lin)


bayes_R2(model_indiv_lin)


