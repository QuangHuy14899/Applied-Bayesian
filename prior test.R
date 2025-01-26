#model 3 with normal priors
priors2 = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "sds"),
  set_prior("normal(0, 5)", class = "b", coef = "field_positionForward"),
  set_prior("normal(0, 5)", class = "b", coef = "field_positionMidfielder")
)
model_bayes3 <- brm(
  goal_contribution ~ dribbles + total_attempts + attempts_on_target + fouls_suffered + 
    t2(log(distance_covered.km.h.), log(minutes_played), k = 10) + field_position + (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  warmup = 500,
  iter = 1000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)
model_bayes3 <- add_criterion(model_bayes3, "loo",moment_match = TRUE)


#model 3 with stu. priors
priors2 = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("student_t(1, 0, 5)", class = "b"),
  set_prior("student_t(1, 0, 5)", class = "sds"),
  set_prior("student_t(1, 0, 5)", class = "b", coef = "field_positionForward"),
  set_prior("student_t(1, 0, 5)", class = "b", coef = "field_positionMidfielder")
)

model_bayes_stu <- brm(
  goal_contribution ~ dribbles + total_attempts + attempts_on_target + fouls_suffered + 
    t2(scale(distance_covered.km.h.), scale(minutes_played), k = 10) + field_position + (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  warmup = 500,
  iter = 1000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)
model_bayes_stu <- add_criterion(model_bayes3, "loo",moment_match = TRUE)
summary(model_bayes_stu)
bayes_R2(model_bayes_stu)
prior_predict(model_bayes3)


# model 3 with cauchy priors

priors2 = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("cauchy(0, 5)", class = "b"),
  set_prior("cauchy(0, 5)", class = "sds"),
  set_prior("cauchy(0, 5)", class = "b", coef = "field_positionForward"),
  set_prior("cauchy(0, 5)", class = "b", coef = "field_positionMidfielder")
)
model_bayes_cau <- brm(
  goal_contribution ~ dribbles + total_attempts + attempts_on_target + fouls_suffered + 
    t2(log(distance_covered.km.h.), log(minutes_played), k = 10) + field_position + (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  warmup = 500,
  iter = 1000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)
model_bayes_cau <- add_criterion(model_bayes3, "loo",moment_match = TRUE)

summary(model_bayes_cau)
bayes_R2(model_bayes_cau)


#model 3 with logistic
priors2 = c(
  set_prior("logistic(0, 10)", class = "Intercept"),
  set_prior("logistic(0, 2)", class = "b"),
  set_prior("logistic(0, 2)", class = "sds"),
  set_prior("logistic(0, 2)", class = "b", coef = "field_positionForward"),
  set_prior("logistic(0, 2)", class = "b", coef = "field_positionMidfielder")
)

model_bayes_logi <- brm(
  goal_contribution ~ dribbles + total_attempts + attempts_on_target + fouls_suffered + 
    t2(scale(distance_covered.km.h.), scale(minutes_played), k = 10) + field_position + (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  warmup = 500,
  iter = 1000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)
model_bayes_logi <- add_criterion(model_bayes3, "loo",moment_match = TRUE)
summary(model_bayes_logi)
bayes_R2(model_bayes_logi)

#model 3 with laplace
priors2 = c(
  set_prior("double_exponential(0, 10)", class = "Intercept"),
  set_prior("double_exponential(0, 5)", class = "b"),
  set_prior("double_exponential(0, 5)", class = "sds"),
  set_prior("double_exponential(0, 5)", class = "b", coef = "field_positionForward"),
  set_prior("double_exponential(0, 5)", class = "b", coef = "field_positionMidfielder")
)
model_bayes_lap <- brm(
  goal_contribution ~ dribbles + total_attempts + attempts_on_target + fouls_suffered + 
    t2(scale(distance_covered.km.h.), scale(minutes_played), k = 10) + field_position + (1|team),
  data = data1,
  family = zero_inflated_negbinomial(),
  prior = priors2,
  warmup = 500,
  iter = 1000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)
model_bayes_lap <- add_criterion(model_bayes3, "loo",moment_match = TRUE)
summary(model_bayes_lap)
bayes_R2(model_bayes_lap)

#compare
loo_compare(model_bayes_stu,model_bayes3,model_bayes_cau,model_bayes_logi,model_bayes_lap)



