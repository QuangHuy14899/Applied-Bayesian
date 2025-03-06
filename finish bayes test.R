library(rstan)
library(posterior)
library(rstantools)
library(bayesplot)
library(shinystan)
library(loo)
library(projpred)
library(rstanarm)
library(brms)


data1 <- read.csv("C:/Users/Quang Huy/Desktop/7.Semester/Bayesian/UCL_football_data.csv", fileEncoding = "UTF-8")
data1$goal_contribution = data1$goals + data1$assists
data1 = na.omit(data1)
data1$field_position = as.factor(data1$field_position)


# best team model with nonlinear
priors_nonlinear = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 5)", class = "sds")
  # set_prior(exponential(1), class = "sigma")  # Prior for residual variance
)

model_test <- brm(
  goal_contribution ~ 
    s(attempts_on_target,k=5) +
    s(minutes_played,k=5) +
    (1|field_position) +
    (1|team),
  data = data1,
  family = negbinomial(),
  prior = priors_nonlinear,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
model_test <- add_criterion(model_test, "loo",moment_match = TRUE)

summary(model_test)


# best team model with linear
prior_linear = c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b", lb = 0)
  #set_prior("normal(0, 5)", class = "sds")
  # set_prior(exponential(1), class = "sigma")  # Prior for residual variance
)

model_test_lin <- brm(
  goal_contribution ~ 
    attempts_on_target +
    minutes_played +
    (1|field_position) +
    (1|team),
  data = data1,
  family = negbinomial(),
  prior = prior_linear,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
model_test_lin <- add_criterion(model_test_lin, "loo",moment_match = TRUE)

summary(model_test_lin)



# model for individual with nonlinear

model <- brm(
  goal_contribution ~ 
    s(attempts_on_target, k=5) +
    s(minutes_played, k=5)+
    (1|field_position),                              # Categorical predictors (gamma coefficients)
  family = negbinomial(),                                  # Poisson likelihood for count data
  data = data1,                                     # Data frame containing the variables
  prior = priors_nonlinear,
  control = list(adapt_delta = 0.99),                                 # Warm-up iterations per chain
  save_pars = save_pars(all = TRUE)
)
model <- add_criterion(model, "loo",moment_match = TRUE)



# model for individual with linear
model_lin <- brm(
  goal_contribution ~ 
    attempts_on_target +
    minutes_played+
    (1|field_position),                              # Categorical predictors (gamma coefficients)
  family = negbinomial(),                                  # Poisson likelihood for count data
  data = data1,                                     # Data frame containing the variables
  prior = prior_linear,
  control = list(adapt_delta = 0.99),                                 # Warm-up iterations per chain
  save_pars = save_pars(all = TRUE)
)
model_lin <- add_criterion(model_lin, "loo",moment_match = TRUE)


loo_compare(model_test,model,model_test_lin,model_lin)

