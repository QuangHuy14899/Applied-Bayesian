library(brms)
library(tidyverse)

df_final <- read.csv("final_data.csv")
df_final$Midfielder <- as.numeric(df_final$Midfielder == "True")
df_final$Forward <- as.numeric(df_final$Forward == "True")

# Verify changes by checking the first few rows of the dataset
head(df_final)

head(df_final)

str(df_final)



# Model 1
# Fit a Bayesian Poisson regression model with splines
model <- brm(
  contribution ~ 
    dribbles + total_attempts + fouls_suffered +       # Linear predictors (beta coefficients)
    s(distance_covered, bs = "bs", k = 5) +            # Spline effect (theta coefficients)
    Midfielder + Forward,                              # Categorical predictors (gamma coefficients)
  family = poisson(),                                  # Poisson likelihood for count data
  data = df_final,                                     # Data frame containing the variables
  prior = c(
    prior(normal(0, 10), class = "b"),                 # Priors for linear terms (beta)
    prior(normal(0, 10), class = "b", coef = "Midfielder"),
    prior(normal(0, 10), class = "b", coef = "Forward"),
    prior(normal(0, 1), class = "sds")                # Prior for spline smoothness parameters (theta)
  ),
  chains = 4,                                          # Number of MCMC chains
  iter = 3000,                                         # Total iterations per chain (includes warm-up)
  warmup = 1000,                                        # Warm-up iterations per chain
  save_pars = save_pars(all = TRUE)
)

pairs(model)

# Print model summary
summary(model)

# Plot posterior distributions and diagnostics
plot(model)

# Posterior predictive checks
pp_check(model)
