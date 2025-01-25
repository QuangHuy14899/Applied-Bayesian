library(brms)
library(tidyverse)

rescale_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

df_final <- read.csv("final_data.csv")
df_final$Midfielder <- as.numeric(df_final$Midfielder == "True")
df_final$Forward <- as.numeric(df_final$Forward == "True")

# Rescale covariates to [0, 1] range
df_final$dribbles_scaled <- rescale_01(df_final$dribbles)
df_final$total_attempts_scaled <- rescale_01(df_final$total_attempts)
df_final$fouls_suffered_scaled <- rescale_01(df_final$fouls_suffered)
df_final$distance_covered_scaled <- rescale_01(df_final$distance_covered)


df_final$distance_covered_per_min <- df_final$distance_covered / df_final$minutes_played

summary(df_final$distance_covered_per_min)

# Verify changes by checking the first few rows of the dataset
head(df_final)

head(df_final)

str(df_final)

# Fit a Bayesian Poisson regression model with interaction terms
model_interaction <- brm(
  contribution ~ 
    s(dribbles_scaled, by = field_position, bs = "bs", k = 5) +
    total_attempts_scaled + 
    fouls_suffered_scaled +
    distance_covered_scaled +
    (1|field_position) +
    (1|team),
  family = poisson(),                            # likelihood for count data
  data = df_final,
  prior = c(
    prior(normal(0, 10), class = "b"),           # Priors for main effects (beta coefficients)
    prior(normal(0, 1), class = "sds")
  ),
  chains = 4,
  iter = 3000,
  warmup = 1000,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)             # Increase adapt_delta to avoid divergences if needed
)





# Print summary of the fitted model
summary(model_interaction)

# Plot posterior diagnostics and results
plot(model_interaction)

# Perform posterior predictive checks to evaluate fit quality
pp_check(model_interaction)


cor(df_final[, c("total_attempts_scaled", "fouls_suffered_scaled", "distance_covered_scaled")])
