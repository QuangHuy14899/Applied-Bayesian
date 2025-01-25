library(brms)
library(tidyverse)

df_final <- read.csv("final_data.csv")
df_final$Midfielder <- as.numeric(df_final$Midfielder == "True")
df_final$Forward <- as.numeric(df_final$Forward == "True")

# Verify changes by checking the first few rows of the dataset
head(df_final)

head(df_final)

str(df_final)

# Create new columns with averages per minute
df_final$avg_dribbles <- df_final$dribbles / df_final$minutes_played
df_final$avg_total_attempts <- df_final$total_attempts / df_final$minutes_played
df_final$avg_fouls_suffered <- df_final$fouls_suffered / df_final$minutes_played
df_final$avg_distance_covered <- df_final$distance_covered / df_final$minutes_played

# Check the structure of the dataset to confirm the new columns are added
str(df_final)

# Fit a Bayesian Poisson regression model using averages per minute
library(brms)
model_avg <- brm(
  contribution ~ avg_dribbles + avg_total_attempts + avg_fouls_suffered +
    s(avg_distance_covered, bs = "bs", k = 5) + # Spline for distance covered per minute
    Midfielder + Forward,                       # Categorical predictors
  family = poisson(),                           # Poisson likelihood for count data
  data = df_final,
  prior = c(
    prior(normal(0, 10), class = "b"),          # Priors for linear terms (beta coefficients)
    prior(normal(0, 10), class = "b", coef = "Midfielder"),
    prior(normal(0, 10), class = "b", coef = "Forward"),
    prior(normal(0, 1), class = "sds")         # Prior for spline smoothness parameters (theta)
  ),
  chains = 4,
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.95)            # Increase adapt_delta to avoid divergences if needed
)

# Print summary of the fitted model
summary(model_avg)

# Plot posterior diagnostics and results
plot(model_avg)

# Perform posterior predictive checks to evaluate fit quality
pp_check(model_avg)
