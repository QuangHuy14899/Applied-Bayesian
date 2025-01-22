
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
#data1 = data1[data1$goal_contribution!=0,]
data1 = na.omit(data1)

# normalize the predictors
data1$total_attempts = (data1$total_attempts-min(data1$total_attempts))/(max(data1$total_attempts)-min(data1$total_attempts))
data1$passes_completed = (data1$passes_completed-min(data1$passes_completed))/(max(data1$passes_completed)-min(data1$passes_completed))
data1$distance_covered.km.h. = (data1$distance_covered.km.h.-min(data1$distance_covered.km.h.))/(max(data1$distance_covered.km.h.)-min(data1$distance_covered.km.h.))


#plot(data1$passes_completed, data1$goal_contribution, main = "Scatter Plot", xlab = "X Variable", ylab = "Goal Contribution", pch = 16, col = "black")
#mean(data1$goal_contribution)/var(data1$goal_contribution)


priors = prior(normal(0, 1e4), class = "Intercept") +
  prior(normal(0, 1e4), class = "b", coef="total_attempts") +
  prior(normal(0, 1e4), class = "b", coef="passes_completed") +
  prior(normal(0, 1e4), class = "b", coef="distance_covered.km.h.") 

#run this two lines before running the brm function
Sys.unsetenv("LOCAL_CPPFLAGS")
rstan_options(CXXFLAGS = "-O2 -fPIC")

# model 1
model_bayes1 <- brm(
  goal_contribution ~ total_attempts + passes_completed + distance_covered.km.h.,
  data = data1,
  family = poisson,
  prior = priors
)

#the result from the model
summary(model_bayes1)
conditional_effects(model_bayes1)
mcmc_plot(model_bayes1, type = "trace")
mcmc_plot(model_bayes1, type = "dens")
pp_check(model_bayes1)

ce1 = conditional_effects(model_bayes1,method = "posterior_predict")
plot(ce1, points=TRUE)



# model 2
model_bayes2 <- brm(
  goal_contribution ~ total_attempts + passes_completed + distance_covered.km.h.+ (1|team),
  data = data1,
  family = poisson,
  prior = priors
)
summary(model_bayes2)

conditional_effects(model_bayes2)




# debug stuff (no need to look)
install.packages(c("rstan", "StanHeaders"), repos = "https://cloud.r-project.org")


remotes::install_github("stan-dev/cmdstanr")
install_cmdstan() 
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
Sys.getenv("PATH")

file.edit(file.path(Sys.getenv("HOME"), "R", "Makevars.win"))


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')


Sys.unsetenv("LOCAL_CPPFLAGS")
Sys.setenv(LOCAL_CPPFLAGS = "-O2")
rstan_options(CXXFLAGS = "-O2 -fPIC")


sessionInfo()
