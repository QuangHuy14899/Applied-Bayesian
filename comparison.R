# Compute LOO-CV for the fitted model
loo_baseline <- loo(model_bayes1)
loo_model_team <- loo(model_bayes2, moment_match = TRUE)
loo_model1 <- loo(model, moment_match = TRUE)
loo_model_avg <- loo(model_avg, moment_match = TRUE)
loo_model_interaction <- loo(model_interaction, moment_match = TRUE)
loo_model_test <- loo(model_test, momment_match=TRUE)

loo_compare(loo_baseline, loo_model_team, loo_model1, loo_model_avg, loo_model_interaction)
