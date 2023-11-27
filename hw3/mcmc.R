library(rstan)
library(bayesplot)
library(ggplot2)

# run mcmc
fit_mcmc <- stan("./model.stan", 
                 data = stan_df, 
                 iter = 3000, 
                 warmup = 1000,
                 chains = 4,
                 control = list(max_treedepth = 15, adapt_delta = 0.95))

# convergence diagnostics
range(extract(fit_mcmc)$Rhat)
color_scheme_set("mix-blue-pink")
p1 <- mcmc_trace(fit_mcmc,  pars = c("lambda"))
ggsave("./lambda.png",p1)
p2 = mcmc_trace(fit_mcmc, pars = c("beta[5,9]"))
ggsave("./beta_5_9.png", p2)
p3 = mcmc_trace(fit_mcmc, pars = c("beta[21,8]"))
ggsave("./beta_21_8.png", p3)
p4 = mcmc_trace(fit_mcmc, pars = c("beta[18,4]"))
ggsave("./beta_18_4.png", p4)
p5 = mcmc_trace(fit_mcmc, pars = c("beta[9,1]"))
ggsave("./beta_9_1.png", p5)

# posterior predictive
post_pred = extract(fit_mcmc)$y_pred # extract the predicted values
post_pred = ifelse(post_pred > 0.5, 1, 0) # convert predictions 0 / 1
post_pred_1 = extract(fit1_mcmc)$y_pred # extract the predicted values
post_pred_1 = ifelse(post_pred_1 > 0.5, 1, 0) # convert predictions 0 / 1

# calculate classification error rate
classification_error <- mean(abs(stan_df$Y - post_pred))

# visualize all beta coefficients
betas = summary(fit_mcmc)$summary[2:221, 1]
