set.seed(2023)
library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(rstan)
library(ggplot)
library(bayesplot)

# helper functions
extract_pos_params <- function(fit, param_name) {
  # Extract the samples for the specified parameter
  param_samples <- extract(fit)[[param_name]]
  
  # Get the number of iterations and chains
  num_iterations <- nrow(fit)
  num_chains <- length(fit@sim$samples)
  
  fin_vals <- array(dim = c(dim(param_samples)[1], num_chains))
  
  return(final_values)
}


# data partition into training/testing
orig_df = read_csv("./data/processed_df.csv")
df_mtx = model.matrix(bp_control_jnc7 ~ ., data = orig_df)[, -1] %>% 
  janitor::clean_names()
trainRows <- createDataPartition(y = orig_df$bp_control_jnc7, 
                                 p = 0.9, 
                                 list = FALSE)
train_x = df_mtx[trainRows, ][, -1]
train_y = orig_df$bp_control_jnc7[trainRows]
group_var = as.integer(train_x[, 1])
stan_train = list(N = nrow(train_x),
                  P = ncol(train_x) - 1,
                  group = group_var,
                  X = train_x[, !colnames(train_x) %in% 
                                c("svy_year")],
                  Y = train_y)

###--------- model fitting for parameter tuning ---------###

# tau = 1, log-prob = -9032.873
fit_mcmc_1 <- stan("./hierarhical_bayes/model.stan", 
                  data = stan_train, 
                  iter = 1000, 
                  warmup = 200,
                  chains = 3)

# tau = 10^1, log-prob = -9006.707
fit_mcmc_2 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

# tau = 10^-1, log-prob = -9012.820
fit_mcmc_3 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

# tau = 10^2, log-prob = -9449.221
fit_mcmc_4 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

# tau = 10^-2, log prob = -9013.901
fit_mcmc_5 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

# tau = 10^-3, log prob = -9019.131
fit_mcmc_6 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

# tau = 10^-2.5, log prob = -9018.914
fit_mcmc_7 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

# tau = 10^-1.5
fit_mcmc_8 <- stan("./hierarhical_bayes/model.stan", 
                   data = stan_train, 
                   iter = 1000, 
                   warmup = 200,
                   chains = 3)

## calculate the log-joint probability for test data
test_x = df_mtx[-trainRows, ][, -1]
test_group_var = as.integer(test_x[, 1])
test_x = test_x[, -1] # delete svy_year
test_y = orig_df$bp_control_jnc7[-trainRows]

# this function takes in a fit and outputs the log joint likelihood on test data
calc_test_log_joint = function(fit) {
  fit_summary = summary(fit)
  
  # Extracting the mean values of the parameters
  parameter_means <- fit_summary$summary[, "mean"]
  mean_beta <- matrix(parameter_means[grep("beta", names(parameter_means))], 
                      nrow = ncol(test_x), 
                      ncol = length(unique(test_group_var)))
  Y_pred <- numeric(length(test_y))
  
  for (j in 1:length(test_y)) {
    
    # Determine the group of the j-th observation in the test set
    group_idx <- test_group_var[j]
    
    # Generate prediction for this observation using the mean beta values
    Y_pred[j] <- plogis(mean_beta[, group_idx] %*% test_x[j, ])
    
  }
  
  log_prob =  sum(dbinom(test_y, size = 1, prob = Y_pred, log = TRUE))
  return(log_prob)
}

# calculate for each fit that we have inputed
calc_test_log_joint(fit_mcmc_1)
calc_test_log_joint(fit_mcmc_2)
calc_test_log_joint(fit_mcmc_3)
####

# plot the training log-joint prob alongside test log-joint prob
tau = c("10^{-3}", "10^{-2.5}", "10^{-2}", "10^{-1.5}", "10^{-1}", 
        "10^{0}", "10^1", "10^2")
in_samp_log_prob = c(-9019.131, -9018.914, -9013.901, -9015.389, -9012.820,
                     -9032.873, -9030.261, -9449.221)
out_samp_log_prob = c(-1388.319, -1391.381, -1385.732, -1390.158, -1387.479,
                      -1389.355, -1389.908, -1417.227)
log_prob_samp = data.frame(
  tau = tau,
  in_samp_log_prob = in_samp_log_prob,
  out_samp_log_prob = out_samp_log_prob
)
log_prob_samp$tau <- factor(log_prob_samp$tau, levels = log_prob_samp$tau)
log_p1 = ggplot(log_prob_samp, aes(x = tau, y = out_samp_log_prob, group = 1)) +
  geom_point() +
  geom_line() +
  geom_line(color = "#028A0F") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Tau (Shape)") +
  ylab("Log Probability") +
  ggtitle("Out-Sample Log Probability")

log_p2 = ggplot(log_prob_samp, aes(x = tau, y = in_samp_log_prob, group = 1)) +
  geom_point() +
  geom_line() +
  geom_line(color = "#028A0F") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Tau (Shape)") +
  ylab("Log Probability") +
  ggtitle("In-Sample Log Probability")

# refit model with selected optimal parameter
fit = stan("./hierarhical_bayes/model.stan", 
           data = stan_train, 
           iter = 5000, 
           warmup = 1000,
           chains = 4)

# calculate training/test error
posterior_samples <- extract(fit)
generate_predictions <- function(X, group, posterior_samples) {
  # Number of observations
  N <- nrow(X)
  
  # Initialize a vector to store predictions
  predicted_probs <- numeric(N)
  
  # Loop over each observation
  for (i in 1:N) {
    # Extract the group-specific beta coefficients
    group_idx <- group[i]
    beta_vector <- posterior_samples$beta[, group_idx, ]
    
    # Generate prediction for this observation
    predicted_probs[i] <- plogis(X[i, ] %*% beta_vector)
  }
  
  # Convert probabilities to binary class predictions
  predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
  return(predicted_classes)
}

# Generate predictions for training and test datasets
train_predictions <- generate_predictions(train_x, group_var, posterior_samples)
test_predictions <- generate_predictions(test_X, test_group, posterior_samples)


# plot log probability against number of iterations
log_prob <- extract(fit)$log_joint_for_generated_quantities
num_iterations <- length(log_prob)
iterations <- 1:num_iterations
ggplot(data.frame(Iteration = iterations, LogProb = log_prob), aes(x = Iteration, y = LogProb)) +
  geom_line() +
  geom_line(color = "darkblue") +
  xlab("Iteration") +
  theme_minimal() +
  ylab("Log Probability") +
  ggtitle("Log-Joint Probability Across Iterations")
