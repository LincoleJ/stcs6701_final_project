set.seed(2023)
library(readr)
library(dplyr)
library(tidyverse)
# library(glmnet)
library(caret)
library(rstan)
library(bayesplot)

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

### model fitting for parameter tuning

# tau = 1
fit_mcmc_1 <- stan("./hierarhical_bayes/model.stan", 
                  data = stan_train, 
                  iter = 1000, 
                  warmup = 200,
                  chains = 3)

# tau = 