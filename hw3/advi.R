library(dplyr)
library(tidyverse)
library(readr)
library(rstan)

# draw randomly 20,000 observations from study
df_original = read_csv("./df.csv")[-1]
df = df_original[sample(nrow(df_original), size = 10000, replace = FALSE), ]

# split to testing and training data
train_idx = sample(seq_len(10000), size = 5000)
train_df = df[train_idx, ]
test_df = df[-train_idx, ]

### we will only use the training dataset from this point on to fit our model

# prepare data
train_df = train_df %>% 
  mutate(svy_year = case_when(svy_year == "1999-2000" ~ 1,
                              svy_year == "2001-2002" ~ 2,
                              svy_year == "2003-2004" ~ 3, 
                              svy_year == "2005-2006" ~ 4,
                              svy_year == "2007-2008" ~ 5,
                              svy_year == "2009-2010" ~ 6,
                              svy_year == "2011-2012" ~ 7,
                              svy_year == "2013-2014" ~ 8,
                              svy_year == "2015-2016" ~ 9,
                              svy_year == "2017-2020" ~ 10))

# create dataset for stan
train_df_mtx = model.matrix(~ . + htn_jnc7 - 1, data = train_df)
group_var = as.integer(train_df$svy_year)
stan_df = list(N = nrow(train_df_mtx),
               P = ncol(train_df_mtx) - 2,
               group = group_var,
               X = train_df_mtx[, !colnames(train_df_mtx) %in% 
                                  c("svy_year", "htn_jnc7")],
               Y = train_df$htn_jnc7)

# run advi
m1_stan = stan_model(file = "./model.stan")
fit_advi <- vb(m1_stan,
               data = stan_df,
               iter = 10000,
               tol_rel_obj = 0.0001,
               algorithm = "meanfield")
print(fit_advi)

# switch to MCMC
