data {
  int<lower=0> N;             // total # of observations
  int<lower=0> P;             // # of predictors
  int<lower=1, upper=10> group[N]; // group indicator for each observation
  matrix[N, P] X;             // predictor matrix
  int<lower=0, upper=1> Y[N]; // binary outcomes
}

parameters {
  real<lower=0> lambda;       // hyperprior on coefficients
  matrix[P, 10] beta;         // coefficients for each group
}

model {
  lambda ~ gamma(2, 0.1);     // hyperprior
  for (i in 1:10) {
    beta[:, i] ~ normal(0, lambda); // priors for group coefficients
  }
  
  for (j in 1:N) {
    // Likelihood for each observation
    Y[j] ~ bernoulli_logit(X[j] * beta[:, group[j]]);
  }
}

generated quantities {
    vector[N] y_pred;     // posterior predictive
    for (j in 1:N) { 
        y_pred[j] = bernoulli_logit_rng(X[j] * beta[:, group[j]]);
    }
}


