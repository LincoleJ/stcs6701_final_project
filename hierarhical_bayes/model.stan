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
  lambda ~ gamma(10^{-2}, 1);     // hyperprior
  for (i in 1:10) {
    beta[:, i] ~ normal(0, lambda^2); // priors for group coefficients
  }
  
  for (j in 1:N) {
    // Likelihood for each observation
    Y[j] ~ bernoulli_logit(X[j] * beta[:, group[j]]);
  }
}


// generated quantities {
//     vector[N] y_pred;     // posterior predictive
//     for (j in 1:N) { 
//         y_pred[j] = bernoulli_logit_rng(X[j] * beta[:, group[j]]);
//     }
// }

generated quantities {
    real log_joint_for_generated_quantities = 0;

    // log likelihood for the generated quantities
    for (j in 1:N) {
        log_joint_for_generated_quantities += bernoulli_logit_lpmf(Y[j] | X[j] * beta[:, group[j]]);
    }

    // include contribution from priorrs
    for (i in 1:10) {
        log_joint_for_generated_quantities += normal_lpdf(beta[:, i] | 0, lambda^2);
    }
    log_joint_for_generated_quantities += gamma_lpdf(lambda | 10^{-2}, 1);
}



