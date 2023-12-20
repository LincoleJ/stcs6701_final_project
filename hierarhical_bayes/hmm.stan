data {
  int<lower=1> N;            // Number of observations
  int<lower=1> K;            // Number of hidden states
  real Y[N];                 // Observed data
}

parameters {
  simplex[K] theta[N];       // Probability of each state for each observation
  ordered[K] mu;             // Mean of output distribution for each state
  real<lower=0> sigma[K];    // Standard deviation for each state
  simplex[K] trans_mat[K];   // Transition matrix (each row is a simplex)
}

model {
  // Priors
  for (k in 1:K) {
    mu[k] ~ normal(0, 10);
    sigma[k] ~ cauchy(0, 5);
  }

  // Hidden Markov Model
  for (n in 2:N) {
    for (k in 1:K) {
      real emission_prob = normal_lpdf(Y[n] | mu[k], sigma[k]);
      real acc_transition_prob = 0;

      // Calculate the accumulated transition probability
      for (j in 1:K) {
        acc_transition_prob += log(theta[n-1, j]) + log(trans_mat[j, k]);
      }
      target += log_mix(theta[n, k], emission_prob, acc_transition_prob);
    }
  }
}
