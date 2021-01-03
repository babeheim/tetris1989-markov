data {
  int<lower = 1> K; // number of states
  int<lower = 1> T; // length of the one chain
  int<lower = 1, upper = K> z[T]; // Markov chain data
  vector<lower = 0>[K] theta_prior[K]; // prior on transitions
}
parameters {
  simplex[K] theta[K]; // transition probabilities
}
model {
  for (k in 1:K){
    theta[k] ~ dirichlet(theta_prior[k]);
  }
  for (t in 2:T) z[t] ~ categorical(theta[z[t - 1]]);
}
