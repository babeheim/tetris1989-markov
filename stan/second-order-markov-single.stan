data {
  int<lower = 1> K; // number of states
  int<lower = 1> T; // number of instances in chain
  int<lower = 1, upper = K> z[T]; // markovian data vector
  simplex[K] theta_prior[K, K]; // prior on transitions
}
parameters {
  simplex[K] theta[K, K]; // transition probabilities
}
model {
  for (k in 1:K){
    for(j in 1:K){
      theta[k][j] ~ dirichlet(theta_prior[k][j]);
    }
  }
  for (t in 3:T) z[t] ~ categorical(theta[z[t - 2]][z[t - 1]]);
}
