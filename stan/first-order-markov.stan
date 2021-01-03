data {
  int<lower = 1> K; // number of states
  int<lower = 1> N; // number of chains
  int<lower = 1> T[N]; // length of each chain
  int<lower = 0> S[N]; // starting location of chain beginning at 0
  int<lower = 1> Q; // length of all chains together (Stan has no ragged array or list options)
  int<lower = 1, upper = K> z[Q]; // concatenated markovian data vectors
  vector<lower = 0>[K] theta_prior[K]; // prior on transitions
}
parameters {
  simplex[K] theta[K]; // transition probabilities
}
model {
  for (k in 1:K) {
    theta[k] ~ dirichlet(theta_prior[k]);
  }
  for (n in 1:N) {
    for (t in 2:T[n]) z[S[n] + t] ~ categorical(theta[z[S[n] + (t - 1)]]);
  }
}
