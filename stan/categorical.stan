data {
  int<lower = 1> K; // number of unique outcomes (here, 7)
  int<lower = 1> T; // number of outcomes observed
  int<lower = 0> y[T]; // vector of outcome data, as integers
  vector<lower = 0>[K] alpha; // prior concentrations for each outcome
}
parameters {
  simplex[K] theta; // outcome probabilities
}
model {
  theta ~ dirichlet(alpha);
  for (i in 1:T) y[i] ~ categorical(theta);
}