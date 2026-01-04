
data {
  int n;
  int y[n];
}

parameters {
  real<lower=1> lambda;
}

model {
  lambda ~ pareto(1, 1);
  for (i in 1:n) {
    y[i] ~ poisson(lambda);
  }
}

generated quantities {
  real ypred;
  ypred = poisson_rng(lambda);
}
