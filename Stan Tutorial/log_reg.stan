data {
  int<lower=0> N;
  int<lower=1> P;
  matrix[N,P] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  vector[P] beta;
}
model {
  beta ~ normal(0,5);
  y ~ bernoulli_logit(x*beta);
  //y ~ bernoulli(inv_logit(x*beta));
}
