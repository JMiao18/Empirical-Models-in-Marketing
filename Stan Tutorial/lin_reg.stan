data {
  int N;                 // number of observations
  vector[N] x;           // covariate
  vector[N] y;           // dependent variable
} 
parameters {
  real alpha;            // intercept
  real beta;             // coefficients
  real<lower=0> sigma;   // std dev of error
} 
model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 5);
  sigma ~ cauchy(0, 5);
  y ~ normal(rep_vector(alpha,N)+beta*x,sigma);
} 
