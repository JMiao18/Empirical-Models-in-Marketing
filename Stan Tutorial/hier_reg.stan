data {
  int<lower=0>                          N; // number of total observations
  int<lower=1>                          I; // number of individuals
  int<lower=1,upper=I>              ii[N]; // individual of each observation
  int<lower=1>                          P; // number of covariates
  matrix[N,P]                           x; // covariates
  real                               y[N]; // outcome
}
parameters {
  matrix[P,I]                            z; // individual standard normal
  row_vector[P]                    mu_beta; // mean vector
  vector<lower=0,upper=pi()/2>[P] tau_unif; // unif. population standard dev.
  cholesky_factor_corr[P]              L_R; // cholesky decomp. of correlation matrix
  real<lower=0>                    sigma_y; // variance of regression error term
}
transformed parameters{
  matrix[I,P]                        beta; // individual coefficients
  vector<lower=0>[P]                  tau; // population standard deviation
  for (k in 1:P) 
    tau[k] = 2.5 * tan(tau_unif[k]);
  beta = rep_matrix(mu_beta,I) + (diag_pre_multiply(tau,L_R) * z)';
}
model {
  to_vector(z) ~ normal(0, 1);
  L_R ~ lkj_corr_cholesky(2);
  mu_beta ~ normal(0,5);
  sigma_y ~ cauchy(0,2);
  // tau_unif ~ uniform(0,pi()/2); this is not necessary
  y ~ normal(rows_dot_product(beta[ii] , x), sigma_y);
  //y[n] ~ normal(beta[ii[n]] * x[,n,sigma_y]);
}
generated quantities{
  matrix[P,P]                  Sigma_beta; // Sigma beta
  Sigma_beta = diag_pre_multiply(tau, L_R) * diag_pre_multiply(tau, L_R)';
}
