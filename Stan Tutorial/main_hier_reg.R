rm(list=ls())
library(boot) # inverse logit function
library(rstan)
library(ggplot2)
library(shinystan)
rstan_options(auto_write = TRUE)
library(MASS) # mvrnorm

# setwd("C:/Users/npadilla19/Google Drive/Classes/2018 - Spring/TA/PHD Empirical Models/Stan/")

set.seed(0)
I=200
T=40
P=3
N=I*T
X=cbind(rep(1,N),
        rnorm(N),
        rnorm(N))
mu_beta=c(1.5,-1,1)
sigma_beta = diag(rep(0.5^2,P))
beta = mvrnorm(n = I,mu = mu_beta,Sigma = sigma_beta)
ii <- rep(1:I, each = T)

sigma_y <-0.5

Y = rep(0,N)

for (n in 1:N) {
  Y[n] = rnorm(1,mean = beta[ii[n],]%*%X[n,],sd = sigma_y)
}
ggplot(data = data.frame(beta_true=c(beta),param=rep(c("Intercept","Price","Feature"),each = I)),
       aes(x=beta_true,colour=param,fill=param))+geom_density(alpha=0.2)


data_list<-list(N=N,I=I,ii=ii,P=P,x=X,y=Y)

hier_reg <- stan_model(file = "./hier_reg.stan")

hievbfit <- vb(hier_reg,data = data_list,iter=2000, output_samples=50,
               adapt_engaged=FALSE, eta=0.1)
vbfl <- extract(hievbfit)
rm(hievbfit)

axis.mean = function(x, axis){
  n_axes = length(dim(x))
  if(n_axes < 2){
    out = mean(x)
  } else {
    if(axis > n_axes){
      stop("Error: Axis number greater than total number of axes.")
    }
    out = apply(x, (1:n_axes)[-axis], mean)
  }
  return(out)
}

ini_list<- list(lapply(vbfl, axis.mean, axis=1))

hierfit<-sampling(hier_reg,data = data_list,
                  chains = 1,iter = 1000,
                  init=ini_list,
                  pars=c("z","tau_unif","L_Omega"),include = FALSE)

print(hierfit)


save.image("./.hier_reg_results.RData")

#load("./.hier_reg_results.RData")

traceplot(hierfit,pars=c("mu_beta","tau"))

beta_hat <- matrix(get_posterior_mean(hierfit,pars="beta"),nrow = I,ncol = P,byrow = TRUE)
ggplot(data = data.frame(beta_true=c(beta),
                         beta_hat=c(beta_hat),
                         param=rep(c("Intercept","Price","Feature"),each = I)),
       aes(x=beta_true,y=beta_hat))+geom_point()+facet_grid(.~param) + labs(title = "Recovery of individual parameters")
