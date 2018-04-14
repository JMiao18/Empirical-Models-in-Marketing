rm(list=ls())
library(boot) # inverse logit function
library(rstan)
library(ggplot2)
library(shinystan)
rstan_options(auto_write = TRUE)
library(MASS) # mvrnorm

# setwd("C:/Users/npadilla19/Google Drive/Classes/2018 - Spring/TA/PHD Empirical Models/Stan/")

set.seed(0)

library(boot) # inverse logit function
set.seed(0)
N=1500
P=3
X=cbind(rep(1,N),
        rnorm(N),
        rbinom(n = 1,size = 1,prob = 0.5))
beta=c(0.5,-1,1)
u = runif(N)
Y=(u<=inv.logit(c(X%*%beta)))*1


data_list<-list(N=N,P=P,x=X,y=Y)
log_reg <- stan_model(file = "log_reg.stan")
logrfit<-sampling(log_reg,data = data_list,chains = 3,iter = 1000)
#fit<-stan(model_code = lin_reg,data = data_list,chains = 1,iter = 1000)
print(logrfit)


trace <- traceplot(logrfit,pars="beta")
levels(trace$data$parameter) <- c("Intercept","Price","Feature")
plot(trace)
