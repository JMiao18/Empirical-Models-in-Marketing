rm(list=ls())
library(boot) # inverse logit function
library(rstan)
library(ggplot2)
library(shinystan)
rstan_options(auto_write = TRUE)
library(MASS) # mvrnorm

setwd("C:/Users/npadilla19/Google Drive/Classes/2018 - Spring/TA/PHD Empirical Models/Stan/")

set.seed(0)

N=200
X=rnorm(N)
alpha=-0.5
beta=2
sigma=0.5
Y=alpha+beta*X+rnorm(N,sd=sigma)
plot(X,Y)

data_list<-list(N=N,x=X,y=Y)
fit<-stan(file = "lin_reg.stan",data = data_list,chains = 1,iter = 1000)

print(fit)
traceplot(fit,pars=c("alpha","beta","sigma"))

traceplot(fit,pars="alpha",inc_warmup=TRUE)+geom_hline(yintercept = alpha,colour="black")
traceplot(fit,pars="beta",inc_warmup=TRUE)+geom_hline(yintercept = beta,colour="black")
traceplot(fit,pars="sigma",inc_warmup=TRUE)+geom_hline(yintercept = sigma,colour="black")
