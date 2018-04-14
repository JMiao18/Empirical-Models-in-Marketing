# install.packages("arm")
# library("arm")
# # if you don't have the "arm" package
# # Type first - install.packages("arm")
#
# install.packages("numDeriv")
# library("numDeriv")
# if you don't have the "numDeriv" package
# Type first - install.packages("numDeriv")


NX = 2 #number of parameters per segment
NS = 1 #number of consumer segments
NJ = 2 #number of brands
NT = 10 #number of period
NI = 300 #number of people

#Data File data2.CSV
# Col1= Customer ID
# Col2 = Time period
# Col3 = choice 1 if choice brand A and 2 if chose brand B
# Col4-5 = prices for brand A and brand B, respectively

#============================================================================
# Reading in data

data=matrix(scan("C:/Users/on2110/Desktop/Session 2 Capturing Heterogeneity/Codes/data2.txt"),5,NI*NT)
data=t(data)

# 1 segment1
coef.vec = c(2,-1) #parameters to be estimated.

log.lik <- function (coef.vec,data,ns,nj,nt,ni,nx)    #likelihood function
{
  prob = array(NA,nj)

  total = 0
  for(i in 1:ni){
      for(k in 1:nt)
	{
	  row = (i-1)*NT+k
	  V1 = coef.vec[1]+coef.vec[2]*data[row,4] #the systematic utility for 1
 	  V2 = coef.vec[2]*data[row,5]			#the systematic utility for 2

	 	  prob[1] = exp(V1)/(exp(V1)+exp(V2))				# logit for 1
	  prob[2] = exp(V2)/(exp(V1)+exp(V2))				# logit for 2
	  choice = data[row,3]

       total = total +log(prob[choice])

  	}
   }
  return(-total)
}

# optimization procedure to calculate the MLE estimates

mle       <- nlm(log.lik,coef.vec,data=data,ns=NS,nj=NJ,nt=NT,ni=NI,nx=NX, hessian=TRUE)

# calculating the Hessian to obtain stdev
 mode = mle$estimate  					# output parameter estimates
 SE = sqrt(diag(solve(mle$hessian)))	# output parameter SEs
 Tvalue = mode/SE					# output parameter T-values
 ll = 2*mle$minimum				# -2*log-likelihood
 np = length(coef.vec)                    # number of parameters
 AIC = 2*(mle$minimum+np)                  		# calculates AIC
 n = sum(NI*NT)                           # number of observations
 BIC = 2*mle$minimum+np*log(n)            # calculates BIC

list(Estimate=mode,SE=SE,Tvalue=Tvalue,minus2ll=ll,AIC=AIC,BIC=BIC)
write.csv(list(Estimate = mode,SE = SE,Tvalue = Tvalue,minus2ll = ll,AIC = AIC,BIC = BIC), file = "1seg.csv")
