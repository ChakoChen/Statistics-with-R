#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

library(MASS) # needed for cov.trob 
library(mnormt) # needed for dmt

# A helper function to compute the multidimensional loglikelihood: 
#
loglik_fn = function(data,nu_in){
  fit = cov.trob(data,nu=nu_in)
  mu = as.vector(fit$center)
  sigma = as.matrix(fit$cov)
  sum(log(dmt(data,mean=fit$center,S=sigma,df=nu_in)))
}

# Finds the degree-of-freedom parameter (nu) that gives the largest loglikelihood:
# Here we just compute the loglikelihood for a range of values for nu
# (a continuous optimization routine could be used instead)
# 
ml_fit_multivariate_t = function(returns,df_range=seq(2.5,8,0.01)){

  n = length(df_range)
  loglik = rep(0,n)
  for( ii in 1:n ){
    loglik[ii] = loglik_fn(returns,df_range[ii])
  }

  # Find the location of the maximum of loglik:
  #
  max_index = which.max( loglik )

  # Refit with this optimal value for nu:
  #
  fit = cov.trob(returns,nu=df_range[max_index])

  res = list( fit=fit, nu=df_range[max_index], df_range=df_range, logliks=loglik, max_index=max_index )
  return(res)
  
}
