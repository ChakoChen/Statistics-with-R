# Chapter 7-R lab

# Problem 1
# 
berndtInvest = read.csv("berndtInvest.csv")
Berndt = as.matrix(berndtInvest[, 2:5])
C = cov(Berndt)
cor(Berndt)
pairs(Berndt)

w = matrix( c(0.5,0.3,0.2,0) )
S = as.matrix(cov(Berndt))

t(w) %*% (S %*% w) # computes teh variance of the linear combination

# Problem 2
#
source('ml_fit_multivariate_t.R')
result = ml_fit_multivariate_t(Berndt)
df = result$df_range           # extract results ... 
max_index = result$max_index
loglik = result$logliks

if( FALSE ){
  # Compute the derivative of the loglikelihood from the discrete samples evaluated above 
  #
  h = df[2]-df[1] 
  d2_LL_nu2 = ( loglik[max_index+1] - 2*loglik[max_index] + loglik[max_index-1] ) / h^2 
}else{
  # Use the function fdHess in the nlme package to numerically evaluate the derivive of the loglikelihood:
  #
  library(nlme)
  res = fdHess( df[max_index], function (x) loglik_fn(Berndt,x) )
  d2_LL_nu2 = res$Hessian 
}

s_nu = sqrt( 1/(-d2_LL_nu2) ) # the standard error in nu 

alpha = 0.10
z_crit = qnorm( 1-0.5*alpha ) 

plot( df, loglik, type="l", cex.axis=1.5, cex.lab=1.5, ylab="loglikelihood", lwd=2 )
abline(h = max(loglik))
abline(v = df[max_index] - z_crit * s_nu)
abline(v = df[max_index] + z_crit * s_nu)
grid()

# Problem 3
#
library(MASS)

par(mfrow=c(1,4))
N = 2500
nu = 3

set.seed(5640)
cov = matrix(c(1,0.8,0.8,1),nrow=2)
X = mvrnorm(N, mu=c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N,df=nu))
X = X * cbind(w,w) 
plot(X,main="(a)")

set.seed(5640)
cov = matrix(c(1,0.8,0.8,1),nrow=2)
X = mvrnorm(N, mu=c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N,df=nu))
w2 = sqrt(nu/rchisq(N,df=nu))
X = X * cbind(w1,w2) 
plot(X,main="(b)")

set.seed(5640)
cov = matrix(c(1,0,0,1),nrow=2)
X = mvrnorm(N, mu=c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N,df=nu))
w2 = sqrt(nu/rchisq(N,df=nu))
X = X * cbind(w1,w2) 
plot(X,main="(c)")

set.seed(5640)
cov = matrix(c(1,0,0,1),nrow=2)
X = mvrnorm(N, mu=c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N,df=nu))
X = X * cbind(w,w) 
plot(X,main="(d)")

# Problem 6
#
# Generate data for R (a one-dimensional projection of two dimensional t data) 
#
alpha = 0.01 

m = c(0.001,0.002) 
Sgma = matrix( data=c( 0.1, 0.03, 0.03, 0.15 ), nrow=2, ncol=2, byrow=T )
df = 5

w = c(1/2,1/2)

nSamples = 10000

X = rmt( nSamples, m, S=Sgma )
Rs = X %*% w

q = quantile( Rs, 1-alpha ) # the 99% point
inds = Rs >= q
print( sprintf( "Using N= %10d has a mean of upper %10.6f quantile of= %10.6f", nSamples, 1-alpha, mean( Rs[inds] ) ) )


# Problem 7
#
library(mnormt)
data(CRSPday,package="Ecdat")
Y = CRSPday[,c(5,7)]
loglik = function(par){
  mu = par[1:2]
  A = matrix( c(par[3],par[4],0,par[5]), nrow=2, byrow=T )
  scale_matrix = t(A) %*% A
  df = par[6]
  f = -sum(log(dmt(Y,mean=mu,S=scale_matrix,df=df)))
  f
}
A = chol(cov(Y))
start = as.vector( c(apply(Y,2,mean),A[1,1],A[1,2],A[2,2],4) )
fit_mvt = optim( start, loglik, method="L-BFGS-B", lower=c(-0.02,-0.02,-0.1,-0.1,-0.1,2), upper=c(+0.02,+0.02,+0.1,+0.1,+0.1,15), hessian=T )
print(fit_mvt) 

fit_mvt$par
fit_mvt$hessian

# Compute the inverse of the negative sample based Fisher information matrix using "solve" 
inv_fisher = solve( fit_mvt$hessian, diag(length(fit_mvt$par)) )

# Extract the diagonal values and take the square root:
sqrt( diag(inv_fisher) )

par = fit_mvt$par
A = matrix( c(par[3],par[4],0,par[5]), nrow=2, byrow=T )
cov_matrix = t(A) %*% A

D = diag( c( sqrt( cov_matrix[1,1] ), sqrt( cov_matrix[2,2] ) ) )
DInv = solve( D, diag(2) )

cor_matrix = DInv %*% ( cov_matrix %*% DInv )

