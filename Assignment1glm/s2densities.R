## ---- s2q99
qlognorm <- function(p,mu,sigma){
  exp(qnorm(p,mu,sigma))
}

qzip <- function(p,zip_lambda,zip_pi){
  p = max(1-(1-p)/(1-zip_pi),0)
  qpois(p,zip_lambda)
}

distr = c('Zero Inflated Poisson','Gamma', 'Weibull',
          'log-Normal','Negative Binomial')
q99_zip = qzip(0.99,zip_lambda,zip_pi)
q99_gamma = qgamma(0.99,gamma_alpha,gamma_beta)
q99_wb = qweibull(0.99,wb_k,wb_lambda)
q99_lnorm = qlognorm(0.99,lnorm_mu,lnorm_sigma)
q99_nbin = qnbinom(0.99,nbin_r,nbin_p)
q99 = data.frame(distr,c(q99_zip,q99_gamma,q99_wb,q99_lnorm,q99_nbin))
## ---- s2densities
dlognorm <- function(x,mu,sigma){
    dnorm(log(x),mu,sigma)/x
}

dzip <- function(x,lambda,pi){
  dpois(x,lambda)*(1-pi)+(x==0)*pi
}


## ---- s2rfunc
rlognorm <- function(n,mu,sigma){
  exp(rnorm(n,mu,sigma))
}

rzip <- function(n,lambda,pi){
  (runif(n)>pi) * rpois(n,lambda)
}

## ---- s2sample20
n=20
zip_s = rzip(n,zip_lambda,zip_pi)
gamma_s = rgamma(n,gamma_alpha,gamma_beta)
wb_s = rweibull(n,wb_k,wb_lambda)
lnorm_s = rlognorm(n,lnorm_mu,lnorm_sigma)
nbin_s = rnbinom(n,nbin_r,nbin_p)
s_mat = rbind(zip_s,gamma_s,wb_s,lnorm_s,nbin_s)
s_mv = data.frame(distr,apply(s_mat,1,mean),apply(s_mat,1,var))
