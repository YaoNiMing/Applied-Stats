## ---- s1
x <- seq(-10, 10, len=40)
off <- rep(c(1,-1), c(25, length(x)-25))
n <- 100
count_beta_x <- 0
beta_x <- rep(0,n)
fit_lr <- rep(0,n)
for (i in 1:n){
  set.seed(i)
  y <- rpois(length(x), exp(off + 0.5 +0.2*x))
  fit1 <- glm(y~x+offset(off), family='poisson')
  beta_x[i] <- summary(fit1)[['coefficients']][[2]]
  se <- summary(fit1)[['coefficients']][[4]]
  if (beta_x[i]+2*se>0.2 & beta_x[i]-2*se<0.2) count_beta_x <- count_beta_x+1
  fit2 <- glm(y~offset(x*0.2)+offset(off),family='poisson')
  fit_lr[i] <- 2*(logLik(fit1)-logLik(fit2))
}