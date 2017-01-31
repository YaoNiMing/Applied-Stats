## ---- s3read
library(lmtest)
library(multcomp)
library(boot)
data('fruitfly',package='faraway')

## ---- s3fit
#centering thorax
ff_mthorax <- mean(fruitfly$thorax)
fruitfly$thorax <- fruitfly$thorax - ff_mthorax
ff_fit <- glm(longevity~activity + thorax,
              data=fruitfly, family = Gamma(link='log'))

## ---- s3diag
ff_diag = glm.diag(ff_fit)

## ---- s3contrast
K <- rbind(matrix(c(0,-1,1,0,0,0),1),matrix(c(0,1,-1,-1,1,0),1))
ff_ctrst <- glht(ff_fit,K,alternative='less')
ff_test <- summary(ff_ctrst)$test

## ---- s395ci
ff_95ci <- ff_test$coefficient+qnorm(0.975)*matrix(c(-1,-1,1,1),2)*ff_test$sigma

## ---- s3interaction
ff_fit1 <- glm(longevity~activity + thorax + activity*thorax, data=fruitfly, family = Gamma(link='log'))
lrtest(ff_fit,ff_fit1)

