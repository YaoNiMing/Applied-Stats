## ---- tobacco 

library(multcomp)
load('smoke.RData')
smoke <- smoke[,c('Age','Sex','Race','RuralUrban',
    'chewing_tobacco_snuff_or','ever_tobacco_hookah_or_wa')]
colnames(smoke) = c('age','sex','race','rural','chewing','hookah')

#testing if race (white over hispanic and black) has effect 
#on usage of chewing tobacco, after fixing residential condition
smoke1 <- smoke[smoke$race %in% c('white','hispanic','black') &
              !is.na(smoke$chewing),c('race','rural','chewing')]
chewing_group_mean <- aggregate(chewing~race*rural, data=smoke1, FUN=mean)
chewing_group_mean$group <- 
  as.factor(paste(chewing_group_mean$rural,chewing_group_mean$race))

smoke1_fit <- glm(chewing ~ race+rural+race*rural,
                  family="binomial", data=smoke1)
smoke1_M <- rbind(matrix(c(1,0,0,0,0,0),1),matrix(c(1,1,0,0,0,0),1),
                  matrix(c(1,0,1,0,0,0),1),matrix(c(1,0,0,1,0,0),1),
                  matrix(c(1,1,0,1,1,0),1),matrix(c(1,0,1,1,0,1),1))
smoke1_odds <- as.data.frame(t(matrix(exp(smoke1_M %*% 
                          smoke1_fit$coefficients),ncol=2,nrow=3)))
rownames(smoke1_odds)=c('Urban','Rural')
colnames(smoke1_odds)=c('White','Black','Hispanic')

smoke1_K <- rbind(matrix(c(0,1,0,0,0,0),1),matrix(c(0,0,1,0,0,0),1),
                  matrix(c(0,1,0,0,1,0),1),matrix(c(0,0,1,0,0,1),1))
smoke1_ctrst <- glht(smoke1_fit,smoke1_K)
smoke1_95ci <- summary(smoke1_ctrst)$test$coefficient+qnorm(0.975)*
      matrix(c(-1,-1,-1,-1,1,1,1,1),4)*summary(smoke1_ctrst)$test$sigma

#testing if sex has effect on usage of hookah, after fixing other
#demographic info
smoke2 <- smoke[,c('age','sex','race','rural','hookah')]
smoke2 <- smoke2[complete.cases(smoke2),]
hookah_age_mean <- aggregate(hookah~sex*age, data=smoke2, FUN=mean)
hookah_race_mean <- aggregate(hookah~sex*race, data=smoke2, FUN=mean)
hookah_rural_mean <- aggregate(hookah~sex*rural, data=smoke2, FUN=mean)
smoke2_fit <- glm(hookah ~ age + sex + race + rural, data=smoke2)

smoke3 <- smoke[,c('sex','age','race','chewing')]
smoke3 <- smoke3[complete.cases(smoke3) & smoke3$age>10,]
chewing_age_mean <- aggregate(chewing~age, data=smoke3, FUN=mean)
chewing_race_mean <- aggregate(chewing~race, data=smoke3, FUN=mean)
chewing_sex_mean <- aggregate(chewing~sex, data=smoke3, FUN=mean)
smoke3$age <- smoke3$age-11
smoke3_fit <- glm(chewing~sex+age+race,data=smoke3,family = 'binomial')