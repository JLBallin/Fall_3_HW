library(tidyverse)
library(survival)
library(foreign)
library(ggplot2)
library(survminer)
library(rms)
library(flexsurv)
library(ciTools)
library(here)
library(visreg)
library(cmprsk)
library(reticulate)

hurricane <- read.csv('https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv')

######creating censor variable for everything that isn't flood failure
hurricane <- hurricane %>% 
  mutate(censored = ifelse(reason==1,1,0))

hurricane.fit = survfit(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation + trashrack,data=hurricane)

######determining best distribution
like.e = flexsurvreg(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation + trashrack, data = hurricane, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation + trashrack, data = hurricane, dist = "weibull")$loglik
like.ln <- flexsurvreg(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation + trashrack, data = hurricane, dist = "lnorm")$loglik
like.g = flexsurvreg(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation + trashrack, data = hurricane, dist = "gamma")$loglik

pval.e.g = pchisq((-2*(like.e-like.g)), 2,lower.tail=F)
pval.w.g = pchisq((-2*(like.w-like.g)), 1,lower.tail=F)
pval.ln.g = pchisq((-2*(like.ln-like.g)), 1,lower.tail=F)

Tests = c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam')
P_values = c(pval.e.g, pval.w.g, pval.ln.g)
cbind(Tests, P_values)
#Weibull Distribution is best

###### Variable selection
hurricane.fit2 <- coxph(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation + trashrack,data=hurricane)

# Automatic Selection Techniques #
full.model <- hurricane.fit2

empty.model <- coxph(Surv(time = hour, event = censored) ~ 1, data = hurricane)

step.model <- step(full.model, 
                   scope = list(lower=formula(empty.model), 
                                upper=formula(full.model)), 
                   direction = "backward", k =qchisq(0.03,1,lower.tail=FALSE))
summary(step.model)


###Making model with weibull distribution and selected variables
hurricane.fit.w <- survreg(Surv(hour, censored) ~ backup + servo + slope, data = hurricane, dist = 'weibull')
summary(hurricane.fit.w)

#### Most impactful Variable
# Predicted Survival Probabilities
survprob.actual = 1 - psurvreg(hurricane$hour,
                               mean = predict(hurricane.fit.w, type = "lp"),
                               scale = hurricane.fit.w$scale, distribution = hurricane.fit.w$dist)

# Predicted Change in Event Time for Backup
new_time = qsurvreg(1 - survprob.actual,
                    mean = predict(hurricane.fit.w, type = "lp") +
                      coef(hurricane.fit.w)['backup'],
                    scale = hurricane.fit.w$scale,
                    distribution = hurricane.fit.w$dist)

hurricane$new_time = new_time
hurricane$diff = hurricane$new_time - hurricane$hour

impact.backup=data.frame(hurricane$hour, hurricane$new_time, hurricane$diff,hurricane$censored,hurricane$backup)
colnames(impact.backup)=c("O.Hour","N.Hour","Diff","Censored","Backup")


impact.backup2=subset(impact.backup,Censored==1 & Backup==0)
head(impact.backup2)

# Predicted Change in Event Time for Servo
new_time_servo = qsurvreg(1 - survprob.actual,
                    mean = predict(hurricane.fit.w, type = "lp") +
                      coef(hurricane.fit.w)['servo'],
                    scale = hurricane.fit.w$scale,
                    distribution = hurricane.fit.w$dist)

hurricane$new_time_servo = new_time_servo
hurricane$diff_servo = hurricane$new_time_servo - hurricane$hour

impact.servo=data.frame(hurricane$hour, hurricane$new_time_servo, hurricane$diff_servo,hurricane$censored,hurricane$servo)
colnames(impact.servo)=c("O.Hour","N.Hour","Diff","Censored","Servo")


impact.servo2=subset(impact.servo,Censored==1 & Servo==0)
head(impact.servo2)

# Predicted Change in Event Time for Slope
new_time_slope = qsurvreg(1 - survprob.actual,
                    mean = predict(hurricane.fit.w, type = "lp") +
                      coef(hurricane.fit.w)['slope'],
                    scale = hurricane.fit.w$scale,
                    distribution = hurricane.fit.w$dist)

hurricane$new_time_slope = new_time_slope
hurricane$diff_slope = hurricane$new_time_slope - hurricane$hour

impact.slope=data.frame(hurricane$hour, hurricane$new_time_slope, hurricane$diff_slope,hurricane$censored,hurricane$slope)
colnames(impact.slope)=c("O.Hour","N.Hour","Diff","Censored","Slope")


impact.slope2=subset(impact.slope,Censored==1 & Slope==0)
head(impact.slope2)
