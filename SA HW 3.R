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

#assuming if data is missing the pump wasn't running
hourcols <- colnames((hurricane[,9:56]))

hurricane[hourcols][is.na(hurricane[hourcols])] <- 0

colSums(is.na(hurricane))

######creating censor variable for everything that isn't motor failure
hurricane <- hurricane %>% 
  mutate(censored = ifelse(reason==2,1,0))

###### Variable selection
#getting rid of trashrack becuase of seperation issues
hurricane.fit <- coxph(Surv(time = hour, event = censored) ~ backup + age + bridgecrane + servo + gear + slope + elevation,data=hurricane)

# Automatic Selection Techniques #
full.model <- hurricane.fit

empty.model <- coxph(Surv(time = hour, event = censored) ~ 1, data = hurricane)

step.model <- step(full.model, 
                   scope = list(lower=formula(empty.model), 
                                upper=formula(full.model)), 
                   direction = "backward", k =qchisq(0.03,1,lower.tail=FALSE))
summary(step.model)

#######Testing assumptions
#making model with selected variables to test assumptions on
hurricane.fit2 <- coxph(Surv(time = hour, event = censored) ~ age + servo + slope, data = hurricane)

#concordance 0.786
concordance(hurricane.fit2)

#Martingale Residuals
#sqrt(age and slope) doesn't help
survminer::ggcoxfunctional(hurricane.fit2, data=hurricane)

#schoenfield residuals
#fail to reject so variables don't depend on time
hurricane.fit.zph <- cox.zph(hurricane.fit2, transform = 'identity')
ggcoxzph(hurricane.fit.zph)





#####Binning age and slope
# Define breaks based on quantiles
breaks1 <- quantile(hurricane$age, probs = seq(0, 1, 0.2), na.rm = TRUE)
breaks2 <- quantile(hurricane$slope, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Bin the continuous variable using cut()
hurricane$age <- cut(hurricane$age, breaks = c(-Inf, breaks1, Inf), labels = FALSE, include.lowest = TRUE)
hurricane$slope <- cut(hurricane$slope, breaks = c(-Inf, breaks2, Inf), labels = FALSE, include.lowest = TRUE)


#Creating ID column
hurricane2 <- hurricane
hurricane2 <- mutate(hurricane2,id=row_number())

#making failure time column from hour
hurricane2$time <- hurricane2$hour
hurricane2 <- hurricane2[, !(colnames(hurricane2) %in% c("hour"))]



#Pivot long
hurricane2long <- pivot_longer(hurricane2,cols = starts_with('h'), names_to = 'hour', values_to = 'activity')

#Get rid of h at beginning of hour 
hurricane2long <- hurricane2long %>%
  mutate(hour = str_replace(hour, "^h", ""))

#Get rid of rows past end time
hurricane2long <- hurricane2long %>%
  rowwise()%>%
  filter(as.numeric(time) >= as.numeric(hour))


#Create hour12 variable - binary 1 if has been running for 12 hours, 0 if not
hurricane2long$hour12 <- sapply(1:nrow(hurricane2long), function(i) {
  if (i <= 11) {
    return(0)
  } else {
    sum_previous_11 <- sum(hurricane2long$activity[(i - 11):i])
    return(as.numeric(sum_previous_11 == 12))
  }
})

#Create start and end variables
hurricane2long <- hurricane2long %>%
  mutate(start = as.numeric(hour) - 1)

hurricane2long <- hurricane2long %>%
  mutate(end = as.numeric(hour))



#Creating censored variable
hurricane2long <- hurricane2long %>%
  rowwise() %>%
  mutate(censored = ifelse(start <= time & end >= time & time != 48, 1, 0))

#Accounting for if failed at 48
hurricane2long <- hurricane2long %>%
  rowwise() %>%
  mutate(censored = ifelse(survive == 0 & start <= time & end >= time & time == 48, 1, censored))




#Factors and numerics 
hurricane2long$servo <- as.numeric(hurricane2long$servo)
hurricane2long$age <- as.factor(hurricane2long$age)
hurricane2long$slope <- as.factor(hurricane2long$slope)
hurricane2long$start_time <- as.numeric(hurricane2long$start)
hurricane2long$stop_time <- as.numeric(hurricane2long$end) + 1
hurricane2long$censored <- as.numeric(hurricane2long$censored)


#Run model
pump.rse <- coxph(Surv(start, end, censored) ~ servo + age + slope + hour12, data = hurricane2long)
summary(pump.rse)




