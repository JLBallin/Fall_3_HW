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
#getting rid of trashhrack becuase of seperation issues
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

#making new survival and checking martingale
hurricane.fit3 <- coxph(Surv(time = hour, event = censored) ~ age + servo + slope, data = hurricane)
survminer::ggcoxfunctional(hurricane.fit3, data=hurricane)


#####Creating time dependant variable
#create pump id column
hurricane2 <- hurricane
hurricane2 <- mutate(hurricane2,id=row_number())
#making failure time column from hour
hurricane2$time <- hurricane2$hour
hurricane2 <- hurricane2[, !(colnames(hurricane2) %in% c("hour"))]

##split up the data with the time dependant variable
#creating long data
hurricane2long <- pivot_longer(hurricane2,cols = starts_with('h'), names_to = 'hour', values_to = 'activity')

#converting hour to numeric
hurricane2long$hour <- as.numeric(gsub("h", "", hurricane2long$hour))
#removing rows where hour column is NA from where converting hour to numeric introduced NA
hurricane2long <- hurricane2long[!is.na(hurricane2long$hour),]

#create a group variable based on streaks
rle_result <- rle(hurricane2long$activity)
hurricane2long$group <- rep(seq_along(rle_result$lengths), rle_result$lengths)

#group by streaks of activity
hurricane2long <- hurricane2long %>% 
  group_by(id,group,activity) %>% 
  summarise(duration = n(), start_time = min(hour), stop_time = max(hour))

#merging start and stop time info with servo, age and slope
selectvars = subset(hurricane2, select = c('id','servo','age','slope','censored','time'))
pump_data <- left_join(hurricane2long, selectvars, by='id')

#filter for active hours and create time-dependant variable
longrun <- pump_data %>% 
  mutate(longrun = ifelse(max(duration)>= 12 & activity == 1,1,0))

#making so censored is 1 only for row where time is in the start and stop time
censored2 <- function(row) {
  if (row["start_time"] <= row["time"] & row["stop_time"] >= row["time"] & row["time"] != 48) {
    row["censored"] <- 1
  }
  return(row)
}

# Apply the function to each row
longrun <- as.data.frame(apply(longrun, 1, censored2))
longrun <- data.frame(t(longrun))

######Making Final Model
longrun$stop_time <- longrun$stop_time+1
pump.rse <- coxph(Surv(start_time, stop_time, censored) ~ servo + age + slope + longrun, data = longrun)
summary(pump.rse)

