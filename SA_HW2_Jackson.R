#Survival Analysis HW 2
#Author: Jackson Perry
library(sqldf)
library(survival)
library(survminer)
library(dplyr)
library(lattice)
library(lawstat)
library(flexsurv)

#######################################
#Read in the SAS file
#######################################
df <- read_sas("/Users/jacksonperry/Desktop/Fall3/Homework1_SA/hurricane.sas7bdat")

#reason:
#0 – no failure
#1 – flood failure
#2 – motor failure
#3 – surge failure
#4 – jammed failure

#hour: tenure variable

#survive: survival variable

########################################
#Create the new target variable and predictors
########################################
newt <- vector(length=nrow(df))
newt[which(df$reason==1)] <- 1
df$flood <- newt
newTable <- df[,c(1:8,58,62)]

########################################
#Fitting the AFTs
########################################

#weibull
aft.w <- flexsurvreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'weibull')
plot(aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard",
     main = "Weibull Distribution")

#lognormal
aft.ln <- flexsurvreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'lognormal')
plot(aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard",
     main = "LogNormal Distribution")

#log logistic
aft.ll <- flexsurvreg(Surv(hour, flood == 1) ~
                        backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                      data = newTable, dist = 'llogis')
plot(aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard",
     main = "LogLogistic Distribution")

#exponential
aft.exp <- flexsurvreg(Surv(hour, flood == 1) ~
                        backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                      data = newTable, dist = 'exp')
plot(aft.exp, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard",
     main = "Exponential Distribution")

#gamma
aft.g <- flexsurvreg(Surv(hour, flood == 1) ~
                         backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                       data = newTable, dist = 'gamma')
plot(aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard",
     main = "Gamma Distribution")

#statistical tests for differences between tests
like.e <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "weibull")$loglik
like.ln <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "lnorm")$loglik
like.g <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "gamma")$loglik
like.ll <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "llogis")$loglik
like.f <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "genf")$loglik
#F did not converge, so we don't use it

pval.e.w <- 1 - pchisq((-2*(like.e-like.w)), 1) #1e-6
pval.e.g <- 1 - pchisq((-2*(like.e-like.g)), 2) #1e-5
pval.w.g <- 1 - pchisq((-2*(like.w-like.g)), 1) #1
pval.ln.g <- 1 - pchisq((-2*(like.ln-like.g)), 1) #1e-2

#weibull is just as good as gamma
#gamma is better than exponential, and better than lognormal
#weibull is better than exponential
#we will choose the weibull for variable selection 


####################################
#variable selection
####################################
aft <- flexsurvreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'gamma')

summary(aft)
#age least sig with .47737

aft <- survreg(Surv(hour, flood == 1) ~
                    backup + bridgecrane + servo + gear + trashrack + slope + elevation,
                  data = newTable, dist = 'weibull')

summary(aft)
#elevation least sig with .49817

aft <- survreg(Surv(hour, flood == 1) ~
                    backup + bridgecrane + servo + gear + trashrack + slope ,
                  data = newTable, dist = 'weibull')

summary(aft)
#bridgecrane least sig with .38606

aft <- survreg(Surv(hour, flood == 1) ~
                    backup + servo + gear + trashrack + slope ,
                  data = newTable, dist = 'weibull')

summary(aft)
#trashrack least sig with .04917

aft <- survreg(Surv(hour, flood == 1) ~
                    backup + servo + gear + slope ,
                  data = newTable, dist = 'weibull')

summary(aft)
#gear least sig with 10875

aft <- survreg(Surv(hour, flood == 1) ~
                    backup + servo  + slope ,
                  data = newTable, dist = 'weibull')

summary(aft)
#backup least sig with .02831

aft <- survreg(Surv(hour, flood == 1) ~
                    servo + slope ,
                  data = newTable, dist = 'weibull')

summary(aft)
#yay! servo is significant (.00319) and slope is significant (.00064)
#interpretation: pumps with a servomechanism were 100(e^.3860 - 1)% = 47.11% as likely to 
#fail due to flooding on average than those without, holding all else constant


############################
#looking at improving servo on some pumps
############################
#quantiles
survprob.75.50.25 <- predict(aft, type = "quantile",
                             se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(survprob.75.50.25$fit)

#means
p.time.mean <- predict(aft, type = "response",
                       se.fit = TRUE)
head(p.time.mean$fit, n = 10)

#predicted survival probability
survprob.actual <- 1 - psurvreg(newTable$hour,
                                mean = predict(aft,
                                               type = "lp"),
                                scale = aft$scale,
                                distribution = aft$dist)
head(survprob.actual, n = 10)

#difference by adding a servo 
new_time <- qsurvreg(1 - survprob.actual,
                     mean = predict(aft.ln, type = "lp") + aft$coefficients[2], 
                     scale = aft$scale,
                     distribution = aft$dist)
newTable$new_time <- new_time
newTable$diff <- newTable$new_time - newTable$hour
head(data.frame(newTable$hour, newTable$new_time, newTable$diff))

#interpretation on this is open to interpretation
#we can pick 16 for which the improvement is most notable
#if it fails due to flood, we have a spike at 42 and 45 hrs in the hazard prob
#for that reason, let's look at pumps that fail at those times
x <- data.frame(newTable$hour, newTable$new_time, newTable$diff)
x[which(x$newTable.hour==42 | x$newTable.hour==45),]
#there are 51 pumps in this group
#maybe we look at the ones with low slope and at these hours
z <- newTable$slope[which(x$newTable.hour==42 | x$newTable.hour==45)]
#there are 23 with no slope or slope = 1
#let's also look if they had a backup
check <- newTable[which(x$newTable.hour==42 | x$newTable.hour==45),c('slope','backup')]
#there are 17 with slope <=2 and no backup, so I say we try those
