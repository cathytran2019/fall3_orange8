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

pval.e.w <- 1 - pchisq((-2*(like.e-like.w)), 1) #1e-5
pval.e.g <- 1 - pchisq((-2*(like.e-like.g)), 2) #1e-6
pval.w.g <- 1 - pchisq((-2*(like.w-like.g)), 1) #1e-2
pval.ln.g <- 1 - pchisq((-2*(like.ln-like.g)), 1) #1

#lognormal is just as good as gamma
#gamma is better than weibull, which is better than gamma
#lognormal is simpler than gamma, so we might go that way
#but the plot for gamma is the best - so we will go with gamma

####################################
#variable selection
####################################
aft.ln <- survreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'lognormal')

summary(aft.ln)
#age least sig with .9999

aft.ln <- survreg(Surv(hour, flood == 1) ~
                    backup + bridgecrane + servo + gear + trashrack + slope + elevation,
                  data = newTable, dist = 'lognormal')

summary(aft.ln)
#elevation least sig with .9142

aft.ln <- survreg(Surv(hour, flood == 1) ~
                    backup + bridgecrane + servo + gear + trashrack + slope ,
                  data = newTable, dist = 'lognormal')

summary(aft.ln)
#bridgecrane least sig with .2411

aft.ln <- survreg(Surv(hour, flood == 1) ~
                    backup + servo + gear + trashrack + slope ,
                  data = newTable, dist = 'lognormal')

summary(aft.ln)
#trashrack least sig with .04489

aft.ln <- survreg(Surv(hour, flood == 1) ~
                    backup + servo + gear + slope ,
                  data = newTable, dist = 'lognormal')

summary(aft.ln)
#gear least sig with .05914

aft.ln <- survreg(Surv(hour, flood == 1) ~
                    backup + servo  + slope ,
                  data = newTable, dist = 'lognormal')

summary(aft.ln)
#backup least sig with .04045

aft.ln <- survreg(Surv(hour, flood == 1) ~
                    servo + slope ,
                  data = newTable, dist = 'lognormal')

summary(aft.ln)
#yay! servo is significant (.00102) and slope is significant (.01012)
#interpretation: pumps with a servomechanism were 100(e^.5057 - 1)% = 65.8% as likely to 
#fail due to flooding on average than those without, holding all else constant


############################
#looking at improving servo on some pumps
############################
#quantiles
survprob.75.50.25 <- predict(aft.ln, type = "quantile",
                             se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(survprob.75.50.25$fit)

#means
p.time.mean <- predict(aft.ln, type = "response",
                       se.fit = TRUE)
head(p.time.mean$fit, n = 10)

#predicted survival probability
survprob.actual <- 1 - psurvreg(newTable$hour,
                                mean = predict(aft.ln,
                                               type = "lp"),
                                scale = aft.ln$scale,
                                distribution = aft.ln$dist)
head(survprob.actual, n = 10)

#difference by adding a servo - I for sure did this wrong
new_time <- qsurvreg(1 - survprob.actual,
                     mean = predict(aft.ln, type = "lp") + aft.ln$coefficients[2], #coef(aft.ln['servo']),
                     scale = aft.ln$scale,
                     distribution = aft.ln$dist)
newTable$new_time <- new_time
newTable$diff <- newTable$new_time - newTable$hour
head(data.frame(newTable$hour, newTable$new_time, newTable$diff))
