# HW2: Survival Analysis
# Hurricane Readiness Project
# Nov 11, 2019

# Needed Libraries for Analysis #
install.packages("survival")
install.packages("sqldf")
install.packages("survminer")
install.packages("lawstat")
install.packages("flexsurv")

install.packages('useful')
library(useful)

library(survival)
library(survminer)
library(flexsurv)
library(haven)
options(gsubfn.engine = "R")
library(sqldf)
library(dplyr)
library(lattice)
library(lawstat)

# Import data
df <- read_sas("/Users/CathyTran/Documents/Fall III/Survival Analysis/Homework1_SA/hurricane.sas7bdat")

#reason:
#0 – no failure
#1 – flood failure
#2 – motor failure
#3 – surge failure
#4 – jammed failure

# response:
# floor or no flood: 1 means flood    0 means no flood
# R wants the event == to be CENSORED obs (meaning obs that had the event)
# In this case, event == 1

# hour: tenure variable

#######################################
# NewTable creatation 
#######################################

# Only include flood failure as reason
newt <- vector(length=nrow(df)) #newt is a vector with length of 770
newt[which(df$reason==1)] <- 1 #assign 1 for failure due to flood else assign 0
df$flood <- newt #create a new col called flood 
# not sure how R label the column pos. Got an error and put random number until 
# discover which pos hour and flood cols are in
newTable <- df[,c(1:8,58,61)] 

########################################
# Accelerated Failure Time Modeling 
#######################################

# week: event time
# arrest == 1 --> obs that have been censored

# 1. Log-normal Dis.
hur.aft.ln <- flexsurv(Surv(hour, flood == 1) ~
                        backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                      data = newTable, dist = 'lognormal')

summary(hur.aft.ln)

plot(hur.aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week", xlim=c(0, 50), ylab = "Cumulative Hazard",
     main = "Log-Normal Distribution")

# 2. Weibull Dis.
aft.w <- flexsurvreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'weibull')

plot(aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE,
     las = 1, bty = "n", xlab = "week",xlim=c(0, 50), ylab = "Cumulative Hazard",
      main = "Weibull Distribution")

# 3. Exponential Dis
aft.e <- flexsurvreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'exp')
plot(aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week",xlim=c(0, 50), ylab = "Cumulative Hazard",
     main = "Exponential Distribution")

# 4. Gamma
aft.gamma <- flexsurvreg(Surv(hour, flood == 1) ~
                       backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                     data = newTable, dist = 'gamma')
plot(aft.gamma, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Gamma Distribution")

# 5. Log-Logistic Dist
aft.ll <- flexsurvreg(Surv(hour, flood == 1) ~
                        backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                      data = newTable, dist = "llogis")

plot(aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")

#Gen-f --> did not converge
aft.genf <- flexsurvreg(Surv(hour, flood == 1) ~
                        backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
                      data = newTable, dist = "genf")

#statistical tests for differences between tests
# Pull out the log likelihood coefficients of different dist.
like.e <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "weibull")$loglik
like.ln <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "lnorm")$loglik
like.g <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "gamma")$loglik
like.ll <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = newTable, dist = "llogis")$loglik

# Calculate the log likelihood ratio test and check what the p-value is on a chi sq distr.
# Can only compare nested model
pval.e.g <- 1 - pchisq((-2*(like.e-like.g)), 2) # 1.157569e-05 significant
pval.w.g <- 1 - pchisq((-2*(like.w-like.g)), 1) # 1
pval.ln.g <- 1 - pchisq((-2*(like.ln-like.g)), 1) # 0.01137493 significant
pval.e.w <- 1 - pchisq((-2*(like.e-like.w)), 1) # 1.58226583102383e-06 significant

# Combine Values into a List
Tests <- c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam', 'Exp vs. Wei')
P_values <- c(pval.e.g, pval.w.g, pval.ln.g, pval.e.w)
# Combine Tests and P_values by columns
cbind(Tests, P_values)

# 'Exp vs. Gam' - choose Gamma H0: full and simple models provide the same info Ha: full model provides more info
# 'Wei vs. Gam' - choose Wei
# 'LogN vs. Gam'- choose Gamma
# 'Exp vs. Wei - choose Wei

# choose weibull for variable distribution

####################################
# Variable Selection
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
#Conclusion: servo is significant (.00319) and slope is significant (.00064)
#interpretation: pumps with a servomechanism were 100(e^.3860 - 1)% = 47.11% as likely to 
#fail due to flooding on average than those without, holding all else constant

############################
# Upgrading servo on some pumps
############################

# Predicted Survival Quantiles #
survprob.75.50.25 <- predict(aft, type = "quantile",
                             se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(survprob.75.50.25$fit)

# Predicted Mean Event Time #
p.time.mean <- predict(aft, type = "response",
                       se.fit = TRUE)
head(p.time.mean$fit, n = 10)

# Predicted Survival Probabilities #
survprob.actual <- 1 - psurvreg(newTable$hour,
                                mean = predict(aft,
                                               type = "lp"),
                                scale = aft$scale,
                                distribution = aft$dist)
head(survprob.actual, n = 10)

plot(survprob.actual)

#Predicted change by adding a servo 
new_time <- qsurvreg(1 - survprob.actual,
                     mean = predict(aft, type = "lp") + aft$coefficients[2], 
                     scale = aft$scale,
                     distribution = aft$dist)
newTable$new_time <- new_time
newTable$diff <- newTable$new_time - newTable$hour
head(data.frame(newTable$hour, newTable$new_time, newTable$diff), n=100)


#interpretation
#we can pick 16 for which the improvement is most notable
#if it fails due to flood, we have a spike at 42 and 45 hrs in the hazard prob
#for that reason, let's look at pumps that fail at those times
x <- data.frame(newTable$hour, newTable$new_time, newTable$diff)
countx <-  x[which(x$newTable.hour==42 | x$newTable.hour==45),]
count(countx)
#there are 51 pumps in this group
#maybe we look at the ones with low slope and at these hours
z <- newTable$slope[which(x$newTable.hour==42 | x$newTable.hour==45)]
#there are 23 with no slope or slope = 1
#let's also look if they had a backup
check <- newTable[which(x$newTable.hour==42 | x$newTable.hour==45),c('slope','backup')]
#there are 17 with slope <=2 and no backup, so I say we try those

#######################################
# Conditional failure probabilities Plots
# aka Hazard Probabilities
#######################################

# Conditional failure probabilities across time for all pumps together
# Survival probability across time for all pumps together 
# survival obj
pump_surv <- Surv(time = df$hour, event = df$survive == 0)

simple_km <- survfit(pump_surv ~ 1, data = df)

simple_km$hp <- simple_km$n.event/simple_km$n.risk
simple_haz <- merge(data.frame(time = seq(1,48,1)), 
                    data.frame(time = simple_km$time, 
                               hp = simple_km$hp), by = "time",
                    all = TRUE)
simple_haz[is.na(simple_haz) == TRUE] <- 0

plot(y = simple_haz$hp, x = simple_haz$time, main = "Hazard Probability Function", 
     xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(recid_km, data = simple, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Cumulative Hazard", legend = "none")


# Conditional failure probabilities across time for pumps broken down by failure type overlaid into one graph
pump_strat$hp <- pump_strat$n.event/pump_strat$n.risk

#create the grouping factor variable
gp1 <- matrix(rep(1,45)) # reason = 1 
gp2 <- matrix(rep(2,24)) # reason = 2 
gp3 <- matrix(rep(3,28)) # reason = 3 
gp4 <- matrix(rep(4,21)) # reason = 4 
# rbind to create gp col of 118 rows
pump_strat$gp <- rbind(gp1,gp2,gp3,gp4)
pump_haz <- merge(data.frame(time = seq(1,48,1)),
                  data.frame(time = pump_strat$time, hp = pump_strat$hp, gp=pump_strat$gp),
                  by = "time", all = TRUE)
pump_haz[is.na(pump_haz) == TRUE] <- 0
print(pump_haz)
pump_haz$gp <- factor(pump_haz$gp)

ggplot(data=pump_haz, aes(x=time, y=hp, group=gp, color=gp)) +
  theme_classic() +
  theme(title = element_text(size=13), axis.text = element_text(size=12, color='black'), legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)) + 
  xlab("Hour") +
  ylab("Hazard Probability") +
  ggtitle("Hazard Probability By Failure Reason") +
  scale_color_manual(name="Failure Reason", labels = c("Flood","Motor","Surge", "Jammed"), values=c("purple", "red", "blue", "green")) +
  geom_line(size=1)