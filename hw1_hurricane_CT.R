# HW1: Survival Analysis
# Hurrican Readiness Project
# Nov 3, 2019

# Needed Libraries for Analysis #
install.packages("survival")
install.packages("sqldf")
install.packages("survminer")
install.packages("lawstat")

library(survival)
library(survminer)
library(haven)
options(gsubfn.engine = "R")
library(sqldf)
library(dplyr)
library(lattice)
library(lawstat)

# Import data
hurricane <- read_sas("/Users/CathyTran/Documents/Fall III/Survival Analysis/Homework1_SA/hurricane.sas7bdat")

#reason:
#0 – no failure
#1 – flood failure
#2 – motor failure
#3 – surge failure
#4 – jammed failure

#survive:
# indicator for pump failure: 1 means failed    0 means did not failed

# hour: tenure variable

#######################################
#Summary Statistics
#######################################

# Percentage of pumps that survived the hurricane:  0.6960352 or 69.60%
table(hurricane$survive)
316/(316+454)
# Alternative code
print("Percentage of pumps that survived the hurricane:")
print(count(hurricane, vars=survive)[2,2]/(count(hurricane, vars=survive)[1,2] + count(hurricane, vars=survive)[2,2]))

# Percentage of pumps in each type of failure
print("Percentage of pumps in each type of failure:")
for (i in 2:5){
  print(paste((i-1), ": ", (count(hurricane,vars=reason)[i,2])/(sum(count(hurricane, vars=reason)[,2]))))
}

# Average failure time for each failure type
print("Average Failure Time By Type")
avgtime <- sqldf('SELECT reason, avg(hour) as AvgFailTime, count(*) as count FROM hurricane GROUP BY reason')
print(avgtime)

# ANOVA for reason
# generated the factor variable needed for Tukey 
hurricane$group <- factor(hurricane$reason)
fit <- aov(hour ~ group, data=hurricane)
summary(fit)
TukeyHSD(fit)

# Check assumptions
# 1) equal variance btwn groups (homoskedasticity)
# 2) residuals are normally distributed
# 3) independence of samples
# QQ plot
qqnorm(fit$residuals)
qqline(fit$residuals) # Significant departures from the line suggest violations of normality.
# Histograms
hist(fit$residuals, col='black', density=20, prob=TRUE) 
# Perform Shaprio-Wilk test for normality of residuals
# H0: distribution of residuals = normal distribution
# Ha: distribution of residuals ≠ normal distribution 
shapiro.test(fit$residuals) #not normal
# Perform Levene's Test for homogenity of variances
# H0: equal variance among all reason
# Ha: unequal variance amond all reason
levene.test(hurricane$hour, hurricane$group) #not equal variance

#######################################
#Survival probability plots
#######################################
# Survival probability across time for all pumps together 
# survival obj
pump_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)

simple_km <- survfit(pump_surv ~ 1, data = hurricane)
summary(simple_km)
ggsurvplot(simple_km, palette = c("purple"), 
           main = "Survival Function", xlab = "Tenure", ylab = "Survival Probability", 
           break.y.by = 0.1, conf.int = FALSE, legend='none')

# Survival probability across time for pumps broken down by failure type overlaid into one graph
# grouped by reason
# Stratified Analysis
# filter out reason=1,2,3,4
fail <- hurricane[which(hurricane$reason %in% c("1","2","3","4")),]
pump_surv1 <- Surv(time = fail$hour, event = fail$survive == 0)
pump_strat <- survfit(pump_surv1 ~ reason, data = fail)
# LogRank Test H0: all survival curves are equal
survdiff(pump_surv1 ~ reason, rho = 0, data = fail) # rho=1 is wilcoxon
summary(pump_strat)
ggsurvplot(pump_strat, data = fail,
           conf.int = FALSE, palette = c("purple", "red", "blue", "green"),
           xlab = "Hour", ylab = "Survival Probability", break.y.by = 0.1,
           title="Survival Probability By Failure Reason",
           legend.title = "Failure Reason", legend.labs = c("Flood","Motor","Surge","Jammed"),
           legend="bottom")

survdiff(recid_surv ~ wexp, rho = 0, data = recid)

#######################################
# Conditional failure probabilities Plots
# aka Hazard Probabilities
#######################################

# Conditional failure probabilities across time for all pumps together
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

###################################
#flood/surge VS motor/jammed comparison
###################################

# statistical test if the major types of failure have similar survival probabilities across time
fail$theirs <- fail$reason %% 2
#0 corresponds to motor/jammed (water-based), 1 to flood/surge (mechanical)
fail$ours <- ceiling(sqrt(fail$reason) %% 1)
#0 corresponds to flood/jammed, 1 to motor/surge

#their categories statistical difference test
survdiff(pump_surv1 ~ fail$theirs, rho = 0, data = fail)

#our categories statistical difference test
survdiff(pump_surv1 ~ fail$ours, rho = 0, data = fail)

