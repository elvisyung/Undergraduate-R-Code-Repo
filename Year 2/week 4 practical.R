getwd()

setwd("~/Desktop/Bristol/year 2/Quant and Comp Methods/Week 4")

ant_data <- read.csv("ant_conflict.csv", header = TRUE, ",")

summary(ant_data)

# Plotting the y ~ x 
plot(attrition ~ treatment, data = ant_data)
plot(attrition ~ treatment_numeric, data = ant_data)

# Making variables into categorical data 
ant_data$treatment_numeric <- factor(ant_data$treatment_numeric)
ant_data$treatment <- factor(ant_data$treatment)

# Data transformation 
ant_data$sqrt_attrition <- sqrt(ant_data$attrition)
plot(sqrt_attrition ~ treatment, data = ant_data)

#lm Ant_data (Fitted Model)
lm_ant_data <-lm(sqrt_attrition ~ treatment, data = ant_data)

# Residuals testing # Looked normally distributed 
ant_residuals <- residuals(lm_ant_data)
hist(ant_residuals)
qqnorm(ant_residuals)
qqline(ant_residuals)

# Testing independence of data points # 
summary(lm_ant_data)

#Anova of lm data 
anova(lm_ant_data)

#Homogeneity of data points (data spread is the same)
fitted_ant_data <- fitted(lm_ant_data)
plot(ant_residuals ~ fitted_ant_data)

#Fitted model = lm data 
summary(lm_ant_data)

#Aggregate (Moving the treatment groups around)
aggregate( sqrt(attrition) ~ treatment, data = ant_data, FUN=mean)

ant_data$treatment_bis <- factor(ant_data$treatment, levels=c("QW","WQ","QQ","WW","Control"))

new_ant_data <- lm(sqrt_attrition ~ treatment_bis, data = ant_data)
summary(new_ant_data)

# Anova runs on lm data 
anova(lm_ant_data)

#--------------Post-hoc tests --------------
library(multcomp)
lm_ant_data <-lm(sqrt_attrition ~ treatment, data = ant_data)
pairwise <- glht(lm_ant_data, linfct=mcp(treatment="Tukey"))
summary(pairwise,test=adjusted("bonferroni"))
summary(pairwise,test=adjusted("BH"))

#plot graph
plot(sqrt_attrition ~ treatment, data=ant_data)
mtext(c("a","b","b","b","b"),side=3, at = 1:5)
