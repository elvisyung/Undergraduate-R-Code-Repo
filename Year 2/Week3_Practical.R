#--------------Practical 1-----------------
setwd("~/Desktop/Bristol/year 2/Quant and Comp methods/Week 3")

drug_dataset <- read.csv("DrugUptake_vs_pH.csv", header = TRUE, ",")

View(drug_dataset)
head(drug_dataset)

plot(drug_dataset$DrugUptake ~ drug_dataset$pH, data=drug_dataset)

model <- lm(DrugUptake ~ pH, data=drug_dataset)

coef(model)

abline(a=coef(model)[1], b=coef(model)[2], col = "red")

predicted_value <- 0.58 + 0.034*10.5

abline(v = 10.5, col = "blue")
abline(h = predicted_value, untf = FALSE)

residual_model <- residuals(model)

hist(residual_model)

qqnorm(residual_model)
qqline(residual_model)

fitted_model <- fitted(model)

plot(residual_model ~ fitted_model)

summary(fitted_model)
coef(model)

anova(model)
#-------------------Practical 2----------------
setwd("~/Desktop/Bristol/year 2/Quant and Comp methods/Week 3")

fish_data <- read.csv("FishHatching_Pauly_Pullin_1988.csv", header = TRUE, ",")

head(fish_data)
plot(fish_data$HatchingTime_Days ~ fish_data$Temperature_Degrees, data = fish_data)

fish_model <- lm(HatchingTime_Days ~ Temperature_Degrees, data=fish_data)

coef(fish_model)

abline(a=coef(fish_model)[1], b=coef(fish_model)[2], col = "red")

#checking assumptions 
#independence of data points 
#looks like the data points were chosen independently from each other 

#straight-line relationships 
#abline function graphed a negaitvely correlated line 

#Normal residuals 
fish_residual <- residuals(fish_model)
hist(fish_residual) # looks skewed left, not normally distributed 
qqnorm(fish_residual) #produced a graph with a linear relationship developing 
qqline(fish_residual)

# Looks not random for Homogeneity 
fitted_fish <- fitted(fish_model)
plot(fish_residual ~ fitted_fish)
  
#-------Transformation-------------

#logs
log_fish_model_Hatching <- log(fish_data$HatchingTime_Days)
log_fish_model_Temperature <- log(fish_data$Temperature_Degrees)

plot(fish_data$HatchingTime_Days ~ log_fish_model_Temperature)

#Most appropriate [Plotting, abline, anova, summary]
plot(log_fish_model_Hatching ~ fish_data$Temperature_Degrees) 
log_fish_model <- lm(log_fish_model_Hatching ~ fish_data$Temperature_Degrees, data=fish_data)

coef(log_fish_model)
abline(coef(log_fish_model)[1], coef(log_fish_model)[2], col = "red")

summary(log_fish_model)
anova(log_fish_model)

#Sqrt
sqrt_fish_model_Hatching <- sqrt(fish_data$HatchingTime_Days)
sqrt_fish_model_Temperature <- sqrt(fish_data$Temperature_Degrees)

plot(fish_data$HatchingTime_Days ~ sqrt_fish_model_Temperature)
plot(sqrt_fish_model_Hatching ~ fish_data$Temperature_Degrees)

#Squared^2
squared_fish_model_Hatching <- fish_data$HatchingTime_Days^2
squared_fish_model_Temperature <- fish_data$Temperature_Degrees^2

plot(fish_data$HatchingTime_Days ~ squared_fish_model_Temperature)
plot(squared_fish_model_Hatching ~ fish_data$Temperature_Degrees)

par (mfrow=c(1,1))


