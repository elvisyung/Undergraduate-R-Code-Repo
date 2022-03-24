#Get working directory and setting working directory 
getwd()
setwd("~/Desktop/Bristol/year 2/Quant and Comp Methods/Week 5")

#---------------------Part 1----------------------

#Reading data files
soil_data <- read.csv("soil_biomass.csv", header = TRUE, ",")

#Viewing Data 
summary(soil_data)
View(soil_data)
head(soil_data)

#Turning categorical predictor into categorical variable or a factor
soil_data$temperature <- factor(soil_data$temperature) 

#Subset the data {Subset of soil patches on high temperature}
#my_data [ condition_on_rows , condition_on_columns ]
subset_high <- soil_data[ soil_data$temperature == "High", ]

subset_low <- soil_data[ soil_data$temperature == "Low", ]

# Understanding the minimum and maximum values in the entire dataset to fit the plot
Ymin <- min(soil_data$soil_biomass, na.rm =T)
Ymax <- max(soil_data$soil_biomass, na.rm =T)

Xmin <- min(soil_data$nitrogen, na.rm =T)
Xmax <- max(soil_data$nitrogen, na.rm =T)

#Setting data to subset_high, extraplolating data from the subset group 
plot(soil_biomass ~ nitrogen, data = subset_high, pch=16, col="red", xlim =c(Xmin, Xmax), ylim = c(Ymin, Ymax))

#Adding points to the plot
points(soil_biomass ~ nitrogen, data = soil_data, pch=8, col = "blue")

#Fitting the model (lm)
model <- lm(soil_biomass ~ temperature + nitrogen + temperature:nitrogen, data=soil_data)

#Assumptions
soil_residuals <- residuals(model)
hist(soil_residuals)
qqnorm(soil_residuals)
qqline(soil_residuals)

#Model Summary
anova(model)
summary(model)

# Plotting plot against data = subset_high
plot (soil_biomass ~ nitrogen , data = subset_high, pch=16, col="red", xlim = c(Xmin, Xmax),  ylim = c(Ymin, Ymax))
points (soil_biomass ~ nitrogen , data = subset_low, pch=8, col="blue")

# Adding lines of best fit
abline (a = 15.9188, b = 4.0280, col="red")
abline (a = 19.321, b = 2.1945, col="blue")

#-----------------------Part 2-------------------------
getwd()

ant_data <- read.csv("ant_habituation.csv", header = TRUE, ",") 
View(ant_data)
head(ant_data)

#categorical data --> factors
ant_data$habituation_odour <- factor(ant_data$habituation_odour,levels=c("OwnColony","ColonyB"))
ant_data$test_fight <- factor(ant_data$test_fight, levels=c("ColonyB","ColonyC"))

boxplot(aggressiveness ~ habituation_odour:test_fight, data=ant_data)

model <- lm(sqrt(aggressiveness) ~ habituation_odour + test_fight + habituation_odour:test_fight, data=ant_data)

hist(residuals(model),col="grey")
qqnorm(residuals(model))
qqline(residuals(model),col="blue")
plot(residuals(model)~fitted(model), pch=16)

anova(model)

# Preforming ad-host tests
ant_data$dummy_interaction <- interaction(ant_data$habituation_odour, ant_data$test_fight)
head(ant_data$dummy_interaction)

model_dummy <- lm(sqrt(aggressiveness) ~ dummy_interaction, data = ant_data)
library (multcomp)
pairwise <- glht (model_dummy, linfct=mcp(dummy_interaction="Tukey"))

summary(pairwise, test=adjusted("BH"))

boxplot(sqrt(aggressiveness) ~ habituation_odour + test_fight, data = ant_data)
mtext(c("b","a","b","b"),side=3, at = 1:4)
# Displays letters 
cld(summary(pairwise, test=adjusted("BH")))
