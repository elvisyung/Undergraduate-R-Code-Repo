getwd()
setwd("~/Desktop/Bristol/year 2/Quant and Comp methods/Week 7")

my_data <- read.csv("Report Data 1.csv") # read data
attach(my_data) # don't have to use my_data$tool, can be used at tool [rerun]

rowSums(my_data[,4:7]) #code sums column 4 - 7 (forage + rest + travel + socialize)

str(my_data) # checking the structure of the data 

#Turning categorical variables into factors, categorical/qualitiative variables 
sex <- factor(sex)
tool <- factor(tool)

head(my_data) #Checks the first 10 lines, can check variable name 
# variables present - ID, Tool, Sex, Forage, Rest, Travel, Socialize 

# data[row number, column number] - subsetting data
my_data[1,4] # asked for value of row 1 and column 4 
my_data[1,] # gives all variables in row one 
my_data[,4:7] # gives all variables in columns 4 to 7 (activity budgets)

# Referring to specific factors in a variable 
Forage[tool=="S"]
Forage[tool=="NS"]

# Exploring the data first, visualization 
hist(Forage) # Kinda normally distributed 
hist(Rest) # Most data points are on the left 
hist(Travel) # Kinda normally distributed 
hist(Socialize) # Skewed left 
par(mfrow=c(2,2)) # Allows 2 plots 2 x 2 
par(mfrow=c(1,1))

# Using c("min","max") for comparison of limited from x-axis and y-axis 
hist(Forage)
hist(Forage, xlim=c(0,1))

#Adding title 
hist(Forage, main= "Forage Plot") 
hist(Forage, xlab= "Forage!", ylab="Frequency!") 
#Adding horizontal line to represent a cut-off
hist(Forage)
abline(v=mean(Forage), col="red") # Adding a red line at the mean of forage plot

hist(Socialize)
abline(v=mean(Socialize),col="red")

# Explore activity budget distributions differ between dolphins sponger and non-sponger
hist(Forage[tool=="S"]) # Looks normally distributed
abline(v=mean(Forage[tool=="S"]), col="red")
mean(Forage[tool=="S"])

hist(Forage[tool=="NS"]) # Looks positively skewed (left side)
abline(v=mean(Forage[tool=="NS"]), col="red")
mean(Forage[tool=="NS"])

hist(Rest[tool=="S"]) # Looks positively skewed (left side)
hist(Rest[tool=="NS"]) # Looks normally distributed
hist(Travel[tool=="S"]) # Values are around the value | not doing 
hist(Travel[tool=="NS"]) # Looks normally distributed, with outliers present | not doing 

hist(Socialize[tool=="S"]) # Looks positively skewed (left side)
hist(Socialize[tool=="NS"]) # Looks positively skewed (left side)

#Exploring how Sex impacts activity budget
hist(Forage[sex=="M"]) # Normally distributed
abline(v=mean(Forage[sex=="M"]), col="red")
mean(Forage[sex=="M"])

hist(Forage[sex=="F"]) # Looks normally distributed 
abline(v=mean(Forage[sex=="F"]), col="red")
mean(Forage[sex=="F"])

hist(Rest[sex=="M"]) # Normally distributed
hist(Rest[sex=="F"]) # Positively skewed (left side)
hist(Travel[sex=="M"]) # Normally distributed 
hist(Travel[sex=="F"]) # Normally distributed
hist(Socialize[sex=="M"]) # Looks same values 
hist(Socialize[sex=="F"]) # Positively skewed (left side)

#Fitting a general linear model on normally distributed data 
model <- lm(Forage ~ tool, data=my_data)
hist(model)
plot(model)

#Confirming assumptions, normality of residuals and homogeneity of variance 
res_data <- residuals(model)
hist(res_data) # Looks nofrmally distributed 
qqnorm(res_data)
qqline(res_data, col ="Red") # Residual linear line seems to fit the qqnorm graph

fitted_forage <- fitted(model)
plot(fitted_forage ~ res_data)

# homogeneity assumption not met, data transformation can't help, use a non-parametric test
# Wilcoxon-mann-whitney Test 
#Assume models assumption are not tree and data transformation does not work 
x <- Forage[tool=="S"]
y <- Forage[tool=="NS"]

wilcox.test(x,y, alternative = c("two.sided")) # two.sided = x is greater or less than y

wilcox.test(Forage[tool=="S"],Forage[tool=="NS"], alternative = c("two.sided"))

# for Forage tool - S and NS 

# for forage sex - M and F 
wilcox.test(Forage[sex=="M"],Forage[sex=="F"], alternative = c("two.sided"))

#single categorical predictor variable with more than two grouping levels
# Combining Tools + Sex to predict activity budget 
combined<-with(my_data, interaction(tool,  sex), drop = TRUE)
combined 

model2 <- lm(Forage ~ combined) #Building a lm for Forage ~ combined 
anova(model2)

hist(residuals(model2)) # does not show normal distribution 
qqnorm(residuals(model2)) 
qqline(residuals(model2)) # deviations from the straight line is found 
# therefore assumptions are not met 

plot(fitted(model2) ~ residuals(model2)) # does not apply to this variable

# if assumptions of residual normality or homogeneity of variance is not met use this test
kruskal.test(Forage ~ combined) #non-parametric of a one-way ANOVA

# Looking at the pairs of group's differences using bonferroni, multiple testing
# calculating p-values for different groups with corrections for multiple testing 
pairwise.wilcox.test(Forage, combined, p.adjust.method = "bonferroni", exact = FALSE) # use this

pairwise.wilcox.test(Forage, combined, p.adjust.method = "BH", exact = FALSE)

plot(Forage ~ combined, xlab = "Tool use and Sex Variables", ylab = "Foraging Activity Budget Proportion", data=my_data)

plot(Socialize ~ combined, xlab = "Tool use and Sex Variables", ylab = "Socializing Activity Budget Proportion", data = my_data)

# Data transformation for Forage Combined
# SQRT, does not work 
SQRT_combined_model <-lm(sqrt(Forage) ~ combined, data = my_data)
SQRT_residuals <- residuals(SQRT_combined_model)
hist(SQRT_residuals)
qqnorm(SQRT_residuals)
qqline(SQRT_residuals, col = "blue")
SQRT_fitted <- fitted(SQRT_combined_model)
plot(SQRT_residuals ~ SQRT_fitted)

# Power, does not work 
power_combined_model <- lm((Forage^2) ~ combined, data = my_data)
power_residuals <- residuals(power_combined_model)
hist(power_residuals)
qqnorm(power_residuals)
qqline(power_residuals)
power_fitted <- fitted(power_combined_model)
plot(power_residuals ~ power_fitted)

# Reciprocal, does not work 
reciprocal_model <- lm((1/Forage) ~ combined, data = my_data)
reciprocal_resdiuals <- residuals(reciprocol_model)
hist(reciprocal_resdiuals)
qqnorm(reciprocal_resdiuals)
qqline(reciprocal_resdiuals)
reciprocal_fitted <- fitted(reciprocol_model)
plot(reciprocal_resdiuals ~ reciprocal_fitted)

# Log Does not work 

#===============Independent Research===================#
# Activity Budget Socialize 
model3 <- lm(Socialize ~ combined)
anova(model3) # can't run anova as this is a parametric test 

hist(residuals(model3)) # looks normally distributed
qqnorm(residuals(model3))
qqline(residuals(model3)) # does not display linearity 
# there is no normality in the data therefore conduct a non parametric test 

kruskal.test(Socialize ~ combined)
pairwise.wilcox.test(Socialize,combined,p.adjust.method = "bonferroni", exact= FALSE)
plot(Socialize ~ combined, data=my_data, xlab="Sex and Tool", ylab = "Proportion of Socializing", names = c("Non-sponging Females","Sponging Females","Non-sponging Males","Sponging Males")) 

# Data | does not work Transformation for Socialize combined
# SQRT 
SQRT_combined_model <-lm(sqrt(Socialize) ~ combined, data = my_data)
SQRT_residuals <- residuals(SQRT_combined_model)
hist(SQRT_residuals)
qqnorm(SQRT_residuals)
qqline(SQRT_residuals, col = "blue")
SQRT_fitted <- fitted(SQRT_combined_model)
plot(SQRT_residuals ~ SQRT_fitted)

# Power | does not work 
power_combined_model <- lm((Socialize^2) ~ combined, data = my_data)
power_residuals <- residuals(power_combined_model)
hist(power_residuals)
qqnorm(power_residuals)
qqline(power_residuals)
power_fitted <- fitted(power_combined_model)
plot(power_residuals ~ power_fitted)

# Reciprocal | does not work 
reciprocal_model <- lm((1/Socialize) ~ combined, data = my_data)
reciprocal_resdiuals <- residuals(reciprocol_model)
hist(reciprocal_resdiuals)
qqnorm(reciprocal_resdiuals)
qqline(reciprocal_resdiuals)
reciprocal_fitted <- fitted(reciprocol_model)
plot(reciprocal_resdiuals ~ reciprocal_fitted)

# Log, does not work 




