getwd()
setwd("~/Desktop/Bristol/year 2/Quant and Comp methods/Week 9 ")

data2 <- read.csv("Guppy sizes Trinidad.csv")

str(data2)

model1 <- lm (SBL~Predation, data=data2)
anova(model1)
summary(model1)

# Testing for power 
High_Sample <- sample( data2[data2$Predation=="High",]$SBL, size=20, replace=F) # gives a sample size of 20 from $SBL
# from high predation | replace F = without replacement, values cannot be chosen again  

Low_Sample <- sample( data2[data2$Predation=="Low",]$SBL, size=20, replace=F)

SBL_Sample <- c(High_Sample, Low_Sample)

rep("High",20)
rep("Low", 20)

Predation_Sample:
Predation_Sample <- c(rep("High",20), rep("Low", 20))

model2 <- lm(SBL_Sample~Predation_Sample)
anova(model2) # Larger P value ~ P > 0.05 
summary(model2)

# Testing for 100 samples 
High_Sample2 <- sample( data2[data2$Predation=="High",]$SBL, size=100, replace=F)
Low_Sample2 <- sample( data2[data2$Predation=="Low",]$SBL, size=100, replace=F)

SBL_Sample2 <- c(High_Sample2, Low_Sample2)

Predation_Sample2 <- c(rep("High", 100), rep("Low", 100))
model3 <- lm(SBL_Sample2 ~ Predation_Sample2)
anova(model3) # P < 0.05
summary(model3)

# Testing for 200 samples
High_Sample3 <- sample( data2[data2$Predation=="High",]$SBL, size=200, replace=F)
Low_Sample3 <- sample( data2[data2$Predation=="Low",]$SBL, size=200, replace=F)

SBL_Sample3 <- c(High_Sample3, Low_Sample3)

Predation_Sample3 <- c(rep("High", 200), rep("Low", 200))
model4 <- lm(SBL_Sample3 ~ Predation_Sample3)
anova(model4)
summary(model4)

# Can assign an "item" to a number which can represent the sample size. Replace the number with a variable

# Running a loop that reruns the code and saves the estimated coefficient from the model and P-value 
Est_sample <- numeric()
P_sample <- numeric()

# 300 sample size
for(i in 1:100) {
  High_sample4 <- sample(data2[data2$Predation=="High",]$SBL, size=i, replace=F)
  Low_sample4 <- sample(data2[data2$Predation=="Low",]$SBL, size=i, replace=F)
  
  SBL_sample4 <- c(High_sample4,Low_sample4)
  Predation_sample4 <- c(rep("High", i), rep("Low", i))
  
  model5 <- lm(SBL_sample4~Predation_sample4)
  Est_sample[i] <- coefficients(model5) [2]
  P_sample[i] <- anova(model5)$"Pr(>F)" [1]
}
# without [i], the previous random sample gets overridden and replaced 

# using data from loop
length(Est_sample)
length(P_sample)

Sample_size <- 1:100 # 300

plot(Est_sample~Sample_size)
plot(P_sample~Sample_size)

# Exercise 2 

Penguins <- read.csv("Penguins_for_randomisations.csv", header = TRUE, ",")

# Test assumptions
m1 <- lm(flipper_length_mm ~ species, data = Penguins)
boxplot(Penguins$flipper_length_mm ~ Penguins$species)
plot(m1)
qqnorm(m1$residuals)
summary(m1)
anova(m1)

str(Penguins)

Mean_Adelie <- mean(Penguins[Penguins$species=="Adelie",]$flipper_length_mm)
Mean_Chinstrap <- mean(Penguins[Penguins$species=="Chinstrap",]$flipper_length_mm)
Observed_difference <- Mean_Chinstrap - Mean_Adelie # 4.460 Difference
# Reshuffle a variable and make sure that it works before looping it

Penguins$Flipper_reshuffle <- sample(Penguins$flipper_length_mm, replace=F)

Shuffled_Adelie <- mean(Penguins[Penguins$speices=="Adelie",]$Flipper_reshuffle)
Shuffled_Chinstrap <- mean(Penguins[Penguins$species=="Chinstrap",]$Flipper_reshuffle)

Resampled_difference <- Shuffled_Chinstrap - Shuffled_Adelie

Resampled_difference <- numeric() # Creates an empty vector to fill with data in the loop 

for (i in 1:1000) {
  Penguins$Flipper_reshuffle <- sample(Penguins$flipper_length_mm, replace=F)
  
  Shuffled_Adelie <- mean(Penguins[Penguins$species=="Adelie",]$Flipper_reshuffle)
  Shuffled_Chinstrap <- mean(Penguins[Penguins$species=="Chinstrap",]$Flipper_reshuffle)
  
  Resampled_difference[i] <- Shuffled_Chinstrap - Shuffled_Adelie
}

hist(Resampled_difference, xlim=c(-5,5))
abline(v=Observed_difference,col="red")

# Getting the P-value from the observed mean difference in flipper length betwee two species
sum (abs(Resampled_difference) > abs(Observed_difference))
# 0 

# Bootstrap test needs to have n=50
Chinstraps <- Penguins[Penguins$species=="Chinstrap",]
m2 <- lm(flipper_length_mm~body_mass_g,data=Chinstraps)
plot(m2)

Slopes_sample <- numeric()

for (i in 1:1000) {
  Chinstraps_sample <- Chinstraps[sample(1:nrow(Chinstraps), replace=T),]
  model_sample <- lm(flipper_length_mm~body_mass_g, data=Chinstraps_sample)
  Slopes_sample[i] <- coefficients(model_sample)[2]
}

Chinstraps_sample <- Chinstraps[sample(1:nrow(Chinstraps),replace=T),]

hist(Slopes_sample)
quantile( Slopes_sample ,c(0.025,0.975)) # Find the 95% confidence interval # check 2.5% and 97.5%








