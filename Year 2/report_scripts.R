# Getting WD and setting WD
getwd()
setwd("~/Desktop/Bristol/year 2/Evolutionary biology/Report [40%]")

# Reading the data and understanding what the data is about 
data <- read.table('darwinsfinches.csv', header = TRUE, sep = ",")
head(data)
str(data)

# Making columns Factors
data$survive <- factor(data$survive, levels=0:1, labels=c("Dead","Alive"))
data$location <- factor(data$location, levels =0:1, labels=c("Daphne", "Santa"))
str(data)

#--------------~Postulate 1 Variability~-----------------
# Produce histograms classifying the bill depths (Daphne Island(non-survivors)
# and survivors done separately) and Santa Cruz Island into classes that show 
# relatively fine resolution.
# Calculate mean and SDs for each of the 3 distributions and present in a table

# Descriptive Statistics 
# Calculating mean and SDS of dead birds on Daphne and all birds on Santa Cruz 

# Calculating the sd for alive and dead birds on Daphne Major
tapply(data$daphne_all, data$survive, sd, na.rm = TRUE) 
tapply(data$daphne_all, data$survive, mean, na.rm = TRUE) # 9.59 | 10.085

# Calculating the sd and mean for Daphne Major and Santa Cruz
tapply(data$both_islands, data$location, mean, na.rm = TRUE) # 9.64 | 10.77
tapply(data$both_islands, data$location, sd, na.rm = TRUE) # 1.06 | 1.023

 # Plotting 
# B4 plotting, must standardize the x-axes 
min_x <- min(data$both_islands - 1.5)
max_x <- max(data$both_islands + 1)

# Tell R to plot all three histograms 
par(mfrow = c(3,1))

# alive + dead are for Daphne Major and Santa Cruz All
hist(data$alive, xlim = c(min_x, max_x), ylim = c(0, 40), xlab = "Bill Depths Length (mm)", main = " ")
hist(data$dead, xlim =c(min_x, max_x), ylim = c(0, 300), xlab = "Bill Depths Length (mm)", main = " ")
hist(data$santa_all, xlim=c(min_x, max_x), ylim = c(0, 25), xlab = "Bill Depths Length (mm)", main = " ")

# Make it publication ready 
# ~ Look at Key Concepts for Biologists 2020/Week 5/Workshop 3/Part2/Loading Files and Making Plots
#----------------~Postulate 2 Heritability~-------------------
# Regression analysis, and plot a graph 
# Mid parent value is the mean bill depth of both parents of the finches 
data$mid <- (data$mother + data$father)/2

# Linear model to describe relationship between mid-parent bill depth
# and offspring bill depth 
lin_mod <- lm(offspring ~ mid, data = data) # use data = data to avoid errors in post-hoc testing
summary(lin_mod)
coef(lin_mod) # slope 0.7663 | 1= suggests that additive effects of genes cause all variation 
# Slope is closer to 1 than 0 which suggests the addictive effect of genes caused all variation 

# Adding regression line 
par(mfrow = c(1,1))
plot(data$mid, data$offspring, xlab = "Mid-Parent Bill Depth (mm)", ylab = "Offspring Bill Depth (mm)")
abline(lin_mod, col = "Red")

# Whether results was statistically significant using ANOVA
anova(lin_mod) # There is significance as the P < 0.05. 

# Make it publication ready + extract essential information 

#----------------~ Postulate 3 More offspring are produced than survive to breed~-------------
# Calculate the mortality rate 
survival <- table(data$survive)
mortality_rate <- survival[1]/(survival[1] + survival[2]) * 100 # (661 / 661 + 90) * 100 = 661/751 
names(mortality_rate) <- NULL
mortality_rate # 88.01598 % or 0.88015979 mortality rate 
# High mortality rate, displays that more offspring are produced, but they don't necessary survive
# as evident from the high mortality rate calculated 

#----------~ Postulate 4 Survival and Reproduction are not random~----------
# 1) Calculating strength of selection (S) 
# t* is the mean bill depth of survivors and t is the mean bill depth of the entire population 
# S = t* - t (Daphne Major Alive - Daphne Major Whole Population)
# S = 10.08 - 9.649 = 0.431
# 2) Calculate the response to selection (R)
# R = h^2 * S
# H = narrow-sense heritability 
# H^2 = the slope of the linear correlation between two variables
# https://faculty.uca.edu/benw/biol4415/presentations/lect6a.pdf
# R = 0.7663 * 0.431 = 0.3302 mm predicted evolutionary increase in bill depth of next generation 
# How a population will change from one generation to the next 
# R = predicts micro evolutionary changes, strength of selection results from differences in survival 

# Testing for differences between Daphne Major alive and dead finches in the population
shapiro.test(data$alive)
shapiro.test(data$dead)
# Since P < 0.05, the data is not normally distributed 

# Two sample t-test ~ testing for differences between two groups, assuming equal variances
t.test(daphne_all~survive, data = data, var.equal = T)
# Since P < 0.05, the true difference in means between group Dead and group Alive is not equal to 0

#Does not fit a normal distribution ~ use Mann-Whitney U test
wilcox.test(daphne_all~survive, data = data)
# Since P < 0.05, the true location shift is not equal to 0 
table(data$survive) # Dead 661 | Alive 90 # 751 on daphne major 

#----------~ Additional Exercises~-----------
# --------------- Exercise 1----------------
# 1) Compare differences in mean bill depth between all birds on Daphne and Santa
# Why might this difference exist. On santa Cruz G. Fortis coexits with smaller species
# G. fuligionsa and how might this affect bill morphology in G.fortis 
# # Character displacement + hybridization

# Compare the differences between dead and alive finches on Daphne vs Santa Cruz
# The bill depths of all birds that died on Daphne Major 
tapply(data$daphne_all,data$survive, mean, na.rm = TRUE) # avg 9.8379975
tapply(data$santa_all,data$survive, mean, na.rm = TRUE) # avg 10.11119
# Difference of means between Santa_all and Daphne_all is 0.2731925 (mm) bill depth 

# Testing for normal distribution. Since one variable P > 0.05, it does not have normality, resulting 
# in the usage of a non parametric statistical test. 
shapiro.test(data$santa_all) # p = 0.1721 
shapiro.test(data$daphne_all) # p = 1.171e-08
# Need normality in both data sets to use parametric stats (therefoer use wilcox.test)

# P < 0.05 is present --> explain significance 
wilcox.test(both_islands ~ location, data = data) # P < 0.05 

#-------------------Exercise 2-------------------
# 2) Determine the coefficient of variation (SD/mean) x 100 of all birds and survivors on Daphne 
# What do the coefficients of variation and the distributions of trait sizes (i.e. bill depths) 
# tell you about the type(s) of natural selection acting during the population crash?
# Selection types - directional selection - several selections type are going on 
# what does selection does on variation 

sd(data$daphne_all, na.rm=TRUE)/mean(data$daphne_all, na.rm=TRUE) * 100 # all birds 10.985 (dead + alive)

tapply(data$daphne_all, data$survive, sd, na.rm = TRUE)/tapply(data$daphne_all, data$survive, mean, na.rm = TRUE) * 100
# Coefficient of Variation = survivors 10.19861 

length(data$both_islands) # 794 sample size of Santa + Daphne 

