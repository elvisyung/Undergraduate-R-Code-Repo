getwd()
setwd("~/Desktop/Bristol/year 2/Animal Behavior/practical 4 - sum 40%")
par(mfrow=c(1,1))

my_data <- read.csv("Practical4data_Long.csv")
my_data_short <- read.csv("Practical4data.csv")
attach(my_data)
attach(my_data_short)

my_data1 <-read.csv("Practical4data_Long2.csv")

summary(log(RT_mud))
summary(log(RT_mix))
summary(log(RT_grass))

# P < 0.001 - does not tell you which group is statistically significant from each other
kruskal.test(RT ~ treatment, data = my_data)
pairwise.wilcox.test(RT, treatment, p.adjust.method = "bonferroni", exact = FALSE)

# 
stat.test <- aov(logRT ~ treatment, data = my_data1) %>%
  tukey_hsd()

ggboxplot(my_data1, x = "treatment", y = "logRT", ylim = c(-0.3, 8), xlab = "Background Treatments", ylab = "log (Mean Response Time [s])", color = "treatment", palette = "jco") +
  stat_compare_means(method = "anova", label.y = 7) +
  stat_pvalue_manual(
    stat.test, label = "p.adj",
    y.position = c(4,5.5,6.5)
  )

#-------------------------------------------------
# Data Visualization 
head(my_data)
my_data$treatment <- factor(my_data$treatment) # turning categorical variables into factors, categorical/qualitative variable
boxplot((RT) ~ treatment, data = my_data) # people spent longer time mean response time on mud as compared to grass 

# Graphs - Transformed graph   
boxplot(log(RT) ~ treatment, data = my_data, names = c("Grass/Vegetation", "50:50 Mixture of Grass and Mud", "Mud"), xlab = "3 Different Background Treatments", ylab ="log (Mean Response Time (s))", ylim = c(-1, 4))
legend("topleft", legend = c("Grass to Mud - P < 0.001", "Grass to Mix - P < 0.001", "Mud to Mix - P = 0.0034"), bty = "n")
yep <- log(my_data$RT)

df <- data.frame(values= c(yep))

write.csv(df, "~/Desktop/Bristol\\file.csv", row.names = FALSE)

# Testing for normality
model_RT <- lm(RT ~ treatment, data = my_data)
hist(model_RT) # does not work
plot(model_RT) # not normal

res_RT <- residuals(model_RT)
hist(res_RT) # not normal
qqnorm(res_RT) 
qqline(res_RT) # does not show normality 
fitted_RT <- fitted(model_RT)
plot(fitted_RT ~ res_RT) # plotting against residuals and fitted data (does not show normality)

# Data transformation for RT ~ Treatment
# Power Transformation (Does not display normality )
power_RT_model <- lm((RT^2) ~ treatment, data = my_data)
hist(power_RT_model) # does not work
plot(power_RT_model) # not normal
power_RT_residuals <- residuals(power_RT_model)
hist(power_RT_residuals)
qqnorm(power_RT_residuals)
qqline(power_RT_residuals) # not normal
power_fitted_RT <- fitted(power_RT_model)
plot(power_RT_residuals ~ power_fitted_RT) # not homogeneity 

# Reciprocal Transformation (Not normal)
reciprocal_RT_model <- lm((1/RT) ~ treatment, data = my_data)
hist(reciprocal_RT_model) # does not work
plot(reciprocal_RT_model) # not normal
reciprocal_RT_residuals <- residuals(reciprocal_RT_model)
hist(reciprocal_RT_residuals) # not normal
qqnorm(reciprocal_RT_residuals)
qqline(reciprocal_RT_residuals) # not normal
reciprocal_fitted_RT <- fitted(reciprocal_RT_model)
plot(reciprocal_RT_residuals ~ reciprocal_fitted_RT) # not homogeneity 

# SQRT Transformation (Semi Normal)
sqrt_RT_model <- lm(sqrt(RT) ~ treatment, data = my_data)
hist(sqrt_RT_model) # does not work
plot(sqrt_RT_model) # semi normal 
sqrt_RT_residuals <- residuals(sqrt_RT_model)
hist(sqrt_RT_residuals) # looks semi normal
qqnorm(sqrt_RT_residuals)
qqline(sqrt_RT_residuals) # semi straight 
sqrt_fitted_RT <- fitted(sqrt_RT_model)
plot(sqrt_RT_residuals ~ sqrt_fitted_RT) # pretty homogeneity 

# Logging (WORKS!)
log_RT_model <- lm(log(RT) ~ treatment, data = my_data)
hist(log_RT_model) # does not work
plot(log_RT_model) # semi normal 
log_RT_residuals <- residuals(log_RT_model)
hist(log_RT_residuals) # looks normal
qqnorm(log_RT_residuals)
qqline(log_RT_residuals) # ;ooks normal
log_fitted_RT <- fitted(log_RT_model)
plot(log_RT_residuals ~ log_fitted_RT) # looks really normal - homogeneity 

#Google (easier way to do this)
anova_model <- aov(log(RT) ~ treatment, data = my_data)
summary(anova_model)
TukeyHSD(aov(log(RT) ~ treatment), data = my_data)

# Final Results for RT ~ Treatment
model_RT_lm <-lm(log(RT) ~ treatment, data = my_data)
summary(model_RT_lm) # F = 55.64, 2 and 471 DF
anova(model_RT_lm) # P < 0.001

library(multcomp)
model_RT_lm <-lm(log(RT) ~ treatment, data = my_data)
pairwise <- glht(model_RT_lm, linfct=mcp(treatment="Tukey"))
summary(pairwise,test=adjusted("bonferroni"))
summary(pairwise,test=adjusted("BH"))

# ----------- Secondary Variable ------------
boxplot(ppnHit ~ treatment, data = my_data)

boxplot((ppnHit^2) ~ treatment, data = my_data)

model_ppnHit <- lm(ppnHit ~ treatment, data = my_data) 
hist(model_ppnHit) # does not work
plot(model_ppnHit) # not normal

res_ppnHit <- residuals(model_ppnHit)
hist(res_ppnHit) # not normal
qqnorm(res_ppnHit) 
qqline(res_ppnHit) # does not show normality 
fitted_ppnHit <- fitted(model_ppnHit)
plot(fitted_ppnHit ~ res_ppnHit) 

# Data Transformation 
# Power Transformation (WORKS THE BEST!)
power_ppnHit_model <- lm((ppnHit^2) ~ treatment, data = my_data)
power_ppnHit_residuals <- residuals(power_ppnHit_model)
hist(power_ppnHit_residuals) # normal 
qqnorm(power_ppnHit_residuals)
qqline(power_ppnHit_residuals) # very normal 
power_fitted_ppnHit <- fitted(power_ppnHit_model)
plot(power_ppnHit_residuals ~ power_fitted_ppnHit) # not homogeneity 

# Reciprocal Transformation (Not normal)
reciprocal_ppnHit_model <- lm((1/ppnHit) ~ treatment, data = my_data)
reciprocal_ppnHit_residuals <- residuals(reciprocal_RT_model)
hist(reciprocal_ppnHit_residuals) # not normal
qqnorm(reciprocal_ppnHit_residuals)
qqline(reciprocal_ppnHit_residuals) # not normal
reciprocal_fitted_ppnHit <- fitted(reciprocal_ppnHit_model)
plot(reciprocal_ppnHit_residuals ~ reciprocal_fitted_ppnHit) # not homogeneity 

# SQRT Transformation (Not normal)
sqrt_ppnHit_model <- lm(sqrt(ppnHit) ~ treatment, data = my_data)
sqrt_ppnHit_residuals <- residuals(sqrt_ppnHit_model)
hist(sqrt_ppnHit_residuals) # looks semi normal
qqnorm(sqrt_ppnHit_residuals)
qqline(sqrt_ppnHit_residuals) # semi straight 
sqrt_fitted_ppnHit <- fitted(sqrt_ppnHit_model)
plot(sqrt_ppnHit_residuals ~ sqrt_fitted_ppnHit) # pretty homogeneity 

# Logging (Does not work) 
log_ppnHit_model <- lm(log(ppnHit) ~ treatment, data = my_data)
log_ppnHit_residuals <- residuals(log_ppnHit_model)
hist(log_ppnHit_residuals) # looks normal
qqnorm(log_ppnHit_residuals)
qqline(log_ppnHit_residuals) # ;ooks normal
log_fitted_ppnHit <- fitted(log_ppnHit_model)
plot(log_ppnHit_residuals ~ log_fitted_ppnHit)

# F-stat + df 
model_ppnHit_lm <-lm((ppnHit^2) ~ treatment, data = my_data)
summary(model_ppnHit_lm) # F = 148.7, 2 and 471 DF, P < 0.001

#Google (easier way to do this)
anova_model_ppnHit <- aov((ppnHit^2) ~ treatment, data = my_data)
summary(anova_model_ppnHit) # p < 0.001
TukeyHSD(aov(log(ppnHit) ~ treatment), data = my_data)
# mix - grass = P < 0.001
# Mud - grass = p < 0.001
# Mud - mix = p = 0.84 (there is no significant difference)


