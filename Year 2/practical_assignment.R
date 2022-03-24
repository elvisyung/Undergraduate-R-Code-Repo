#get working directory and set to the correct working directory 
getwd() 

# import data into R {turn dataset into .csv file}
cherry_trees_two_sample <- read.csv("cherry_trees_two_samples.csv", header = TRUE, sep = ",")

# Plot graph girth vs height for better visualization and linear regression  
plot(cherry_trees_two_sample$Girth ~ cherry_trees_two_sample$Height, xlab = "Height", ylab = "Girth")
fit <- lm(Girth ~ Height, data = cherry_trees_two_sample)
abline(fit, col = "red")

# Plot graph boxplot between Samples 1 and 2 for Height
boxplot(cherry_trees_two_sample$Height ~ cherry_trees_two_sample$Sample, xlab = "Sample", ylab ="Height", col = c("Red", "Blue"))

# Plot graph boxplot bewteen Samples 1 and 2 for Girth
boxplot(cherry_trees_two_sample$Girth ~ cherry_trees_two_sample$Sample,  xlab = "Sample", ylab ="Girth", col = c("Red", "Blue"))

# Run useful statistical analysis on data set 

