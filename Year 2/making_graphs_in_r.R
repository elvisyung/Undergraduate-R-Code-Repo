getwd()

mammals_sleep <- read.csv("mammals_sleep.csv", header = TRUE, sep = ",")
plant_growth <- read.csv("plant_growth(2).csv", header = TRUE, sep = ",")

plot(log(mammals_sleep$body_wt_kg), log(mammals_sleep$brain_wt_kg), xlab = "Body weight (kg) on log scale", ylab = "Brain weight (kg) on log scale")
fit <- lm(log(brain_wt_kg) ~ log(body_wt_kg), data = mammals_sleep)
#find out R2
summary(fit)

abline(fit, col = "red")

head(plant_growth)

#subsetting allows us to extract a certain part of the data that we are interested in. In this example, we are interested in a particular group within a study - the control group.
control_group <- subset(plant_growth, plant_growth$group=="ctrl")
#If we wanted to subset the 2nd treatment group, we could use plant_growth$group=="trt2" and so on.

hist(control_group$weight, xlab = "Weight", ylab = "Frequency", main = "", col = "darkgrey")

#convert data to table
table(plant_growth$group)

#create a barplot
barplot(table(plant_growth$group), xlab = "Group", ylab = "Frequency", names = c("Control", "Treatment 1", "Treatment 2"))

# the factor function shows you the different levels of a categorical variable (i.e. different groups). The square brackets with a ,2 in represent all rows and the whole second column - if we wanted to look at just the first row and second column we would use 'factor(plant_growth [1,2])'.
factor_plant_growth <- factor(plant_growth [,2])
factor_plant_growth

# tapply is a useful function that allows us calculate multiple means or SDs at once - this is very useful and can save a lot of time!
tapply (plant_growth$weight, factor_plant_growth, mean)

tapply (plant_growth$weight, factor_plant_growth, sd)

tapply (plant_growth$weight, factor_plant_growth, median)

plant_growth_weight_barplot <- barplot (tapply(plant_growth$weight, factor_plant_growth, mean), xlab = "Group", ylab = "Weight", ylim = c(0,6), names = c("Control", "Treatment 1", "Treatment 2"))


plant_growth_weight_barplot <- barplot (tapply(plant_growth$weight, factor_plant_growth, mean), cex.axis = 1, ylim = c(0,6), col = c("grey","lightgrey", "darkgrey"), font = 2, font.lab = 2, cex = 1,  lwd = 3, axis.lty = 1, xlab = "Group", ylab = "Weight", cex.lab = 1, xpd = FALSE, names = c("Control", "Treatment 1", "Treatment 2"))
#ylim = sets the y limits
#cex = various sizes
#lty = line type
# col with 3 different colours adds a different colour to each experimental group

# this code below wraps a box around the bar chart
box (lwd = 3)

# The datasets package needs to be loaded to access our data 
# For a full list of these datasets, type library(help = "datasets")
library(datasets)
head(iris)
View(iris)

range(iris$Petal.Length)

tapply(iris$Petal.Length,factor_iris,mean)

factor_iris <- factor(iris [,5]) # all rows (blank before the comma; column 5 contains the species categorical data
factor_iris

iris_barplot <- barplot (tapply (iris$Petal.Length, factor_iris, mean), xlab = "Species", ylab ="Petal Length (cm)", ylim = c(0,6))


boxplot(weight ~ group, data = plant_growth, main = "", xlab = "Group", ylab = "Weight", col = c("darkred", "darkblue", "darkgreen"), names = c("Control", "Treatment 1", "Treatment 2"))

boxplot(iris$Sepal.Width ~ iris$Species, data = iris, main = "", xlab = "Species", ylab = "Sepal width (cm)", col = c("darkred", "darkblue", "darkgreen"))
