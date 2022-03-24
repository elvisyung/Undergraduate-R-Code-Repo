# Get working directory 
getwd()

#----------histogram + correlation test----------

# Opening data 
bird_data <- read.csv("bird_data (1).csv", header = TRUE, ",")

# Creating subest for single species 
great_tit <- subset(bird_data,species == "Great_tit")

#Coorelation Spearman Method 
cor.test(great_tit$daily_energy_expenditure, great_tit$time_spent_foraging, method="spearman")

#histogram 
hist(great_tit$daily_energy_expenditure, breaks = 2)
hist(great_tit$time_spent_foraging, breaks = 2)

# QQ plot
qqnorm(great_tit$daily_energy_expenditure)
qqnorm(great_tit$time_spent_foraging)

#Imaging -> gives same image in a row 
par(mfcol=c(2,2)) 
#Imaging -> gives same image in a column 
par(mfrow=c(2,2))

#Correlation Pearson's Method
cor.test(great_tit$daily_energy_expenditure, great_tit$time_spent_foraging, method="pearson")

#----------------Chi Square Test--------------

#Creating vectors
Observed_bills = c(1752, 1895)  
Expected_bills_proportions = c(0.5, 0.5) 

# Chisq test running 
chisq.test(x = Observed_bills, p = Expected_bills_proportions) 

#-----------Jordi Chi Square Test--------------

#Creating tables for New_genes and Old_genes
New_genes <- c(22,1,9)
Old_genes <- c(27,17,57)
Number_of_genes <- matrix(c(New_genes,Old_genes),ncol=3,byrow=TRUE)

#Chi-square test 
chisq.test(Number_of_genes)



