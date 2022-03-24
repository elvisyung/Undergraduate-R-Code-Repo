#==========================Part 1============================
?runif
x_random <- runif(40, min = 0, max = 200) # Creates random points between 0 to 200 
y_random <- runif(40,min = 0, max = 100) # Creates random points bewteen 0 to 100 
# 40 = n, the observed samples
plot(y_random~x_random,asp=1)

?seq
x_random <- seq(from=0, to=200, by=20) # go by increments of 20 from 0 to 200
y_random <- seq(from=0, to=100, by=20)# go by increments of 20 from 0 to 100
plot(y_random~x_random,asp=1) # error cuz x-axis are not lined up 

Grid <- expand.grid(y_random,x_random) # creates a grid that expands the x-axis

plot(Grid$Var1~Grid$Var2,asp=1) # plots the grid points 

nrow(Grid)

Noise1 <- runif(66, min = -1, max = 1) # generates 66 random numbers from -1 to 1
Var1_noise <- Grid$Var1 + Noise1 # Stores Noise1 into var1_noise with Grid$Var1
Noise2 <- runif(66, min = -1, max = 1) # generates 66 random numbers from -1 to 1
Var2_noise <- Grid$Var2 + Noise2 # Stores Noise2 into var1_noise with Grid$Var2
plot(Var1_noise ~ Var2_noise, asp=1) # Plots Var1_noise against Var2_noise

Noise1 <- runif(66, min = -3, max = 3) # generates 66 random numbers from -3 to 3
Var1_noise <- Grid$Var1 + Noise1 # Stores Noise1 into var1_noise with Grid$Var1
Noise2 <- runif(66, min = -3, max = 3) # generates 66 random numbers from -3 to 3
Var2_noise <- Grid$Var2 + Noise2 # Stores Noise2 into var1_noise with Grid$Var2
plot(Var1_noise ~ Var2_noise, asp=1) # Plots Var1_noise against Var2_noise

Noise1 <- runif(66, min = -6, max = 6) # generates 66 random numbers from -6 to 6
Var1_noise <- Grid$Var1 + Noise1 # Stores Noise1 into var1_noise with Grid$Var1
Noise2 <- runif(66, min = -6, max = 6) # generates 66 random numbers from -6 to 6
Var2_noise <- Grid$Var2 + Noise2 # Stores Noise2 into var1_noise with Grid$Var2
plot(Var1_noise ~ Var2_noise, asp=1) # Plots Var1_noise against Var2_noise

#==========================Part 2============================
# Treatments - different concentration of CO2 in tanks of clown fish 
# Treatment needs to be assigned to 20 tanks that house the fish, labelled 1 to 20
Researchers <- c("Claire","Jane","Innes","Kerry") # Converting strings to numbers 
Researchers <- rep(Researchers, 5)
Researchers <- sample(Researchers, replace=F)

CO2 <- c(390, 600, 700, 900)
CO2 <- rep(CO2 ,5) # Repeat sequence of treatment 5 times, 20 tanks assign 5 tanks to each treatment
CO2 <- sample(CO2,replace=T) # Takes random samples of CO2, 
# replace=F ~ number of samples is the same as the number of values in the vector, 
# randomly shuffles the values
# Replace=T randomly shuffles the values instead of keeping it uniform 
# Replace= F randomimises the order of the vector 
table(CO2) # Displays the number of treatments present in CO2
plot(CO2~c(1:20)) # check if there are patterns, treatment ~ tank number, 

# Example 2, trees 
Altitude <- c(0, 500, 700, 1000, 1500, 2000)
Altitude <- rep(Altitude, 50)
Altitude <- sample(Altitude, replace=T)

table(Altitude)
plot(Altitude~c(1:300))

# Example 3 
Altitude <- c(0, 500, 700, 1000, 1500, 2000)
Altitude <- sample(Altitude,replace=F) # Randomizes the order in this vector 

for(i in 1:50) sample(Altitude,replace=F) # looping a function 50 times for 50 samples per altitude

for(i in 1:50) print(sample(Altitude,replace=F))

Altitude_orders <- numeric()
for(i in 1:50) Altitude_orders <- c(Altitude_orders,sample(Altitude,replace=F)) # Loops function 6 times
# includes latest reordering from sample(Altitude,replace=F)
plot(Altitude_orders~c(1:300))

Altitude_orders <- c(Altitude_orders,sample(Altitude,replace=F))
Altitude_orders
write.csv(Altitude_orders,"Altitude_orders.csv",row.names=F) # saves csv in wd



