# check working directory 
getwd()

# import data 
mammaldata <- read.csv("mversuspdata.csv", sep = ",", header = TRUE)

# make a basic pic chart 
pie(mammaldata$frequency)

# add labels 
mammallabels <- mammaldata$species
pie(mammaldata$frequency, labels = mammallabels)

# italicise the font 
pie(mammaldata$frequency, labels = mammallabels, font = 3)

# resize plot 
pie(mammaldata$frequency, labels = mammallabels, font = 3, radius = 1.05)

