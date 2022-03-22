# check working directory 
getwd()

# import data 
mammaldata <- read.csv("bio2mvp.csv", sep = ",", header = TRUE)

# make a basic pic chart 
pie(mammaldata$biomass)

# add labels 
mammallabels <- mammaldata$species
pie(mammaldata$biomass, labels = mammallabels)

# italicise the font 
pie(mammaldata$biomass, labels = mammallabels, font = 3)

# resize plot 
pie(mammaldata$biomass, labels = mammallabels, font = 3, radius = 1.05)
