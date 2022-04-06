# ----- Setting up Directory ------- 
getwd()
setwd("~/Desktop/Bristol/year 2/Neuroethology/Lab 3 - sum 20%")

# -------- Notes ----------
# sma(y ~ x, slope.test=B) --> To test Isometry: B = 1, Enable us to test if the slope is significantly different to one
# ^ See if there are any evidence of non-isometric scaling 

# sma(y ~ x*groups) --> test evidence if there are any shifts in slopes between the two groups 
# ^ tries to display if there are evidence of distinct scaling parameters - groups could be brain structure, species
# is the yellow line (testing for slope) significantly steeper than the red line (what we are comparing it to) 

# sma(y  ~ x + groups, type = "elevation") > test if the intercept is significantly different between groups 
# Is there any evidence of non-allometric scaling > displays a grade shift (evolving independently) 
# type = "shift" looks at shifts at the common axis | elevation = y-intercept

# Testing hypotheses with SMARTR 
# Looking at structure volumes: 
# Do brain components scale isometrically or allometrical within species (subset data to look at teach species independently)
# Is there any evidence of non-allometric variation between species (evidence for difference between slopes )
# potential evidence of grade shift between factory bulb scaling and the rest of the brain where pigs have significantly larger olfactory bulb than sheep 
# olfactory bulb could be explained by the need for pigs and their incredible sense of smell 

# Testing White and Grey Matter: 
# Does white/grey matter scaling vary between brain components? 
# Testing this idea that white and grey matter scaling might vary between components and a steeper relationship between
# the neocortex and the cerebellum (explain this with extra reading on literature)

# ------ Reading Brain Strucutre Data ----------

Sheep <- subset(STRUCTURE_VOLS_FINAL, Species == "Sheep")
Pig <- subset(STRUCTURE_VOLS_FINAL, Species == "Pig")

# Looking at relationships ONLY in ONE SPECIES for the different brain structures (Sheep)
Slope <-sma(log10.OlfactoryBulb. ~ log10.RoB., data = Sheep) # Sheep Olfactory, simple slope function
summary(Slope)
plot(Slope)

Slope_isometryOl <- sma(log10.OlfactoryBulb. ~ log10.RoB., slope.test = 1, data = Sheep) # test if slope is significantly different from 1 
# deviation from isometrics and under isometrics the slope would be one 
summary(Slope_isometryOl) # the slope is significantly different from 1 - therefore the olfactory bulb does not scale with the rest of the brain 
plot(Slope_isometryOl, xlab = "log (rest of brain mass [g])", ylab = "log (olfactory bulb mass [g])")
text(x = 1.49, y = -0.90, "p < 0.001") # Done

Slope_isometryNeo <- sma(log10.Neocortex. ~ log10.RoB., slope.test = 1, data = Sheep)
summary(Slope_isometryNeo) # p < 0.05, therefore the neocortex deviates from 1 and is not isometrically scaled 
# potential scaling of the Neocortex to the rest of brain as it seems like the neorcortex is scaling with a shallower slope
# therefore implying that there is more of a liklihood that the neocortex scales isometrically with the rest of the brain 
plot(Slope_isometryNeo, xlab = "log (rest of brain mass [g])", ylab = "log (neocortex mass [g])", main = "Intraspecific Isometric Scaling Relationships in Sheep")
text(x = 1.50, y = 1.77, "p = 0.0302") # Done

Slope_isometryCere <- sma(log10.Cerebellum. ~ log10.RoB., slope.test = 1, data = Sheep)
summary(Slope_isometryCere)
plot(Slope_isometryCere, xlab = "log (rest of brain mass [g])", ylab = "log (cerebellum mass [g])")
text(x = 1.50, y = 1.0, "p = 0.2689") # Done

# Looking at relationships in different brain structures ONLY IN PIGS
Slope_isometryOl_pig <- sma(log10.OlfactoryBulb. ~ log10.RoB.,slope.test = 1, data = Pig)
summary(Slope_isometryOl_pig)
plot(Slope_isometryOl_pig, xlab = "log (rest of brain mass [g]", ylab = "log (olfactory bulb mass [g])")
text(x = 1.385, y = 0.07, "p < 0.001")

Slope_isometryNeo_pig <- sma(log10.Neocortex. ~ log10.RoB.,slope.test = 1, data = Pig)
summary(Slope_isometryNeo_pig)
plot(Slope_isometryNeo_pig, xlab = "log (rest of brain mass [g])", ylab = "(neocortex mass [g])", main = "Intraspecific Isometric Scaling Relationships in Pig")
text(x = 1.385, y = 1.74, "p = 0.6895")

Slope_isometryCere_pig <- sma(log10.Cerebellum. ~ log10.RoB.,slope.test = 1, data = Pig)
summary(Slope_isometryCere_pig)
plot(Slope_isometryCere_pig, xlab = "log (rest of brain mass [g])", ylab = "log (cerebellum mass [g])")
text(x = 1.385, y = 0.96, "p = 0.0667")

#Looking at slope differences between species | comparing slope scaling relationships 
Slope_differentNeo <- sma(log10.Neocortex. ~ log10.RoB.*Species, data = STRUCTURE_VOLS_FINAL) 
plot(Slope_differentNeo, xlab = "log (rest of brain mass [g])", ylab = "log (neocortex mass [g])") # * looks at differences in slopes between the two species
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 1.49, y = 1.75, 'p = 0.20')
summary(Slope_differentNeo) # slopes are equal and elevation are quite similar: displays 

Slope_differentCere <- sma(log10.Cerebellum. ~ log10.RoB.*Species, data = STRUCTURE_VOLS_FINAL)
plot(Slope_differentCere, xlab = "log (rest of brain mass [g])", ylab = "log (cerebellum mass [g])", main = "Interspecific Scaling Relationship - Slope")
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 1.49, y = 0.97, 'p = 0.4345')
summary(Slope_differentCere) # slopes are equal, therefore can test for elevation 

Slope_differentOl <- sma(log10.OlfactoryBulb. ~ log10.RoB.*Species,data = STRUCTURE_VOLS_FINAL)
plot(Slope_differentOl, xlab = "log (rest of brain mass [g])", ylab = "log (olfactory bulb mass [g])")
legend("bottom",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 1.49, y = -0.9, 'p = 0.3585')
summary(Slope_differentOl) # slopes are equal 

# Looking at elevation differences between species 
Elevation_differentNeo <- sma(log10.Neocortex. ~ log10.RoB.+Species, data = STRUCTURE_VOLS_FINAL) # * looks at elevation shifts/difference
# shouldn't be running this test if the slopes are not equal 
plot(Elevation_differentNeo,xlab = "log (rest of brain mass [g])", ylab = "log (neocortex mass [g])") # assume slopes are the same, but they have different elevation for difference testing 
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 1.49, y = 1.76, 'p = 0.2419')
summary(Elevation_differentNeo) # no difference in elevation 

Elevation_differentCere <- sma(log10.Cerebellum. ~ log10.RoB.+Species, data= STRUCTURE_VOLS_FINAL)
plot(Elevation_differentCere, xlab = "log (rest of brain mass [g])", ylab = "log (cerebellum mass [g])", main = "Interspecific Scaling Relationship - Elevation ")
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 1.49, y = 0.97, 'p = 0.7616')
summary(Elevation_differentCere) # no difference in elevation, no slope change 

Elevation_differentOl <- sma(log10.OlfactoryBulb. ~ log10.RoB.+Species, data = STRUCTURE_VOLS_FINAL)
plot(Elevation_differentOl, xlab = "log (rest of brain mass [g])", ylab = "log (olfactory bulb mass [g])") # difference in elevation displays a grade shift (evolve independently)
legend("bottom",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 1.49, y = -0.9, 'p < 0.001') # shows grade shift 
summary(Elevation_differentOl)

# ** ^ Above ALL DONE with GRAPHING ^ **

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (volume mm³)

# ---- Importing + Exploring Data on White and Gray Matter Scaling -------------

Sheep2 <- subset(WHITEGREY_VOLS_FINAL, Species == "sheep")
Pig2 <- subset(WHITEGREY_VOLS_FINAL, Species == "pig")

# Slope Testing for isometric scaling for Sheep (TESTS FOR ISOMETRIC)
Slope_isometryNeo_MSH <- sma(log10.NEO_WM. ~ log10.NEO_GM., slope.test = 1, data = Sheep2)
plot(Slope_isometryNeo_MSH, xlab = "log (neocortex grey matter volume mm³)", ylab = "log (neocortex white matter volume mm³)", main = "Neocortex W/G Matter Isometric Scaling - Sheep")
text(x = 4.6, y = 2.75, "p = 0.2617") # Done
summary(Slope_isometryNeo_MSH) # slope is similar to 1 

Slope_isometryCERE_MSH <- sma(log10.CEREB_WM. ~ log10.CEREB_GM., slope.test = 1, data = Sheep2)
plot(Slope_isometryCERE_MSH, xlab = "log (cerebellum grey matter volume mm³)", ylab = "log (cerebellum white matter volume mm³)", main = "Cerebellum W/G Matter Isometric Scaling - Sheep")
text(x = 3.9, y = 2.45, "p = 0.1146") 
summary(Slope_isometryCERE_MSH) # slope is similar to 1 

# Slope testing for isometry scaling for pig PIG
Slope_isometryNeo_MPIG <- sma(log10.NEO_WM. ~ log10.NEO_GM., slope.test = 1, data = Pig2)
plot(Slope_isometryNeo_MPIG, xlab = "log (neocortex grey matter volume mm³)", ylab = "log (neocortex white matter volume mm³)", main = "Neocortex W/G Matter Isometric Scaling - Pig")
text(x = 4.6, y = 3.0, "p = 0.0747") 
summary(Slope_isometryNeo_MPIG) # slope is similar to 1 

Slope_isometryCERE_MPIG <- sma(log10.CEREB_WM. ~ log10.CEREB_GM., slope.test = 1, data = Pig2)
plot(Slope_isometryCERE_MPIG, xlab = "log (cerebellum grey matter volume mm³)", ylab = "log (cerebellum white matter volume mm³)", main = "Cerebellum W/G Matter Isometric Scaling - Pig")
text(x = 3.75, y = 2.75, "p = 0.0112") 
summary(Slope_isometryCERE_MPIG) # slope is not similar to 1 (hypoallometric since slope is 0.714)

# Looking at slope differences between species 
Slope_differentNEO_M <- sma(log10.NEO_WM. ~ log10.NEO_GM.*Species, data = WHITEGREY_VOLS_FINAL)
plot(Slope_differentNEO_M, xlab = "log (neocortex grey matter volume mm³)", ylab = "log (neocortex white matter volume mm³)", main = "Interspecies Neocortex W/G Matter Slope Comparison")
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 4.6, y = 2.75, 'p = 0.7205') # test for elevation
summary(Slope_differentNEO_M) # slopes are equal to each other, therefore can test for eval

Slope_differentCEREB_M <- sma(log10.CEREB_WM. ~ log10.CEREB_GM.*Species, data = WHITEGREY_VOLS_FINAL)
plot(Slope_differentCEREB_M, xlab = "log (cerebellum grey matter volume mm³)", ylab = "log (cerebellum white matter volume mm³)", main = "Interspecies Cerebellum W/G Matter Slope Comparison")
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 3.9, y = 2.5, 'p = 0.0031') # shows slope shift
summary(Slope_differentCEREB_M) # Slope are not equal - slope shift - plot indicates lines overlap 
# Pig - hypo | Sheep - hyper 

# Looking at elevation differences 
Elevation_differentNEO_M <- sma(log10.NEO_WM. ~ log10.NEO_GM.+Species, data = WHITEGREY_VOLS_FINAL)
plot(Elevation_differentNEO_M, xlab = "log (neocortex grey matter volume mm³)", ylab = "log (neocortex white matter volume mm³)", main = "Interspecies Neocortex W/G Matter Elevation Comparison")
legend("topleft",legend = c("Sheep", "Pig"), bty ="n", pch = c(1,1), col = c(7, 4), pt.cex = 2, inset = c(0.1,0.1))
text(x = 4.6, y = 2.8, 'p = 0.0251') # shows grade shift
summary(Elevation_differentNEO_M) # there are differences in the elevation - grade shift 

# ------ Fin -------
# ----- Testing ---- 
library(ggplot2)

library(gridExtra)
library(grid)
library(lattice)

summary(Slope_differentNEO_M)
cc <- data.frame(site=1:2, coef(Slope_differentNEO_M))
qplot(log10.NEO_GM., log10.NEO_WM.,data=WHITEGREY_VOLS_FINAL) + 
  facet_wrap(~site, ncol=2) + 
  geom_abline(data=cc, aes(intercept=elevation, slope=slope))

par(mfrow=c(2,2))
#-------------
mat <- matrix(c(1, 2,  # First, second
                3, 3), # and third plot
              nrow = 2, ncol = 2,
              byrow = TRUE)
layout(mat = mat)


