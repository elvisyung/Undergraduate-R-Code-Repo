getwd()
setwd("~/Desktop/Bristol/year 2/Quant and Comp methods/Week 10 (report)")

install.packages("asnipe")
install.packages("sna")
install.packages("igraph")

library("asnipe")
library("igraph")

data <-read.csv('Report Data 2.csv') # load social network data in the report 
gbi <- get_group_by_individual(data,data_format="individuals") # transforming the group by individual matrix 

gbi2 <- gbi[,order(colnames(gbi))] # orders the individuals (column names) alphabetically. comparing 'gbi' and 'gbi2' to see the difference 
head(gbi2) # 1's means that the individual was in a particular group, 0 the indivudal was not in a particular group

# adjacency matrix - association index based on the Simple Ratio Index (SRI)
# measuring the proportion of time two animals spend together (0 pairs of animal never observed association, 1 pair are always seen together)

network_data <- get_network(gbi2, association_index="SRI", data_format="GBI")
# Contains the strength of each relationship - used to calculate undirected (rows and columns are the same)
# and weighted (edge thickness proportional to the association index) network 

# Association index is the number between two interaction measuring how strong a social bond is 

write.csv(network_data, "adjacency matrix.csv")

# Calculating social network metrics
detach(package:asnipe) # detach program to avoid problems later on 
library("sna")

# use network_data to calculate node-level social network metrics 
degree_metric <- degree(network_data, gmode ="graph", ignore.eval=TRUE) # values tells you how many connections/relationship each individual has

individuals <- colnames(network_data) # extracts the individual names from our adjacency matrix as the individual's names are column names

data_metrics_final <- data.frame(individuals,degree_metric) # combining degree metrics and individual name in one data frame 

# calculate strength centrality 
strength_metric <- degree(network_data,gmode="graph",ignore.eval=FALSE) # strength sums up all these values, want edge values

# Eigenvector centrality 
eigen_metric <- evcent(network_data,gmode="graph")

# Betweenness Centrality 
between_metric <- betweenness(network_data,gmode="graph")

# not using closeness as its not suitable for this dataset as individuals are already highly connected

data_metrics_final <- data.frame(individuals,degree_metric, strength_metric, eigen_metric, between_metric)
# combining strength centrality, eigenvector, and betweenness into one data_metrics_final

write.csv(data_metrics_final, "data metrics final.csv")

# visualizing social network # plotting!
library(igraph)

# creating igraph graphs from adjacency matrices 
net <- graph_from_adjacency_matrix(network_data,mode="undirected",weighted=TRUE)

plot.igraph(net,vertex.size=10, vertex.label.cex=0.5, edge.color="black")
# node size (10), size of the node label (0.5), edge color as black 
# change thickness to represent the values from the adjacency matrix net refers to the edge of the network
plot.igraph(net,vertex.size=10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)
# the lines then represent the relationship strength between individuals 

# Node Color
attributes <- read.csv('Individual attributes.csv')
# color code nodes by sex or tool-use by using attributes$sex or attributes$tool

plot.igraph(net,vertex.col=attributes$sex, vertex.size=10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

#V(net) = vertices for our network
V(net)[attributes$sex == "M"]$color <-"tomato2"
V(net)[attributes$sex =="F"]$color <- "lightblue2"
#Node Size
plot.igraph(net, vertex.col=attributes$sex, vertex.size= strength_metric, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

plot.igraph(net, vertex.col=attributes$sex, vertex.size= strength_metric*10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

data3<-data.frame(individuals, attributes$sex, attributes$tool, degree_metric,  strength_metric, eigen_metric, between_metric)

write.csv(data3, "data metrics final file.csv") 

# Tool Use Plots 
V(net)[attributes$tool == "S"]$color <-"tomato2"
V(net)[attributes$tool =="NS"]$color <- "lightblue2"
# Node color for tool use  
plot.igraph(net,vertex.col=attributes$tool, vertex.size=10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

# Sex node color plots
V(net)[attributes$sex == "F"]$color <-"tomato2"
V(net)[attributes$sex =="M"]$color <- "lightblue2"
 
plot.igraph(net,vertex.col=attributes$sex, vertex.size=10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

#Combined 
net2 <- graph_from_adjacency_matrix(network_data,mode="undirected",weighted=TRUE)
V(net2)[attributes$sex == "F"]$shape <-"circle"
V(net2)[attributes$sex == "M"]$shape <- "square"
V(net2)[attributes$tool == "NS"]$color <- "tomato2"
V(net2)[attributes$tool == "S"]$color <- "lightblue2"

# Visualize Social Network 
plot.igraph(net2,vertex.col=attributes$sex, vertex.shapes=attributes$tool, vertex.size=10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)
legend("topleft", legend=c("Female spongers", "Female non-spongers", "Male spongers", "Male non-spongers"),
  pch=c(21,21,22,22), pt.bg=c("lightblue2", "tomato2", "lightblue2", "tomato2"), pt.cex=2, bty="n")

# Node Size for tool use | not use 
plot.igraph(net, vertex.col=attributes$tool, vertex.size= strength_metric, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

plot.igraph(net, vertex.col=attributes$tool, vertex.size= strength_metric*10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*10)

#betweenness
plot.igraph(net2, vertex.col=attributes$tool, vertex.shapes=attributes$sex, vertex.size = between_metric*0.065, vertex.label.cex=0.55, edge.color="black", edge.width=E(net)$weight*10)
legend("topleft", legend=c("Female spongers", "Female non-spongers", "Male spongers", "Male non-spongers"),
       pch=c(21,21,22,22), pt.bg=c("lightblue2", "tomato2", "lightblue2", "tomato2"), pt.cex=2, bty="n")

# Degree
plot.igraph(net2, vertex.col=attributes$tool, vertex.shapes=attributes$sex, vertex.size= degree_metric*0.45, vertex.label.cex=0.31, edge.color="black", edge.width=E(net)$weight*10)
# NS = tomato, S = lightblue, F = circle, M = square 
legend("topleft", legend=c("Female spongers", "Female non-spongers", "Male spongers", "Male non-spongers"),
       pch=c(21,21,22,22), pt.bg=c("lightblue2", "tomato2", "lightblue2", "tomato2"), pt.cex=2, bty="n")

#strength
plot.igraph(net2, vertex.col=attributes$tool, vertex.shapes=attributes$sex, vertex.size= strength_metric*7, vertex.label.cex=0.31, edge.color="black", edge.width=E(net)$weight*10)
#NS = tomato, S = lightblue, F = circle, M = square
legend("topleft", legend=c("Female spongers", "Female non-spongers", "Male spongers", "Male non-spongers"),
       pch=c(21,21,22,22), pt.bg=c("lightblue2", "tomato2", "lightblue2", "tomato2"), pt.cex=2, bty="n")

# Eigenvector | not use 
plot.igraph(net, vertex.col=attributes$tool, vertex.size= eigen_metric*0.45, vertex.label.cex=0.20, edge.color="black", edge.width=E(net)$weight*10)

#---------------Report Analysis-----------------
# include at least one social network figure to help you explain how dolphin social 
# structure differs by sex and/or tool use 
report_data <- get_network(gbi2)

# also include one statistical analysis on two of the four node-level social network metrics 
# - what can you tell about the node-level social networks vary between dolphins 
# do tool-use and/or sex influence social network structure? 
# 4 node-level - degree, closeness, eigenvector and betweenness

#--------Individual Stat Testing for Report----Using Eigenvector and Degree--for NS and S---
# 1) Eigenvector Centrality - NS and S  # don't use 
eigen_model <- lm(eigen_metric ~ attributes$tool, data = data)
anova(eigen_model)# assumption are not met 
kruskal.test(eigen_metric ~ attributes$tool, data = data) # P < 0.05 DO NOT USE
wilcox.test(eigen_metric ~ attributes$tool, data = data) # P < 0.05
# ^ two sample t-test *GLM single categorical predictor (2 groups)
wilcox.test(eigen_metric ~ attributes$sex, data = data)

# Assumptions - Eigenvector NS and S
eigen_residuals <- residuals(eigen_model)
hist(eigen_residuals)
qqnorm(eigen_residuals)
qqline(eigen_residuals)

# Data transformation Eigenvector NS and S 

# 2) Degree Centrality - NS and S | Number of edges a node is connected to 
degree_model <- lm(degree_metric ~ attributes$tool, data = data)
kruskal.test(degree_metric ~ attributes$tool, data = data) # P < 0.05 DO NOT USE
wilcox.test(degree_metric ~ attributes$tool, data = data) # P < 0.05 2 groups 
wilcox.test(degree_metric ~ attributes$sex, data = data)

# Assumptions
degree_residuals <- residuals(degree_model)
hist(degree_residuals)
qqnorm(degree_residuals)
qqline(degree_residuals)

# 3) Strength Centrality | Sum of the weights of the edges connected to the node
wilcox.test(strength_metric ~ attributes$tool, data = data)
wilcox.test(strength_metric ~ attributes$sex, data = data)

# 4) Betweenness Centrality 
wilcox.test(between_metric ~ attributes$tool, data = data)
wilcox.test(between_metric ~ attributes$sex, data = data)

# assumptions are not met 
#--------------------------

