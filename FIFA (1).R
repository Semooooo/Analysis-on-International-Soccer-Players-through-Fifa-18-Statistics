# Clearning the working environment 
rm(list = ls())

# Loading the libraries 
library(readr)
library(data.table)
library(sqldf)
library(dplyr)
library(radarchart)
library(tidyr)
library(dtplyr)
library(knitr)
library(DT)
library(ggplot2)
library(plotly)
library(rworldmap)

# Importing the dataset 

setwd("C:/Priyesh/fifa-18-demo-player-dataset")
dataset = read.csv("CompleteDataset.csv")
setDT(dataset) # Coercing the dataset 

# Exploratory Data Analysis

# Age density plot 
x11()
ggplot(dataset, aes(Age, fill = Age)) +
  geom_density(position = "stack")

# Best Club
TeamDF = arrange(dataset[, list(Avg=mean(Overall)), by= "Club" ], desc(Avg))
x11()
barplot(height = TeamDF$Avg[1:10], names.arg = TeamDF$Club[1:10], cex.names = 0.7,
        main = 'Top Clubs based on the average player ratings', xlab = "Clubs",ylab = "Average 
        Player Rating")

# Players by their overall points.
PlayerDF = arrange(dataset[, list(Avg=mean(Overall)), by= "Name" ], desc(Avg))
x11()
barplot(height = PlayerDF$Avg[1:10], names.arg = PlayerDF$Name[1:10], cex.names = 0.7,
        main = 'Top 10 Players based on their overall ratings', xlab = "Player",ylab = "Average 
        Player Rating")


# Age vs Ratings 
x11()
agerating <- dataset[Age<41,.("Overall"=mean(Overall)),by=Age][order(-Age)]
ggplot(data = agerating,aes(x=Age,y=Overall))+
  geom_line(color="blue",size=2)+labs(title="Player Ratings with Age")+
  annotate("text", x = 30, y = max(agerating$Overall),color="Black", label = "Maximum", 
           parse = TRUE, size = 5)

# Players from Countries 
pais <- dataset[,.N,by=Nationality] # number of players 

fr <- joinCountryData2Map(dF = pais,joinCode = "NAME",nameJoinColumn = "Nationality",verbose=F) # Prepare data to plot

x11()
mapCountryData(mapToPlot = fr,nameColumnToPlot = "N",catMethod = "fixedWidth",
               oceanCol = "skyblue",missingCountryCol = "white",
               mapTitle = "Number of Players per country",
               aspect = "variable", addLegend = TRUE) # Plot Worlmap

##     nationality    N
##  1:     England 1631
##  2:     Germany 1147
##  3:       Spain 1020
##  4:      France  966
##  5:   Argentina  962
##  6:      Brazil  806
##  7:       Italy  800
##  8:    Colombia  593
##  9:       Japan  471
## 10: Netherlands  430


# Reducing the size of the data by considering only the top 100 players (Due to computational constraints)

top_players_0 = dataset %>% subset(.,.$Overall >= 85) %>% select(2,14:63, 65:75)
top_players = dataset %>% subset(.,.$Overall >= 85) %>% select(14:63, 65:75)
rownames(top_players) = top_players_0$Name
fix(top_players)


# Coercing the dataset from type list to type float/numeric/matrix 

clust_data = as.matrix(as.data.frame(lapply(top_players, as.numeric))) # Gaand Fatli Ikde!! 
rownames(clust_data) = top_players_0$Name
fix(clust_data)
clust_data[is.na(clust_data) == T] = 0 # Making NA values = 0 for K-means 
scaled_data = scale(clust_data)

# # Finding the optimal number of clusters
# 
# library("NbClust")
# nb = NbClust(scaled_data, distance = "euclidean", min.nc = 2,
#              max.nc = 10, method = "kmeans")
# 
# # Visualizing the results of the NbClust output
# library("factoextra")
# x11()
# fviz_nbclust(nb) 

# Using the k-means algorithm on the dataset
km = kmeans(scaled_data, 3)
km$size # Cluster 1 = 13, Cluster 2 = 32, Cluster 3 = 47
table(row.names(top_players),km$cluster)

# Plotting the results 

library("cluster")

# Checking the players within the clusters  
x11()
clusplot(scaled_data, km$cluster, lines = 0, shade = T, color = T, labels = 2
         , main = "K-means clustering on FIFA 18 Player Statistics", cex = 0.75)

# Principal Component Analysis 
PCA = prcomp(scaled_data, center = T)
x11()
biplot(PCA, expand = 2, cex = 0.7, main = "Principal Component Analysis of the Players")
