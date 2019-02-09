# Clearing the working environment 
rm(list = ls())
#install.packages("Momocs")
library(dplyr)
library(stats)
library(readr)
library(data.table)
library(sqldf)
library(radarchart)
library(tidyr)
library(dtplyr)
library(knitr)
library(DT)
library(ggplot2)
library(plotly)
library(rworldmap)
library(splitstackshape)
library(cluster)
library(rgl)
library(car)
#library(NbClust)
############################ Preprocessing ########################################
# Importing the dataset 
setwd("D:/Study/Semester 2/Data Mining 2/project")
dataset = read.csv("CompleteDataset.csv")
setDT(dataset) # Coercing the dataset 
newdata = as.data.frame(dataset[, c(2,3,5,7,8,9,11,12,14:52,54:75)])

data = cSplit(newdata, c("Value", "Wage"), "¬")
u_data = subset(data, select =  -c(Value_1, Wage_1, Wage_2))
names(u_data)
fifadata = cSplit(u_data, "Value_2", "M")
colnames(fifadata)[68] = "Market Value"

fifadata$Name <- as.factor(fifadata$Name)
fifadata$Nationality <- as.factor(fifadata$Nationality)
fifadata$Club <- as.factor(fifadata$Club)
fifadata$Preferred.Positions <- as.factor(fifadata$Preferred.Positions)


fifadata = cSplit(fifadata, "Preferred.Positions", " ")
fifadata <- subset(fifadata, select = -c(Preferred.Positions_2, Preferred.Positions_3, Preferred.Positions_4))


StrikerID <- c("ST","CF")
WingerID <- c("LW","RW","LM","RM")
CentralMidFielderID <- c("CM","CDM") 
CAMID <- "CAM"
WingBackID <- c("RB","LB","LWB","RWB")
CenterBackID <- ("CB")
GKID <- "GK"

fifadata <- mutate(fifadata, Position = ifelse(Preferred.Positions_1 %in% StrikerID, "Striker", ifelse(Preferred.Positions_1 %in% WingerID, "Winger", ifelse(Preferred.Positions_1 %in% CentralMidFielderID, "CentralMidfielder", ifelse(Preferred.Positions_1 %in% CAMID, "CAM", ifelse(Preferred.Positions_1 %in% WingBackID, "WingBack", ifelse(Preferred.Positions_1 %in% CenterBackID, "CenterBack", ifelse(Preferred.Positions_1 %in% GKID, "GoalKeeper", "0")))) ))))
fifa_striker <- subset(fifadata, fifadata$Position == "Striker")
fifa_striker_names  <- as.data.frame(fifa_striker[, c(1,2,4,5,7:40)])
fifa_striker <- as.data.frame(fifa_striker[, c(4,7:40)])

fifa_winger <- subset(fifadata, fifadata$Position == "Winger")
fifa_winger_names  <- as.data.frame(fifa_winger[, c(1,2,4,5,7:40)])
fifa_winger <- as.data.frame(fifa_winger[, c(4,7:40)])

fifa_CentralMidfielder <- subset(fifadata, fifadata$Position == "CentralMidfielder")
fifa_CentralMidfielder_names  <- as.data.frame(fifa_CentralMidfielder[, c(1,2,4,5,7:40)])
fifa_CentralMidfielder <- as.data.frame(fifa_CentralMidfielder[, c(4,7:40)])

fifa_CAM <- subset(fifadata, fifadata$Position == "CAM")
fifa_CAM_names  <- as.data.frame(fifa_CAM[, c(1,2,4,5,7:40)])
fifa_CAM <- as.data.frame(fifa_CAM[, c(4,7:40)])

fifa_WingBack <- subset(fifadata, fifadata$Position == "WingBack")
fifa_WingBack_names  <- as.data.frame(fifa_WingBack[, c(1,2,4,5,7:40)])
fifa_WingBack <- as.data.frame(fifa_WingBack[, c(4,7:40)])

fifa_CenterBack <- subset(fifadata, fifadata$Position == "CenterBack")
fifa_CenterBack_names <- as.data.frame(fifa_CenterBack[, c(1,2,4,5,7:40)])
fifa_CenterBack <- as.data.frame(fifa_CenterBack[, c(4,7:40)])

fifa_GoalKeeper <- subset(fifadata, fifadata$Position == "GoalKeeper")
fifa_GoalKeeper_names <- as.data.frame(fifa_GoalKeeper[, c(1,2,4,5,7:40)])
fifa_GoalKeeper <- as.data.frame(fifa_GoalKeeper[, c(4,7:40)])

set.seed(123)
# Kmedoids

#Striker
#pca_striker <- prcomp(as.matrix(as.data.frame(lapply(fifa_striker, as.numeric))))
#nb = NbClust(fifa_striker, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_striker <- pam(as.matrix(as.data.frame(lapply(fifa_striker, as.numeric))), 2)
#x11()
#plot3d(pca_striker$x[,1], pca_striker$x[,2], pca_striker$x[,3], surface = F, col = km_striker$cluster)
x11()
plot(km_striker)
overall_striker <- subset(fifa_striker_names, km_striker$cluster == 1)
dr_Strikers <- as.data.frame(overall_striker$Name[1:10])
                                

#Winger
pca_Winger <- prcomp(as.matrix(as.data.frame(lapply(fifa_Winger, as.numeric))))
#nb = NbClust(fifa_Winger, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_winger <- pam(as.matrix(as.data.frame(lapply(fifa_winger, as.numeric))), 4)
x11()
plot3d(pca_striker$x[,1], pca_striker$x[,2], pca_striker$x[,3], surface = F, col = km_striker$cluster)
x11()
plot(km_winger)
overall_winger <- subset(fifa_winger_names, km_winger$cluster == 1)
dr_winger <- as.data.frame(overall_winger$Name[1:10])


#Central Midfielder
pca_CentralMidfielder <- prcomp(as.matrix(as.data.frame(lapply(fifa_CentralMidfielder, as.numeric))))
#nb = NbClust(fifa_Winger, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_CentralMidfielder <- pam(as.matrix(as.data.frame(lapply(fifa_CentralMidfielder, as.numeric))), 4)
x11()
plot3d(pca_CentralMidfielder$x[,1], pca_CentralMidfielder$x[,2], pca_CentralMidfielder$x[,3], surface = F, col = km_CentralMidfielder$cluster)
x11()
plot(km_CentralMidfielder)
overall_CentralMidfielder <- subset(fifa_CentralMidfielder_names, km_CentralMidfielder$cluster == 1)
dr_CentralMidfielder <- as.data.frame(overall_CentralMidfielder$Name[1:10])

#CAM
pca_CAM <- prcomp(as.matrix(as.data.frame(lapply(fifa_CAM, as.numeric))))
#nb = NbClust(fifa_Winger, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_CAM <- pam(as.matrix(as.data.frame(lapply(fifa_CAM, as.numeric))), 4)
x11()
plot3d(pca_CAM$x[,1], pca_CAM$x[,2], pca_CAM$x[,3], surface = F, col = km_CAM$cluster)
x11()
plot(km_CAM)
overall_CAM <- subset(fifa_CAM_names, km_CAM$cluster == 1)
dr_CAM <- as.data.frame(overall_CAM$Name[1:10])


#WingBack
pca_WingBack <- prcomp(as.matrix(as.data.frame(lapply(fifa_WingBack, as.numeric))))
#nb = NbClust(fifa_Winger, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_WingBack <- pam(as.matrix(as.data.frame(lapply(fifa_WingBack, as.numeric))), 4)
x11()
plot3d(pca_WingBack$x[,1], pca_WingBack$x[,2], pca_WingBack$x[,3], surface = F, col = km_WingBack$cluster)
x11()
plot(km_WingBack)
overall_WingBack <- subset(fifa_WingBack_names, km_WingBack$cluster == 1)
dr_WingBack <- as.data.frame(overall_WingBack$Name[1:10])


#CenterBack
pca_CenterBack <- prcomp(as.matrix(as.data.frame(lapply(fifa_CenterBack, as.numeric))))
#nb = NbClust(fifa_Winger, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_CenterBack <- pam(as.matrix(as.data.frame(lapply(fifa_CenterBack, as.numeric))), 4)
x11()
plot3d(pca_CenterBack$x[,1], pca_CenterBack$x[,2], pca_CenterBack$x[,3], surface = F, col = km_CenterBack$cluster)
x11()
plot(km_CenterBack)
overall_CenterBack <- subset(fifa_CenterBack_names, km_CenterBack$cluster == 1)
dr_CenterBack <- as.data.frame(overall_CenterBack$Name[1:10])

#GoalKeeper
pca_GoalKeeper <- prcomp(as.matrix(as.data.frame(lapply(fifa_GoalKeeper, as.numeric))))
#nb = NbClust(fifa_Winger, distance = "euclidean", min.nc = 3,max.nc = 7, method = "kmeans")
km_GoalKeeper <- pam(as.matrix(as.data.frame(lapply(fifa_GoalKeeper, as.numeric))), 4)
x11()
plot3d(pca_GoalKeeper$x[,1], pca_GoalKeeper$x[,2], pca_GoalKeeper$x[,3], surface = F, col = km_GoalKeeper$cluster)
x11()
plot(km_GoalKeeper)
overall_GoalKeeper <- subset(fifa_GoalKeeper_names, km_GoalKeeper$cluster == 1)
dr_GoalKeeper <- as.data.frame(overall_GoalKeeper$Name[1:10])

############################################################ Using Multi Directional Scaling to Club similar players together #######################################################################################################

###################################################################################### Strikers ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_Striker_top20 <- fifa_striker_names[1:20,]
ex_matrix_st_20 <- as.data.frame(lapply(df_Striker_top20[,2:36], as.numeric))

df_Striker_youngPotential <- filter(fifa_striker_names, Age <= 20 & Potential >= 85)
ex_matrix_st_young <- as.data.frame(lapply(df_Striker_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_st <- as.data.frame(rbindlist(list(ex_matrix_st_20, ex_matrix_st_young), fill = TRUE))
df_Striker_new <- rbindlist(list(df_Striker_top20, df_Striker_youngPotential), fill = TRUE)

row.names(ex_matrix_st) <- df_Striker_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_st <- as.matrix(ex_matrix_st) %*% t(as.matrix(ex_matrix_st))

#Calculate Distance between Strikers
ex_dist_st <- dist(ex_mult_st)

# perform MDS
ex_mds_st <- as.data.frame(cmdscale(ex_dist_st))
names(ex_mds_st) <- c("x","y")
ex_mds_st$Name <- rownames(ex_mds_st)
ex_mds_st$Label <- "Pro"
ex_mds_st$Label[21:nrow(ex_mds_st)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_st, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_st$Name, size=2)) + ggtitle("Top 20 and Young Striker"))


###################################################################################### CM ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_CentralMidfielder_top20 <- fifa_CentralMidfielder_names[1:20,]
ex_matrix_CentralMidfielder_20 <- as.data.frame(lapply(df_CentralMidfielder_top20[,2:36], as.numeric))


df_CentralMidfielder_youngPotential <- filter(fifa_CentralMidfielder_names, Age <= 18 & Potential >= 80)
ex_matrix_CentralMidfielder_young <- as.data.frame(lapply(df_CentralMidfielder_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_CentralMidfielder <- as.data.frame(rbindlist(list(ex_matrix_CentralMidfielder_20, ex_matrix_CentralMidfielder_young), fill = TRUE))
df_CentralMidfielder_new <- rbindlist(list(df_CentralMidfielder_top20, df_CentralMidfielder_youngPotential), fill = TRUE)

row.names(ex_matrix_CentralMidfielder) <- df_CentralMidfielder_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_CentralMidfielder <- as.matrix(ex_matrix_CentralMidfielder) %*% t(as.matrix(ex_matrix_CentralMidfielder))

#Calculate Distance between Strikers
ex_dist_CentralMidfielder <- dist(ex_mult_CentralMidfielder)

# perform MDS
ex_mds_CentralMidfielder <- as.data.frame(cmdscale(ex_dist_CentralMidfielder))
names(ex_mds_CentralMidfielder) <- c("x","y")
ex_mds_CentralMidfielder$Name <- rownames(ex_mds_CentralMidfielder)
ex_mds_CentralMidfielder$Label <- "Pro"
ex_mds_CentralMidfielder$Label[21:nrow(ex_mds_CentralMidfielder)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_CentralMidfielder, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_CentralMidfielder$Name, size=2)) + ggtitle("Top 20 and Young CM"))

###################################################################################### Wingers ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_winger_top20 <- fifa_winger_names[1:20,]
ex_matrix_winger_20 <- as.data.frame(lapply(df_winger_top20[,2:36], as.numeric))


df_winger_youngPotential <- filter(fifa_winger_names, Age <= 20 & Age != 18 & Potential >= 85)
ex_matrix_winger_young <- as.data.frame(lapply(df_winger_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_winger <- as.data.frame(rbindlist(list(ex_matrix_winger_20, ex_matrix_winger_young), fill = TRUE))
df_winger_new <- rbindlist(list(df_winger_top20, df_winger_youngPotential), fill = TRUE)

row.names(ex_matrix_winger) <- df_winger_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_winger <- as.matrix(ex_matrix_winger) %*% t(as.matrix(ex_matrix_winger))

#Calculate Distance between Strikers
ex_dist_winger <- dist(ex_mult_winger)

# perform MDS
ex_mds_winger <- as.data.frame(cmdscale(ex_dist_winger))
names(ex_mds_winger) <- c("x","y")
ex_mds_winger$Name <- rownames(ex_mds_winger)
ex_mds_winger$Label <- "Pro"
ex_mds_winger$Label[21:nrow(ex_mds_winger)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_winger, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_winger$Name, size=2)) + ggtitle("Top 20 and Young Wingers"))


###################################################################################### CAM ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_CAM_top20 <- fifa_CAM_names[1:20,]
ex_matrix_CAM_20 <- as.data.frame(lapply(df_CAM_top20[,2:36], as.numeric))


df_CAM_youngPotential <- filter(fifa_CAM_names, Age <= 19 & Potential >= 85)
ex_matrix_CAM_young <- as.data.frame(lapply(df_CAM_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_CAM <- as.data.frame(rbindlist(list(ex_matrix_CAM_20, ex_matrix_CAM_young), fill = TRUE))
df_CAM_new <- rbindlist(list(df_CAM_top20, df_CAM_youngPotential), fill = TRUE)

row.names(ex_matrix_CAM) <- df_CAM_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_CAM <- as.matrix(ex_matrix_CAM) %*% t(as.matrix(ex_matrix_CAM))

#Calculate Distance between Strikers
ex_dist_CAM <- dist(ex_mult_CAM)

# perform MDS
ex_mds_CAM <- as.data.frame(cmdscale(ex_dist_CAM))
names(ex_mds_CAM) <- c("x","y")
ex_mds_CAM$Name <- rownames(ex_mds_CAM)
ex_mds_CAM$Label <- "Pro"
ex_mds_CAM$Label[21:nrow(ex_mds_CAM)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_CAM, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_CAM$Name, size=2)) + ggtitle("Top 20 and Young CAM"))

###################################################################################### WingBack ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_WingBack_top20 <- fifa_WingBack_names[1:20,]
ex_matrix_WingBack_20 <- as.data.frame(lapply(df_WingBack_top20[,2:36], as.numeric))


df_WingBack_youngPotential <- filter(fifa_WingBack_names, Age <= 20 & Potential >= 85)
ex_matrix_WingBack_young <- as.data.frame(lapply(df_WingBack_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_WingBack <- as.data.frame(rbindlist(list(ex_matrix_WingBack_20, ex_matrix_WingBack_young), fill = TRUE))
df_WingBack_new <- rbindlist(list(df_WingBack_top20, df_WingBack_youngPotential), fill = TRUE)

row.names(ex_matrix_WingBack) <- df_WingBack_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_WingBack <- as.matrix(ex_matrix_WingBack) %*% t(as.matrix(ex_matrix_WingBack))

#Calculate Distance between Strikers
ex_dist_WingBack <- dist(ex_mult_WingBack)

# perform MDS
ex_mds_WingBack <- as.data.frame(cmdscale(ex_dist_WingBack))
names(ex_mds_WingBack) <- c("x","y")
ex_mds_WingBack$Name <- rownames(ex_mds_WingBack)
ex_mds_WingBack$Label <- "Pro"
ex_mds_WingBack$Label[21:nrow(ex_mds_WingBack)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_WingBack, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_WingBack$Name, size=2)) + ggtitle("Top 20 and Young WingBack"))

###################################################################################### CB ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_CenterBack_top20 <- fifa_CenterBack_names[1:20,]
ex_matrix_CenterBack_20 <- as.data.frame(lapply(df_CenterBack_top20[,2:36], as.numeric))

df_CenterBack_youngPotential <- filter(fifa_CenterBack_names, Age <= 20 & Potential >= 85)
ex_matrix_CenterBack_young <- as.data.frame(lapply(df_CenterBack_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_CenterBack <- as.data.frame(rbindlist(list(ex_matrix_CenterBack_20, ex_matrix_CenterBack_young), fill = TRUE))
df_CenterBack_new <- rbindlist(list(df_CenterBack_top20, df_CenterBack_youngPotential), fill = TRUE)

row.names(ex_matrix_CenterBack) <- df_CenterBack_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_CenterBack <- as.matrix(ex_matrix_CenterBack) %*% t(as.matrix(ex_matrix_CenterBack))

#Calculate Distance between Strikers
ex_dist_CenterBack <- dist(ex_mult_CenterBack)

# perform MDS
ex_mds_CenterBack <- as.data.frame(cmdscale(ex_dist_CenterBack))
names(ex_mds_CenterBack) <- c("x","y")
ex_mds_CenterBack$Name <- rownames(ex_mds_CenterBack)
ex_mds_CenterBack$Label <- "Pro"
ex_mds_CenterBack$Label[21:nrow(ex_mds_CenterBack)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_CenterBack, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_CenterBack$Name, size=2)) + ggtitle("Top 20 and Young CenterBack"))

###################################################################################### GK ######################################################################################################################################

#Scale seperately and Create Matrix for top 20 Strikers and young players aged <20 and Ratings > 70
df_GoalKeeper_top20 <- fifa_GoalKeeper_names[1:20,]
ex_matrix_GoalKeeper_20 <- as.data.frame(lapply(df_GoalKeeper_top20[,2:36], as.numeric))

df_GoalKeeper_youngPotential <- filter(fifa_GoalKeeper_names, Age <= 20 & Potential >= 85)
ex_matrix_GoalKeeper_young <- as.data.frame(lapply(df_GoalKeeper_youngPotential[,2:36], as.numeric))

#Append Both Dataset
ex_matrix_GoalKeeper <- as.data.frame(rbindlist(list(ex_matrix_GoalKeeper_20, ex_matrix_GoalKeeper_young), fill = TRUE))
df_GoalKeeper_new <- rbindlist(list(df_GoalKeeper_top20, df_GoalKeeper_youngPotential), fill = TRUE)

row.names(ex_matrix_GoalKeeper) <- df_GoalKeeper_new$Name
#colnames(ex_matrix_st) <- names(df_Striker_new)[c(2:37)]

#Multiply the matrix with its transpose
ex_mult_GoalKeeper <- as.matrix(ex_matrix_GoalKeeper) %*% t(as.matrix(ex_matrix_GoalKeeper))

#Calculate Distance between Strikers
ex_dist_GoalKeeper <- dist(ex_mult_GoalKeeper)

# perform MDS
ex_mds_GoalKeeper <- as.data.frame(cmdscale(ex_dist_GoalKeeper))
names(ex_mds_GoalKeeper) <- c("x","y")
ex_mds_GoalKeeper$Name <- rownames(ex_mds_GoalKeeper)
ex_mds_GoalKeeper$Label <- "Pro"
ex_mds_GoalKeeper$Label[21:nrow(ex_mds_GoalKeeper)] <- "Young"
#Plot the strikers
base.110 <- ggplot(ex_mds_GoalKeeper, aes(x=x, y=y, fill = as.factor(Label)))+ theme_bw()
plot(base.110)
# print(base.110+geom_point(aes(shape = Label,alpha=0.75, size=2)))
print(base.110+geom_text(aes(color = Label, alpha=0.75, label=ex_mds_GoalKeeper$Name, size=2)) + ggtitle("Top 20 and Young GoalKeeper"))

