## Read in the data, and provide overview of the variables

cars_survey.df <-read.csv('cars_survey_clustering.csv') 
str(cars_survey.df)


## Overview of the essentials
summary(cars_survey.df)


## Question1: How many segments can we distinguish in the market??

## Step1: "standardize" variables
cars_standardize.df <- cars_survey.df
cars_standardize.df[,3:12] <- data.frame(scale(cars_survey.df[,3:12]))

summary(cars_standardize.df)


# Step2: Calculates Euclidean distance with "dist function"

d <- dist(cars_standardize.df[,c(7:12)])     
as.matrix(d)
as.matrix(d)[1:5, 1:5]

# hclust() + Preview data
library(cluster)  

cars_standardize.dist <- daisy(cars_standardize.df[,c(7:12)])
as.matrix(cars_standardize.dist)[1:5, 1:5]

# Step3: Apply different clustering methods
# (3.1) the agglomeration schedules and the dendograms

cars_standardize1.hc <- hclust(cars_standardize.dist, method="single")
plot(cars_standardize1.hc)
cars_agglo1 <- cbind(as.data.frame(cars_standardize1.hc [1]),as.data.frame(cars_standardize1.hc[2]))
cars_agglo1      


cars_standardize2.hc <- hclust(cars_standardize.dist, method="complete")
plot(cars_standardize2.hc)
cars_agglo2 <- cbind(as.data.frame(cars_standardize2.hc [1]),as.data.frame(cars_standardize2.hc[2]))
cars_agglo2      


cars_standardize3.hc <- hclust(cars_standardize.dist, method="average")
plot(cars_standardize3.hc)
cars_agglo3 <- cbind(as.data.frame(cars_standardize3.hc [1]),as.data.frame(cars_standardize3.hc[2]))
cars_agglo3  


cars_standardize4.hc <- hclust(cars_standardize.dist, method="centroid")
plot(cars_standardize4.hc)
cars_agglo4 <- cbind(as.data.frame(cars_standardize4.hc [1]),as.data.frame(cars_standardize4.hc[2]))
cars_agglo4  


cars_standardize5.hc <- hclust(cars_standardize.dist, method="ward.D2")
plot(cars_standardize5.hc)
cars_agglo5 <- cbind(as.data.frame(cars_standardize5.hc [1]),as.data.frame(cars_standardize5.hc[2]))
cars_agglo5
tail(cars_agglo5, 10)

# (3.2) Construct the scree plots for the different solutions

cars_standardize_scree1 <- sort(cars_agglo1[,c(3)], decreasing = TRUE)
plot(cars_standardize_scree1, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,420,by = 10))

cars_standardize_scree2 <- sort(cars_agglo2[,c(3)], decreasing = TRUE)
plot(cars_standardize_scree2, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,420,by = 10))

cars_standardize_scree3 <- sort(cars_agglo3[,c(3)], decreasing = TRUE)
plot(cars_standardize_scree3, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,420,by = 10))

cars_standardize_scree4 <- sort(cars_agglo4[,c(3)], decreasing = TRUE)
plot(cars_standardize_scree4, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,420,by = 10))

cars_standardize_scree5 <- sort(cars_agglo5[,c(3)], decreasing = TRUE)
plot(cars_standardize_scree5, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,420,by = 1), )

install.packages("zoom") 
library("zoom")
zm()

# (3.3) Method1: WARD method 

# see hclust's proposal for 5 groups
plot(cars_standardize5.hc)
rect.hclust(cars_standardize5.hc, k=5, border="red")
                 
# see hclust's proposal for 4 groups
plot(cars_standardize5.hc)
rect.hclust(cars_standardize5.hc, k=4, border="red")

# see hclust's proposal for 3 groups
plot(cars_standardize5.hc)
rect.hclust(cars_standardize5.hc, k=3, border="red")

# see hclust's proposal for 2 groups
plot(cars_standardize5.hc)
rect.hclust(cars_standardize5.hc, k=2, border="red")


## (3.3.1) Test for significance with different groups

## 5 groups
cars_standardize5.hc.segment5 <- cutree(cars_standardize5.hc, k=5)     
table(cars_standardize5.hc.segment5)

# what did hclust come up with? Use the standardized variables!
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5.hc.segment5)

# (1) Test for significance: mileage
clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize5.aov <- aov(mileage ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_standardize5.aov)

# (2) Test for significance: power 
clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize5.aov <- aov(power ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_standardize5.aov)

# (3) Test for significance: design
clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize5.aov <- aov(design ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_standardize5.aov)

# (4) Test for significance: comfort
clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize5.aov <- aov(comfort ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_standardize5.aov)

# (5) Test for significance: entertainment
clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize5.aov <- aov(entertainment ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_standardize5.aov)

# (6) Test for significance: environment
clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize5.aov <- aov(environment ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_standardize5.aov)




## 4 groups
cars_standardize5.hc.segment4 <- cutree(cars_standardize5.hc, k=4)     
table(cars_standardize5.hc.segment4)

# what did hclust come up with? Use the standardized variables!
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5.hc.segment4)

# (1) Test for significance: mileage
clusmember4 <- as.factor(cars_standardize5.hc.segment4)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize4.aov <- aov(mileage ~ clusmember4, data = cars_standardize_aov)
summary(cars_standardize4.aov)
TukeyHSD(cars_standardize4.aov)

# (2) Test for significance: power 
clusmember4 <- as.factor(cars_standardize5.hc.segment4)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize4.aov <- aov(power ~ clusmember4, data = cars_standardize_aov)
summary(cars_standardize4.aov)
TukeyHSD(cars_standardize4.aov)

# (3) Test for significance: design
clusmember4 <- as.factor(cars_standardize5.hc.segment4)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize4.aov <- aov(design ~ clusmember4, data = cars_standardize_aov)
summary(cars_standardize4.aov)
TukeyHSD(cars_standardize4.aov)

# (4) Test for significance: comfort
clusmember4 <- as.factor(cars_standardize5.hc.segment4)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize4.aov <- aov(comfort ~ clusmember4, data = cars_standardize_aov)
summary(cars_standardize4.aov)
TukeyHSD(cars_standardize4.aov)

# (5) Test for significance: entertainment
clusmember4 <- as.factor(cars_standardize5.hc.segment4)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize4.aov <- aov(entertainment ~ clusmember4, data = cars_standardize_aov)
summary(cars_standardize4.aov)
TukeyHSD(cars_standardize4.aov)


# (6) Test for significance: environment
clusmember4 <- as.factor(cars_standardize5.hc.segment4)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize4.aov <- aov(environment ~ clusmember4, data = cars_standardize_aov)
summary(cars_standardize4.aov)
TukeyHSD(cars_standardize4.aov)


## 3 groups 
cars_standardize5.hc.segment3 <- cutree(cars_standardize5.hc, k=3)     
table(cars_standardize5.hc.segment3)

# what did hclust come up with? Use the standardized variables!
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5.hc.segment3)

# (1) Test for significance: mileage
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(mileage ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_standardize3.aov)

# (2) Test for significance: power 
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(power ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_standardize3.aov)

# (3) Test for significance: design
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(design ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_standardize3.aov)

# (4) Test for significance: comfort
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(comfort ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_standardize3.aov)

# (5) Test for significance: entertainment
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(entertainment ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_standardize3.aov)


# (6) Test for significance: environment
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(environment ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_standardize3.aov)



# what did hclust come up with? Use the standardized variables!
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5.hc.segment3)

# Test for significance: mileage
clusmember3 <- as.factor(cars_standardize5.hc.segment3)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])
cars_standardize3.aov <- aov(mileage ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)



## compare with "elbow" method (choose 3 groups = knee of elbow)
## Ref: https://uc-r.github.io/kmeans_clustering#optimal

install.packages('factoextra')
library("factoextra")

set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cars_survey.df, k, nstart = 10 )$tot.withinss
}

fviz_nbclust(cars_survey.df[,7:12], FUN = hcut, method = "wss")

# plot this for the 5-segment solution ????
cars_standardize5_plot5.hc.segment <- cutree(cars_standardize5.hc, k=5)     
cars_standardize5_plot5.hc.means <- seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5_plot5.hc.segment)

plot(jitter(cars_standardize.df$mileage) ~ jitter(cars_standardize.df$design), 
     col=cars_standardize5_plot5.hc.segment, yaxt="n", xaxt="n", ylab="Mileage", xlab="Design", xlim = c(-4,4), ylim = c(-4,4), lwd = 10)
axis(1, at=c(-4,-2,0,2,4))
axis(2, at=c(-4,-2,0,2,4))


# plot this for the 4-segment solution ????
cars_standardize5_plot4.hc.segment <- cutree(cars_standardize5.hc, k=4)     
cars_standardize5_plot4.hc.means <- seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5_plot4.hc.segment)

plot(jitter(cars_standardize.df$mileage) ~ jitter(cars_standardize.df$design), 
     col=cars_standardize5_plot4.hc.segment, yaxt="n", xaxt="n", ylab="Mileage", xlab="Design", xlim = c(-4,4), ylim = c(-4,4), lwd = 10)
axis(1, at=c(-4,-2,0,2,4))
axis(2, at=c(-4,-2,0,2,4))

# plot this for the 3-segment solution ????
cars_standardize5_plot3.hc.segment <- cutree(cars_standardize5.hc, k=3)     
cars_standardize5_plot3.hc.means <- seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5_plot3.hc.segment)

plot(jitter(cars_standardize.df$mileage) ~ jitter(cars_standardize.df$design), 
     col=cars_standardize5_plot3.hc.segment, yaxt="n", xaxt="n", ylab="Mileage", xlab="Design", xlim = c(-4,4), ylim = c(-4,4), lwd = 10)
axis(1, at=c(-4,-2,0,2,4))
axis(2, at=c(-4,-2,0,2,4))


## (3.4) Method2: K-MEANS (only K-Means)
# Do it with a random seed
library(RColorBrewer)

## 5-cluster solution ##
set.seed(96743)
cars_standardize.k5 <- kmeans(cars_standardize.df[,c(7:12)], centers=5)
# inspect it
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k5$cluster)
# plot one of the variables
boxplot(cars_standardize.df$mileage ~ cars_standardize.k5$cluster, ylab="Mileage", xlab="Cluster")
# plot the result
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.k5$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="K-means cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))


## 4-cluster solution ##
set.seed(96743)
cars_standardize.k4 <- kmeans(cars_standardize.df[,c(7:12)], centers=4)
# inspect it
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k4$cluster)
# plot one of the variables
boxplot(cars_standardize.df$mileage ~ cars_standardize.k4$cluster, ylab="Mileage", xlab="Cluster")
# plot the result
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.k4$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="K-means cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))


## 3-cluster solution ##
set.seed(96743)
cars_standardize.k3 <- kmeans(cars_standardize.df[,c(7:12)], centers=3)
# inspect it
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k3$cluster)
# plot one of the variables
boxplot(cars_standardize.df$mileage ~ cars_standardize.k3$cluster, ylab="Mileage", xlab="Cluster")
# plot the result
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.k3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="K-means cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))



## (3.5) K-MEANS + Hierarchical Cluster (as start point)

## Do "5 clusters"
kmeanstart5 <- cars_standardize5_plot5.hc.means[,c(2:7)]
cars_standardize.k5 <- kmeans(cars_standardize.df[,c(7:12)],kmeanstart5)

# inspect it
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k5$cluster)

# (1) plot one of the variables (mileage)
boxplot(cars_standardize.df$mileage ~ cars_standardize.k5$cluster, ylab="mileage", xlab="Cluster")

# (2) plot one of the variables (power)
boxplot(cars_standardize.df$power ~ cars_standardize.k5$cluster, ylab="power", xlab="Cluster")

# (3) plot one of the variables (design)
boxplot(cars_standardize.df$design ~ cars_standardize.k5$cluster, ylab="design", xlab="Cluster")

# (4) plot one of the variables (comfort)
boxplot(cars_standardize.df$comfort ~ cars_standardize.k5$cluster, ylab="comfort", xlab="Cluster")

# (5) plot one of the variables (entertainment)
boxplot(cars_standardize.df$entertainment ~ cars_standardize.k5$cluster, ylab="entertainment", xlab="Cluster")

# (6) plot one of the variables (environment)
boxplot(cars_standardize.df$environment ~ cars_standardize.k5$cluster, ylab="environment", xlab="Cluster")

# plot the result
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.k5$cluster, color=TRUE, shade=TRUE, 
         label


## Do "4 clusters"
kmeanstart4 <- cars_standardize5_plot4.hc.means[,c(2:7)]
cars_standardize.k4 <- kmeans(cars_standardize.df[,c(7:12)],kmeanstart4)

# inspect it
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k4$cluster)

# (1) plot one of the variables (mileage)
boxplot(cars_standardize.df$mileage ~ cars_standardize.k4$cluster, ylab="mileage", xlab="Cluster")

# (2) plot one of the variables (power)
boxplot(cars_standardize.df$power ~ cars_standardize.k4$cluster, ylab="power", xlab="Cluster")

# (3) plot one of the variables (design)
boxplot(cars_standardize.df$design ~ cars_standardize.k4$cluster, ylab="design", xlab="Cluster")

# (4) plot one of the variables (comfort)
boxplot(cars_standardize.df$comfort ~ cars_standardize.k4$cluster, ylab="comfort", xlab="Cluster")

# (5) plot one of the variables (entertainment)
boxplot(cars_standardize.df$entertainment ~ cars_standardize.k4$cluster, ylab="entertainment", xlab="Cluster")

# (6) plot one of the variables (environment)
boxplot(cars_standardize.df$environment ~ cars_standardize.k4$cluster, ylab="environment", xlab="Cluster")


# plot the result
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.k4$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="K-means cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))



## Do "3 clusters" 
kmeanstart3 <- cars_standardize5_plot3.hc.means[,c(2:7)]
cars_standardize.k3 <- kmeans(cars_standardize.df[,c(7:12)],kmeanstart3)

# inspect it
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k3$cluster)
eg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k3$cluster)



# (1) plot one of the variables (mileage)
boxplot(cars_standardize.df$mileage ~ cars_standardize.k3$cluster, ylab="mileage", xlab="Cluster")

# (2) plot one of the variables (power)
boxplot(cars_standardize.df$power ~ cars_standardize.k3$cluster, ylab="power", xlab="Cluster")

# (3) plot one of the variables (design)
boxplot(cars_standardize.df$design ~ cars_standardize.k3$cluster, ylab="design", xlab="Cluster")

# (4) plot one of the variables (comfort)
boxplot(cars_standardize.df$comfort ~ cars_standardize.k3$cluster, ylab="comfort", xlab="Cluster")

# (5) plot one of the variables (entertainment)
boxplot(cars_standardize.df$entertainment ~ cars_standardize.k3$cluster, ylab="entertainment", xlab="Cluster")

# (6) plot one of the variables (environment)
boxplot(cars_standardize.df$environment ~ cars_standardize.k3$cluster, ylab="environment", xlab="Cluster")


# plot the result
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.k3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="K-means cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))



## (3.6) Method3: model-based clustering

# do mclust for segments
install.packages('mclust')
library('mclust')


cars_standardize.mod <- densityMclust(cars_standardize.df[,c(7:12)])
summary(cars_standardize.mod)

plot(cars_standardize.mod, what = "density", type = "persp", col=brewer.pal(10,"YlOrRd"))

# check all model solutions up to 9 clusters
mclustBIC(cars_standardize.df[,c(7:12)])

# what if we estimate 5 clusters?
cars_standardize.mc5 <- Mclust(cars_standardize.df[,c(7:12)], G=5, modelName = "EEV")
summary(cars_standardize.mc5)

# what if we estimate 4 clusters?
cars_standardize.mc4 <- Mclust(cars_standardize.df[,c(7:12)], G=4, modelName = "EEV")
summary(cars_standardize.mc4)

# what if we estimate 7clusters?
cars_standardize.mc7 <- Mclust(cars_standardize.df[,c(7:12)], G=7, modelName = "VEE")
summary(cars_standardize.mc7)


# compare the models
BIC(cars_standardize.mc5, cars_standardize.mc4)


seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

# examine the 5-cluster model
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.mc5$class)


# examine the 4-cluster model
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.mc4$classification)


# plot the 5-cluster model
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.mc5$class, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="Model-based cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))


# plot the 4-cluster model
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.mc4$class, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="Model-based cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))

# plot the 7-cluster model
library(cluster)
clusplot(cars_standardize.df[,c(7:12)], cars_standardize.mc7$class, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main="Model-based cluster plot", lwd = 5, col.p = "black", col.clus = brewer.pal(4,"YlOrRd"))



## Question2: Are these segments significantly different from each others??
## Test for the differences by means of ANOVA
## Differences between clusters > within cluster (want to reject H0)

## with 5 cluster
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize5.hc.segment5)

clusmember5 <- as.factor(cars_standardize5.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])

## Test for significance of "Active variables"
# Active Var1_ Mileage (result = significance/ p < 0.05)
cars_mileage5.aov <- aov(mileage ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_mileage5.aov) 
#Result: group1 diff but group2&3 not that diff


# Active Var2_ Power (result = significance/ p < 0.05)
cars_power5.aov <- aov(power ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_power5.aov) 

# Active Var3_ Design (result = significance/ p < 0.05)
cars_design5.aov <- aov(design ~ clusmembe5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_design5.aov) 

# Active Var4_ Comfort (result = significance/ p < 0.05)
cars_comfort5.aov <- aov(comfort ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_comfort5.aov) 

# Active Var5_ Entertainment (result = significance/ p < 0.05)
cars_entertainment5.aov <- aov(entertainment ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_entertainment5.aov) 

# Active Var6_ Environment (result = significance/ p < 0.05)
cars_environment5.aov <- aov(environment ~ clusmember5, data = cars_standardize_aov)
summary(cars_standardize5.aov)
TukeyHSD(cars_environment5.aov) 


## Test for significance of "Passive variables" 
cars_standardize_aov2 <- cbind(cars_standardize.df[,c(3:6)])

# Passive Var1_ Gender (result = significance/ p < 0.05)
cars_gender5.aov <- aov(gender ~ clusmember5, data = cars_standardize_aov2)
summary(cars_standardize5.aov)
TukeyHSD(cars_gender5.aov) 

# Passive Var2_ Age (result = significance/ p < 0.05)
cars_age5.aov <- aov(age ~ clusmember5, data = cars_standardize_aov2)
summary(cars_standardize5.aov)
TukeyHSD(cars_age5.aov)

# Passive Var3_ Education (result = significance/ p < 0.05)
cars_education5.aov <- aov(education ~ clusmember5, data = cars_standardize_aov2)
summary(cars_standardize5.aov)
TukeyHSD(cars_education5.aov)

# Passive Var4_ Area (result = significance/ p < 0.05)
cars_area5.aov <- aov(area ~ clusmember5, data = cars_standardize_aov2)
summary(cars_standardize5.aov)
TukeyHSD(cars_area5.aov) 


## With 4 clusters
## Test for significance of "Passive variables" ???????????
cars_standardize_aov2 <- cbind(cars_standardize.df[,c(3:6)])

# Passive Var1_ Gender (result = significance/ p < 0.05)
cars_gender4.aov <- aov(gender ~ clusmember4, data = cars_standardize_aov2)
summary(cars_standardize4.aov)
TukeyHSD(cars_gender4.aov) 

# Passive Var2_ Age (result = significance/ p < 0.05)
cars_age4.aov <- aov(age ~ clusmember4, data = cars_standardize_aov2)
summary(cars_standardize4.aov)
TukeyHSD(cars_age4.aov)

# Passive Var3_ Education (result = significance/ p < 0.05)
cars_education4.aov <- aov(education ~ clusmember4, data = cars_standardize_aov2)
summary(cars_standardize4.aov)
TukeyHSD(cars_education4.aov)

# Passive Var4_ Area (result = significance/ p < 0.05)
cars_area4.aov <- aov(area ~ clusmember4, data = cars_standardize_aov2)
summary(cars_standardize4.aov)
TukeyHSD(cars_area4.aov) 


## With 3 clusters
## Test for significance of "Passive variables" ???????????
cars_standardize_aov2 <- cbind(cars_standardize.df[,c(3:6)])

## with 3 cluster
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize3.hc.segment5)

clusmember3 <- as.factor(cars_standardize3.hc.segment5)
cars_standardize_aov <- cbind(cars_standardize.df[,c(7:12)])

## Test for significance of "Active variables"
# Active Var1_ Mileage (result = significance/ p < 0.05)
cars_mileage3.aov <- aov(mileage ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_mileage3.aov) 
#Result: group1 diff but group2&3 not that diff


# Active Var2_ Power (result = significance/ p < 0.05)
cars_power3.aov <- aov(power ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_power3.aov) 

# Active Var3_ Design (result = significance/ p < 0.05)
cars_design3.aov <- aov(design ~ clusmembe3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_design3.aov) 

# Active Var4_ Comfort (result = significance/ p < 0.05)
cars_comfort3.aov <- aov(comfort ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_comfort3.aov) 

# Active Var5_ Entertainment (result = significance/ p < 0.05)
cars_entertainment3.aov <- aov(entertainment ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_entertainment3.aov) 

# Active Var6_ Environment (result = significance/ p < 0.05)
cars_environment3.aov <- aov(environment ~ clusmember3, data = cars_standardize_aov)
summary(cars_standardize3.aov)
TukeyHSD(cars_environment3.aov) 


## Test for significance of "Passive variables" 
cars_standardize_aov3 <- cbind(cars_standardize.df[,c(3:6)])

# Passive Var1_ Gender (result = significance/ p < 0.05)
cars_gender3.aov <- aov(gender ~ clusmember3, data = cars_standardize_aov2)
summary(cars_standardize3.aov)
TukeyHSD(cars_gender3.aov) 

# Passive Var2_ Age (result = significance/ p < 0.05)
cars_age3.aov <- aov(age ~ clusmember3, data = cars_standardize_aov2)
summary(cars_standardize3.aov)
TukeyHSD(cars_age3.aov)

# Passive Var3_ Education (result = significance/ p < 0.05)
cars_education3.aov <- aov(education ~ clusmember3, data = cars_standardize_aov2)
summary(cars_standardize3.aov)
TukeyHSD(cars_education3.aov)

# Passive Var4_ Area (result = significance/ p < 0.05)
cars_area3.aov <- aov(area ~ clusmember3, data = cars_standardize_aov2)
summary(cars_standardize3.aov)
TukeyHSD(cars_area3.aov) 






## NOTE: Preview all result

## Summarize 5 clusters
seg.summ(cars_standardize.df[,c(7:12)], cars_standardize.k5$cluster)

## Summarize clusters (include "demographic")
## ** Standardize only "Active Variables"
cars_standvar.df <- cars_survey.df
cars_standvar.df[,7:12] <- data.frame(scale(cars_survey.df[,7:12]))
summary(cars_standvar.df)

## 5 clusters
seg.summ(cars_standvar.df[,c(3:12)], cars_standardize.k5$cluster)

## 4 clusters
seg.summ(cars_standvar.df[,c(3:12)], cars_standardize.k4$cluster)

## 3 clusters
seg.summ(cars_standvar.df[,c(3:12)], cars_standardize.k3$cluster)


## Appendix: Find No. of clusters


## Method1: Elbow
set.seed(31)

fviz_nbclust(cars_standardize.df[,7:12], kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")

## Method2: Gap statistic
gap_stat <- clusGap(cars_standardize.df[,7:12], FUN = kmeans, nstart = 30, K.max = 24, B = 100, iter.max = 50)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")


## Method3: The Silhouette Method
fviz_nbclust(cars_standardize.df[,7:12], kmeans, method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")


## Method4: NbClust
install.packages('NbClust')
library(NbClust)
install.packages('factoextra')
library('factoextra')


res.nbclust <- NbClust(cars_standardize.df[,7:12], distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

## Test: correlation
library(corrplot)
corrplot(cor(cars_survey.df[,7:12]), type = "upper", method = "ellipse", tl.cex = 0.9)


## Test: PCA
install.packages('FactoMineR')
library(factoextra)
library(FactoMineR)
library(corrplot)

cars.pca <- PCA(cars_standardize.df[,7:12],  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(cars.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(cars.pca)
# Contributions of variables to PC1
fviz_contrib(cars.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(cars.pca, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(cars.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")




## Find demographic each group
install.packages("tidyverse")

library(dplyr)
library(tidyr)
library(readr)

cars_segment3 <- cbind(cars_survey.df[,3:6],cars_standardize5.hc.segment3)
head(cars_segment3)

str(cars_segment3)

## ** Standardize only "Active Variables"
cars_standvar.df <- cars_survey.df
cars_standvar.df[,7:12] <- data.frame(scale(cars_survey.df[,7:12]))
summary(cars_standvar.df)

## Preview 3 segments
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
seg.summ(cars_standvar.df[,c(3:12)], cars_standardize.k3$cluster)


## Dimension1: Gender
cars_segment3 %>%
  group_by(cars_standardize5.hc.segment3) %>%
  mutate(female = sum(case_when(gender == 1 ~ 1, TRUE ~ 0)),
         male = sum(case_when(gender== 2 ~ 1, TRUE ~ 0)),
         others = sum(case_when(gender == 3 ~ 1, TRUE ~ 0))) %>%
  select(cars_standardize5.hc.segment3, female, male, others) 

## Dimension2: Area
cars_segment3 %>%
  group_by(cars_standardize5.hc.segment3) %>%
  mutate(metropolitan = sum(case_when(area == 1 ~ 1, TRUE ~ 0)),
         urban = sum(case_when(area == 2 ~ 1, TRUE ~ 0)),
         suburban = sum(case_when(area == 3 ~ 1, TRUE ~ 0)),
         countryside = sum(case_when(area == 4 ~ 1, TRUE ~ 0))) %>%
  select(cars_standardize5.hc.segment3, metropolitan, urban, suburban, countryside) 


## Dimension3: Age
cars_segment3 %>%
  group_by(cars_standardize5.hc.segment3) %>%
  mutate(age_g1 = sum(case_when(age <= 24 ~ 1, TRUE ~ 0)),
         age_g2 = sum(case_when(age >= 25 & age <= 34 ~ 1, TRUE ~ 0)),
         age_g3 = sum(case_when(age >= 35 & age <= 44 ~ 1, TRUE ~ 0)),
         age_g4 = sum(case_when(age >= 45 & age <= 64 ~ 1, TRUE ~ 0)),
         age_g5 = sum(case_when(age >= 65 ~ 1, TRUE ~ 0))) %>%
  select(cars_standardize5.hc.segment3, age_g1, age_g2, age_g3, age_g4, age_g5)


## Dimension4: Education
cars_segment3 %>%
  group_by(cars_standardize5.hc.segment3) %>%
  mutate(hs_profession = sum(case_when(education == 1 ~ 1, TRUE ~ 0)),
         hs_theory = sum(case_when(education == 2 ~ 1, TRUE ~ 0)),
         high_nonuni = sum(case_when(education == 3 ~ 1, TRUE ~ 0)),
         university = sum(case_when(education == 4 ~ 1, TRUE ~ 0)),
         others = sum(case_when(education == 5 ~ 1, TRUE ~ 0))) %>%
  select(cars_standardize5.hc.segment3, hs_profession, hs_theory, high_nonuni, university, others) 



## Missforest
library(randomForest)

set.seed(04625)
train.prop <- 0.65
train.cases <- sample(nrow(cars_survey.df[,7:12]), nrow(cars_survey.df[,7:12])*train.prop)
seg.df.train <- seg.raw[train.cases , ]
seg.df.test <- seg.raw[-train.cases , ]

library(e1071)
(seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train))



set.seed(98040)
seg.rf <- randomForest(cars_survey.df$ ~ ., data = cars_survey.df[,7:12], ntree=3000,
                          importance=TRUE)

importance(seg.rf)


varImpPlot(seg.rf, main="Variable importance by segment")
library(gplots)
library(RColorBrewer) heatmap.2(t(importance(seg.rf)[ , 1:4]),
                                col=brewer.pal(9, "Blues"), dend="none", trace="none", key=FALSE , margins=c(10, 10),
                                main="Variable importance by segment"
