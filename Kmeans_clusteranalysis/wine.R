###LECTURE 7 CLUSTERING, CODE FOR KMEAN, COSINE


rm(list = ls())
set.seed(12345)


#install the following packages
library(clue)
library(cluster)
library(factoextra)
library(skmeans)
library(gridExtra)


wine<-read.csv("winedata.csv")
wine[is.na(wine)]<-0

# create a data set for clusteirng analysis
# the rows are what you want to cluster
# the columns are the variables you use for clustering
wine.transposed<-t(wine[,-(1:7)])

# get distance of each pair of customers
distance <- get_dist(wine.transposed, method="euclidean")

# visualize the distance matrix
fviz_dist(distance)


########################################################
#ORDINARY K-MEANS (EUCLIDEAN DISTANCE) WITH 4 CLUSTERS
########################################################
wine.k4<-kmeans(wine.transposed, centers=4, nstart=25)

str(wine.k4)

wine.k4

wine.k4$centers

k4.clustermeans<-cbind(wine[,1:7], t(wine.k4$centers))
k4.clustermeans

# how to give each cluster a label?
k4.clustermeans[order(-k4.clustermeans[,8]),]
k4.clustermeans[order(-k4.clustermeans[,9]),]
k4.clustermeans[order(-k4.clustermeans[,10]),]
k4.clustermeans[order(-k4.clustermeans[,11]),]


########################################################
#how to decide on the optimal cluster size?
########################################################
wss <-  c(wine.k4$totss, rep(0,14))
bss <- c(rep(0,15))
pseudoF <- c(rep(0,15))
wssbss<- c(rep(0,15))

for (i in 2:15){ 
  kmeansi <- kmeans(wine.transposed,centers=i,nstart=5)
  wss[i] <- sum(kmeansi$withinss)
  bss[i] <- kmeansi$betweenss
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(wine.transposed)-i)) 
  wssbss[i] <- wss[i]/bss[i]
}

par(mfrow=c(2,2))
plot(2:15, bss[2:15], type="b", xlab="Number of Clusters",ylab="",main ="between groups sum of squares") 
plot(2:15, wss[2:15], type="b", xlab="Number of Clusters",ylab="",main ="Within groups sum of squares") 
plot(2:15, wssbss[2:15], type="b", xlab="Number of Clusters",ylab="",main ="Within divided by between groups sum of squares") 
plot(2:15, pseudoF[2:15], type="b", xlab="Number of Clusters",ylab="",main ="Pseudo F") 

# an alternative optimal cluster detection method - "silhouette"
# it says 6! yet, remember rule of 3 for ease of interpretation and business plan implementation
fviz_nbclust(wine.transposed, kmeans, method="silhouette")


########################################################
# ORDINARY K-MEANS (EUCLIDEAN DISTANCE) WITH 3 CLUSTERS
########################################################
wine.k3<-kmeans(wine.transposed, centers=3,nstart=25)
wine.k3
k3.clustermeans<-cbind(wine[,1:7], t(wine.k3$centers))
k3.clustermeans[order(-k3.clustermeans[,8]),]
k3.clustermeans[order(-k3.clustermeans[,9]),]
k3.clustermeans[order(-k3.clustermeans[,10]),]

fviz_cluster(wine.k3, data=wine.transposed)


# plots to compare different cluster results visually
wine.k2<-kmeans(wine.transposed, centers=2,nstart=25)
wine.k5<-kmeans(wine.transposed, centers=5,nstart=25)
wine.k6<-kmeans(wine.transposed, centers=6,nstart=25)

p1 <- fviz_cluster(wine.k2, geom = "point", data = wine.transposed) + ggtitle("k = 2")
p2 <- fviz_cluster(wine.k3, geom = "point",  data = wine.transposed) + ggtitle("k = 3")
p3 <- fviz_cluster(wine.k4, geom = "point",  data = wine.transposed) + ggtitle("k = 4")
p4 <- fviz_cluster(wine.k5, geom = "point",  data = wine.transposed) + ggtitle("k = 5")
p5 <- fviz_cluster(wine.k6, geom = "point",  data = wine.transposed) + ggtitle("k = 6")

grid.arrange(p1, p2, p3, p4, p5, nrow = 3)


###########################################################################
# let us try the SPHERICAL K-MEANS (COSINE DISTANCE) given the sparse data
###########################################################################
wine.cos3<-skmeans(wine.transposed, 3,method="genetic")

wine.cos3

cos3.clustermeans<-cbind(wine[,1:7],t(aggregate(wine.transposed,by=list(wine.cos3$cluster),mean)[,2:33]))

cos3.clustermeans[order(-cos3.clustermeans[,8]),] 
cos3.clustermeans[order(-cos3.clustermeans[,9]),] 
cos3.clustermeans[order(-cos3.clustermeans[,10]),] 


#EVALUATE D% DISSIMILARITY ACCOUNTED FOR IN THE COSINE DISTANCE CLUSTERING METHOD
cl_validity(wine.cos3)   #17.34% DISSIMILARITY IS ACCOUNTED FOR BY THE 3 CLUSTERS


## decide on the optimal cluster size? look for an elbow
remaining.disimilarity <- numeric()

max.clusters<-25
for (i in 2:max.clusters) {
  remaining.disimilarity[i-1] <- 1 - as.numeric(cl_validity(skmeans(as.matrix(wine.transposed),i)))
}
par(mfrow=c(1,1))
plot(2:25, remaining.disimilarity, type="b", xlab="Number of Clusters", ylab="Remaining Disimilarity")


###########################################################################
# try the 4-cluster cosine distance measure solution
###########################################################################
wine.cos4<-skmeans(wine.transposed, 4,method="genetic")
summary(wine.cos4)
cos4.clustermeans<-cbind(wine[,1:7],t(aggregate(wine.transposed,by=list(wine.cos4$cluster),mean)[,2:33]))
cos4.clustermeans[order(-cos4.clustermeans[,8]),] 
cos4.clustermeans[order(-cos4.clustermeans[,9]),] 
cos4.clustermeans[order(-cos4.clustermeans[,10]),] 
cos4.clustermeans[order(-cos4.clustermeans[,11]),] 


###########################################################################
# try the 3-cluster cosine distance measure solution
###########################################################################
wine.cos3<-skmeans(wine.transposed, 3,method="genetic")
summary(wine.cos3)
wine.cos3

cos3.clustermeans<-cbind(wine[,1:7],t(aggregate(wine.transposed,by=list(wine.cos3$cluster),mean)[,2:33]))
cos3.clustermeans[order(-cos3.clustermeans[,8]),] 
cos3.clustermeans[order(-cos3.clustermeans[,9]),] 
cos3.clustermeans[order(-cos3.clustermeans[,10]),] 




########################################################
# hierarchical clustering
########################################################
wine.hc <- hclust(dist(wine.transposed), "ward.D2")
plot(wine.hc)


# comparison across the three approaches of clustering results
table(data.frame(cos3=wine.cos3$cluster,hc3=cutree(wine.hc,3)))

table(data.frame(km3=wine.k3$cluster,hc3=cutree(wine.hc,3)))

table(data.frame(km3=wine.k3$cluster,cos3=wine.cos3$cluster))



