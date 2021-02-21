###LECTURE 8 RECOMMENDATION SYSTEMS, MINKOWSKI DISTANCE, CORRELATION, COSINE


#Demonstrate different distance metrics, how to compute them, pros and cons
rm(list=ls())


#install.packages('proxy') # This package adds cosine to the distance functions that can be computed
library('proxy') 



# Minkowski distance (manhattan and euclidean)
similarity.1<-t(data.frame(c(1,2),c(5,3)))
rownames(similarity.1)<-c("Alice","Bernard")
colnames(similarity.1)<-c("Movie.1","Movie.2")
similarity.1

plot(similarity.1[,"Movie.1"],similarity.1[,"Movie.2"],ylim=c(0,7),xlim=c(0,7))


similarity.1.manhattan<-as.matrix(dist(similarity.1, method="manhattan"))
similarity.1.manhattan


similarity.1.euclidean<-as.matrix(dist(similarity.1, method="euclidean"))
similarity.1.euclidean


# Correlation 
similarity.1.correlation<-cor(t(similarity.1), use="pairwise.complete.obs")
similarity.1.correlation


# cosine similarity
similarity.1.cosine<-as.matrix(dist(similarity.1, method="cosine"))
similarity.1.cosine<-1-similarity.1.cosine    #This function actually returns 1-cosine, so need to convert
similarity.1.cosine

angle.1<-acos(similarity.1.cosine)/(2*pi/360)
angle.1



#How do Manhattan and Euclidean distance functions treat missing data?
similarity.2<-t(data.frame(c(NA,NA,4,4,5),c(1,2,2,2,3)))
rownames(similarity.2)<-c("Alice","Bernard")
colnames(similarity.2)<-c("Movie.1","Movie.2","Movie.3","Movie.4","Movie.5")
similarity.2

similarity.2.manhattan<-as.matrix(dist(similarity.2, method="manhattan"))
similarity.2.manhattan

similarity.2.euclidean<-as.matrix(dist(similarity.2, method="euclidean"))
similarity.2.euclidean

similarity.2.correlation<-cor(t(similarity.2), use="pairwise.complete.obs")
similarity.2.correlation

similarity.2.cosine<-as.matrix(dist(similarity.2, method="cosine"))
similarity.2.cosine



#Compare distance functions after scaling one set of values. Cindy's scores are Alice's plus a constant
similarity.3<-t(data.frame(c(2,2,4,4,5),c(1,2,2,2,3),c(2+5,2+5,4+5,4+5,5+5) ))
rownames(similarity.3)<-c("Alice","Bernard","Cindy")
colnames(similarity.3)<-c("Movie.1","Movie.2","Movie.3","Movie.4","Movie.5")
similarity.3

similarity.3.manhattan<-as.matrix(dist(similarity.3, method="manhattan"))
similarity.3.manhattan

similarity.3.euclidean<-as.matrix(dist(similarity.3, method="euclidean"))
similarity.3.euclidean

similarity.3.correlation<-cor(t(similarity.3), use="pairwise.complete.obs")
similarity.3.correlation

similarity.3.cosine<-1-as.matrix(dist(similarity.3, method="cosine"))
similarity.3.cosine

angle.3<-acos(similarity.3.cosine)/(pi/180)
angle.3

