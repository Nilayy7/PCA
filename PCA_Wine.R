install.packages("gdata")
library(gdata)
install.packages("cluster")
library(cluster)
View(wine)
View(wine[-1])
data <-wine[,-1]
attach(data)
cor(data)

#Obtaining PCA Scores using correlation
pca <-princomp(data,cor = TRUE,covmat = NULL,scores = TRUE)
summary(pca)

#Plotting graph
plot(pca)

#We come to a small infernce that Comp.1 has highestimportance or Variance

#PCA scores of top three components
pca$scores[,1:3]

#Add PCA Scores of the three components to wine data
mydata <- cbind(wine,pca$scores[,1:3])
View(mydata)

#considering only pca scores as they represent the data
clust_data <-mydata[,15:17]


#Normalize the data
norm_clust<-scale(clust_data) 
dist1<-dist(norm_clust,method = "euclidean") 


# Clustering (Hierarchical)
fit<-hclust(dist1,method="complete") 

plot(fit) 

rect.hclust(fit, k=7, border="green")

group<-cutree(fit1,7)

cluster <-as.matrix(group)
View(cluster)
final <-cbind(cluster,mydata)
View(final)

###K-means Clustering
install.packages("plyr")
library(plyr)
View(final)
norm_data <- scale(final[,15:17])

#Scree Plot 
wss =(nrow(norm_data)-1)*sum(apply(norm_data, 2, var)) 
for (i in 1:7) wss[i] = sum(kmeans(norm_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Sum of squares")

#K-means using value of k = 7
install.packages("animation")
library(animation)
kn<-kmeans.ani(norm_data,7)
table(kn$cluster)


