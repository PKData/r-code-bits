df<-read.csv("E:/cate.csv",header=TRUE)

#number clusters you're trying
n<-3

#scaling data to give distances.

df1<-scale(df)

#kmeans clustering into our pre-assigned number of clusters
clust<-kmeans(df1,n)

#hierarchical clustering model
clust2<-hclust(dist(df1),method = "centroid")
plot(clust2)

#cutting tree at number of clusters you selected (replace n with a number if your graphic suggests)
clust2result<-cutree(clust2,n)

#attaching columns with cluster assignments to the original data.
out<-data.frame(kmean=factor(clust$cluster),hclust= clust2result,df)
out


df2<-read.csv("E:/cate2.csv",header=TRUE)
df3<-data.matrix(df2)
library(cluster)
library(klaR)
df2<-read.csv("E:/cate2.csv",header=TRUE)
dist<-daisy(df2, metric = "gower")
model<-hclust(dist)
plot(model)
clustmember<-cutree(model,3)
df1<-data.frame(df2,cluster=clustmember)
df1

