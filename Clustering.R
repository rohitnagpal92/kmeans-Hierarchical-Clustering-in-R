#****************Clustering with R*****************************
#1)Import wine data
wine <- read.csv(file.choose())

#use of structure and summary commands to check imported data
str(wine)
summary(wine)

#2)Use SimpleKMeans to perform the clustering task for k=3
Sys.setenv('JAVA_HOME'="C:/Program Files/Java/jdk-11.0.5/") #its a required statement to run RWeka in Windows
install.packages("rJava")   #its a required statement to run RWeka in Windows
install.packages("RWeka")
library(RWeka)
library(rJava)

set.seed(100)
k <- 3
wine_clusters <- SimpleKMeans(wine, Weka_control(N = k, V = T))
wine_clusters

#3)the differences among the three clusters
#Cluster 0: has 60 wines
#Cluster 1: has 55 wines
#Cluster 2: has 63 wines

#comaprison of variables centroids across clusters:
#a) for vairable Alcohol,Cluster0 (13.72) and Cluster1 (13.10) has almost same centroid value but the for Cluster2 (12.23) centroid value is quite low.
#b) for Flavanoids, Cluster1 has the lowest value of centroid i.e. 0.8478, whereas Cluster0 and Cluster2 have 2.97 and 2.16 value respectively.
#c) for variable Color as well, Cluster0 (5.46) and Cluster1 (6.93) has somewhat close value to each other but for Cluster2 (3.03) the value is very low.

#within-cluster-variance is: 48.97

#4)Use of Manhattan distance function in SimpleKMeans to perform the clustering task for k=3
wine_clusters_mh <- SimpleKMeans(wine, Weka_control(N = k, V = T, A= "weka.core.ManhattanDistance"))
wine_clusters_mh

#the differences among the three clusters after using Manhattan distance
#Cluster 0: has 63 wines
#Cluster 1: has 51 wines
#Cluster 2: has 64 wines

#comaparing the same variables centroids across clusters (that we compared above for Euclidean distance above):
#a) for vairable Alcohol,Cluster0 (13.74) and Cluster1 (13.11) has almost same centroid value but the for Cluster2 (12.24) centroid value is quite low.
#b) for Flavanoids, Cluster1 has the lowest value of centroid i.e. 0.7, whereas Cluster0 and Cluster2 have 2.98 and 2.03 value respectively.
#c) for variable Color, Cluster0 (5.40) and Cluster1 (7.30) has somewhat close value to each other but for Cluster2 (2.83) the value is very low.

#within-cluster-variance is: 248.45

#from the above results of Euclidean distance and Manhattan distance, we can confirm that:
#a) the distribution of wines across the clusters is almost the same for both type of distnace. There is no vast difference.
#b) also, the values of variable's centroid across clusters are almost the same for both type of distnace with very small difference.

#*********************************Hierarchical clustering using protein data*******************************
#1)Import wine data
protein <- read.csv(file.choose())

#use of structure and summary commands to check imported data
str(protein)
summary(protein)

#2)the hierarchical clustering using all the attributes except country 
protein_clusters <- hclust(dist(protein[,-1]))

#3)Plot the dendrogram of the hierarchical clusters
plot(protein_clusters)

#if height<=20, then minimal cluster number we could build are 4 clusters

#4)Cut off the tree at 4 cluster numbers using cuttree
k <- 4
clusters_cut <- cutree(protein_clusters,k)

#5)Assign cluster id back to original data and check first 10 records of cluster number
protein$clusterID <- clusters_cut
protein[1:10,c(1,11)]

#6)Amongst the first ten records, following countries are in the same cluster with "Albania":
#Czechoslovakia
#Greece

#7)data frame with only country and cluster ID in order and order them by cluster ID
orderindex <- order(protein$clusterID)
country_cluster_df <- data.frame(Country = protein$Country[orderindex],Cluster = protein$clusterID[orderindex])
country_cluster_df

#cluster ID for "E Germany" is 4.
#There are 2 other countries in the same cluster with "E Germany" i.e. there are total 3 countries in cluster 4.
#Following countries have the similar protein intakes as "E Germany" (same cluster):
  #Portugal
  #Spain

#8)Use average link instead of complete link to build another hierarchical clustering model
protein_clusters_avg <- hclust(dist(protein[,-1]), method = 'average')

#9)Plot the dendrogram of the new hierarchical clusters
plot(protein_clusters_avg)
#Based on the dendrogram, if height<=20, then minimal cluster number we could build are 2 clusters



