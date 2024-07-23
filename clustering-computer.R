#1. PREPARE DATASET
setwd("C:/Users/jingh/OneDrive - Nanyang Technological University/SG NTU Curriculum/Y2S2 23-24/PS0002 Intro to Data Science and AI/Lab 10")
computer<-read.csv("computers.csv", header=T)
summary(computer)
#to remove any NA values 
computer<-computer[complete.cases(computer),]
summary(computer)

#in clustering, we can only use numeric variables 
str(computer)
#only select numerical variables
numvar<-sapply(computer, is.numeric)
computer<-computer[,numvar]
str(computer)
class(computer)

#use scale to normalize data, convert it back to dataframe
computer<-scale(computer)
computer<-as.data.frame(computer)
class(computer)
View(computer)


#2. PERFORM INTIIAL VALUE OF K 
k3<-kmeans(computer, centers=3, nstart=25) #max.iter is left as default=10
str(k3)
k3
k3$tot.withinss

#3. PERFORM ELBOW METHOD TO FIND OPTIMAL K
#given k, update value of css
#when running for different k values (to find value of k), can use nstart=10 (lower) instead of 25 
#to save computational cost
wcss<-function(k){
  kmeans(computer, centers=k,iter.max=25, nstart=10)$tot.withinss
}
#warning: did not converge in 10 iterations, we increase max.iter to 25

#we set k from 1 to 30: originally k:1 to 15, we increase bcos bend was hard to observe
k.values<-1:15
set.seed(100)
#sapply (vector, function)
wcss_k<-sapply(k.values,wcss)
#plot within-cluster ss for different values of k
plot(k.values, wcss_k, type="b", pch=19, frame=FALSE, 
     xlab="Number of clusters k", 
     ylab="Total within-clusters sum of squares")
#optimal value of k = 7 

#4. PERFORM K-MEANS CLUSTERING WTIH K-7
set.seed(100)
#We decide k=
k7.final<-kmeans(computer, centers=7,nstart=25)
k7.final

library(dplyr)
#extract clusters and summarize descriptive statistics at cluster level 
#add to the original dataset
computer %>% mutate(Cluster=k7.final$cluster)%>%group_by(Cluster)%>%summarise_all("mean")
View(computer)

#5. PERFORM HIERARCHICAL CLUSTERING
#create dissimilarity matrix 
d=dist(computer, method = "euclidean")
#hierarchical clustering using complete linkage
hc1<-hclust(d, method="complete") #using complete linkage\#plot the dendrogram 
#cex adjusts the size of labels
#hang: where the labels to be at, -1 = bottom
plot(hc1, cex=0.6, hang=-1)
#draw a border around the 7 clusters, option "border" specifies colors, 1 is black
rect.hclust(hc1, k=7, border=2:8)
#to determine how many clusters there are, besides visual inspection, we could look at the Within-cluster sum of squares (WCSS) plot 




