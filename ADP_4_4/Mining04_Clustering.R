#===========================#
# 04. 군집분석 (Clustering) #
#===========================#

###########################
# 5. R을 이용한 거리 계산 #
###########################

x1<-c(1,2)
x2<-c(1,3)
x3<-c(3,6)
x4<-c(4,7)
x5<-c(1,5)
x<-t(data.frame(x1,x2,x3,x4,x5))

? dist
(euc_dist<-dist(x,method="euclidean"))
(che_dist<-dist(x,method="maximum"))
(man_dist<-dist(x,method="manhattan"))
(can_dist<-dist(x,method="canberra"))
(min_dist<-dist(x,method="minkowski"))


##################################
# 7. R을 이용한 계층적 군집 분석 #
##################################

sin_den<-hclust(euc_dist^2,method="single")
plot(sin_den, main="Cluster Dendrogram - single")

com_den<-hclust(euc_dist^2,method="complete")
plot(com_den, main="Cluster Dendrogram - complete")

avg_den<-hclust(euc_dist^2,method="average")
plot(avg_den, main="Cluster Dendrogram - average")

med_den<-hclust(euc_dist^2,method="median")
plot(med_den, main="Cluster Dendrogram - median")

cen_den<-hclust(euc_dist^2,method="centroid")
plot(cen_den, main="Cluster Dendrogram - centroid")

war_den<-hclust(euc_dist^2,method="ward.D")
plot(war_den, main="Cluster Dendrogram - ward.D")


######################################
# 9. R을 이용한 비계층적 군집화 결과 #
######################################

## (1) k-means
install.packages("HDclassif")
library(HDclassif)
data(wine)
(k_wine <- kmeans(wine[,-c(1)],3))

table(k_wine$cluster,wine$class)

plot(wine[,c("V1","V13")], col=k_wine$cluster)
points(k_wine$centers[,c("V1","V13")],col=1:3,pch=8,cex=2)

wine_s<-scale(wine[,-c(1)])
(k_wine_s <- kmeans(wine_s,3))

table(k_wine_s$cluster,wine$class)

plot(wine_s[,c("V1","V13")], col=k_wine_s$cluster)
points(k_wine_s$centers[,c("V1","V13")],col=1:3,pch=8,cex=2)

k_wine_res<-wine
k_wine_res$clst_s<-k_wine_s$cluster

table(k_wine_res$clst_s)

ind<-which(k_wine_res$clst_s==1)
summary(k_wine_res[ind,])

table(k_wine_res[ind,"class"])

## (2) pam
install.packages("cluster")
library(cluster)
(pam_wine <- pam(wine[,-c(1)],3))

table(pam_wine$cluster,wine$class)

plot(wine[,c("V1","V13")], col=pam_wine$cluster)
points(pam_wine$medoids[,c("V1","V13")],col=1:3,pch=8,cex=2)

pam_wine_s <- pam(wine_s,3)
table(pam_wine_s$cluster,wine$class)

plot(wine_s[,c("V1","V13")], col=pam_wine_s$cluster)
points(pam_wine_s$medoids[,c("V1","V13")],col=1:3,pch=8,cex=2)

## (3) Density-based Clustering
install.packages("fpc")
library(fpc)

(den_wine_s <- dbscan(wine_s, eps=2.6, MinPts=10, scale=TRUE))

table(den_wine_s$cluster,wine$class)

(den_wine_s1 <- dbscan(wine_s, eps=2.5, MinPts=8, scale=TRUE))

table(den_wine_s1$cluster,wine$class)

str(den_wine_s1)

plot(den_wine_s,wine_s[,c("V1","V13")])

## (4) Fuzzy Clustering
set.seed(2014)
f1<-cbind(rnorm(10, -5, 0.5), rnorm(10, -5, 0.5))
f2<-cbind(rnorm(10, 5, 0.5), rnorm(10, 5, 0.5))
f3<-cbind(rnorm(5, 0.5*1.67, 0.5), rnorm(5, 0.5*1.67, 0.5))
f<-rbind(f1,f2,f3)
(fuz_f_2 <- fanny(f,2))

(fuz_f_3 <- fanny(f,3))

plot(fuz_f_2)

#########################################################
# 10. R의 k-means clustering에서 적정한 cluster 수 찾기 #
#########################################################
wss<-0
for(i in 1:15) wss[i]<-kmeans(wine_s,centers=i)$tot.withinss
plot(1:15,wss,type="b",xlab="Number of Cluster", ylab="Within group sum of squares")
