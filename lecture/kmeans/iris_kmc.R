setwd("d:\\R/kmeans")
library(ggplot2)
library(cluster)  #silhouette을 위해 필요 
data(iris)
nm <- c("Piping","Process","Civil","Rotating", "Cluster")
colnames(iris) <- nm
summary(iris)
boxplot(iris[,-5])


# kmaens = 3
set.seed(4)  # 군집번호가 set.seed에 따라 달라질 수 있다.
ir3<-kmeans(iris[,-5], centers=3, iter.max=200, nstart=20)
ir3
t(ir3$centers)
s<-silhouette(ir3$cluster, dist(iris[,-5]))
plot(s)

#실제 값을 아는 경우
iris$Cluster <- as.numeric(iris$Cluster)
a <- which(iris$Cluster==1)
b <- which(iris$Cluster==2) 
c <- which(iris$Cluster==3)
iris$Cluster[a] <- 3
iris$Cluster[b] <- 1
iris$Cluster[c] <- 2

table(ir3$cluster, iris$Cluster)
cm3 <- table(ir3$cluster, iris$Cluster)
1-sum(diag(cm3))/sum(cm3)

#Visualization
ggplot(iris, aes(x=factor(Cluster),y=Piping))+geom_boxplot()+geom_jitter(size=3.5, col ="red", alpha =0.4)
ggplot(iris,aes(x=Civil,y=Rotating, color=factor(Cluster))) +geom_point(size=3)
ggplot(iris,aes(x=Piping,y=Process, color=factor(Cluster))) +geom_point(size=3)

library(pairsD3)
pairsD3(iris[,1:4], group = iris[,5])

#pairs(iris[1:4], main = "Man Power for each Disciplein", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Cluster)])

panel.pearson <- function(x, y, ...) {
     horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
     vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
     text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}
pairs(iris[1:4], main = "Man Power for each Discipline", pch = 21, bg = c("red","green3","blue")[unclass(iris$Cluster)],
      upper.panel=panel.pearson)


#k =2, 4는 혼동행렬 불가
# kmaens = 2
set.seed(4)  # 군집번호가 set.seed에 따라 달라질 수 있다.
ir2<-kmeans(iris[,-5],center=2,iter.max=200, nstart =30)
ir2

s<-silhouette(ir2$cluster, dist(iris[,-5]))
plot(s)

#k =1 ~ 6까지
set.seed(4)
d <- dist(iris[,-5])
avgS <- c()
for(k in 2:6){     # k=1 일 경우, 실루엣 구할 수 없ㅇ
     cl <- kmeans(iris[,-5], centers=k, iter.max=200, nstart=20)
     s <- silhouette(cl$cluster,d)
     avgS <- c(avgS, mean(s[,3]))
}
sil_mat <- data.frame(nClus =2:6, Silh=avgS)


# kmaens = 4
set.seed(4)  # 군집번호가 set.seed에 따라 달라질 수 있다.
ir4<-kmeans(iris[,-5],center=4,iter.max=200)
ir4

s<-silhouette(ir4$cluster, dist(iris[,-5]))
plot(s)

########### PCA  ######
pca <- princomp(iris[,-5])
#loadings(pca)
#pca$scores[1:20,]
#summary(pca)

pr <- prcomp(iris[,-5]) # 단위 같아 SCALE하지 않ㅇ
pr$rotation
biplot(pr)

pr.var <- pr$sdev^2 #주성분의 표준 편차 
pve <- pr.var/sum(pr.var)
pve

plot(pve, xlab ="Principal Component", ylab ="Proportion of Variance", ylim=c(0,1), type="b")
