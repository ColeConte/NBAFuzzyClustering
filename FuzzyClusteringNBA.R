#Cole Conte
#AMS 597 Project

library(ggplot2)
library(dispRity)
library(cluster)
library(e1071)
library(corrplot)
library(reshape2)
library(LICORS)
set.seed(123)

#Part 1: Cluster Method Exploration and Visualization

#Specifying k example
strip1 = data.frame(
  x = runif(1000),
  y = runif(1000),
  distribution = c(rep(1,1000))
)
strip2 = data.frame(
  x = runif(1000) - 5,
  y = runif(1000),
  distribution = c(rep(2,1000))
)
strip3 = data.frame(
  x = 5 + runif(1000),
  y = runif(1000),
  distribution = c(rep(3,1000))
)

strips = rbind(strip1,strip2,strip3)
strips$distribution = as.factor(strips$distribution)
ggplot(strips, aes(x=x,y=y,color=distribution))+
  geom_point()
#Ran this bit three times to generate dependence on starting location plots
stripK = kmeans(strips,5)
strips$cluster = as.factor(stripK$cluster)
ggplot(strips, aes(x=x,y=y,color=cluster))+
  geom_point()

#Silhouette plots
dis = dist(strips)^2
kmeansSil = silhouette(kmeans(strips,3)$cluster,dist=dis)
plot(kmeansSil,col=1:3,border=NA)
kmeansSil2 = silhouette(kmeans(strips,5)$cluster,dist=dis)
plot(kmeansSil2,col=1:5,border=NA)

#Non-optimal k-means example
circleDf = as.data.frame(rbind(random.circle(1000,runif,outer=1),random.circle(1000, rnorm, inner = 2, outer = 5)))
colnames(circleDf) = c("x","y")
circleK = kmeans(circleDf,2)
d = dist(circleDf, method = "euclidean")
circleH = hclust(d,method="single")
circleDf$cluster = as.factor(circleK$cluster)
circleDf$hierarchical_cluster = as.factor((as.numeric(circleH$order >= 1000)+1))
circleDf$distribution = as.factor(c(rep(1,1000),rep(2,1000)))
ggplot(circleDf, aes(x=x,y=y,color=cluster))+
         geom_point()
ggplot(circleDf, aes(x=x,y=y,color=distribution))+
  geom_point()
ggplot(circleDf, aes(x=x,y=y,color=hierarchical_cluster))+
  geom_point()

#Fuzzy clustering strips example
data("USArrests")
ss = sample(1:50, 15)
arrest = scale(USArrests[ss,])
arrest.cm = cmeans(arrest, 4)
corrplot(arrest.cm$membership,is.corr=F)
arrest = as.data.frame(arrest)
arrest$Cluster = as.factor(arrest.cm$cluster)
arrest = cbind(State = rownames(arrest), arrest)
rownames(arrest) = 1:nrow(arrest)
ggplot(arrest, aes(x=UrbanPop,y=Rape,color=Cluster))+
  geom_point()+
  geom_text(aes(label=State,hjust=-.1))


#Part 2: Clustering NBA players
#Load data set
nbaData = read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/nba-player-advanced-metrics/master/nba-data-historical.csv"))
nbaData = nbaData[nbaData$Min>=1500,]
nbaData = nbaData[complete.cases(nbaData),]

#Simple k-means
nbaData.km = kmeans(nbaData[,15:16],5)
nbaData$PosCluster = as.factor(nbaData.km$cluster)
ggplot(nbaData, aes(A.36,R.36, color = pos))+
  geom_point()
ggplot(nbaData, aes(A.36,R.36, color = PosCluster))+
  geom_point()
table(nbaData$PosCluster,droplevels(nbaData)$pos)
nbadata.dist = dist(nbaData[,-c(1:2,5:6,8,41)],method="euclidean")
nbadata.sil = silhouette(nbaData.km$cluster,dist=nbadata.dist)
plot(nbadata.sil,col=1:5,border=NA)

#PG Clusters with k-means++
pgData = nbaData[nbaData$pos=="PG",]
pgData = pgData[pgData$Min>2500,]
pgData.kmpp = kmeanspp(pgData[,c(12:13,25:29,31:32,36:38)],2)
pgData$PGCluster = as.factor(pgData.kmpp$cluster)
ggplot(pgData, aes(X3P.,STL.,color=PGCluster))+
  geom_point()


#Optimizing number of clusters
nbaData.km2 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],2)
nbaData.sil2 = silhouette(nbaData.km2$cluster,dist=nbadata.dist)
plot(nbaData.sil2,col=1:2,border=NA)

nbaData$Cluster2 = as.factor(nbaData.km2$cluster)
nbaData$isBig = nbaData$pos %in% c("PF","C")
ggplot(nbaData, aes(Raptor.O,Raptor.D, color = isBig))+
  geom_point()
ggplot(nbaData, aes(Raptor.O,Raptor.D, color = Cluster2))+
  geom_point()
table(nbaData$Cluster2,nbaData$isBig)


nbaData.km3 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],3)
nbaData.sil3 = silhouette(nbaData.km3$cluster,dist=nbadata.dist)
plot(nbaData.sil3,col=1:3,border=NA)

nbaData.km4 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],4)
nbaData.sil4 = silhouette(nbaData.km4$cluster,dist=nbadata.dist)
plot(nbaData.sil4,col=1:4,border=NA)

nbaData.km5 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],5)
nbaData.sil5 = silhouette(nbaData.km5$cluster,dist=nbadata.dist)
plot(nbaData.sil5,col=1:5,border=NA)

nbaData.km6 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],6)
nbaData.sil6 = silhouette(nbaData.km6$cluster,dist=nbadata.dist)
plot(nbaData.sil6,col=1:6,border=NA)

nbaData.km7 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],7)
nbaData.sil7 = silhouette(nbaData.km7$cluster,dist=nbadata.dist)
plot(nbaData.sil7,col=1:7,border=NA)

nbaData.km8 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],8)
nbaData.sil8 = silhouette(nbaData.km8$cluster,dist=nbadata.dist)
plot(nbaData.sil8,col=1:8,border=NA)

nbaData.km9 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],9)
nbaData.sil9 = silhouette(nbaData.km9$cluster,dist=nbadata.dist)
plot(nbaData.sil9,col=1:9,border=NA)

nbaData.km10 = kmeans(nbaData[,c(12:13,25:29,31:32,36:38)],10)
nbaData.sil10 = silhouette(nbaData.km10$cluster,dist=nbadata.dist)
plot(nbaData.sil10,col=1:10,border=NA)

#Implementing fuzzy clustering for 2 clusters
nbaData.cm2 = cmeans(nbaData[,c(12:13,25:29,31:32,36:38)],2)
nbaData$CMeans2.1 = nbaData.cm2$membership[,1]
nbaData$CMeans2.2 = nbaData.cm2$membership[,2]

nbaData.cm2.means = aggregate(nbaData[,44:45],by=list(nbaData$pos),mean)
nbaData.cm2.means.long = melt(nbaData.cm2.means,id.vars="Group.1")

ggplot(nbaData.cm2.means.long, aes(x=Group.1, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  labs(x="Listed Position",y="Membership Grade",fill="Cluster")

#Implementing fuzzy clustering for 10 clusters
nbaData.cm10 = cmeans(nbaData[,c(12:13,25:29,31:32,36:38)],10)
nbaData$CMeans10.1 = nbaData.cm10$membership[,1]
nbaData$CMeans10.2 = nbaData.cm10$membership[,2]
nbaData$CMeans10.3 = nbaData.cm10$membership[,3]
nbaData$CMeans10.4 = nbaData.cm10$membership[,4]
nbaData$CMeans10.5 = nbaData.cm10$membership[,5]
nbaData$CMeans10.6 = nbaData.cm10$membership[,6]
nbaData$CMeans10.7 = nbaData.cm10$membership[,7]
nbaData$CMeans10.8 = nbaData.cm10$membership[,8]
nbaData$CMeans10.9 = nbaData.cm10$membership[,9]
nbaData$CMeans10.10 = nbaData.cm10$membership[,10]
nbaData.cm10.means = aggregate(nbaData[,46:55],by=list(nbaData$pos),mean)
nbaData.cm10.means.long = melt(nbaData.cm10.means,id.vars="Group.1")
ggplot(nbaData.cm10.means.long, aes(x=Group.1, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  labs(x="Listed Position",y="Membership Grade",fill="Cluster")


#Fuzziness and Raptor correlated?
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

nbaData.cm5 = cmeans(nbaData[,c(12:13,25:29,31:32,36:38)],5)
nbaData$CMeans5.1 = nbaData.cm5$membership[,1]
nbaData$CMeans5.2 = nbaData.cm5$membership[,2]
nbaData$CMeans5.3 = nbaData.cm5$membership[,3]
nbaData$CMeans5.4 = nbaData.cm5$membership[,4]
nbaData$CMeans5.5 = nbaData.cm5$membership[,5]
nbaData$CMeans5.Max = apply(nbaData[,56:60], 1, function(x)x[maxn(1)(x)])
nbaData$CMeans5.SecondMax = apply(nbaData[,56:60], 1, function(x)x[maxn(2)(x)])
nbaData$CMeans5.Diff12 = nbaData$CMeans5.Max - nbaData$CMeans5.SecondMax
raptorMeanDiffCor = cor(nbaData$Raptor...,nbaData$CMeans5.Diff12)
cor.test(nbaData$Raptor...,nbaData$CMeans5.Diff12)
