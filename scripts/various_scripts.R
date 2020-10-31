#HOPKINS - Supervised


#Calcultaing 1-hopking statistic 
res = get_clust_tendency(iris[,1:4], n = 30, graph = T)
res
 
#hopking statisticfor random data
dataRandom = matrix(0, nrow = 150, ncol = 4)
for (i in 1:4){
  dataRandom[,i] = runif(150,min(iris[,i]),max(iris[,i]))
}
res = get_clust_tendency(dataRandom, n = 30, graph = T)
res

 

#### MEtodo No supervisado (visual. matrix de distancia)
#Generating distance matrix 
K=3
data=iris[,1:4] 
model = kmeans(data, K,nstart=50)
cluster=model$cluster
tempDist=as.matrix(dist(data))
index=order(cluster)
tempDist2=tempDist[index, index]

#melting the data to use ggplot
library(reshape2)
tempDF=melt(tempDist2)
ggplot(tempDF)+aes(x=Var1,y=Var2,fill=value)+geom_raster()+
  scale_x_continuous(expand=c(0,0),breaks=seq(0,150,10))+scale_y_continuous(expand=c(0,0),breaks=seq(0,150,10))+
  labs(title="Data points distance matrix",x="data points",y="data points",fill="distance")

  

#Cohesion
#install.packages("flexclust")
library(flexclust)
K=3
model = kmeans(iris[,1:4],K,nstart=5)
cluster=model$cluster
withinCluster=c()
for (i in 1:K){
  tempData=iris[which(cluster==i),1:4]
  # subset(iris,cluster==1) ## equivalente a la linea de arriba. Es simplemente un subset de los datos
  withinCluster[i]=sum(dist2(tempData,colMeans(tempData))^2)
}
cohesion = sum(withinCluster)

  

#Separation
meanData=colMeans(iris[,1:4])
SSB=c()
for (i in 1:K){
  tempData=iris[which(cluster==i),1:4]
  SSB[i]=nrow(tempData)*sum((meanData-colMeans(tempData))^2)
}
separation = sum(SSB)

#Distance from all points to the middle point
separation+cohesion
#totalSum=sum(dist2(iris[,1:4],meanData)^2)   # Es lo mismo que arriba

