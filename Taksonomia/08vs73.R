library(clusterSim)
library(cluster)
library(e1071)

dane_08=read.csv2("http://web2.ue.katowice.pl/trzesiok/FP/3edycjaszkolenML/3swiat08.csv", row.names=2)
dane_73=read.csv2("http://web2.ue.katowice.pl/trzesiok/FP/3edycjaszkolenML/3swiat73.csv", row.names=2)
dane_08.oryg=dane_08
dane_73.oryg=dane_73
dane_08=dane_08[,-1]
dane_73=dane_73[,-1]
dane_08 = scale(dane_08)
dane_73 = scale(dane_73)

set.seed(123)
cluster_kmeans_08 = kmeans(dane_08, centers = 3)$cluster
cluster_kmeans_73 = kmeans(dane_73, centers = 3)$cluster

# cluster_kmeans_08[cluster_kmeans_08==1] = 4
# cluster_kmeans_08[cluster_kmeans_08==3] = 1
# cluster_kmeans_08[cluster_kmeans_08==4] = 3
# 
# cluster_kmeans_73[cluster_kmeans_73==2] = 4
# cluster_kmeans_73[cluster_kmeans_73==3] = 2
# cluster_kmeans_73[cluster_kmeans_73==4] = 3

classAgreement(table(cluster_kmeans_08, cluster_kmeans_73))$rand

# które kraje zyska³y status najbogatszych miêdzy 73 a 2008 rokiem
setdiff(row.names(dane_08)[cluster_kmeans_08==3],row.names(dane_73)[cluster_kmeans_73==3])
# które kraje straci³y status najbogatszych miêdzy 73 a 2008 rokiem
setdiff(row.names(dane_73)[cluster_kmeans_73==3],row.names(dane_08)[cluster_kmeans_08==3])
# które kraje zyska³y status najbiedniejszych miêdzy 73 a 2008 rokiem
setdiff(row.names(dane_08)[cluster_kmeans_08==1],row.names(dane_73)[cluster_kmeans_73==1])
# które kraje straci³y status najbiedniejszych miêdzy 73 a 2008 rokiem
setdiff(row.names(dane_73)[cluster_kmeans_73==1],row.names(dane_08)[cluster_kmeans_08==1])

j=3
par(mfrow=c(1,3))
y.gorna=max(dane_08.oryg[,j])
y.dolna=min(dane_08.oryg[,j])
for (i in 1:3){
  boxplot(dane_08.oryg[cluster_kmeans_08==i,j], ylim=c(y.dolna,y.gorna), xlab=names(dane_08.oryg)[j], main = paste("Klasa ",i))
}
y.gorna=max(dane_73.oryg[,j])
y.dolna=min(dane_73.oryg[,j])
for (i in 1:3){
  boxplot(dane_73.oryg[cluster_kmeans_73==i,j], ylim=c(y.dolna,y.gorna), xlab=names(dane_73.oryg)[j], main = paste("Klasa ",i))
}