library(clusterSim)
library(cluster)
library(e1071)

dane=read.csv2("http://web2.ue.katowice.pl/trzesiok/FP/3edycjaszkolenML/3swiat08.csv", row.names=2)
dane.oryg=dane
dane=dane[,-1]
head(dane)
dane = scale(dane)

dist.matrix=dist(dane, method = "euclidean")
model_complete=hclust(dist.matrix, method = "complete")
model_ward=hclust(dist.matrix, method = "ward.D2")

cluster_ward = cutree(model_ward, k=k.opt)
cluster_complete = cutree(model_complete, k=k.opt)
set.seed(123)
cluster_kmeans = kmeans(dane, centers = 3)$cluster
cluster_kmedoids = clara(x=dane, k=3, metric="euclidean")$clustering
set.seed(0)
cluster_kmeans2 = kmeans(dane, centers = 3)$cluster


classAgreement(table(cluster_ward, cluster_complete))$rand
classAgreement(table(cluster_ward, cluster_kmeans))$rand
classAgreement(table(cluster_ward, cluster_kmedoids))$rand
classAgreement(table(cluster_complete, cluster_kmeans))$rand
classAgreement(table(cluster_complete, cluster_kmedoids))$rand
classAgreement(table(cluster_kmeans, cluster_kmedoids))$rand

classAgreement(table(cluster_kmeans, cluster_kmeans2))$rand