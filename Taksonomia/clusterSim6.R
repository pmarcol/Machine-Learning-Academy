library(clusterSim)

set.seed(12345)
dane = cluster.Gen(200, model = 6, dataType = 'm', numNoisyVar = 0)
dataset = dane$data
skupienia = dane$clusters


##### Hierarchical clustering #####
dist.matrix=dist(dataset, method = "euclidean")

model=hclust(dist.matrix, method = "average")

index.CH=c()
index.Sil=c()
k=c(2:10)
for (k.par in k) {
  model.klasa=cutree(model, k=k.par)
  index.CH=c(index.CH, index.G1(dataset, model.klasa))
  index.Sil=c(index.Sil, index.S(dist.matrix, model.klasa))
}
k.opt=which.max(index.CH)+1
print(k.opt)
print(index.CH[k.opt-1])
print(index.Sil[k.opt-1])

model.klasa=cutree(model, k=k.opt)
print(classAgreement(table(skupienia, model.klasa))$rand)
par(mfrow=c(1,2))
plot(dataset, col=model.klasa)
plot(dataset, col=skupienia)

##### K-means #####
index.CH=c()
index.Sil=c()
k=c(2:10)
for (k.par in k) {
  model.klasa=kmeans(dataset, centers = k.par)$cluster
  index.CH=c(index.CH, index.G1(dataset, model.klasa))
  index.Sil=c(index.Sil, index.S(dist.matrix, model.klasa))
}
k.opt=which.max(index.CH)+1
print(k.opt)
print(index.CH[k.opt-1])
print(index.Sil[k.opt-1])

model.klasa=cutree(model, k=k.opt)
print(classAgreement(table(skupienia, model.klasa))$rand)
par(mfrow=c(1,2))
plot(dataset, col=model.klasa)
plot(dataset, col=skupienia)

##### K-medoids #####
index.CH=c()
index.Sil=c()
k=c(2:10)
for (k.par in k) {
  model.klasa=clara(x=dataset, k=k.par, metric="euclidean")$clustering
  index.CH=c(index.CH, index.G1(dataset, model.klasa))
  index.Sil=c(index.Sil, index.S(dist.matrix, model.klasa))
}
k.opt=which.max(index.CH)+1
print(k.opt)
print(index.CH[k.opt-1])
print(index.Sil[k.opt-1])

model.klasa = clara(x=dataset, k=k.opt, metric="euclidean")$clustering
print(classAgreement(table(skupienia, model.klasa))$rand)
par(mfrow=c(1,2))
plot(dataset, col=model.klasa)
plot(dataset, col=skupienia)