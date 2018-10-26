library(clusterSim)
# library(e1071)

#########################################################
### Zadanie 3.:

dane=read.csv2("http://web2.ue.katowice.pl/trzesiok/FP/3edycjaszkolenML/3swiat08.csv", row.names=2)
dane.oryg=dane
dane=dane[,-1]
head(dane)
dane = scale(dane)

index.CH=c()
index.Sil=c()

k=c(2:10)
for (k.par in k) {
  model.klasa=kmeans(dane, centers = k.par)$cluster
  index.CH=c(index.CH, index.G1(dane, model.klasa))
  index.Sil=c(index.Sil, index.S(dist.matrix, model.klasa))
}
k.opt=which.max(index.CH)+1
print(k.opt)
print(index.CH[k.opt-1])
print(index.Sil[k.opt-1])
print(index.CH)
print(index.Sil)

model.klasa = kmeans(dane, centers = k.opt)