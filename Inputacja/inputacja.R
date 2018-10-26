library(mlbench)
library(stats)
library(nnet)
library(neuralnet)
library(e1071)
library(rpart)
library(randomForest)
library(gbm)
library(FNN)
library(xgboost)

# MSE
MSE=function(response, predicted){ 	#mean square error (blad sredniokwadratowy)
  MSE=sum((predicted-response)^2)/length(predicted)
  print(paste("Model ma wartosc MSE =",MSE), quote=F)
  return(MSE)
  invisible()
}

##### wczytanie danych #####

dane = read.csv2(file = "Ceny_transakcyjne_1m2mieszkan_ze_zm_sztucznymi.csv", sep = ";", row.names = 1)

##### dzielenie na podzbiory #####

set.seed(1234)
train=sample(1:nrow(dane), 0.67*nrow(dane))

dane.train = dane[train, ]
dane.test = dane[-train, ]

dane.train.bez.y = dane.train[, -ncol(dane.train)]
dane.train.y = dane.train[, ncol(dane.train)]

dane.test.bez.y = dane.test[, -ncol(dane.test)]
dane.test.y = dane.test[, ncol(dane.test)]

braki = rep(0, ncol(dane.train))
for(i in 1:ncol(dane.train)){
  braki[i] = sum(is.na(dane.train[,i]))
}

ile = length(which(braki>0))
braki=order(-braki)
do_zmiany = braki[1:ile]
