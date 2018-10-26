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

# funkcja do tablicy kontyngencji:
MSE=function(response, predicted){ 	#mean square error (blad sredniokwadratowy)
  MSE=sum((predicted-response)^2)/length(predicted)
  print(paste("Model ma wartosc MSE =",MSE), quote=F)
  return(MSE)
  invisible()
}

##### wczytanie danych #####

dane = read.csv2(file = "Ceny_transakcyjne_1m2mieszkan_ze_zm_sztucznymi.csv", sep = ";", row.names = 1)

# usuwamy zmienna "pietro" - duzo braków danych
dane = dane[, -4]

# usuwamy braki danych
dane=dane[!apply(is.na(dane[,]),1,any),]

names(dane)[ncol(dane)] = 'y'

dane.bez.y = dane[, -ncol(dane)]

##### dzielenie na podzbiory #####

set.seed(1234)
train=sample(1:nrow(dane), 0.67*nrow(dane))

dane.train = dane[train, ]
dane.test = dane[-train, ]

dane.train.bez.y = dane.train[, -ncol(dane.train)]
dane.train.y = dane.train[, ncol(dane.train)]

dane.test.bez.y = dane.test[, -ncol(dane.test)]
dane.test.y = dane.test[, ncol(dane.test)]

# usuwanie wartoœci odstaj¹cych
gamma_par = c(0.01, 0.5, 1, 5)
nu_par = c(0.001, 0.005, 0.01, 0.05)

final = rep(0, nrow(dane.train))

for(gamma_val in gamma_par){
  for(nu_val in nu_par){
    model = svm( ~ .,
                 data = dane.train,
                 type = "one-classification",
                 gamma = gamma_val,
                 nu = nu_val,
                 kernel = "radial")
    typical = predict(model, dane.train)
    outSVC = which(typical==FALSE)
    for(i in outSVC){
      final[i] = final[i] + 1
    }
  }
}

ranking = cbind(order(-final), final[order(-final)])
sum(ranking[,2]>9)
indices = ranking[which(ranking[,2]>9),1]


dane.train.bez_outlierow = dane.train[-indices,]

set.seed(12345)
RF.tuning = tune.randomForest(y~.,
                              data = dane.train.bez_outlierow,
                              ntree = c(100, 150, 200, 250),
                              mtry = c(1, 2, 5, 10)
)
summary(RF.tuning)

# budowanie modelu
model.RF = RF.tuning$best.model

# predykcja
pred.RF = predict(model.RF, dane.test.bez.y)

print("Random Forest - dane surowe")
MSE(dane.test.y, pred.RF)0