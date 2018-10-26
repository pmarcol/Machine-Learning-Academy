##### zaladowanie pakietów #####

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

##### pomoce #####

MSE=function(response, predicted){ 	#mean square error (blad sredniokwadratowy)
  MSE=sum((predicted-response)^2)/length(predicted)
  print(paste("Model ma wartosc MSE =",MSE), quote=F)
  return(MSE)
  invisible()
}

pred.p=function(response, predicted, prob=0.25){ 	# prediction quality indicator (frakcja obserwacji o akceptowalnych resztach)
  pred.p = sum((predicted >= (1-prob)*response) & (predicted <= (1+prob)*response))/length(predicted)
  print(paste("Model ma wartosc pred.p =", round(100*pred.p,2),"% (czyli taka frakcje obserwacji o akceptowalnych resztach) dla p =",round(100*prob,0),"%"), quote=F)
  return(pred.p)
}

# funkcja do normalizacji danych w oparciu o przeksztalcenie ilorazowe z wektorem max dostarczonym zewnetrznie jako argument
normalizacja1A = function(dane, maks.wektor) {
  return(t(apply(dane, 1, function(x){x/maks.wektor})))
}

##### wczytanie danych #####

dane = read.csv2(file = "Ceny_transakcyjne_1m2mieszkan_ze_zm_sztucznymi.csv", sep = ";", row.names = 1)
dane_raw = read.csv2(file = "Ceny_transakcyjne_mieszkan.csv", sep = ";", row.names = NULL)

# usuwamy zmienna "pietro" - duzo braków danych
dane = dane[, -4]
dane_raw = dane_raw[, -5] # usuwamy zmienna "pietro" - duzo braków danych

# usuwamy braki danych
dane=dane[!apply(is.na(dane[,]),1,any),]
dane_raw=dane_raw[!apply(is.na(dane_raw[,]),1,any),] # usuwamy braki danych

names(dane)[ncol(dane)] = 'y'
names(dane_raw)[ncol(dane_raw)] = 'y'

dane.bez.y = dane[, -ncol(dane)]
dane_raw.bez.y = dane_raw[, -ncol(dane_raw)]

##### dzielenie na podzbiory #####

set.seed(1234)
train=sample(1:nrow(dane), 0.67*nrow(dane))

dane.train = dane[train, ]
dane.test = dane[-train, ]

dane.train.bez.y = dane.train[, -ncol(dane.train)]
dane.train.y = dane.train[, ncol(dane.train)]

dane.test.bez.y = dane.test[, -ncol(dane.test)]
dane.test.y = dane.test[, ncol(dane.test)]

dane_raw.train = dane_raw[train, ]
dane_raw.test = dane_raw[-train, ]

dane_raw.train.bez.y = dane_raw.train[, -ncol(dane_raw.train)]
dane_raw.train.y = dane_raw.train[, ncol(dane_raw.train)]

dane_raw.test.bez.y = dane_raw.test[, -ncol(dane_raw.test)]
dane_raw.test.y = dane_raw.test[, ncol(dane_raw.test)]

# dzielenie zbioru uczacego na podzbiory: uczacy i walidacyjny
set.seed(123)
train_valid = sample(1:nrow(dane.train), 0.67*nrow(dane.train))

dane.train.2 = dane.train[train_valid, ]
dane.valid = dane.train[-train_valid, ]

dane.train.2.bez.y = dane.train.2[, -ncol(dane.train.2)]
dane.train.2.y = dane.train.2[, ncol(dane.train.2)]

dane.valid.bez.y = dane.valid[, -ncol(dane.valid)]
dane.valid.y = dane.valid[, ncol(dane.valid)]

##### normalizacja #####

# wektor maksimów do normalizacji
wektor.maks = as.vector(apply(dane.bez.y, 2, max))

# normalizacja
dane.train.2.bez.y.norm = normalizacja1A(dane.train.2.bez.y, wektor.maks)
dane.valid.bez.y.norm = normalizacja1A(dane.valid.bez.y, wektor.maks)
dane.train.bez.y.norm = normalizacja1A(dane.train.bez.y, wektor.maks)
dane.test.bez.y.norm = normalizacja1A(dane.test.bez.y, wektor.maks)

##### linear model #####
model.lm = lm(y~.,
             data = dane.train)
summary(model.lm)

# predykcja
pred.lm = predict(model.lm, dane.test.bez.y)

MSE(dane.test.y, pred.lm)
pred.p(dane.test.y, pred.lm)

##### PPR #####

# szukanie optymalnych parametrów

best.mse = 1000000
best.max_term = 0
best.n_term = 0

for (max_term in c(25, 20, 15)){
  for (n_term in 1:10){
    temp.model.ppr = ppr(y~.,
                         data = dane.train.2,
                         max.terms = max_term,
                         nterms = n_term)
    pred.temp = predict(temp.model.ppr, dane.valid.bez.y)
    cat("max.terms: ", max_term, ", nterms: ", n_term)
    temp.mse = MSE(pred.temp, dane.valid.y)
    if(temp.mse < best.mse){
      best.mse = temp.mse
      best.max_term = max_term
      best.n_term = n_term
    }
  }
}

cat("best max.terms: ", best.max_term, ", best nterms: ", best.n_term)

model.ppr = ppr(y~.,
                data = dane.train,
                max.terms = best.max_term,
                nterms = best.n_term)

# predykcja
pred.ppr = predict(model.ppr, dane.test.bez.y)

MSE(dane.test.y, pred.ppr)
pred.p(dane.test.y, pred.ppr)

##### kNN #####

best.mse = 1000000
best.k = 0
for (k_val in 1:100){
  pred.temp = knn.reg(train = dane.train.2.bez.y.norm,
                           y = dane.train.2.y,
                           test = dane.valid.bez.y.norm,
                           k = k_val)$pred
  
  cat("tested k: ", k_val)
  temp.mse = MSE(pred.temp, dane.valid.y)
  if(temp.mse < best.mse){
    best.mse = temp.mse
    best.k = k_val
  }
}

cat("best k: ", best.k)

pred.knn = knn.reg(train = dane.train.bez.y.norm,
                   y = dane.train.y,
                   k = best.k,
                   test = dane.test.bez.y.norm)$pred


MSE(dane.test.y, pred.knn)
pred.p(dane.test.y, pred.knn)

##### rpart - dane sztuczne #####

# szukanie optymalnego modelu

set.seed(12345)
rpart.tuning = tune.rpart(formula = y~.,
                          data = dane.train,
                          cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                          minsplit = c(1, 2, 6, 9, 12))
summary(rpart.tuning)

# budowanie modelu
model.rpart = rpart.tuning$best.model

# predykcja
pred.rpart = predict(model.rpart, dane.test.bez.y)

print("rpart - dane sztuczne")
MSE(dane.test.y, pred.rpart)
pred.p(dane.test.y, pred.rpart)

##### rpart - dane surowe #####

# szukanie optymalnego modelu

set.seed(12345)
rpart_raw.tuning = tune.rpart(formula = y~.,
                          data = dane_raw.train,
                          cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                          minsplit = c(1, 2, 6, 9, 12))
summary(rpart_raw.tuning)

# budowanie modelu
model.rpart_raw = rpart_raw.tuning$best.model

# predykcja
pred.rpart_raw = predict(model.rpart_raw, dane_raw.test.bez.y)

print("rpart - dane surowe")
MSE(dane_raw.test.y, pred.rpart_raw)
pred.p(dane_raw.test.y, pred.rpart_raw)

##### Random Forest #####

set.seed(12345)
RF.tuning = tune.randomForest(y~.,
                              data = dane.train,
                              ntree = c(100, 150, 200, 250),
                              mtry = c(1, 2, 5, 10)
                              )
summary(RF.tuning)

# budowanie modelu
model.RF = RF.tuning$best.model

# predykcja
pred.RF = predict(model.RF, dane.test.bez.y)

print("Random Forest - dane surowe")
MSE(dane.test.y, pred.RF)
pred.p(dane.test.y, pred.RF)

##### nnet #####

# zbudowanie zbioru danych
input.dataset = as.data.frame(cbind(dane.train.bez.y.norm, dane.train.y))
names(input.dataset)[ncol(input.dataset)] = 'y'

set.seed(12345)
nnet.tuning = tune.nnet(y~.,
                        data = input.dataset,
                        size = c(1, 2, 3, 5, 10),
                        decay = c(0, 0.1, 0.01),
                        linout=TRUE
                        )
summary(nnet.tuning)

# budowanie modelu
model.nnet = nnet.tuning$best.model

# predykcja
pred.nnet = predict(model.nnet, dane.test.bez.y.norm)

print("nnet")
MSE(dane.test.y, pred.nnet)
pred.p(dane.test.y, pred.nnet)

##### SVR #####

input.dataset = as.data.frame(cbind(dane.train.bez.y.norm, dane.train.y))
names(input.dataset)[ncol(input.dataset)] = 'y'

set.seed(12345)
svr.tuning = tune.svm(y~.,
                      data = input.dataset,
                      kernel = "radial",
                      cost = c(25, 50, 75, 100, 200),
                      gamma = c(0.01, 0.1, 1, 3),
                      epsilon = c(0.2, 0.5, 0.65, 0.75),
                      scale = FALSE)
summary(svr.tuning)

# budowanie modelu
model.svr = svr.tuning$best.model

# predykcja
pred.svr = predict(model.svr, dane.test.bez.y.norm)

print("svr")
MSE(dane.test.y, pred.svr)
pred.p(dane.test.y, pred.svr)

##### XGBoost #####

eta_vector = c(0.025, 0.05, 0.1, 0.2)
maxdepth_vector = c(2, 3, 5, 6)
n_rounds = 120

best_params = c(0, 0, 0) # nrounds, eta, maxdepth

data_matrix = matrix(unlist(dane_raw.train.bez.y), ncol = ncol(dane_raw.train.bez.y), byrow = FALSE)
test_data_matrix = matrix(unlist(dane_raw.test.bez.y), ncol = ncol(dane_raw.train.bez.y), byrow = FALSE)

current_min = 1000000

# szukanie optymalnych parametrów

for (eta_val in eta_vector){
  for (maxdepth_val in maxdepth_vector){
    set.seed(12345)
    temp_cv = xgb.cv(data = xgb.DMatrix(data = data_matrix, label = dane_raw.train.y),
                     nrounds = n_rounds,
                     max_depth = maxdepth_val,
                     eta = eta_val,
                     nfold = 10,
                     lambda = 1.5,
                     objective = "reg:linear",
                     verbose = FALSE)

    temp_min = min(temp_cv$evaluation_log$test_rmse_mean)
    if(temp_min < current_min){
      current_min = temp_min
      best_params[1] = which.min(temp_cv$evaluation_log$test_rmse_mean)
      best_params[2] = eta_val
      best_params[3] = maxdepth_val
    }
  }
}

# budowanie modelu
model.xgboost = xgb.train(max_depth = best_params[3],
                          eta = best_params[2],
                          nrounds = best_params[1],
                          objective = "reg:linear",
                          data = xgb.DMatrix(data = data_matrix,
                                             label = dane_raw.train.y)#,
                          #lambda = 1.5
                          )

# predykcja
pred.xgboost = predict(model.xgboost, test_data_matrix)

print("xgboost")
MSE(dane_raw.test.y, pred.xgboost)
pred.p(dane_raw.test.y, pred.xgboost)