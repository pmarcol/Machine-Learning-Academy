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

# funkcja do normalizacji danych w oparciu o przeksztalcenie ilorazowe z wektorem max dostarczonym zewnetrznie jako argument
normalizacja1A = function(dane, maks.wektor) {
  return(t(apply(dane, 1, function(x){x/maks.wektor})))
}

# wektor maksimów do normalizacji
wektor.maks = as.vector(apply(dane.bez.y, 2, max))

# normalizacja
dane.train.bez.y.norm = normalizacja1A(dane.train.bez.y, wektor.maks)
names(dane.train.bez.y.norm) = names(dane.bez.y)
dane.test.bez.y.norm = normalizacja1A(dane.test.bez.y, wektor.maks)

# funkcja do eliminacji zmiennych
backward_elimination_svm = function(data, target){
  var_names = names(data)
  variables_ranking = c()
  classification_errors = c()
  
  input_dataset = as.data.frame(cbind(data, target))
  names(input_dataset)[ncol(input_dataset)] = 'y'
  
  # find the best model
  tuning = tune.svm(y~.,
                    data = input_dataset,
                    cost = c(0.01, 0.1, 1, 10, 100, 1000),
                    gamma = c(0.01, 0.1, 0.1, 1, 3, 5, 7),
                    kernel = "radial",
                    scale=FALSE)
  
  best_cost = tuning$best.parameters$cost
  best_gamma = tuning$best.parameters$gamma
  
  varset = 1:ncol(data)
  
  for(i in 1:(ncol(data)-1)){
    min_err = 2000000
    
    for(var_num in varset){
      temp_varset = varset[varset != var_num]
      
      input_dataset = as.data.frame(cbind(data[,temp_varset], target))
      names(input_dataset)[ncol(input_dataset)] = 'y'
      
      temp_tuning = tune.svm(y~.,
                             data = input_dataset,
                             cost = best_cost,
                             gamma = best_gamma,
                             kernel = "radial",
                             scale=FALSE)
      if(temp_tuning$performances$error[1] < min_err){
        variables_ranking[ncol(data)+1-i] = var_names[var_num]
        classification_errors[ncol(data)-i] = temp_tuning$performances$error[1]
        min_err = temp_tuning$performances$error[1]
        next_varset = temp_varset
      }
    }
    varset = next_varset
  }
  variables_ranking[1] = var_names[varset[1]]
  return(list(var_ranking=variables_ranking,errors=classification_errors))
}

my_list = backward_elimination_svm(data = dane.train.bez.y.norm, target = dane.train.y)

