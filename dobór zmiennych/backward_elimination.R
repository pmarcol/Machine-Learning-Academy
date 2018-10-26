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

# wczytanie danych
dane = as.data.frame(mlbench.friedman1(1000, sd=1))

#podzia³ na zbiory: ucz¹cy i testowy
set.seed(1234)
train=sample(1:nrow(dane), 0.67*nrow(dane))

dane.train = dane[train, ]
dane.test = dane[-train, ]

dane.train.bez.y = dane.train[, -ncol(dane.train)]
dane.train.y = dane.train[, ncol(dane.train)]

dane.test.bez.y = dane.test[, -ncol(dane.test)]
dane.test.y = dane.test[, ncol(dane.test)]

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
                    kernel = "radial")
  
  best_cost = tuning$best.parameters$cost
  best_gamma = tuning$best.parameters$gamma
  
  varset = 1:ncol(data)
  
  for(i in 1:(ncol(data)-1)){
    min_err = 2000000
    
    for(var_num in varset){
      temp_varset = varset[varset != var_num]
      
      input_dataset = as.data.frame(cbind(data[temp_varset], target))
      names(input_dataset)[ncol(input_dataset)] = 'y'
      
      temp_tuning = tune.svm(y~.,
                             data = input_dataset,
                             cost = best_cost,
                             gamma = best_gamma,
                             kernel = "radial")
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

my_list = backward_elimination_svm(data = dane.train.bez.y, target = dane.train$y)

