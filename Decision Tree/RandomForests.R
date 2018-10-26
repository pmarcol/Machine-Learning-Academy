##### POMOCE ########################################


# funkcja do tablicy kontyngencji:
con=function(x,y){
  tab=table(x,y)
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}

# zaladowanie biblioteki pod R:
library(class)
library(mlbench)
library(e1071)
library(rpart)
library(rpart.plot)
library(adabag)
library(randomForest)

##### Zadanie 12 #######################################

# budowa zbioru
set.seed(12345)
x=mlbench.spirals(1000,2,0.08)
plot(x)
Spirals=as.data.frame(x)

# Zmiana nazwy ostatniej kolumny
names(Spirals)[length(names(Spirals))] = 'y'

# dzielenie na zbiór uczacy i testowy
set.seed(1234)
train_spirals = sample(1:nrow(Spirals), 0.67*nrow(Spirals))

dane_spirals_train = Spirals[train_spirals,]
dane_spirals_test = Spirals[-train_spirals,]

# budowanie modelu
tuning = tune.randomForest(y~.,
                           data = dane_spirals_train,
                           ntree = c(100, 150, 200),
                           mtry = c(1, 2))
summary(tuning)

tuned_model = tuning$best.model
pred = predict(tuned_model, dane_spirals_test[,-ncol(dane_spirals_test)], type='class')
con(pred, dane_spirals_test[,ncol(dane_spirals_test)])

##### Satellites #######################################

# tworzenie zbioru danych
data("Satellite")
x = Satellite
dane_satellite = as.data.frame(x)
names(dane_satellite)[length(names(dane_satellite))] = 'y'

# dzielenie na zbiór uczacy i testowy
set.seed(1234)
train_sat=sample(1:nrow(dane_satellite), 0.67*nrow(dane_satellite))
dane_satellite_train = dane_satellite[train_sat,]
dane_satellite_test = dane_satellite[-train_sat,]

# tuning modelu
vars_num = ncol(dane_satellite_train)-1

tuning_RF = tune.randomForest(y~.,
                  data = dane_satellite_train,
                  ntree = c(100, 150, 200),
                  mtry = c(round(sqrt(vars_num)/2), round(sqrt(vars_num)), round(2*sqrt(vars_num))))
summary(tuning_RF)

model_RF = tuning_RF$best.model

y_satellite_pred = predict(model_RF, dane_satellite_test[,-ncol(dane_satellite_test)])
con(y_satellite_pred, dane_satellite_test[,ncol(dane_satellite_test)])