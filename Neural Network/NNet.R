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
library(nnet)
library(neuralnet)

##### Zadanie 6 #######################################

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

# Budowa sieci

tuning_nnet = tune.nnet(y~.,
                        data = dane_spirals_train,
                        size = c(1, 10, 50, 200),
                        decay = c(0, 0.1, 0.01))

model_nnet = tuning_nnet$best.model

# Predykcja
pred = predict(model_nnet, dane_spirals_test[, -ncol(dane_spirals_test)], type = 'class')

# Macierz kontyngencji
con(pred, dane_spirals_test[, ncol(dane_spirals_test)])

##### Zadanie 8 #######################################

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

# dostrajanie modelu
tuning_nnet_satellite = tune.nnet(y~.,
                        data = dane_satellite_train,
                        size = c(10, 20, 30),
                        decay = c(0, 0.1, 0.01),
                        maxit = 500)
summary(tuning_nnet_satellite)

model_nnet_satellite = tuning_nnet_satellite$best.model
pred_satellite = predict(model_nnet_satellite,
                         dane_satellite_test[,-ncol(dane_satellite_test)],
                         type = 'class')
con(pred_satellite, dane_satellite_test[,ncol(dane_satellite_test)])