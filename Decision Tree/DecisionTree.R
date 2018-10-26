##### POMOCE ########################################


# funkcja do tablicy kontyngencji:
con=function(x,y){
  tab=table(x,y)
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}

##### Zadanie 1 ########################################

# zaladowanie biblioteki pod R:
library(class)
library(mlbench)
library(e1071)
library(rpart)
library(rpart.plot)
library(adabag)

##### Zadanie 3 ########################################

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

# budowanie modelu
classifier = rpart(formula = y ~ .,
                   data = dane_satellite_train)

# predykcja
y_satellite_pred = predict(classifier, dane_satellite_test[,-ncol(dane_satellite_test)], type = 'class')

# macierz kontyngencji
con(y_satellite_pred, dane_satellite_test[,ncol(dane_satellite_test)])

# print + wizualizacja drzewa
summary(classifier)
print(classifier)
prp(classifier)

##### Zadanie 4 ########################################

# budowanie modelu
classifier_pruned = rpart(formula = y ~ .,
                   data = dane_satellite_train,
                   control = rpart.control(cp=0.02))

# predykcja
y_satellite_pruned_pred = predict(classifier_pruned, dane_satellite_test[,-ncol(dane_satellite_test)], type = 'class')

# macierz kontyngencji
con(y_satellite_pruned_pred, dane_satellite_test[,ncol(dane_satellite_test)])

# print + wizualizacja drzewa
summary(classifier_pruned)
print(classifier_pruned)
prp(classifier_pruned)

##### Zadanie 5 ########################################

# dostrajanie modelu
tuning = tune.rpart(formula = y~.,
                    data = dane_satellite_train,
                    cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                    minsplit = c(1, 2, 6, 9, 12))
summary(tuning)

# budowanie modelu
classifier_tuned = tuning$best.model

# predykcja
y_satellite_tuned_pred = predict(classifier_tuned, dane_satellite_test[,-ncol(dane_satellite_test)], type = 'class')

# macierz kontyngencji
con(y_satellite_tuned_pred, dane_satellite_test[,ncol(dane_satellite_test)])

# print + wizualizacja drzewa
summary(classifier_tuned)
print(classifier_tuned)
prp(classifier_tuned)

# wyswietlenie pola cptable
classifier_tuned$cptable

# istotnosc zmiennych
classifier_tuned$variable.importance

##### Zadanie 6 ########################################

# dzielenie na dwa zbiory
sampling2 = sample(1:nrow(dane_satellite_train), 0.5*nrow(dane_satellite_train))

train_set_1 = dane_satellite_train[sampling2,]
train_set_2 = dane_satellite_train[-sampling2,]

# dostrajanie mmodeli
tuning_1 = tune.rpart(formula = y~.,
                    data = train_set_1,
                    cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                    minsplit = c(1, 2, 6, 9, 12))
summary(tuning_1)

tuning_2 = tune.rpart(formula = y~.,
                      data = train_set_2,
                      cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                      minsplit = c(1, 2, 6, 9, 12))
summary(tuning_2)

# budowanie modeli
classifier_1 = tuning_1$best.model
classifier_2 = tuning_2$best.model

# predykcja
pred_1 = predict(classifier_1, dane_satellite_test[,-ncol(dane_satellite_test)], type = 'class')
pred_2 = predict(classifier_2, dane_satellite_test[,-ncol(dane_satellite_test)], type = 'class')

# porównanie
con(pred_1, pred_2)

# wizualizacja
par(mfrow=c(1,2))
prp(classifier_1)
prp(classifier_2)

##### Zadanie 7 ########################################

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

# drostrajanie modelu
tuning_spirals = tune.rpart(formula = y~.,
                      data = dane_spirals_train,
                      cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                      minsplit = c(1, 2, 6, 9, 12))
summary(tuning_spirals)

# budowanie modelu
classifier_spirals = tuning_spirals$best.model

# predykcja
pred_spirals = predict(classifier_spirals, dane_spirals_test[,-ncol(dane_spirals_test)], type = 'class')

# macierz kontyngencji
con(pred_spirals, dane_spirals_test[, 3])

##### Zadanie 8 ########################################

# tworzenie zbioru danych
dane_spirals_train_disturbed = Spirals[train_spirals,]
set.seed(123)
disturb_pointers = sample(1:nrow(dane_spirals_train_disturbed), 0.3*nrow(dane_spirals_train_disturbed))

# zaburzenie 
for (i in disturb_pointers){
  dane_spirals_train_disturbed$y[i] = as.factor(3 - as.numeric(dane_spirals_train_disturbed$y[i]))
}

# dostrajanie modelu
tuning_spirals_disturbed = tune.rpart(formula = y~.,
                            data = dane_spirals_train_disturbed,
                            cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                            minsplit = c(1, 2, 6, 9, 12))
summary(tuning_spirals_disturbed)

# budowanie modelu
model_spirals_disturb_tuned = tuning_spirals_disturbed$best.model

# predykcja
y_disturbed_pred_tuned = predict(model_spirals_disturb_tuned, dane_spirals_test[,1:2], type = "class")

# macierz kontyngencji
con(y_disturbed_pred_tuned, dane_spirals_test[,3])

##### Zadanie 9 ########################################

# budowanie zbioru danych
set.seed(12345)
x=mlbench.spirals(1000,2,0.08)
plot(x)
Spirals_max=as.data.frame(x)

# Zmiana nazwy ostatniej kolumny
names(Spirals_max)[length(names(Spirals_max))] = 'y'

# budowanie modelu
classifier_max = rpart(formula = y~.,
                       data = Spirals_max,
                       cp = 0,
                       minsplit = 2)

# predykcja
pred_max = predict(classifier_max, Spirals_max[,1:2], type = "class")

con(pred_max, Spirals_max[,3])
prp(classifier_max)
