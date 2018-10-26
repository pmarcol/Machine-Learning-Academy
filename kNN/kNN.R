############################################
# Zadanie 1
# instalowanie bibliotek pod R przez komende:
# install.packages("animation")
# install.packages("mlbench")
# install.packages("ElemStatLearn")
# install.packages("class")
# install.packages("e1071")

# zaladowanie biblioteki pod R:
library(animation)
library(class)
library(mlbench)
library(ElemStatLearn)
library(e1071)

############################################
# Zadanie 3

data("spam")
dane = spam

# Zmiana nazwy ostatniej kolumny
names(dane)[length(names(dane))] = 'y'

############################################
# Zadanie 4

set.seed(1234)

############################################
# Zadanie 5 i 6

train=sample(1:nrow(dane), 0.67*nrow(dane))

x_train = dane[train,-ncol(dane)]
x_test = dane[-train,-ncol(dane)]

y_train = dane[train,ncol(dane)]
y_test = dane[-train,ncol(dane)]

############################################
# POMOCE

# funkcja do normalizacji danych w oparciu o przeksztalcenie ilorazowe z max liczonym dla tych danych
normalizacja1 = function(dane) {
  maks.wektor=as.vector(apply(dane, 2, max))	
  return(t(apply(dane, 1, function(x){x/maks.wektor})))
}

# funkcja do normalizacji danych w oparciu o przeksztalcenie ilorazowe z wektorem max dostarczonym zewnetrznie jako argument
normalizacja1A = function(dane, maks.wektor) {
  return(t(apply(dane, 1, function(x){x/maks.wektor})))
}

# funkcja do normalizacji danych w oparciu o przeksztalcenie ilorazowe z wektorem max dostarczonym zewnetrznie jako argument (wersja alternatywna)
normalizacja1B = function(dane, maks.wektor) {	
  maks.macierz=matrix(rep(maks.wektor, nrow(dane)), nrow=nrow(dane), ncol=ncol(dane), byrow=TRUE)
  return(dane/maks.macierz)
}

# funkcja do tablicy kontyngencji:
con=function(x,y){
  tab=table(x,y)
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}


############################################
# Zadanie 7

# znalezienie wektora maksimów z kolumn
max_vector = as.vector(apply(x_train, 2, max))

# normalizacja - uzywamy tego samego wektora
x_train = normalizacja1A(x_train, max_vector)
x_test = normalizacja1A(x_test, max_vector)

############################################
# Zadanie 8

prediction = knn(train = x_train, test = x_test, cl = y_train, k = 10)
con(y_test, prediction)

############################################
# Zadanie 9

model = tune.knn(x_train, y_train, k = 1:15)
#summary(model)

#model$best.model

tuned_prediction = knn(train = x_train, test = x_test, cl = y_train, k = model$best.parameters$k)
con(y_test, tuned_prediction)

############################################
# Zadanie 10

set.seed(123)

restrict = sample(1:ncol(x_train), 0.8*ncol(x_train))
x_train_restricted = x_train[, restrict]
x_test_restricted = x_test[, restrict]

model_restricted = tune.knn(x_train_restricted, y_train, k = 1:15)

restricted_tuned_prediction = knn(train = x_train_restricted, test = x_test_restricted, cl = y_train, k = model_restricted$best.parameters$k)
con(y_test, restricted_tuned_prediction)

############################################
# Zadanie 11


############################################
# Zadanie 12

set.seed(12345)
x=mlbench.spirals(1000,2,0.08)
plot(x)
Spirals=as.data.frame(x)

# losowanie na zbiór uczacy i testowy + podzial na zmienne objasniajace i objasniana
set.seed(1234)
spirals_train = sample(1:nrow(Spirals), 0.67*nrow(Spirals))

spirals_x_train = Spirals[spirals_train,1:2]
spirals_x_test = Spirals[-spirals_train,1:2]

spirals_y_train = Spirals[spirals_train,3]
spirals_y_test = Spirals[-spirals_train,3]

# znalezienie wektora maksimów z kolumn
spirals_x_train_abs = abs(spirals_x_train)
spirals_max_vector = as.vector(apply(spirals_x_train_abs, 2, max))

# normalizacja - uzywamy tego samego wektora
spirals_x_train = normalizacja1A(spirals_x_train, spirals_max_vector)
spirals_x_test = normalizacja1A(spirals_x_test, spirals_max_vector)

# model z k=10
spirals_prediction = knn(train = spirals_x_train, test = spirals_x_test, cl = spirals_y_train, k = 10)
con(spirals_y_test, spirals_prediction)

# model ze strojeniem parametru k
spirals_model = tune.knn(spirals_x_train, spirals_y_train, k = 1:15)

spirals_tuned_prediction = knn(train = spirals_x_train, test = spirals_x_test, cl = spirals_y_train, k = spirals_model$best.parameters$k)
con(spirals_y_test, spirals_tuned_prediction)