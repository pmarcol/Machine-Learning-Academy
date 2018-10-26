############################################
# POMOCE


# funkcja do tablicy kontyngencji:
con=function(x,y){
  tab=table(x,y)
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}

############################################
# Zadanie 1

# zaladowanie biblioteki pod R:
library(class)
library(mlbench)
library(e1071)

############################################
# Zadanie 2

set.seed(12345)
x=mlbench.spirals(1000,2,0.08)
plot(x)
dane=as.data.frame(x)

# Zmiana nazwy ostatniej kolumny
names(dane)[length(names(dane))] = 'y'

############################################
# Zadanie 3

set.seed(1234)
# dzielenie na zbiór uczacy i testowy
train=sample(1:nrow(dane), 0.67*nrow(dane))

dane_train = dane[train,]
dane_test = dane[-train,]

# budowanie modelu
model = svm(formula = y~.,
            data = dane_train,
            kernel = "radial",
            cost = 0.1,
            gamma = 0.5)

y_pred = predict(model, dane_test[,1:2])
con(y_pred, dane_test[,3])

############################################
# Zadanie 4

# tuning = tune.svm(y~.,
#                        data = dane_train,
#                        cost = c(0.01, 0.1, 1, 10, 100, 1000),
#                        gamma = c(0.01, 0.1, 0.1, 1, 3, 5, 7),
#                        kernel = "radial")
# 
# summary(tuning)
# 
# model_tuned = svm(formula = y~.,
#                   data = dane_train,
#                   kernel = "radial",
#                   cost = tuning$best.parameters$cost,
#                   gamma = tuning$best.parameters$gamma)
# y_pred_tuned = predict(model_tuned, dane_test[,1:2])
# con(y_pred_tuned, dane_test[,3])

############################################
# Zadanie 5

# # tworzenie zbioru danych
# data("Satellite")
# x = Satellite
# dane_satellite = as.data.frame(x)
# names(dane_satellite)[length(names(dane_satellite))] = 'y'
# 
# # dzielenie na zbiór uczacy i testowy
# set.seed(1234)
# train_sat=sample(1:nrow(dane_satellite), 0.67*nrow(dane_satellite))
# dane_satellite_train = dane_satellite[train_sat,]
# dane_satellite_test = dane_satellite[-train_sat,]
# 
# # tuning modelu
# tuning_sat = tune.svm(y~.,
#                   data = dane_satellite_train,
#                   cost = c(0.01, 0.1, 1, 10, 100),
#                   gamma = c(0.01, 0.1, 0.1, 1, 3, 5, 7),
#                   kernel = "radial")
# summary(tuning_sat)
# 
# model_sat = svm(formula = y~.,
#                 data = dane_satellite_train,
#                 kernel = "radial",
#                 cost = tuning_sat$best.parameters$cost,
#                 gamma = tuning_sat$best.parameters$gamma)
# 
# y_satellite_pred = predict(model_sat, dane_satellite_test[,-ncol(dane_satellite_test)])
# con(y_satellite_pred, dane_satellite_test[,ncol(dane_satellite_test)])

############################################
# Zadanie 6

dane_train_disturbed = dane[train,]
set.seed(123)
disturb_pointers = sample(1:nrow(dane_train_disturbed), 0.3*nrow(dane_train_disturbed))

# zaburzenie 
for (i in disturb_pointers){
  dane_train_disturbed$y[i] = as.factor(3 - as.numeric(dane_train_disturbed$y[i]))
}

# tuning 
tuning_disturbed = tune.svm(y~.,
                       data = dane_train_disturbed,
                       cost = c(0.01, 0.1, 1, 10, 100),
                       gamma = c(0.01, 0.1, 0.1, 1, 3, 5, 7),
                       kernel = "radial")

summary(tuning_disturbed)

model_disturb_tuned = svm(formula = y~.,
                  data = dane_train_disturbed,
                  kernel = "radial",
                  cost = tuning_disturbed$best.parameters$cost,
                  gamma = tuning_disturbed$best.parameters$gamma)
y_disturbed_pred_tuned = predict(tuning_disturbed$best.model, dane_test[,1:2])
con(y_disturbed_pred_tuned, dane_test[,3])
