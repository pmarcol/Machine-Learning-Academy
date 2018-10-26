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

#### Pojedyncze drzewo
# drostrajanie modelu
tuning_tree = tune.rpart(formula = y~.,
                            data = dane_spirals_train,
                            cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                            minsplit = c(1, 2, 6, 9, 12))
summary(tuning_tree)
# budowanie modelu
classifier_tree = tuning_tree$best.model
# predykcja
pred_tree = predict(classifier_tree, dane_spirals_test[,-ncol(dane_spirals_test)], type = 'class')

### bagging
# parametry do sprawdzenia
best_params = c(1, 0, 0, 0) # (error, ntrees, cp, minsplit)
ntrees_list = c(100, 200)
cp_list = c(0.2, 0.1, 0.05, 0.01, 0.001)
minsplit_list = c(1, 2, 6, 9, 12)
for(ntrees_val in ntrees_list){  
  for(cp_val in cp_list){
    for(minsplit_val in minsplit_list){
      cat(sprintf("number of trees: %d, cp: %.2f, min_split: %d\n", ntrees_val, cp_val, minsplit_val))
      set.seed(123)
      bagging_classifier = bagging.cv(formula = y~.,
                                      data = dane_spirals_train,
                                      v=3,
                                      mfinal = ntrees_val,
                                      control = rpart.control(cp = cp_val,
                                                              minsplit = minsplit_val))
      if(bagging_classifier$error < best_params[1]){
        best_params = c(bagging_classifier$error, ntrees_val, cp_val, minsplit_val)
      }
    } 
  }
}
print("Best parameters:")
cat(sprintf("min_error: %.2f, number of trees: %d, cp: %.2f, min_split: %d\n", best_params[1], best_params[2], best_params[3], best_params[4]))

# best: ntrees: 100, cp: 0.001, min_split: 1

bagging_classifier = bagging(formula = y~.,
                             data = dane_spirals_train,
                             v=3,
                             mfinal = 100,
                             control = rpart.control(cp = 0.001,
                                                     minsplit = 1))

pred = predict.bagging(bagging_classifier, dane_spirals_test[,-ncol(dane_spirals_test)], type = 'class')

con(pred$class, dane_spirals_test[, ncol(dane_spirals_test)])