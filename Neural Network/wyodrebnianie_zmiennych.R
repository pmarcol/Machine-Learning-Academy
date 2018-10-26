##### POMOCE ########################################

# funkcja do tablicy kontyngencji:
con=function(x,y,dnn=c()){ 	# zmodyfikowane - na bazie ksiÄ…Lzki Modern Applied Statistics with S-Plus Ripleyâ€™a s.493
  tab=table(x,y,dnn=dnn)		#tablica kontyngencji i bL‚Ä…d klasyfikacji
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}

# funkcja do normalizacji danych w oparciu o przeksztaL‚cenie ilorazowe z wektorem max dostarczonym zewnÄ™trznie jako argument
normalizacja1A = function(dane, maks.wektor) {
  return(t(apply(dane, 1, function(x){x/maks.wektor})))
}

# zaladowanie biblioteki pod R:
library(class)
library(mlbench)
library(e1071)
library(nnet)
library(neuralnet)
library(ElemStatLearn)

##### Zadanie 11 ######################################

warstwy.ukryte = c(20,15,10)

data(spam)
nrow(spam)
names(spam)

dane=spam

names(dane)[length(names(dane))] = 'y'
names(dane)

set.seed(1234)
train.complete=sample(1:nrow(dane), 0.67*nrow(dane))
daneTrain = dane[train.complete,]
daneTest = dane[-train.complete,]

daneTest.bez.y=NULL
daneTest.bez.y= subset(daneTest, select=-y)

##### Sieci neuronowe z wieloma warstwami ukrytymi: #####

# normalizacja zmiennych:
daneTrainNorm=daneTrain; daneTestNorm=daneTest
maks.wektor=as.vector(apply(abs(daneTrain[,-ncol(daneTrain)]), 2, max))
daneTrainNorm[,-ncol(daneTrain)] = normalizacja1A(daneTrain[,-ncol(daneTrain)], maks.wektor)
daneTestNorm[,-ncol(daneTest)] = normalizacja1A(daneTest[,-ncol(daneTest)], maks.wektor)

model.neuralnet=NULL
pred.test=NULL
daneTrainNorm.bez.y=NULL
daneTrainNorm.bez.y= subset(daneTrainNorm, select=-y)
daneTestNorm.bez.y=NULL
daneTestNorm.bez.y= subset(daneTestNorm, select=-y)

# przeksztalcamy zmienna zalezna na kilka zmiennych wskaznikowych, wskazujacych klase obiektu:
daneTrainNorm.nn = cbind(daneTrainNorm[,-ncol(daneTrainNorm)], class.ind(as.factor(daneTrainNorm$y)))
names(daneTrainNorm.nn) = c(names(daneTrainNorm[,-ncol(daneTrainNorm)]), "email", "spam")
head(daneTrainNorm.nn) 

# niestety funkcja neuralnet nie obsluguje zapisu modelu z kropka zastepujaca wszystkie zmienne objasniajace;
# trzeba sobie inaczej poradzic
nazwy.zm = names(daneTrainNorm.nn)
model.formula=as.formula(paste("email + spam ~", paste(nazwy.zm[!nazwy.zm %in% c("email", "spam")], collapse = " + ")))
print(model.formula)

model.neuralnet=neuralnet(model.formula, data=daneTrainNorm.nn, hidden = warstwy.ukryte, learningrate=2.5, lifesign = "full", threshold = 0.05)
prediction(model.neuralnet)
plot(model.neuralnet, rep="best")

print("Ocena jakosci modelu na zbiorze testowym dla modelu sieci:", quote=0)

pred.test=compute(model.neuralnet, daneTestNorm.bez.y)
str(pred.test)
str(pred.test$neurons)
##### wyodrebnianie ostaniej warstwy ukrytej - zmienne objasniajace do zadania 11: ####
# wyciagniecie ostatniej warstwy 10 zmiennych dla zbioru uczacego
pred.train=compute(model.neuralnet, daneTrainNorm.bez.y) 
X.TrainNorm.new = as.data.frame(pred.train$neurons[[4]][,-1])
# wyciagniecie ostatniej warstwy 10 zmiennych dla zbioru testowego
X.TestNorm.new = as.data.frame(pred.test$neurons[[4]][,-1])

# pierwsza kolumna w pred.test$net.result to klasa 'emaile', a druga to 'spam'
pred.class=c()
pred.class=rep("email", times=nrow(pred.test$net.result))
pred.class[round(pred.test$net.result[,2])>=1]='spam'
table(pred.class)
table(daneTestNorm$y)

print("Wyniki i blad klasyfikacji na zbiorze testowym dla modelu sieci:", quote=0)
con(daneTestNorm$y, pred.class, dnn=c("rzeczywiste", "z modelu"))

##### zbudowanie drzewa na zmiennych z ostatniej warstwy sieci #####

# stworzenie calego zbioru uczacego, tzn. doklejenie kolumny 'y'
X.TrainNorm.new.z.y = as.data.frame(cbind(X.TrainNorm.new, daneTrain$y))
names(X.TrainNorm.new.z.y)[length(names(X.TrainNorm.new.z.y))] = 'y'

library(rpart)

# dostrajanie modelu
tuning_rpart_nnet = tune.rpart(formula = y~.,
                    data = X.TrainNorm.new.z.y,
                    cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                    minsplit = c(1, 2, 6, 9, 12))
summary(tuning_rpart_nnet)

# budowanie modelu
classifier_rpart_nnet_tuned = tuning_rpart_nnet$best.model

# predykcja
rpart_nnet_tuned_pred = predict(classifier_rpart_nnet_tuned, X.TestNorm.new, type = 'class')

# macierz kontyngencji
con(rpart_nnet_tuned_pred, dane_test[, ncol(dane_test)])

##### zbudowanie drzewa na surowych danych - dla porównania #####

# dostrajanie modelu
tuning_rpart_raw = tune.rpart(formula = y~.,
                               data = daneTrain,
                               cp = c(0.2, 0.1, 0.05, 0.01, 0.001),
                               minsplit = c(1, 2, 6, 9, 12))
summary(tuning_rpart_raw)

# budowanie modelu
classifier_rpart_raw_tuned = tuning_rpart_raw$best.model

# predykcja
rpart_raw_tuned_pred = predict(classifier_rpart_raw_tuned, daneTest.bez.y, type = 'class')

# macierz kontyngencji
con(rpart_raw_tuned_pred, dane_test[, ncol(dane_test)])
##### zbudowanie RandomForest na zmiennych z ostatniej warstwy sieci #####

library(randomForest)

# dostrajanie modelu
vars_num = ncol(X.TrainNorm.new)
tuning_RF_nnet = tune.randomForest(y~.,
                               data = X.TrainNorm.new.z.y,
                               ntree = c(100, 150, 200),
                               mtry = c(round(sqrt(vars_num)/2), round(sqrt(vars_num)), round(2*sqrt(vars_num))))
summary(tuning_RF_nnet)

# budowanie modelu
classifier_RF_nnet_tuned = tuning_RF_nnet$best.model

# predykcja
RF_nnet_tuned_pred = predict(classifier_RF_nnet_tuned, X.TestNorm.new, type = 'class')

# macierz kontyngencji
con(RF_nnet_tuned_pred, daneTest[, ncol(daneTest)])

##### zbudowanie drzewa na surowych danych - dla porównania #####

# dostrajanie modelu
tuning_RF_raw = tune.randomForest(y~.,
                              data = daneTrain,
                              ntree = c(100, 150, 200),
                              mtry = c(round(sqrt(vars_num)/2), round(sqrt(vars_num)), round(2*sqrt(vars_num))))
summary(tuning_RF_raw)

# budowanie modelu
classifier_RF_raw_tuned = tuning_RF_raw$best.model

# predykcja
RF_raw_tuned_pred = predict(classifier_RF_raw_tuned, daneTest.bez.y, type = 'class')

# macierz kontyngencji
con(RF_raw_tuned_pred, daneTest[, ncol(daneTest)])