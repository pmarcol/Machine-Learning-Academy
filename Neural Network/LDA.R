##### POMOCE ########################################

# funkcja do tablicy kontyngencji:
con=function(x,y,dnn=c()){ 	# zmodyfikowane - na bazie ksiąLzki Modern Applied Statistics with S-Plus Ripley’a s.493
  tab=table(x,y,dnn=dnn)		#tablica kontyngencji i bL�ąd klasyfikacji
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}

# funkcja do normalizacji danych w oparciu o przeksztaL�cenie ilorazowe z wektorem max dostarczonym zewnętrznie jako argument
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

# wczytywanie zbioru danych
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

daneTrainNorm=daneTrain; daneTestNorm=daneTest
maks.wektor=as.vector(apply(abs(daneTrain[,-ncol(daneTrain)]), 2, max))
daneTrainNorm[,-ncol(daneTrain)] = normalizacja1A(daneTrain[,-ncol(daneTrain)], maks.wektor)
daneTestNorm[,-ncol(daneTest)] = normalizacja1A(daneTest[,-ncol(daneTest)], maks.wektor)

daneTrainNorm.bez.y=NULL
daneTrainNorm.bez.y= subset(daneTrainNorm, select=-y)
daneTestNorm.bez.y=NULL
daneTestNorm.bez.y= subset(daneTestNorm, select=-y)

# budowanie modelu
library(MASS)
model = lda(y~.,
            data = daneTrainNorm)

pred_test = predict(model, daneTestNorm.bez.y)$class

con(pred_test, daneTest[, ncol(daneTest)])