library(REdaS)
library(nFactors)
library(GPArotation)
library(ElemStatLearn)
library(class)
library(mlbench)
library(e1071)

# funkcja do normalizacji danych w oparciu o przeksztaL‚cenie ilorazowe z wektorem max dostarczonym zewnÄ™trznie jako argument
normalizacja1A = function(dane, maks.wektor) {
  return(t(apply(dane, 1, function(x){x/maks.wektor})))
}

# funkcja do tablicy kontyngencji:
con=function(x,y){
  tab=table(x,y)
  print(tab)
  cat("error rate = ", round(100*(1-sum(x==y)/length(x)),2),"%\n")
  invisible()
}

data(spam)

dane=spam
names(dane)[ncol(dane)] = 'y'

set.seed(1234)
train.complete=sample(1:nrow(dane), 0.67*nrow(dane))
daneTrain = dane[train.complete,]
daneTest = dane[-train.complete,]

daneTrainNorm=daneTrain; daneTestNorm=daneTest
maks.wektor=as.vector(apply(abs(daneTrain[,-ncol(daneTrain)]), 2, max))
daneTrainNorm[,-ncol(daneTrain)] = normalizacja1A(daneTrain[,-ncol(daneTrain)], maks.wektor)
daneTestNorm[,-ncol(daneTest)] = normalizacja1A(daneTest[,-ncol(daneTest)], maks.wektor)

kmos=KMOS(daneTrainNorm[,-ncol(daneTrainNorm)])

daneTrainNorm_after_kmos = daneTrainNorm[,kmos$MSA>0.5]
daneTestNorm_after_kmos = daneTestNorm[,kmos$MSA>0.5]

liczba_skladowych_tab = nScree(eigen(cor(daneTrainNorm_after_kmos[,-ncol(daneTrainNorm_after_kmos)]))$values)

liczba_skladowych = min(which(liczba_skladowych_tab$Analysis$Cumu>0.9))

PCA = prcomp(daneTrainNorm_after_kmos[,-ncol(daneTrainNorm_after_kmos)], rank. = liczba_skladowych)

daneTrainFinal = predict(PCA, newdata = daneTrainNorm_after_kmos[,-ncol(daneTrainNorm_after_kmos)])
daneTestFinal = predict(PCA, newdata = daneTestNorm_after_kmos[,-ncol(daneTestNorm_after_kmos)])

##### SVM #####
input.dataset = as.data.frame(cbind(daneTrainFinal, daneTrain$y))
names(input.dataset)[ncol(input.dataset)] = 'y'

set.seed(12345)
svm.tuning = tune.svm(y~.,
                      data = input.dataset,
                      kernel = "radial",
                      cost = c(0.01, 0.1, 1, 10, 100),
                      gamma = c(0.01, 0.1, 0.1, 1, 3, 5, 7),
                      scale = FALSE)
summary(svm.tuning)

# budowanie modelu
#model.svm = svm.tuning$best.model
#odel.svm$type= 'C-classification'

model = svm(y~.,
            data = input.dataset,
            kernel = "radial",
            cost = svm.tuning$best.parameters$cost,
            gamma = svm.tuning$best.parameters$gamma,
            type = 'C-classification')

# predykcja
pred.svm = predict(model, daneTestFinal)

con(pred.svm, daneTest$y)
