##### POMOCE ########################################

# potrzebne pakiety
library(pROC)
library(class)
library(mlbench)
library(e1071)

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

# wczytywanie zbioru danych

dane = read.csv(file="credit_rejection.csv")
dane[,1] = 1-dane[,1]

# dzielenie na podzbiory

set.seed(1234)
train.complete=sample(1:nrow(dane), 0.67*nrow(dane))
daneTrain = dane[train.complete,]
daneTest = dane[-train.complete,]

daneTest.bez.y=NULL
daneTest.bez.y= subset(daneTest, select=-y)

# normalizacja

daneTrainNorm=daneTrain; daneTestNorm=daneTest
maks.wektor=as.vector(apply(abs(daneTrain[,-ncol(daneTrain)]), 2, max))
daneTrainNorm[,-ncol(daneTrain)] = normalizacja1A(daneTrain[,-ncol(daneTrain)], maks.wektor)
daneTestNorm[,-ncol(daneTest)] = normalizacja1A(daneTest[,-ncol(daneTest)], maks.wektor)

daneTestNorm.bez.y = daneTestNorm[,-1]
daneTrainNorm.bez.y = daneTrainNorm[,-1]

##### lda #####
library(MASS)
model_lda = lda(y~.,
                data = daneTrainNorm)

pred_lda = predict(model_lda, daneTestNorm.bez.y)$class
con(pred_lda, daneTest[,1], dnn = c("empirical","prediction"))

print("LDA:")
cat("AUC: ", roc(daneTest[,1], as.numeric(pred_lda))$auc)
cat("Sensitivity: ", roc(daneTest[,1], as.numeric(pred_lda))$sensitivities[2])
cat("Specificity: ", roc(daneTest[,1], as.numeric(pred_lda))$specificities[2])

##### kNN #####

tuning_knn = tune.knn(daneTrainNorm.bez.y, as.factor(daneTrainNorm$y), k = 1:15)
summary(tuning_knn)
model_knn = tuning_knn$best.model

pred_knn = knn(train = daneTrainNorm.bez.y, cl = daneTrainNorm[,1], test = daneTestNorm.bez.y, k = model_knn$k)
con(pred_knn, daneTest[,1], dnn = c("empirical","prediction"))

print("kNN:")
cat("AUC: ", roc(daneTest[,1], as.numeric(pred_knn))$auc)
cat("Sensitivity: ", roc(daneTest[,1], as.numeric(pred_knn))$sensitivities[2])
cat("Specificity: ", roc(daneTest[,1], as.numeric(pred_knn))$specificities[2])

##### Random Forest #####

vars_num = ncol(daneTrainNorm)
temp_dataset = as.data.frame(cbind(as.factor(daneTrainNorm$y), daneTrainNorm[,-1]))
names(temp_dataset)[1] = 'y'

tuning_RF = tune.randomForest(y~.,
                              data = temp_dataset,
                              ntree = c(100,200),
                              mtry = c(round(sqrt(vars_num)/2), round(sqrt(vars_num)), round(2*sqrt(vars_num))))
summary(tuning_RF)
model_RF = tuning_RF$best.model

pred_RF = predict(model_RF, daneTestNorm.bez.y)
con(pred_RF, daneTest[,1], dnn = c("empirical","prediction"))

print("RF:")
cat("AUC: ", roc(daneTest[,1], as.numeric(pred_knn))$auc)
cat("Sensitivity: ", roc(daneTest[,1], as.numeric(pred_RF))$sensitivities[2])

##### SVM #####

temp_dataset = as.data.frame(cbind(as.factor(daneTrainNorm$y), daneTrainNorm[,-1]))
names(temp_dataset)[1] = 'y'

tuning_SVM = tune.svm(y~.,
                  data = daneTrainNorm,
                  cost = c(0.1, 1, 10, 100),
                  gamma = c(0.01, 0.1, 1, 3, 5),
                  kernel = "radial")
summary(tuning_SVM)
model_SVM = tuning_SVM$best.model

pred_SVM = predict(model_RF, daneTestNorm.bez.y)
con(pred_SVM, daneTest[,1], dnn = c("empirical","prediction"))

print("RF:")
cat("AUC: ", roc(daneTest[,1], as.numeric(pred_knn))$auc)
cat("Sensitivity: ", roc(daneTest[,1], as.numeric(pred_RF))$sensitivities[2])
cat("Specificity: ", roc(daneTest[,1], as.numeric(pred_RF))$specificities[2])
cat("Specificity: ", roc(daneTest[,1], as.numeric(pred_RF))$specificities[2])