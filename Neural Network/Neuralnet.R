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

##### Zadanie 7 #######################################

# tworzenie zbioru danych
data(iris)
dane=iris
set.seed(1234)
train=sample(1:nrow(dane), 0.67*nrow(dane))
daneTrain=dane[train,]
daneTest=dane[-train,]

# tworzenie modelu
hidden_layers = c(5, 3)
daneTrain.nn = cbind(daneTrain[,-ncol(daneTrain)], class.ind(as.factor(daneTrain$Species)))
independent_vars = names(daneTrain.nn)

# budowanie formuly
model.formula = as.formula(paste("setosa + versicolor + virginica ~",
                                 paste(independent_vars[!independent_vars %in%
                                                          c("setosa","versicolor", "virginica")],
                                       collapse = " + ")
                                 )
                           )
model.neuralnet = neuralnet(model.formula,
                            data = daneTrain.nn,
                            hidden = hidden_layers,
                            lifesign = "minimal")
plot(model.neuralnet)





