dane=read.csv2("http://web2.ue.katowice.pl/trzesiok/FP/3edycjaszkolenML/diagnozaspoleczna2013.csv")

library(e1071)

gamma_par = c(0.01, 0.5, 1, 5)
nu_par = c(0.001, 0.005, 0.01, 0.05)

final = rep(0, nrow(dane))

for(gamma_val in gamma_par){
  for(nu_val in nu_par){
    model = svm( ~ .,
                 data = dane,
                 type = "one-classification",
                 gamma = gamma_val,
                 nu = nu_val,
                 kernel = "radial")
    typical = predict(model, dane)
    outSVC = which(typical==FALSE)
    for(i in outSVC){
      final[i] = final[i] + 1
    }
  }
}

ranking = cbind(order(-final), final[order(-final)])
#sum(ranking[,2]==9)