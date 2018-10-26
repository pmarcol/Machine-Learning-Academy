dane_tmp = read.csv2("E:/Machine Learning FPAcademy/kraje Unii/krajeunii2016.csv",
                 row.names = 1,
                 encoding = "UTF-8")

dane_2016 = dane_tmp[, c(-4, -11)]

dane_tmp = read.csv2("E:/Machine Learning FPAcademy/kraje Unii/krajeunii2004.csv",
                     row.names = 1,
                     encoding = "UTF-8")

dane_2004 = dane_tmp[, c(-4, -11)]

wagi = rep(1, times = ncol(dane_2016))

standardizeData = function(d){
  mean_features = apply(d, 2, mean)
  sd_features = apply(d, 2, sd)
  
  return( t(apply(d, 1, function(x){(x - mean_features)/sd_features})) )
}

standardize_row = function(row, avgs, stds){
  return((row - avgs)/stds)
}

manhattanMetric = function(x, y, wghts){
  return (sum(abs(x - y)*wghts))
}

dane_std_2016 = standardizeData(dane_2016)
mean_features_2016 = apply(dane_2016, 2, mean)
sd_features_2016 = apply(dane_2016, 2, sd)

wzorzec_2016 = c(max(dane_2016[,1]),
            4.5,
            min(dane_2016[,3]),
            max(dane_2016[,4]),
            max(dane_2016[,5]),
            max(dane_2016[,6]),
            min(dane_2016[,7]),
            min(dane_2016[,8]),
            max(dane_2016[,9]))

wzorzec_2016_std = standardize_row(wzorzec_2016, mean_features_2016, sd_features_2016)

dane_std_2004 = standardizeData(dane_2004)
mean_features_2004 = apply(dane_2004, 2, mean)
sd_features_2004 = apply(dane_2004, 2, sd)

wzorzec_2004 = c(max(dane_2004[,1]),
                 4.5,
                 min(dane_2004[,3]),
                 max(dane_2004[,4]),
                 max(dane_2004[,5]),
                 max(dane_2004[,6]),
                 min(dane_2004[,7]),
                 min(dane_2004[,8]),
                 max(dane_2004[,9]))

wzorzec_2004_std = standardize_row(wzorzec_2004, mean_features_2004, sd_features_2004)

odl_stand_euk_2016 = c()

for (i in 1:nrow(dane_std_2016)){
  odl_stand_euk_2016[i] = manhattanMetric(dane_std_2016[i,], wzorzec_2016_std, wagi)
}
row.names(dane_2016)[order(odl_stand_euk_2016)]
 
odl_stand_euk_2004 = c()
 
for (i in 1:nrow(dane_std_2004)){
   odl_stand_euk_2004[i] = manhattanMetric(dane_std_2004[i,], wzorzec_2004_std, wagi)
}
row.names(dane_2004)[order(odl_stand_euk_2004)]

cor(rank(odl_stand_euk_2016), rank(odl_stand_euk_2004), method = "kendall")