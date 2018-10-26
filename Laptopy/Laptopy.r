dane = read.csv2("E:/Machine Learning FPAcademy/Laptopy/laptopy.csv",
                  row.names = 1,
                  encoding = "utf-8")

wagi = c(0.3, 0.15, 0.1, 0.1, 0.2, 0.05, 0.1)

### bez sprowadzenia do porównywalności, 

wzorzec = c(min(dane$Cena),
            max(dane$RAM),
            max(dane$Wielkosc.dysku),
            15,
            max(dane$Czas.na.baterii),
            max(dane$Pojemnosc.baterii..Wh.),
            min(dane$Ciezar))

odl = c()

for (i in 1:nrow(dane)){
  odl[i] = sqrt(sum((dane[i,] - wzorzec)^2*wagi))
}
row.names(dane)[order(odl)]

### przekształcenie ilorazowe, metryka euklidesowa

max_features = apply(dane, 2, max)

dane_ilor = t(apply(dane, 1, function(x){x/max_features}))

wzorzec_ilor = c(min(dane_ilor[,1]),
            max(dane_ilor[,2]),
            max(dane_ilor[,3]),
            15/max(dane[,4]),
            max(dane_ilor[,5]),
            max(dane_ilor[,6]),
            min(dane_ilor[,7]))

odl_ilor_euk = c()

for (i in 1:nrow(dane)){
  odl_ilor_euk[i] = sqrt(sum((dane_ilor[i,] - wzorzec_ilor)^2*wagi))
}
row.names(dane)[order(odl_ilor_euk)]

### przekształcenie ilorazowe, metryka miejska

odl_ilor_manhattan = c()

for (i in 1:nrow(dane_ilor)){
  odl_ilor_manhattan[i] = sum(abs(dane_ilor[i,] - wzorzec_ilor)*wagi)
}
row.names(dane)[order(odl_ilor_manhattan)]

### przekształcenie ilorazowe, metryka czebyszewa

odl_ilor_czebyszew = c()

for (i in 1:nrow(dane_ilor)){
  odl_ilor_czebyszew[i] = max(abs(dane_ilor[i,] - wzorzec_ilor)*wagi)
}
row.names(dane)[order(odl_ilor_czebyszew)]

### standaryzacja danych

mean_features = apply(dane, 2, mean)
sd_features = apply(dane, 2, sd)

dane_stand = t(apply(dane, 1, function(x){(x - mean_features)/sd_features}))

wzorzec_stand = c(min(dane_stand[,1]),
                  max(dane_stand[,2]),
                  max(dane_stand[,3]),
                  (15-mean_features[4])/sd_features[4],
                  max(dane_stand[,5]),
                  max(dane_stand[,6]),
                  min(dane_stand[,7]))

### standaryzacja, metryka euklidesowa

odl_stand_euk = c()

for (i in 1:nrow(dane_ilor)){
  odl_stand_euk[i] = sqrt(sum((dane_stand[i,] - wzorzec_stand)^2*wagi))
}
row.names(dane)[order(odl_stand_euk)]

### unitaryzacja danych

mean_features = apply(dane, 2, mean)
r_features = apply(dane, 2, function(x){max(x) - min(x)})

dane_unit = t(apply(dane, 1, function(x){(x - mean_features)/r_features}))

wzorzec_unit = c(min(dane_unit[,1]),
                  max(dane_unit[,2]),
                  max(dane_unit[,3]),
                  (15-mean_features[4])/r_features[4],
                  max(dane_unit[,5]),
                  max(dane_unit[,6]),
                  min(dane_unit[,7]))

### unitaryzacja, metryka euklidesowa

odl_unit_euk = c()

for (i in 1:nrow(dane_ilor)){
  odl_unit_euk[i] = sqrt(sum((dane_unit[i,] - wzorzec_unit)^2*wagi))
}
row.names(dane)[order(odl_unit_euk)]