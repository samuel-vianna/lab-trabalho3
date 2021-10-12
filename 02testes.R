library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)
library(readxl)
library(mblm)
source('./00manipulando_dados.R')
#SO TRISTEZA 
fit0<-lm(n_potes_mel~comp_canudo,data)
fit1<-mblm(n_potes_mel~comp_canudo,data)
summary(fit1)
par(mfrow = c(1,1), mar = c(4, 5, 2, 1), pch = 19)
plot(data$n_potes_mel~data$comp_canudo, pch = 19)
abline(fit1, lty = 1, lwd = 2, col = "red")
text(4.5,4, "Modelo Theil-Kendall", col = "red")
abline(fit0, lty = 1, lwd = 2, col = "blue")
text(4,8, "Modelo de regressão linear", col = "blue")

# coeficiente de variação

data$cat_comp_canudo<-case_when(
  data$comp_canudo <= 6.314583 ~ 'Pequeno',
  TRUE ~ 'Grande'
)
data$cat_diam_canudo<-case_when(
  data$diam_canudo <= 4 ~ 'Pequeno',
  TRUE ~ 'Grande'
)
data$cat_n_potes_mel<-case_when(
  data$n_potes_mel <= 23.265625 ~ 'Pequena',
  TRUE ~ 'Grande'
)
data$cat_n_discos<-case_when(
  data$n_discos <= 11.1510417 ~ 'Pequena',
  TRUE ~ 'Grande'
)
data$cat_tam_discos<-case_when(
  data$tam_discos <= 6.5000000 ~ 'Pequeno',
  TRUE ~ 'Grande'
)
data$cat_peso<-case_when(
  data$peso <= 5.1 ~ 'Leve',
  TRUE ~ 'Pesado'
)
data$cat_est_pop<-case_when(
  data$est_pop <= 2970.8531250 ~ 'Pequena',
  TRUE ~ 'Grande'
)
data$cat_peso_100_abelhas<-case_when(
  data$peso_100_abelhas <= 0.1902503 ~ 'Leve',
  TRUE ~ 'Pesado'
)
