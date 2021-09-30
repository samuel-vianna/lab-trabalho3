library(readxl)
library(dplyr)
library(stringr)

##### reading the data #####
data <- read_xlsx('./data/Dados abelhas.xlsx', sheet = 1)

# renaming columns
names(data) <- c(
  'meses', 'colonias', 'comp_canudo', 'diam_canudo',
  'n_potes_mel', 'n_discos', 'tam_discos', 'peso', 'est_pop'
)

data$meses <- factor(month.abb[data$meses], levels = month.abb)
data$colonias <- factor(data$colonias, levels = paste('COL', c(1:13, 15:17)))

data$estacao<-case_when(
  data$meses %in% month.abb[1:3] ~ 'Verao' ,
  data$meses %in% month.abb[4:6] ~ 'Outono' ,
  data$meses %in% month.abb[7:9] ~ 'Inverno' ,
  TRUE ~ 'Primavera'
)

data$peso_100_abelhas <- (data$peso / (data$est_pop / 100))  

summary(data$est_pop / 1000)  

###############################
write.table(data, './data/data.csv', sep=',')

