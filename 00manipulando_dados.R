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

# converting months
data$meses <- month.abb[data$meses]


###############################
write.table(data, './data/data.csv', sep=',')
