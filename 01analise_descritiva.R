library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)
library(visdat)
library(plotly)
source('./00manipulando_dados.R')

##### verificando presença de NA #####
vis_dat(data)


a <- data %>% ggplot(aes(x=meses, y=n_potes_mel, col=colonias, group=colonias)) + 
  geom_line() + theme(legend.position = 'bottom')


ggplotly(a)