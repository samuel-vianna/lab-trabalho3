library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(corrplot)
library(leaflet)
library(readxl)
library(nortest)
library(stringr)
source('./00manipulando_dados.R')

######################################################

fill_color <- 'gold3'

######################################################


# funï¿½ï¿½o para fazer tabelas
make_table <- function(table, align='c', booktabs=T){
  kable(table, align = align, booktabs=booktabs)
}

####### teste de normalidade #######

teste_normal <- function(var, round_num=6){
shapiro <- shapiro.test(var)
ad <- ad.test(var)
cvm <- cvm.test(var)
lillie <- lillie.test(var)

rbind(shapiro,ad,cvm, lillie) %>% as.data.frame(row.names = F) %>%
select(-c(statistic, data.name)) %>%
relocate(method) %>%
mutate(method = str_replace_all(method, 'normality test', '')) %>%
mutate(decisao = ifelse(p.value > 0.05, 'Não rejeita H0', 'Rejeita H0')) %>%
mutate(p.value = round(as.numeric(p.value), round_num)) %>%
return()
}
