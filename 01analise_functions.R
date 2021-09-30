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


######################################################

# grï¿½fico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(data, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(data, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}


# funï¿½ï¿½o para fazer tabelas
make_table <- function(table, align='c', booktabs=T){
  kable(table, align = align, booktabs=booktabs)
}


# fun??o para testar normalidade

#teste_normal <- function(var){
 # shapiro <- shapiro.test(var)
  #ad <- ad.test(var)
  #cvm <- cvm.test(var)
  #lillie <- lillie.test(var)
  
  #rbind(shapiro,ad,cvm, lillie) %>% as.data.frame(row.names = F) %>%
   # select(-c(statistic, data.name)) %>%
    #relocate(method) %>% 
    #mutate(method = str_replace_all(method, 'normality test', '')) %>% 
    #mutate(decis?o = ifelse(p.value > 0.05, 'N?o rejeita H0', 'Rejeita H0')) %>%
    #return()
#}

#teste_normal(rnorm(1000))

#grafico ggplotly#####
graf_linha<-function(variavel){
a <-ggplot(data,aes(x=meses, y=variavel,col=colonias, group=colonias)) + 
  geom_line() + theme(legend.position = 'bottom') + theme_minimal() + 
  geom_vline(xintercept = which(levels(data$meses) %in% c('Mar', 'Jun','Sep', 'Dec')),
             size=1, col='gray', alpha=0.5 ) +
  geom_text(aes(x=3-0.5, y=max(variavel), label='Outono'), angle=90, hjust=1.5,colour='black') + 
  geom_text(aes(x=6-0.5, y=max(variavel), label='Inverno'), angle=90, hjust=1.5, colour='black') + 
  geom_text(aes(x=9-0.5, y=max(variavel), label='Primavera'), angle=90, hjust=1.5, colour='black') + 
  geom_text(aes(x=12-0.5, y=max(variavel), label='Verão'), angle=90, hjust=1.5, colour='black') 
 ggplotly(a, tooltip = c("x","y","group"))  
}

graf_linha(data$n_potes_mel)

col<-function(var){
  data %>% ggplot(aes(x=colonias, y=var)) + geom_bar(stat='identity', fill=fill_color) +
    coord_flip() +
    theme_minimal() +
    geom_text(
      aes(label = stat(y)), 
      stat = 'summary', fun = sum, hjust = -0.5
    )
}
