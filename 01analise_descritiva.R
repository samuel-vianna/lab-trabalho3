library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)
library(visdat)
library(plotly)
library(EnvStats)
source('./00manipulando_dados.R')

##### verificando presenÃƒÂ§a de NA #####
vis_dat(data)

fill_color <- '#186818'

# grafico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(data, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(data, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}

# grafico de dispersao

grafico_disp <- function(data, x, y){
  x_var <- unlist(data[,x])
  y_var <- unlist(data[,y])
  
  data %>% ggplot(aes(x=x_var, y = y_var)) + geom_jitter(width = 0.1) +
    ggtitle('Gráfico de dispersão') +
    stat_summary(fun=mean, geom='line', aes(y= y_var, group=1), col='red', lwd=0.8) +
    stat_summary(fun=mean, geom='point', col='blue', size=2) +
    stat_summary(fun=median, geom='line', aes(y= y_var, group=1), col='gold3', lwd=0.8) +
    stat_summary(fun=median, geom='point', col='blue', size=2) +
    labs(x=x,y=y) + theme_minimal()
}


####### grafico ggplotly #######
graf_linha<-function(variavel){
  a <-ggplot(data,aes(x=meses, y=variavel,col=colonias, group=colonias)) + 
    geom_line() + theme(legend.position = 'bottom') + theme_minimal() + 
    geom_vline(xintercept = which(levels(data$meses) %in% c('Mar', 'Jun','Sep', 'Dec')),
               size=1, col='gray', alpha=0.5 ) +
    geom_text(aes(x=3-0.5, y=max(variavel), label='Verão'), angle=90, hjust=1.5, colour='black') +
    geom_text(aes(x=6-0.5, y=max(variavel), label='Outono'), angle=90, hjust=1.5,colour='black') + 
    geom_text(aes(x=9-0.5, y=max(variavel), label='Inverno'), angle=90, hjust=1.5, colour='black') + 
    geom_text(aes(x=12-0.5, y=max(variavel), label='Primavera'), angle=90, hjust=1.5, colour='black')
  ggplotly(a, tooltip = c("x","y","group"))  
}


## correlaÃ§Ã£o

corre<-function(banco){
  ind_var <- sapply(banco, is.numeric)
  data[ind_var] %>%
  cor(method='spearman') %>% corrplot(method='color', type='lower', 
                     addCoef.col = 'black', addgrid.col = 'black',
                     diag=F, tl.col = 'black')
}
