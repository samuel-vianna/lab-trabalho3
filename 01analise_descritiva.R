library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)
library(visdat)
library(plotly)
source('./00manipulando_dados.R')

##### verificando presenÃ§a de NA #####
vis_dat(data)

fill_color <- 'gold3'

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
    ggtitle('Grï¿½fico de dispersï¿½o') +
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
    geom_text(aes(x=3-0.5, y=max(variavel), label='Outono'), angle=90, hjust=1.5,colour='black') + 
    geom_text(aes(x=6-0.5, y=max(variavel), label='Inverno'), angle=90, hjust=1.5, colour='black') + 
    geom_text(aes(x=9-0.5, y=max(variavel), label='Primavera'), angle=90, hjust=1.5, colour='black') + 
    geom_text(aes(x=12-0.5, y=max(variavel), label='Verï¿½o'), angle=90, hjust=1.5, colour='black') 
  ggplotly(a, tooltip = c("x","y","group"))  
}


col<-function(var){
  data %>% ggplot(aes(x=colonias, y=var)) + geom_bar(stat='identity', fill=fill_color) +
    coord_flip() +
    theme_minimal() +
    geom_text(
      aes(label = stat(y)), 
      stat = 'summary', fun = sum, hjust = -0.5
    )
}

################ radar plot

# Set graphic colors
colors_border=c( rgb(0.2,0.5,0.5,0.9),  rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4),  rgb(0.7,0.5,0.1,0.4) )

normalizar <- function(val, min, max) (val - min) / (max - min)

normalizar_medias <- sapply(data[,3:9], range) %>%
  rbind(sapply(data[,3:9], function(x) ifelse(sd(x)/mean(x) < 1, mean(x), median(x)))) %>%
  as.data.frame()

media_geral <- sapply(normalizar_medias, function(x) normalizar(x[3], x[1], x[2]))

radar_plot <- function(colonia){
  
  media_colonia <- data %>%
    filter(colonias == colonia) %>%
    select(3:9) %>%
    sapply(., function(x) {
      normalizar(ifelse(sd(x)/mean(x) < 1, mean(x), median(x)),
                 min(x), max(x))
    })
  
  radar_plot <- data.frame(min=0, max=1, media_geral, media_colonia) %>% t() %>% as.data.frame()
  
  # plot with default options:
  radarchart( radar_plot  , axistype=1 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
              #custom labels
              vlcex=0.8 
  )
  
  legend(x=0.7, y=1.3, legend = c('geral', str_to_title(colonia)), bty = "n",
         pch=15 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
   
}
## tabela mediadas

medidas<-function(x,y){
  data %>%
    rename(grupo = x) %>%
    rename(resp = y) %>%
    group_by(grupo) %>%
    summarise(min = min(resp),
              max = max(resp),
              media = round(mean(resp), 3),
              median = round(median(resp), 3),
              var = round(var(resp), 3),
    ) %>% as.data.frame()  
}

## correlação

corre<-function(banco){
  ind_var <- sapply(banco, is.numeric)
  data[ind_var] %>%
  cor() %>% corrplot(method='color', type='lower',
                     addCoef.col = 'black', addgrid.col = 'black',
                     diag=F, tl.col = 'black')

}