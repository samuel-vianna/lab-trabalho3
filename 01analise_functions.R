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

fill_color <- 'royalblue'

######################################################


######################################################

# gr�fico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(data, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(data, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}


# gr�fico de boxplot de acordo com o m�dico
boxplot_medico <- function(var) {
  ggplot(pacientes, aes(x=Medico, y=var)) + geom_boxplot(fill= fill_color) +
    theme_minimal() 
}



# fun��o para fazer tabelas
make_table <- function(table, align='c', booktabs=T){
  kable(table, align = align, booktabs=booktabs)
}

# fun�ao mapa medico
mapa_geral<-function(radius, blur){
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 12) %>% 
    addHeatmap(lng=~Longitude,lat=~Latitude,max=100,radius=radius,blur=blur)
}

# fun�ao mapa medico
mapa_medico<-function(){
  cof <- colorFactor(c("red", "blue", "orange",'black',"green","pink","purple"), domain=c("A", "B", "C","D","E","F","G"))
  medicos$Local <- factor(medicos$Local)
  new <- c("red3","blue","orange","black","green","pink","purple")[medicos$Local]
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'white',
    library = 'ion',
    markerColor = new
  )
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 12) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                     color=~cof(Medico), stroke = F, fillOpacity = 0.9)  %>%
    addLegend("bottomright", colors= c("red", "blue", "orange","black","green","pink","purple"), labels=c("A'", "B", "C","D","E","F","G"), title="Medico")%>%
    addAwesomeMarkers(lng = ~medicos$Longitude, lat=~medicos$Latitude, icon=icons,
                      popup = ~medicos$Local)
}

# fun�ao mapa medico
mapa_idade<-function(){
  cof <- colorFactor(c("red", "blue", "orange","black"), domain=c("20 a 40","40 a 60","acima de 60","menor que 20"))
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 12) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                     color=~cof(pacientes$categoria), stroke = F, fillOpacity = 0.9)  %>%
    addLegend("bottomright", colors= c("red", "blue", "orange"), labels=c("Menor que 40 anos","Entre 40 anos e 60 anos","Maior que 40 anos"), title="Idade")
}

# Fun��o grafico idade pelo medico
idade_medico<-function(medico){
  dados<-filter(pacientes, Medico == medico)
  ggplot(dados,aes(x=categoria))+
    geom_bar(fill="royalblue")+
    geom_text(aes(y=..count.., label=..count..), vjust=1.5, stat = 'count') + 
    labs(x="Categoria",y="Frequencia") +
    theme_minimal()
  
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
  geom_line() + theme(legend.position = 'bottom')
ggplotly(a, tooltip = c("x","y","group"))  
}


col<-function(var){
  data %>% ggplot(aes(x=colonias, y=var)) + geom_bar(stat='identity') +
    coord_flip() +
    theme_minimal() +
    geom_text(
      aes(label = stat(y)), 
      stat = 'summary', fun = sum, hjust = -0.5
    )
}