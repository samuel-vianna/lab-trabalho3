library(readxl)
library(tidyverse)

library(visdat)
library(gridExtra)

##### reading the data #####
data <- read_xlsx('./data/Distocia.xlsx', sheet = 1, skip = 1, na = 'NA')

data <- as.data.frame(data)

data[,18:22] <- data[,18:22] %>% sapply(as.numeric)

###############################
# write.table(data, './data/data.csv', sep=',')

##############################################################

dados_parto <- data[,2:5]
dados_reprodutivos <- data[,6:10]
dados_produtivos <- data[,11:17]
dados_leite <- data[,18:24]
dados_clima <- data[,25:30]

p1 <- vis_dat(dados_parto, palette = "cb_safe")
p2 <- vis_dat(dados_reprodutivos, palette = "cb_safe")
p3 <- vis_dat(dados_produtivos, palette = "cb_safe")
p4 <- vis_dat(dados_leite, palette = "cb_safe")
p5 <- vis_dat(dados_clima, palette = "cb_safe")

# plist <- mget(paste0('p',1:5))
# nCol <- floor(sqrt(length(plist)))
# do.call("grid.arrange", c(plist, ncol=nCol))

##############################################################

prop_na <- round(apply(data, 2, function(x) sum(is.na(x)) / length(x)) * 100, 3)

group_name <- c('ind',
                rep('Características do parto', 4),
                rep('Parâmetros reprodutivos', 5),
                rep('Parâmetros produtivos', 7),
                rep('Composição do leite', 7),
                rep('Parâmetros climáticos', 6))

na_data <- data.frame(var=names(data), na=prop_na, group=group_name)



########################################################

data_clean <- data[,-c(1,which(na_data$na > 40),25)] %>% na.exclude() 

data_clean_num <- data[,-c(1,which(na_data$na > 40),25)] %>% na.exclude() %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

set.seed(555)
amostra <- sample(1:nrow(data_clean), round(nrow(data_clean) * 0.7), replace = F)

treino <- data_clean[amostra,]
teste <- data_clean[-amostra,]

treino_num <- data_clean_num[amostra,]
teste_num <- data_clean_num[-amostra,]
