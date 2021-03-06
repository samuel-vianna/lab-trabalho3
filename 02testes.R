library(readxl)
library(Amelia)
library(Boruta)
library(tidyverse)

data <- read_xlsx('./data/Distocia.xlsx', sheet = 1, skip = 1, na = 'NA')
data <- as.data.frame(data)
data_a<-data[c(2:24,26:30)]
str(data_a)
summary(data_a)
data_a$`Situa��o do parto`<- as.factor(data_a$`Situa��o do parto`)
data_a$`Bezerro deste parto`<- as.factor(data_a$`Bezerro deste parto`)
data_a$`Tipo de prenhez`<- as.factor(data_a$`Tipo de prenhez`)
data_a$Multiparidade<- as.factor(data_a$Multiparidade)
data_a$`Esta��es do ano`<- as.factor(data_a$`Esta��es do ano`)
data_a$`ES (%)`<-as.numeric(data_a$`ES (%)`)
data_a$`ST (%)`<-as.numeric(data_a$`ST (%)`)
data_a$`LACT (%)`<-as.numeric(data_a$`LACT (%)`)
data_a$`PROT (%)`<-as.numeric(data_a$`PROT (%)`)
data_a$`GORD (%)`<-as.numeric(data_a$`GORD (%)`)
str(data_a)
missmap(data_a)
amelia_bank <- amelia(data_a, m=3, parallel = "multicore",noms=c('Situa��o do parto','Bezerro deste parto',
                                                                 'Tipo de prenhez','Multiparidade',
                                                                 'Esta��es do ano'))
set.seed(111)
boruta.bank_train <- Boruta(`Situa��o do parto`~., data = amelia_bank$imputations[[1]], doTrace = 2)
print(boruta.bank_train)
boruta.bank <- TentativeRoughFix(boruta.bank_train)
print(boruta.bank)
plot(boruta.bank, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.bank$ImpHistory),function(i)
  boruta.bank$ImpHistory[is.finite(boruta.bank$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.bank$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.bank$ImpHistory), cex.axis = 0.7)
getSelectedAttributes(boruta.bank, withTentative = F)
bank_df <- attStats(boruta.bank)
print(bank_df)
bank_dft <- attStats(boruta.bank_train)
print(bank_dft)