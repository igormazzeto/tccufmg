## Modelos com validacao cruzada
## 
## 

rm(list = ls(all = TRUE))

if(!require(scales)) install.packages("scales") else library(scales)  
if(!require(boot)) install.packages("boot") else library(boot)
if(!require(janitor)) install.packages("janitor") else library(janitor)
if(!require(ggplot2)) install.packages("ggplot2") else library(ggplot2)
if(!require(ggalt)) install.packages("ggalt") else library(ggalt)
if(!require(dplyr)) install.packages("dplyr") else library(dplyr)
if(!require(tidyverse)) install.packages("tidyverse") else library(tidyverse)
if(!require(xlsx)) install.packages("xlsx") else library(xlsx)
if(!require(openxlsx)) install.packages("openxlsx") else library(openxlsx)
if(!require(corrplot)) install.packages("corrplot") else library(corrplot)
if(!require(mvShapiroTest)) install.packages("mvShapiroTest") else library(mvShapiroTest)
if(!require(MVar.pt)) install.packages(MVar.pt) else library(MVar.pt)
if(!require(packHV)) install.packages("packHV") else library(packHV)
if(!require(readxl)) install.packages("reaxl") else library(readxl)
if(!require(MASS)) install.packages("MASS") else library(MASS)
if(!require(exploreR)) install.packages("exploreR") else library(exploreR)
if(!require(openxlsx)) install.packages(openxlsx) else require(openxlsx)
if(!require(lmtest)) install.packages(lmtest) else require(lmtest)
if(!require(forecast)) install.packages(forecast) else require(forecast)
if(!require(RColorBrewer)) install.packages("RColorBrewer") else library(RColorBrewer)
if(!require(lpSolve)) install.packages("lpSolve") else library (lpSolve)
if(!require(lmtest)) install.packages("lmtest") else library(lmtest)

### ### ### ### ### ### ### ### ###  1. LEITURA  E EXPLORAÇÃO DE DADOS ### ### ### ### ### ### ### ### ### 

### Functions:

label_reais <- function(x) {
  paste0("R$ ", comma(x, big.mark = ".", decimal.mark = ","))
}

label_dots <- function(x) {
  paste0(comma(x, big.mark = ".", decimal.mark = ","))
}


### MacOS
dados =  read_excel("~/Library/CloudStorage/OneDrive-Pessoal/Documentos/7. Especialização/3. TCC/3. Códigos/tcc/BaseCompilada_TS14_2022.xlsx", 
                    sheet = "DadosEscores")

library(readxl)
setwd("~/OneDrive/Documentos/7. Especialização/3. TCC/3. Códigos/tcc")
dados <- read_excel("BaseCompilada_TS14_2022.xlsx",
                    sheet = "DadosEscores")


### Transformação do IdAgente as char
dados$IdAgente = as.character(dados$IdAgente)


### dataset com variáveis do modelo
db = with(dados,dplyr::select(dados, Concessionaria, Tipo,
                              Ano, PMSO, rede.menor.230,
                              rede.maior.230,MVA,Mvar,
                              modulos.sub.menor230,modulos.sub.maior230,
                              modulos.manobra.menor230,modulos.manobra.maior230))

modelo_mass = masslm(db[,4:12], "PMSO")
modelo_mass =  as.data.frame(modelo_mass)
modelo_mass = modelo_mass[order(modelo_mass$R.squared,decreasing = TRUE),]

colnames(db)[colnames(db) == modelo_mass$IV[1]] = "X1"
colnames(db)[colnames(db) == modelo_mass$IV[2]] = "X2"
colnames(db)[colnames(db) == modelo_mass$IV[3]] = "X3"
colnames(db)[colnames(db) == modelo_mass$IV[4]] = "X4"
colnames(db)[colnames(db) == modelo_mass$IV[5]] = "X5"
colnames(db)[colnames(db) == modelo_mass$IV[6]] = "X6"
colnames(db)[colnames(db) == modelo_mass$IV[7]] = "X7"
colnames(db)[colnames(db) == modelo_mass$IV[8]] = "X8"


## Modelo linear

#validacao cruzada leave one out

# Remover resultados passados
rm(emp,pos,train1,test1,modelo_1,modelo1,contador)
yfit1_linear = rep(NA,nrow(db))


vec1 = unique(db$Concessionaria)
length(vec1)
vec2 = NULL
aux1 = NULL
contador = 0
i = 0

for(emp in vec1){
   pos <- NULL; test1 = NULL;train1 = NULL; vecaic = NULL;
   
   pos   <- which(db$Concessionaria == emp)
   train1 <- db[-pos,]
   test1  <- db[pos,]
   
   modelo1 <- lm(PMSO ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,data=train1)
   modelo_1 <- stepAIC(modelo1, trace=FALSE)
   
   test1$predict <- predict(modelo_1,newdata = test1,type="response")
   yfit1_linear[pos] <- predict(modelo_1,newdata = test1,type="response")
   
   vecaic = modelo_1$anova$AIC
   vec2[i] = tail(vecaic,1)
   
   if(contador == 0){
    aux1 = test1
    contador = contador +1
    } else {
     aux1 = rbind(aux1,test1)
    }
   i = i +1
}

View(aux1)
yfit1_linear
vec2
tail(vecaic,1)


# R2

y1  <- db$PMSO
SQT   <- sum( (y1 - mean(y1))^2 )
SQres <- sum( (y1 - yfit1_linear)^2 )
R12 = (1 - SQres/SQT)
R12
