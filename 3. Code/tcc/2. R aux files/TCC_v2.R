#############################################################
### UNIVERSIDADE FEDERAL DE MINAS GERAIS                  ###
### INSTITUTO DE CIÊNCIAS EXATAS                          ###
### DEPARTAMENTO DE ESTATÍSTICA                           ###
### ESPECIALIZAÇÃO EM ESTATÍSTICA                         ###
### TRABALHO DE CONCLUSÃO DE CURSO                        ###
### TEMA: REGRESSÃO LINEAR - OTIMIZAÇÃO DE BETA ZERO      ###
### AUTOR: IGOR MAZZETO RESENDE SOARES                    ###
### ORIENTADOR: PROF. DR. MARCELO AZEVEDO COSTA           ###
#############################################################

### "Que haja uma luz nos lugares mais escuros
### , quando todas as outras luzes se apagarem." JRR Tolkien

### DESCRIÇÃO DO PROBLEMA - TÓPICOS

### PREVISÃO DO CUSTO OPERACIONAL DAS COMPANHIAS DE ENERGIA ELÉTRICA
### CONSTRUÇÃO DE MODELO QUE REFLITA AS REGRAS DE NEGÓCIO DO SETOR ELÉTRICO (BETA >0)
### COMPARATIVO DA PERFORMANCE DO MODELO PROPOSTO COM DEMAIS TÉCNICAS

### ### ### ### ### ### ### ### ###  I. CARREGAMENTO DE PACOTES ### ### ### ### ### ### ### ### ### 

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
###  Windows
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

# View(db)

### Pré análise
modelo_mass = masslm(db[,4:12], "PMSO")
modelo_mass =  as.data.frame(modelo_mass)
modelo_mass = modelo_mass[order(modelo_mass$R.squared,decreasing = TRUE),]
print(modelo_mass)



### Variáveis do modelo 
### Ordenadas de acordo com a ordem crescem de pré-análise do R²        
### X1: Extensão de rede superior que 230 kV                          
### X2: Módulos de manobra com tensão igual ou superior a 230 kV          
### X3: Equipamentos de subestação com tensão superior a 230 kV           
### x4: Potência aparente total, em MVA, de equipamentos de subestação    
### X5: Potência reativa total, em Mvar, de equipamentos de subestação    
### X6: Equipamentos de subestação com tensão inferior a 230 kV           
### X7: Módulos de manobra com tensão inferior a 230 kV                   
### X8: Extensão de rede menor que 230 kV                                 
### Y : PMSO (variável resposta)                                          

### Modificacao dos nomes das variaveis

colnames(db)[colnames(db) == modelo_mass$IV[1]] = "X1"
colnames(db)[colnames(db) == modelo_mass$IV[2]] = "X2"
colnames(db)[colnames(db) == modelo_mass$IV[3]] = "X3"
colnames(db)[colnames(db) == modelo_mass$IV[4]] = "X4"
colnames(db)[colnames(db) == modelo_mass$IV[5]] = "X5"
colnames(db)[colnames(db) == modelo_mass$IV[6]] = "X6"
colnames(db)[colnames(db) == modelo_mass$IV[7]] = "X7"
colnames(db)[colnames(db) == modelo_mass$IV[8]] = "X8"

summary(db)


ggplot(db, aes(x = PMSO)) + 
  geom_histogram(bins = 10, fill = "blue", color = "white") +
  scale_x_continuous(labels = label_reais, name = "PMSO") +
  scale_y_continuous(name = "Freq.")+
  labs(title = "Histograma  - PMSO")+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold", size = 8))


### charts with year facet

#X1
ggplot(db, aes(X1, PMSO)) + 
  geom_point(pch = 19, color = "darkblue") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[1]))))+
  xlab(expression(paste(bold(X[1]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


#X2
ggplot(db, aes(X2, PMSO)) + 
  geom_point(pch = 19, color = "red") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[2]))))+
  xlab(expression(paste(bold(X[2]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X3
ggplot(db, aes(X3, PMSO)) + 
  geom_point(pch = 19, color = "darkgreen") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[3]))))+
  xlab(expression(paste(bold(X[3]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X4
ggplot(db, aes(X4, PMSO)) + 
  geom_point(pch = 19, color = "navy") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[4]))))+
  xlab(expression(paste(bold(X[4]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X5
ggplot(db, aes(X5, PMSO)) + 
  geom_point(pch = 19, color = "maroon") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[5]))))+
  xlab(expression(paste(bold(X[5]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X6

ggplot(db, aes(X6, PMSO)) + 
  geom_point(pch = 19, color = "orange") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[6]))))+
  xlab(expression(paste(bold(X[6]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X7
ggplot(db, aes(X7, PMSO)) + 
  geom_point(pch = 19, color = "purple") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[7]))))+
  xlab(expression(paste(bold(X[7]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X8
ggplot(db, aes(X8, PMSO)) + 
  geom_point(pch = 19, color = "brown") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[8]))))+
  xlab(expression(paste(bold(X[8]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  facet_wrap(~Ano, nrow = 3, scales = "free_x") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))   


##### without year facet

#X1
ggplot(db, aes(X1, PMSO)) + 
  geom_point(pch = 19, color = "darkblue") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[1]))))+
  xlab(expression(paste(bold(X[1]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


#X2
ggplot(db, aes(X2, PMSO)) + 
  geom_point(pch = 19, color = "red") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[2]))))+
  xlab(expression(paste(bold(X[2]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X3
ggplot(db, aes(X3, PMSO)) + 
  geom_point(pch = 19, color = "darkgreen") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[3]))))+
  xlab(expression(paste(bold(X[3]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X4
ggplot(db, aes(X4, PMSO)) + 
  geom_point(pch = 19, color = "navy") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[4]))))+
  xlab(expression(paste(bold(X[4]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X5
ggplot(db, aes(X5, PMSO)) + 
  geom_point(pch = 19, color = "maroon") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[5]))))+
  xlab(expression(paste(bold(X[5]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X6
ggplot(db, aes(X6, PMSO)) + 
  geom_point(pch = 19, color = "orange") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[6]))))+
  xlab(expression(paste(bold(X[6]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) + 
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X7
ggplot(db, aes(X7, PMSO)) + 
  geom_point(pch = 19, color = "purple") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[7]))))+
  xlab(expression(paste(bold(X[7]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

#X8
ggplot(db, aes(X8, PMSO)) + 
  geom_point(pch = 19, color = "brown") + 
  ggtitle(expression(paste(bold("PMSO vs "), bold(X[8]))))+
  xlab(expression(paste(bold(X[8]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

### Correlation plots
dbcor = with(db,dplyr::select(db, PMSO,X1,X2,X3,X4,X5,X6,X7,X8))

matCor <- cor(dbcor, method="spearman")
# corrplot(matCor, type="upper",
#          order="AOE", diag=FALSE, addgrid.col=NA,
#          outline=TRUE,
#          main = "Matriz de correlação")
corrplot(matCor, type="upper",
         order="AOE", diag=FALSE, addgrid.col=NA,
         outline=TRUE, 
         tl.col = "black",
         tl.srt = 45, 
         #tl.pos = "lt",
         title = "Matriz de correlação", 
         mar=c(2,2,2,2))



### Boxplots


# Ano
ggplot(db, aes(x = Ano, y = PMSO)) +
  geom_boxplot(fill = "steelblue",color = "black",outlier.shape = "x", outlier.size = 3,
               outlier.stroke = 0.2) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  labs(title = "PMSO anual", x = "Ano", y = "PMSO") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
    panel.grid.major = element_line(size = 0.2, linetype = "dotted", color = "gray60"),
    panel.grid.minor = element_line(size = 0.2, linetype = "dotted", color = "gray60")
  )

# Concessionarias
ggplot(db, aes(x = Concessionaria, y = PMSO)) +
  geom_boxplot(fill = "forestgreen",color = "black",outlier.shape = "x", outlier.size = 3,
               outlier.stroke = 0.2) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  labs(title = "PMSO por concessionária", x = "Conc", y = "PMSO") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
    panel.grid.major = element_line(size = 0.2, linetype = "dotted", color = "gray60"),
    panel.grid.minor = element_line(size = 0.2, linetype = "dotted", color = "gray60")
  )  


# Tipo

ggplot(db, aes(x = Tipo, y = PMSO)) +
  geom_boxplot(fill = "sienna",color = "black",outlier.shape = "x", outlier.size = 3,
               outlier.stroke = 0.2) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  labs(title = "PMSO por tipo", x = "Tipo", y = "PMSO") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
    panel.grid.major = element_line(size = 0.2, linetype = "dotted", color = "gray60"),
    panel.grid.minor = element_line(size = 0.2, linetype = "dotted", color = "gray60")
  )  


### ### ### ### ### ### ### ### ###  2. INICIALIZAÇÃO PARA MODELO ### ### ### ### ### ### ### ### ###


### modelo linear múltiplo com todas as variaveis
### 
modelo = lm(PMSO ~ ., data=dbcor)
summary(modelo)

graphics::plot(modelo, lty=1, pch=19, col="blue")

df.modelo = data.frame(fitted = modelo$fitted.values, residuals = modelo$residuals)



round(modelo$coefficients,2)



yfit1_linear= predict(modelo,newdata = dbcor)
y1  <- dbcor$PMSO
SQT   <- sum( (y1 - mean(y1))^2 )
SQres <- sum( (y1 - yfit1_linear)^2 )
R12 = (1 - SQres/SQT)
R12

p = yfit1_linear > 0  
y1_log  <- log(dbcor$PMSO[p])
yfit1 = yfit1_linear[p]
SQT_log   <- sum( (y1_log - mean(y1_log))^2 )
SQres_log <- sum( (y1_log - log(yfit1)^2 ))
R2_log = (1 - SQres_log/SQT_log)
R2_log


### Shapiro-Wilk test | Null-hypothesis: population is normally distributed
### If p-value < alpha then null hypothesis is rejected.
### standard alpha : 0.05
### 

summary(modelo)

shapiro.test(residuals(modelo))
cat('Evidencias de que os residuos nao sao normalmente distribuidos')

library (nortest)

# Normality tests
nortest::ad.test(modelo$residuals)
nortest::lillie.test(modelo$residuals)

# Breusch-Pagan - homoscedasticidade
lmtest::bptest(modelo)

# Durbin - Watson - independência
lmtest::dwtest(modelo)


# Gráficos

# graphics::plot(yfit1_linear ~ db$PMSO, pch=19, col="blue", ylab="estimated PMSO",
#                xlab="observed PMSO"); title(main = "Estimated PMSO x Observed PMSO")
# 
# abline(a=0, b=1, col="red", pch=19)

length(db$PMSO) 
length(yfit1_linear)

ggplot(data = db, aes(x = PMSO, y = yfit1_linear)) + 
  geom_point(color = "blue", size = 3, shape = 19) +
  scale_y_continuous(labels = label_reais, limits = c(-500000, 1500000))+
  scale_x_continuous(labels = label_reais, limits = c(-500000, 1500000))+
  geom_abline(intercept = 0, slope = 1, color = "maroon", size = 1,linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "solid", color = "red",size = 1)+
  annotate("text", x = -500000, y = 50000, label = "Restrição operacional", color = "red",
           size = 2.5, hjust = 0,fontface="bold")+
  xlab("Observado") + ylab("Estimado") +
  ggtitle("PMSO estimado x PMSO observado")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    panel.spacing = unit(0.2, "lines"),
    axis.text.x = element_text(margin = margin(0, 0, 0.5, 0,"cm")),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0.3,"cm")),
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


### ### ### ###  MODELO GAMA  - Implementacao Prof. Marcelo (02/12/2022)

modelo_gm <- glm(PMSO ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,
                 data=db, family=Gamma(link='identity'))
modelo_gm <- MASS::stepAIC(modelo_gm)
summary(modelo_gm)

1 - pchisq(modelo_gm$deviance, modelo_gm$df.residual)


library(AER)


yfit <- rep(NA,nrow(db))

### Inicialização  

for(emp in unique(db$Concessionaria)){
  pos   <- which(db$Concessionaria == emp)
  train <- db[-pos,]
  test  <- db[pos,]
  
  modelo_gm2 <- glm(PMSO ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, 
                    data=train, family=Gamma(link='identity'))
  modelo_gm2 <- stepAIC(modelo_gm2, trace=FALSE)
  
  yfit[pos] <- predict(modelo_gm2, newdata=test, type="response")
  #print(yfit)
}
# yfit

plot(modelo_gm)
### Compara o estimado e o real
## plot

ggplot(data = db, aes(x = PMSO, y = yfit)) + 
  geom_point(color = "blue", size = 3, shape = 19) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000))+
  scale_x_continuous(labels = label_reais, limits = c(0, 2000000))+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1,linetype = "dotted") +
  xlab("Observado") + ylab("Estimado") +
  ggtitle("PMSO estimado x PMSO observado")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))



### Escala log
graphics::plot(yfit ~ db$PMSO, pch=19, col="blue", log="xy", 
               ylab="estimated PMSO", 
               xlab="observed PMSO");title(main = "Estimated PMSO x Observed PMSO - Log")
abline(a=0, b=1, col="red", pch=19)


ggplot(data = db, aes(x = PMSO, y = yfit)) +
  geom_point(shape = 19, size = 2, color = "blue") +
  scale_x_log10(labels = label_dots) +
  scale_y_log10(labels = label_dots) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 0.7) +
  labs(x = "Observado", y = "Estimatedo") +
  ggtitle("PMSO estimado x observado - Log")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
    axis.text = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(size = 0.2, linetype = "dotted", color = "gray60"),
    panel.grid.minor = element_line(size = 0.2, linetype = "dotted", color = "gray60")
  )



### R2 preditivo na escala original e na escala logaritmica
y_r2  <- db$PMSO
SQT   <- sum( (y_r2 - mean(y_r2))^2 )
SQres <- sum( (y_r2 - yfit)^2 )
R2 = (1 - SQres/SQT)
R2

### Escala log
y_log     <- log(db$PMSO)
SQT_log   <- sum( (y_log - mean(y_log))^2 )
SQres_log <- sum( (y_log - log(yfit))^2 )
R2_log = (1 - SQres_log/SQT_log)
R2_log


### ### ### ### ### ### ### ### ###  3. IMPLEMENTAÇÃO DE MODELO LINEAR ### ### ### ### ### ### ### ### ###

### Modelo-  Implementação orientações Prof. Marcelo Azevedo Costa (11/11/2022)

### Parametros
y = db$PMSO
N = nrow(db)
tau = 0.5

### Vetor com parametros
v = c(db$X1,db$X2,db$X3,db$X4,db$X5,db$X6,db$X7,db$X8)

### Matriz para iteracao
m1 = matrix(data = v,nrow = nrow(db),ncol = 8)
# m1[,1] == db$X1

### Matriz auxiliar para armazenar alphas e betas
maux = matrix(data = rep(0,nrow(modelo_mass)*2),
              nrow = 2 ,
              ncol = nrow(modelo_mass))


### Matriz para armazenar os erros
erros_var = matrix(data = rep(0),nrow = nrow(db),ncol = 8)
contador = 0

### Procedimento para otimizar o beta de cada regressao com a variavel 'k'
### Variaveis: X1 a X8 -> x_k

for(k in 1:8){
  
  ### cada valor de k representa uma variavel onde o beta sera regredida
  f.con = NULL
  f.obj = NULL
  f.con = NULL
  f.dir = NULL
  y = db$PMSO
  
  ### (tau*e1,tau*e2,alfa1,alfa2,xx  )
  f.obj = c(rep(tau,N), rep(1-tau,N),0,0,0)
  
  ### Constraints || inicialização
  for(cont in 1:nrow(db)){
    e1 = rep(0, nrow(db)) # Inicializacao do vetor de erro (positivo)
    e2 = rep(0, nrow(db)) # Inicializacao do vetor de erro (negativo)
    
    ### Atribuição dos coeficientes
    e1[cont] = +1
    e2[cont] = -1
    
    ### f.aux -> atribuição dos coeficientes de xi
    f.aux <- c(e1, e2, +1, -1,m1[cont,k])
    f.con <- rbind(f.con, f.aux)
  } 
  
  # View(f.con)
  f.dir = rep("=", N)
  f.rhs = y
  
  ### Gera a solucao
  saida   = lpSolve::lp ("min", f.obj, f.con, f.dir, f.rhs)
  solucao = saida$solution
  
  ### Para algumas componentes: erro e intercepto existem as
  ### 'partes' positivas e negativas. Entao, o valor final eh a
  ### diferenca entre essas partes.
  
  erros_var[,k] = solucao[1:N] - solucao[(N+1):(2*N)]
  maux[1,k] = solucao[(2*N)+1] - solucao[(2*N)+2]
  maux[2,k] = solucao[(2*N)+3]
  
  contador = contador+1
} 

dados_regressoes = as.data.frame(maux)
colnames(dados_regressoes)[1] = "X1"
colnames(dados_regressoes)[2] = "X2"
colnames(dados_regressoes)[3] = "X3"
colnames(dados_regressoes)[4] = "X4"
colnames(dados_regressoes)[5] = "X5"
colnames(dados_regressoes)[6] = "X6"
colnames(dados_regressoes)[7] = "X7"
colnames(dados_regressoes)[8] = "X8"
rownames(dados_regressoes)[1] = "alpha"
rownames(dados_regressoes)[2] = "beta"
# View(dados_regressoes)

###  GRAFICOS COM OS AJUSTES PARA TAU = 0.5

# Modelo linear X1
ggplot(db, aes(x = X1, y = PMSO)) +
  geom_point(color = "darkblue", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[1]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,1], slope = dados_regressoes[2,1], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[1])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

# Modelo linear X2
ggplot(db, aes(x = X2, y = PMSO)) +
  geom_point(color = "red", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[2]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,2], slope = dados_regressoes[2,2], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[2])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

# Modelo linear X3
ggplot(db, aes(x = X3, y = PMSO)) +
  geom_point(color = "darkgreen", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[3]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,3], slope = dados_regressoes[2,3], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[3])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

# Modelo linear X4
ggplot(db, aes(x = X4, y = PMSO)) +
  geom_point(color = "navy", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[4]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,4], slope = dados_regressoes[2,4], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[4])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


# Modelo linear X5
ggplot(db, aes(x = X5, y = PMSO)) +
  geom_point(color = "maroon", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[5]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,5], slope = dados_regressoes[2,5], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "),bold(X[5])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


# Modelo linear X6
ggplot(db, aes(x = X6, y = PMSO)) +
  geom_point(color = "orange", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[6]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,6], slope = dados_regressoes[2,6], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[6])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

# Modelo linear X7
ggplot(db, aes(x = X7, y = PMSO)) +
  geom_point(color = "purple", size = 3, shape = 19) +
  xlab(expression(paste(bold(X[7]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,7], slope = dados_regressoes[2,7], lwd = 1, linetype = 2, color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[7])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


# Modelo linear X8
ggplot(db, aes(x = X8, y = PMSO)) +
  geom_point(color = "brown", size = 2, shape = 19) +
  xlab(expression(paste(bold(X[8]))))+
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_reais, limits = c(0, 2000000)) +
  geom_abline(intercept = dados_regressoes[1,7], slope = dados_regressoes[2,7], lwd = 1, linetype = 2,color = "red") +
  ggtitle(expression(paste(bold("Modelo linear: "),bold("PMSO vs "), bold(X[8])))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))

### ### ### ### ### ### ###  3.1 MODELO COM TODAS AS VARIAVEIS 

x1 = db$X1;x2 = db$X2;x3 = db$X3; x4 = db$X4;
x5 = db$X5;x6 = db$X6;x7 = db$X7;x8 = db$X8

f.obj_t = c(rep(tau,N), rep(1-tau,N),0,0,0,0,0,0,0,0,0,0)
f.con_t = NULL

### Constraints || inicialização
for(cont in 1:nrow(db)){
  e1 = rep(0, nrow(db)) # Inicializacao do vetor de erro (positivo)
  e2 = rep(0, nrow(db)) # Inicializacao do vetor de erro (negativo)
  e1[cont] = +1
  e2[cont] = -1
  f.aux <- c(e1, e2, +1, -1,x1[cont],x2[cont],x3[cont],x4[cont],x5[cont],x6[cont],x7[cont],x8[cont])
  f.con_t <- rbind(f.con_t, f.aux)
} 

# View(f.con)
f.dir_t = rep("=", N)
f.rhs_t = y

### Gera a solucao
saida_t   = lpSolve::lp ("min", f.obj_t, f.con_t, f.dir_t, f.rhs_t)
solucao_t = saida_t$solution

### Erros, alpha e beta

erros_t = solucao_t[1:N] - solucao_t[(N+1):(2*N)]
alpha_t = solucao_t[(2*N)+1] - solucao_t[(2*N)+2]

### vetor para receber os valores dos betas
betas_t = NULL
for(i in 1:8){
  betas_t[i] = solucao_t[(2*N)+(i+2)]
}

resultado_t = c(alpha_t,betas_t)
resultado_t = as.data.frame(resultado_t)
# rownames(resultado)[1] = "alpha"
# rownames(resultado)[2] = "beta1"
# rownames(resultado)[3] = "beta2"
# rownames(resultado)[4] = "beta3"
# rownames(resultado)[5] = "beta4"
# rownames(resultado)[6] = "beta5"
# rownames(resultado)[7] = "beta6"
# rownames(resultado)[8] = "beta7"
# rownames(resultado)[9] = "beta8"

# Comparativo PMSO x PMSO ajustado

mx = with(db,data.frame(X1,X2,X3,X4,X5,X6,X7,X8))
mx  = as.matrix(mx)
coeficientes = as.vector(resultado_t[-1,])

db_r = db

if(nrow(mx) == nrow(db)){
  for(j in 1:nrow(mx)){ # calcula o valor do PMSO previsto
    db_r$pmso_predict[j] = (mx[j,] %*% coeficientes) + alpha_t
  }
} else {
  print("Erro, verificar código")
}

db_r$pmso_predict

# graphics::plot(pmso_predict ~ PMSO,main  = "PMSO x PMSO ajustado", 
#                pch=19, col="blue", ylab="PMSO estimado", 
#                xlab="PMSO observado", data = db_r); title(main = "PMSO x PMSO ajustado")
# abline(a=0, b=1, col="red", pch=1, lty = 2)


ggplot(data = db_r, aes(x = PMSO, y = pmso_predict)) + 
  geom_point(color = "blue", size = 3, shape = 19) +
  scale_y_continuous(labels = label_reais, limits = c(-500000, 1500000))+
  scale_x_continuous(labels = label_reais, limits = c(-500000, 1500000))+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1,linetype = "dotted") +
  xlab("Observado") + ylab("Estimado") +
  ggtitle("Modelo linear: PMSO estimado x observado")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))



# R2 preditivo na escala original e na escala logaritmica
# todas as variaveis no modelo
y_tv    <- db_r$PMSO
yfit_tv  <- db_r$pmso_predict
SQT_tv   <- sum( (y_tv - mean(y_tv))^2 )
SQres_tv <- sum( (y_tv - yfit_tv)^2 )
R2_tv = (1 - SQres_tv/SQT_tv); R2_tv

# log
# todas as variaveis no modelo
pos_tv = yfit_tv > 0
y_tv_log <- log(db_r$PMSO[pos_tv])
yfit_tv_log  <- db_r$pmso_predict[pos_tv]
SQT_tv_log   <- sum( (y_tv_log - mean(y_tv_log))^2 )
SQres_tv_log <- sum( (y_tv_log - log(yfit_tv_log)^2 ))
R2_tv_log = (1 - SQres_tv_log/SQT_tv_log); R2_tv_log

graphics::plot(pmso_predict ~ PMSO, pch=19, col="purple", log="xy", 
               ylab="PMSO ajustado", 
               xlab="PMSO observado", data = db_r); 
title(main = "PMSO x PMSO ajustado (Log)")
abline(a=0, b=1, col="red", pch=19,lty = 2)


ggplot(data = db_r, aes(x = PMSO, y = pmso_predict)) +
  geom_point(shape = 19, size = 2, color = "maroon") +
  scale_x_log10(labels = label_dots) +
  scale_y_log10(labels = label_dots) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 0.7) +
  labs(x = "Observado", y = "Estimatedo") +
  ggtitle("Modelo linear: PMSO estimado x observado - Log")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
    axis.text = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(size = 0.2, linetype = "dotted", color = "gray60"),
    panel.grid.minor = element_line(size = 0.2, linetype = "dotted", color = "gray60")
  )


### ### ### ### ### ### ### ### ###  4. MODELO LINEAR LEAVE ONE OUT  ### ### ### ### ### ### ### ### ###

### Validação -  remover uma empresa completamente da base)
### loop para retirar a empresa e estimar seu PMSO
### Ajustar o modelo e estimar o custo operacional da empresa que foi removida
### Calcular R2 preditivo
### Realizar validacão cruzada
### Calcular R2 -> usar logaritmo do PMSO

### Loop para calcular todas as empresas:
### 1. Criar um vetor contendo o nome de cada empresa presente na base
### 2. Pegar o tamanho do vetor
### 3. Função que cria um subconjunto da base dedos excluindo a empresa selecionada

conc = db$Concessionaria
conc = conc[!duplicated(conc)]
nconc = length(conc)

### Criação de matriz para armazenar os alphas e betas da regressão linear
store = matrix(data = NA,nrow = 9,ncol = nconc) 

### Inicialização do procedimento
L = 0
TABULAR = NULL

for (i in 1:nconc){ # realiza o procedimento para 'n' concessionárias na base de dados
  x1 = NULL;x2 = NULL;x3 = NULL; x4 = NULL;x5 = NULL;x6 = NULL;x7 = NULL; x8 = NULL
  db_tr = NULL
  db_v = NULL
  mt = NULL
  N_tr = NULL
  j = NULL
  
  db_tr = filter(db,Concessionaria!= conc[i]) # base de dados de treino 
  db_v = filter(db,Concessionaria == conc[i]) # base de dados de validação onde serão realizadas as previsões
  
  # preparação para modelo linear
  y_tr = db_tr$PMSO # y da minimização dos erros
  lambda = 0.5 #parametro paras os erros
  N_tr = nrow(db_tr) # número de linhas da base de treino
  
  x1 = db_tr$X1;x2 = db_tr$X2;x3 = db_tr$X3; x4 = db_tr$X4;
  x5 = db_tr$X5;x6 = db_tr$X6;x7 = db_tr$X7;x8 = db_tr$X8
  
  ### Linear program
  
  obj = c(rep(lambda,N_tr), rep(1-lambda,N_tr),0,0,0,0,0,0,0,0,0,0)
  fcon = NULL
  
  for(t in 1:nrow(db_tr)){
    e1 = rep(0, nrow(db_tr)) # Inicializacao do vetor de erro (positivo)
    e2 = rep(0, nrow(db_tr)) # Inicializacao do vetor de erro (negativo)
    e1[t] = +1
    e2[t] = -1
    faux <- c(e1, e2, +1, -1,x1[t],x2[t],x3[t],x4[t],x5[t],x6[t],x7[t],x8[t])
    fcon <- rbind(fcon, faux)
  }
  
  fdir = rep("=", N_tr)
  frhs = y_tr
  outputs   = lpSolve::lp ("min", obj, fcon, fdir, frhs) #roda o algoritmo de programação linear
  solution = outputs$solution # vetor com as soluções
  errors = solution[1:N_tr ] - solution[(N_tr+1):(2*N_tr )] # vetor com os erros
  alpha = solution[(2*N_tr )+1] - solution[(2*N_tr)+2] #vetor com alpha
  
  betas = NULL
  for(l in 1:8){
    betas[l] = solution[(2*N_tr)+(l+2)] # vetor com os betas
  }
  
  resultado = as.matrix(c(alpha,betas)) # armazena resultado do coeficientes
  store[,i] = as.matrix(c(alpha,betas)) # armazena resultado do coeficientes de cada iteração
  param = as.vector(resultado[-1,]) # remove o valor de alpha do vetor 
  mt = with(db_v,data.frame(X1,X2,X3,X4,X5,X6,X7,X8)) #cria matriz somente com as variáveis
  mt  = as.matrix(mt)
  
  for(j in 1:nrow(mt)){ # calcula o valor do PMSO previsto
    db_v$pmso_val[j] = (mt[j,] %*% param) + alpha
  }
  
  if(L ==0){ # procedimento para empilhar os resultados 
    TABULAR = db_v
    L = L+1 
  }else{
    TABULAR = rbind(TABULAR,db_v)
  }
}

TABULAR$diferenca = TABULAR$pmso_val - TABULAR$PMSO
TABULAR$dif_per = round(abs((TABULAR$pmso_val/TABULAR$PMSO))-1,2)


### Armazenamento dos coeficientes de cada iteração da LP
### 
store = as.data.frame(store)
info = c("alpha","beta1","beta2","beta3","beta4"
         ,"beta5","beta6","beta7","beta8")
tabela1 = data.frame(info,store)
# View(tabela1)

tabela2 = as.matrix(
  c(alpha_m = mean(as.numeric(tabela1[1,2:ncol(tabela1)])),
    beta1_m = mean(as.numeric(tabela1[2,2:ncol(tabela1)])),
    beta2_m = mean(as.numeric(tabela1[3,2:ncol(tabela1)])),
    beta3_m = mean(as.numeric(tabela1[4,2:ncol(tabela1)])),
    beta4_m = mean(as.numeric(tabela1[5,2:ncol(tabela1)])),
    beta5_m = mean(as.numeric(tabela1[6,2:ncol(tabela1)])),
    beta6_m = mean(as.numeric(tabela1[7,2:ncol(tabela1)])),
    beta7_m = mean(as.numeric(tabela1[8,2:ncol(tabela1)])),
    beta8_m = mean(as.numeric(tabela1[9,2:ncol(tabela1)])))
)
tabela2

### ### ### ### ### ### ### ### ###  5. COMPARATIVO ESTIMADO x REAL - LEAVE ONE OUT LP  ### ### ### ### ### ### ### ### ###

### Implementacao Prof.Marcelo (02/12/2022) - Regressao lpSolve
### Gráfico comparando o PMSO real com o calculado pelo modelo 
### linear LEAVE ONE OUT

plot(pmso_val ~ PMSO,main  = "PMSO x PMSO ajustado", pch=19, col="blue", ylab="estimated PMSO", 
     xlab="observed PMSO", data = TABULAR); title(main = "PMSO x PMSO ajustado")
abline(a=0, b=1, col="red", pch=19)

ggplot(data = TABULAR, aes(x = PMSO, y = pmso_val)) + 
  geom_point(color = "blue", size = 3, shape = 19) +
  scale_y_continuous(labels = label_reais, limits = c(-500000, 1500000))+
  scale_x_continuous(labels = label_reais, limits = c(-500000, 1500000))+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1,linetype = "dotted") +
  xlab("Observado") + ylab("Estimado") +
  ggtitle("Modelo linear: PMSO estimado x observado")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))


### Gráfico comparando o PMSO real com o calculado pelo modelo linear LEAVE ONE OUT - ESCALA LOG

plot(pmso_val ~ PMSO, pch=19, col="blue", log="xy", 
     ylab="estimated PMSO", 
     xlab="observed PMSO", data = TABULAR); title(main = "PMSO x PMSO ajustado (Log)")
abline(a=0, b=1, col="red", pch=19)


# R2 preditivo na escala original e na escala logaritmica
y_lp     <- TABULAR$PMSO
yfit_lp  <- TABULAR$pmso_val
SQT   <- sum( (y_lp - mean(y_lp))^2 )
SQres <- sum( (y_lp - yfit_lp)^2 )
R2_LP = (1 - SQres/SQT); R2_LP

# Escala log
pos   <- yfit_lp > 0 
y     <- log(TABULAR$PMSO[pos])
yfit  <- TABULAR$pmso_val[pos]
SQT   <- sum( (y - mean(y))^2 )
SQres <- sum( (y - log(yfit))^2 ) 
R2_LP_LOG = (1 - SQres/SQT);  R2_LP_LOG


### ### ### ### ### ### ### ### ###  5. PROCEDIMENTO BOOTSTRAP ### ### ### ### ### ### ### ### ###

# Steps
# 1. Gerar amostra boot com reposicao
# 2. Rodar modelo lienar
# 3. Coletar alpha e betas gerados para a i-esima amostra boot
# 4. Calcular media de alpha e betas
# 5. Construir o intervalo de confianca

### BOOTSTRAP PROCEDURE INICIALIZATION

set.seed(478986)
B = 10000 # Bootstrap resamples
boot_table = NULL # tabela com resultados
Q = 0 # auxiliar


for(w in 1:B){
  db1 = db
  # resample  
  db1 = db1[sample(nrow(db1),nrow(db1),replace = TRUE),]
  # boot variables
  y_boot = db1$PMSO
  N_boot = nrow(db1)
  tau = 0.5
  
  # reset variables
  
  f.aux_boot = NULL; f.con_boot = NULL; erros_boot = NULL;
  f.dir_boot = NULL; f.obj_boot = NULL; f.dir_boot  = NULL;
  betas_boot = NULL; resultado_boot = NULL; alpha_boot = NULL;
  saida_boot = NULL; solucao_boot = NULL;
  
  x1_boot = NULL;x2_boot = NULL; x3_boot = NULL; x4_boot = NULL;x5_boot = NULL;
  x6_boot = NULL;x7_boot = NULL; x8_boot = NULL; rbt = NULL;
  
  # assign variables
  
  x1_boot = db1$X1; x2_boot = db1$X2; x3_boot = db1$X3; x4_boot = db1$X4;
  x5_boot = db1$X5; x6_boot = db1$X6; x7_boot = db1$X7; x8_boot = db1$X8
  
  # objective function
  f.obj_boot = c(rep(tau,N_boot), rep(1-tau,N_boot),0,0,0,0,0,0,0,0,0,0)
  
  ### Constraints inicialization
  for(cont in 1:nrow(db1)){
    e1_boot = rep(0, nrow(db1)) # Inicializacao do vetor de erro (positivo)
    e2_boot = rep(0, nrow(db1)) # Inicializacao do vetor de erro (negativo)
    e1_boot[cont] = +1
    e2_boot[cont] = -1
    f.aux_boot <- c(e1_boot, e2_boot, +1, -1,
                    x1_boot[cont],x2_boot[cont],x3_boot[cont],x4_boot[cont]
                    ,x5_boot[cont],x6_boot[cont],x7_boot[cont],x8_boot[cont])
    f.con_boot <- rbind(f.con_boot, f.aux_boot)
  } 
  
  # constraint direction
  f.dir_boot = rep("=", N_boot)
  
  # right side
  f.rhs_boot = y_boot
  
  # Generate solution
  saida_boot = lpSolve::lp ("min", f.obj_boot, f.con_boot, f.dir_boot, f.rhs_boot)
  
  # store solution
  solucao_boot = saida_boot$solution
  
  # Errors, alpha and beta
  
  erros_boot = solucao_boot[1:N_boot] - solucao_boot[(N_boot+1):(2*N_boot)]
  alpha_boot = solucao_boot[(2*N_boot)+1] - solucao_boot[(2*N_boot)+2]
  
  # vector for betas
  
  for(i in 1:8){
    betas_boot[i] = solucao_boot[(2*N_boot)+(i+2)]
  }
  
  # bind coeficients
  
  resultado_boot = c(alpha_boot,betas_boot)
  resultado_boot = as.data.frame(resultado_boot)
  rbt = t(resultado_boot)
  
  # bind boot table 
  if(Q==0){
    boot_table = rbt
    Q = Q+1
  } else {
    boot_table = rbind(boot_table,rbt)
  }
}


res_boot = as.data.frame(boot_table)
View(res_boot)


colnames(res_boot)[1] = "alpha"
colnames(res_boot)[2] = "beta1"
colnames(res_boot)[3] = "beta2"
colnames(res_boot)[4] = "beta3"
colnames(res_boot)[5] = "beta4"
colnames(res_boot)[6] = "beta5"
colnames(res_boot)[7] = "beta6"
colnames(res_boot)[8] = "beta7"
colnames(res_boot)[9] = "beta8"

packHV::hist_boxplot(res_boot$alpha,main="Histograma", 
                     col="blue",
                     xlab="Alfa")

packHV::hist_boxplot(res_boot$beta1,main="Histograma", 
                     col="red",
                     xlab="Beta 1")

packHV::hist_boxplot(res_boot$beta2,main="Histograma", 
                     col="gray",
                     xlab="Beta 2")

packHV::hist_boxplot(res_boot$beta3,main="Histograma", 
                     col="green",
                     xlab="Beta 3")

packHV::hist_boxplot(res_boot$beta4,main="Histograma", 
                     col="purple",
                     xlab="Beta 4")

packHV::hist_boxplot(res_boot$beta5,main="Histograma", 
                     col="pink",
                     xlab="Beta 5")

packHV::hist_boxplot(res_boot$beta6,main="Histograma", 
                     col="orange",
                     xlab="Beta 6")

packHV::hist_boxplot(res_boot$beta7,main="Histograma", 
                     col="cyan",
                     xlab="Beta 7")

packHV::hist_boxplot(res_boot$beta8,main="Histograma", 
                     col="yellow",
                     xlab="Beta 8")


##### Intervalo de confiança e estatísticas do procdimento Bootstrap

medias = NULL
desvio = NULL
ep = NULL
mediana = NULL

for(k in 1:ncol(res_boot)){
  medias[k] = mean(res_boot[,k])
  desvio[k] = sd(res_boot[,k])
  ep[k] = (sd(res_boot[,k])) / sqrt(nrow(res_boot))
  mediana[k] = median(res_boot[,k])
}

CI.betas = NULL
CI.betas = matrix(0,nrow = ncol(res_boot),ncol =2)

for(i in 1:ncol(res_boot)){
  CI.betas[i,1] = as.numeric(quantile(res_boot[,i],c(.025)))
  CI.betas[i,2] = as.numeric(quantile(res_boot[,i],c(.975)))
}

CI.betas = as.data.frame(CI.betas)
colnames(CI.betas)[1] = "2.5%"
colnames(CI.betas)[2] = "97.5%"

rownames(CI.betas)[1] = "alpha"
rownames(CI.betas)[2] = "beta1"
rownames(CI.betas)[3] = "beta2"
rownames(CI.betas)[4] = "beta3"
rownames(CI.betas)[5] = "beta4"
rownames(CI.betas)[6] = "beta5"
rownames(CI.betas)[7] = "beta6"
rownames(CI.betas)[8] = "beta7"
rownames(CI.betas)[9] = "beta8"

print(CI.betas)

### ### ### ### ### ### ### ### ### ### ### 

