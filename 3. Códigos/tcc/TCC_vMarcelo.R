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
  
  ### "Que haja uma luz nos lugares mais escuros, quando todas as outras luzes se apagarem." JRR Tolkien
  
  ### DESCRIÇÃO DO PROBLEMA - TÓPICOS
  
  ### PREVISÃO DO CUSTO OPERACIONAL DAS COMPANHIAS DE ENERGIA ELÉTRICA
  ### CONSTRUÇÃO DE MODELO QUE REFLITA AS REGRAS DE NEGÓCIO DO SETOR ELÉTRICO (BETA >0)
  ### COMPARATIVO DA PERFORMANCE DO MODELO PROPOSTO COM DEMAIS TÉCNICAS
  
  ### ### ### ### ### ### ### ### ###  I. CARREGAMENTO DE PACOTES ### ### ### ### ### ### ### ### ### 
  
  rm(list = ls(all = TRUE))
  
  if(!require(ggplot2)) install.packages("ggplot2") else library(ggplot2)
  if(!require(ggalt)) install.packages("ggalt") else library(ggalt)
  if(!require(dplyr)) install.packages("dplyr") else library(dplyr)
  if(!require(tidyverse)) install.packages("tidyverse") else library(tidyverse)
  #if(!require(gridExtra)) install.packages("gridExtra") else library(gridExtra)
  #if(!require(grid)) install.packages("grid") else library(grid)
  if(!require(xlsx)) install.packages("xlsx") else library(xlsx)
  if(!require(openxlsx)) install.packages("openxlsx") else library(openxlsx)
  if(!require(corrplot)) install.packages("corrplot") else library(corrplot)
  #if(!require(kableExtra)) install.packages("kableExtra") else library(kableExtra)
  #if(!require(psych)) install.packages("psych") else library(psych)
  if(!require(mvShapiroTest)) install.packages("mvShapiroTest") else library(mvShapiroTest)
  if(!require(MVar.pt)) install.packages(MVar.pt) else library(MVar.pt)
  #if(!require(kableExtra)) install.packages("kableExtra") else library(kableExtra)
  #if(!require(ppclust)) install.packages("ppclust") else library("ppclust")
  #if(!require(NbClust)) install.packages("NbClust") else library("NbClust")
  if(!require(packHV)) install.packages("packHV") else library(packHV)
  if(!require(readxl)) install.packages("reaxl") else library(readxl)
  if(!require(MASS)) install.packages("MASS") else library(MASS)
  if(!require(exploreR)) install.packages("exploreR") else library(exploreR)
  #if(!require(Hmisc)) install.packages("misc") else library(Hmisc)
  #if(!require(moments)) install.packages(moments) else require(moments)
  if(!require(openxlsx)) install.packages(openxlsx) else require(openxlsx)
  #if(!require(nortest)) install.packages(nortest) else require(nortest)
  if(!require(lmtest)) install.packages(lmtest) else require(lmtest)
  if(!require(forecast)) install.packages(forecast) else require(forecast)
  #if(!require(lubridate)) install.packages(lubridate) else library(lubridate)
  #library(xlsx)
  if(!require(RColorBrewer)) install.packages("RColorBrewer") else library(RColorBrewer)
  #library(rcompanion)
  #require(ggpubr)
  #if(!require(rbcb)) install.packages("rbcb") else library(rbcb)
  #if(!require(qqplotr)) install.packages("qqplotr") else library(qqplotr)
  #if(!require(car)) install.packages("car") else library(car)
  #if(!require(pROC)) install.packages("pROC") else library(pROC)
  #if(!require(mlogit)) install.packages("mlogit") else library(mlogit)
  #if(!require(readr)) install.packages("readr") else library(readr)
  #if(!require(rpart)) install.packages("rpart") else library(rpart)
  #if(!require(rpart.plot)) install.packages("rpart.plot") else library(rpart.plot)
  #if(!require(randomForest)) install.packages("randomForest") else library(randomForest)
  if(!require(lpSolve)) install.packages("lpSolve") else library (lpSolve)
  
  ### ### ### ### ### ### ### ### ###  1. LEITURA  E EXPLORAÇÃO DE DADOS ### ### ### ### ### ### ### ### ### 
  
  ### MacOS
  #dados =  read_excel("~/Library/CloudStorage/OneDrive-Pessoal/Documentos/7. Especialização/3. TCC/3. Códigos/tcc/BaseCompilada_TS14_2022.xlsx", 
  #                 sheet = "DadosEscores")
  
  ###  Windows
  library(readxl)
  dados <- read_excel("BaseCompilada_TS14_2022.xlsx",
                                        sheet = "DadosEscores")
  
  # View(dados)  

  ### Estatisticas - resumo
  summary(dados)
  
  ### Transformacao do IdAgente as char
  dados$IdAgente = as.character(dados$IdAgente)
  
  ### dataset com variaveis do modelo
  db = with(dados,dplyr::select(dados, Concessionaria, Tipo,
                        Ano, PMSO, rede.menor.230,
                        rede.maior.230,MVA,Mvar,
                        modulos.sub.menor230,modulos.sub.maior230,
                        modulos.manobra.menor230,modulos.manobra.maior230))
  #View(db)
 
  ### Pre analise
  require(exploreR)

  modelo_mass = masslm(db[,4:12], "PMSO")
  modelo_mass =  as.data.frame(modelo_mass)
  modelo_mass = modelo_mass[order(modelo_mass$R.squared,decreasing = TRUE),]
  print(modelo_mass)
  
  ### Variaveis do modelo 
  ### Ordenadas de acordo com a ordem crescem de pre-analise do R²        
                                                                  
  ### X1: Extensao de rede superior que 230 kV                          
  ### X2: Modulos de manobra com tensao igual ou superior a 230 kV          
  ### X3: Equipamentos de subestacao com tensao superior a 230 kV           
  ### x4: Potencia aparente total, em MVA, de equipamentos de subestacao    
  ### X5: Potencia reativa total, em Mvar, de equipamentos de subestacao    
  ### X6: Equipamentos de subestacao com tensao inferior a 230 kV           
  ### X7: Modulos de manobra com tensão inferior a 230 kV                   
  ### X8: Extensao de rede menor que 230 kV                                 
  ### Y : PMSO (variavel resposta)                                          
  
  ### Modificacao dos nomes das variaveis
  
  colnames(db)[colnames(db) == modelo_mass$IV[1]] = "X1"
  colnames(db)[colnames(db) == modelo_mass$IV[2]] = "X2"
  colnames(db)[colnames(db) == modelo_mass$IV[3]] = "X3"
  colnames(db)[colnames(db) == modelo_mass$IV[4]] = "X4"
  colnames(db)[colnames(db) == modelo_mass$IV[5]] = "X5"
  colnames(db)[colnames(db) == modelo_mass$IV[6]] = "X6"
  colnames(db)[colnames(db) == modelo_mass$IV[7]] = "X7"
  colnames(db)[colnames(db) == modelo_mass$IV[8]] = "X8"
  
  ### Gráficos
  
  ### Histograma
  with(db,packHV::hist_boxplot(PMSO, main="Histograma", 
                       col="light blue",
                       xlab="PMSO"))
  
  ### Scatter plots
  #par(mfrow=c(4,2))
    plot(PMSO ~ X1, data=db, pch=19, col="dark blue", main =  ""); title(main = "PMSO x X1")
    plot(PMSO ~ X2, data=db, pch=19, col="red", main =""); title(main = "PMSO x X2")
    plot(PMSO ~ X3, data=db, pch=19, col="green", main =""); title(main = "PMSO x X3")
    plot(PMSO ~ X4, data=db, pch=19, col="cyan", main =""); title(main = "PMSO x X4")
    plot(PMSO ~ X5, data=db, pch=19, col="grey",main =""); title(main = "PMSO x X5")
    plot(PMSO ~ X6, data=db, pch=19, col="orange",main =""); title(main = "PMSO x X6")
    plot(PMSO ~ X7, data=db, pch=19, col="purple", main =""); title(main = "PMSO x X7")
    plot(PMSO ~ X8, data=db, pch=19, col="black",main =""); title(main = "PMSO x X8")

  ### Correlation plots
  dbcor = with(db,dplyr::select(db, PMSO,X1,X2,X3,X4,X5,X6,X7,X8))
    
  matCor <- cor(dbcor, method="spearman")
    corrplot(matCor, type="upper",
             order="AOE", diag=FALSE, addgrid.col=NA,
             outline=TRUE)
  
  ### Boxplots
  with(db,
       boxplot(PMSO ~ Ano,
          main="PMSO anual", 
          col="dark blue", 
          xlab="Ano",
          ylab="PMSO")
  )
  
  with(db,
       boxplot(PMSO ~ Concessionaria,
               main="PMSO por conc.", 
               col="green", 
               xlab="Conc.",
               ylab="PMSO")
  )
  
  with(db,
       boxplot(PMSO ~ Tipo,
               main="PMSO por tipo", 
               col="orange", 
               xlab="Tipo",
               ylab="PMSO")
  )
  
  ### ### ### ### ### ### ### ### ###  2. INICIALIZAÇÃO PARA MODELO ### ### ### ### ### ### ### ### ###
  
  ### Analisando o R2 de cada variavel separadamente
  exploreR::masslm(dbcor,"PMSO")
  
  ### modelo linear múltiplo com todas as variaveis
  modelo = lm(PMSO ~ ., data=dbcor)
  summary(modelo)
  plot(modelo, lty=0, pch=19, col="blue")
  
  modelo$coefficients
  
  ### Shapiro-Wilk test | Null-hypothesis: population is normally distributed
  ### If p-value < alpha then nul-=hypothesis is rejected.
  ### standard alpha : 0.05
  
  shapiro.test(residuals(modelo))
  cat('Evidencias de que os residuos nao sao normalmente distribuidos')

    
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ## Implementacao Marcelo (02/12/2022) - Modelo Gama
  modelo <- glm(PMSO ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, 
                data=db, family=Gamma(link='identity'))
  modelo <- stepAIC(modelo)
  summary(modelo)
  
  # Validacao cruzada por empresa
  yfit <- rep(NA,nrow(db))
  for(emp in unique(db$Concessionaria)){
    pos   <- which(db$Concessionaria == emp)
    train <- db[-pos,]
    test  <- db[pos,]
    
    modelo <- glm(PMSO ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, 
                  data=train, family=Gamma(link='identity'))
    modelo <- stepAIC(modelo, trace=FALSE)
    
    yfit[pos] <- predict(modelo, newdata=test, type="response")
  }
  
  # Compara o estimado e o real
    plot(yfit ~ db$PMSO, pch=19, col="blue", ylab="estimated PMSO", 
         xlab="observed PMSO")
    abline(a=0, b=1, col="red", pch=19)
  
    # Escala log
    plot(yfit ~ db$PMSO, pch=19, col="blue", log="xy", 
         ylab="estimated PMSO", 
         xlab="observed PMSO")
    abline(a=0, b=1, col="red", pch=19)
    
  # R2 preditivo na escala original e na escala logaritmica
    y     <- db$PMSO
    SQT   <- sum( (y - mean(y))^2 )
    SQres <- sum( (y - yfit)^2 )
    (1 - SQres/SQT)
    
    # Escala log
    y     <- log(db$PMSO)
    SQT   <- sum( (y - mean(y))^2 )
    SQres <- sum( (y - log(yfit))^2 )
    (1 - SQres/SQT)
  ## - - - - - - - - - - - - - - - - - - - - - - - - - -     
  
  
  
  
  
  
  ### ### ### ### ### ### ### ### ###  2. IMPLEMENTAÇÃO DE MODELO LINEAR ### ### ### ### ### ### ### ### ###
  
  ### Modelo: 
  ### min sum(j in n) 0.5*e1_j + 0.5e2_j
  ### subject to: 
  ### (e1_j - e2_j) + (alpha1 - alpha2)  +
  ### beta_1*x1_j + beta_2*x_2_j + 
  ### beta_3*x_3_j + beta_4*x_4_j + 
  ### beta_5*x_5_j + beta_6*x_6_j +
  ### beta_7*x_7_j + beta_8*x_8_j  = y_j 
  ### beta_i >= 0
  
  ### Implementação orientações Prof. Marcelo Azevedo Costa (11/11/2022)
  ### Ajuste modelo linear via programação linear
  
  ### Parametros
  ### 
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
  
  ### Procedimento para otimizar o beta de cada regressao com a variave 'k'
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

  # par(mfrow=c(4,2))
    plot(PMSO ~ X1, data=db, pch=19, col="dark blue");title(main = "PMSO x X1")
    abline(a=dados_regressoes[1,1], b=dados_regressoes[2,1], lwd=2,lty = 2, col="red")
  
    plot(PMSO ~ X2, data=db, pch=19, col="black");title(main = "PMSO x X2")
    abline(a=dados_regressoes[1,2], b=dados_regressoes[2,2], lwd=2,lty = 2, col="red")
    
    plot(PMSO ~ X3, data=db, pch=19, col="green");title(main = "PMSO x X3")
    abline(a=dados_regressoes[1,3], b=dados_regressoes[2,3], lwd=2,lty = 2, col="red")
    
    plot(PMSO ~ X4, data=db, pch=19, col="cyan");title(main = "PMSO x X4")
    abline(a=dados_regressoes[1,4], b=dados_regressoes[2,4], lwd=2,lty = 2, col="red")
    
    plot(PMSO ~ X5, data=db, pch=19, col="grey");title(main = "PMSO x X5")
    abline(a=dados_regressoes[1,5], b=dados_regressoes[2,5], lwd=2,lty = 2, col="red")
    
    plot(PMSO ~ X6, data=db, pch=19, col="orange");title(main = "PMSO x X6")
    abline(a=dados_regressoes[1,6], b=dados_regressoes[2,6], lwd=2,lty = 2, col="red")
    
    plot(PMSO ~ X7, data=db, pch=19, col="purple");title(main = "PMSO x X7")
    abline(a=dados_regressoes[1,7], b=dados_regressoes[2,7], lwd=2,lty = 2, col="red")
    
    plot(PMSO ~ X8, data=db, pch=19, col="blue");title(main = "PMSO x X8")
    abline(a=dados_regressoes[1,8], b=dados_regressoes[2,], lwd=1, lty = 2, col="red")
    
    
    
    
  ###  MODELO COM TODAS AS VARIAVEIS 
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
    
  erros = solucao_t[1:N] - solucao_t[(N+1):(2*N)]
  alpha = solucao_t[(2*N)+1] - solucao_t[(2*N)+2]
    
  ### vetor para receber os valores dos betas
  betas = NULL
  for(i in 1:8){
    betas[i] = solucao_t[(2*N)+(i+2)]
  }
    
  resultado = c(alpha,betas)
  resultado = as.data.frame(resultado)
  rownames(resultado)[1] = "alpha"
  rownames(resultado)[2] = "beta1"
  rownames(resultado)[3] = "beta2"
  rownames(resultado)[4] = "beta3"
  rownames(resultado)[5] = "beta4"
  rownames(resultado)[6] = "beta5"
  rownames(resultado)[7] = "beta6"
  rownames(resultado)[8] = "beta7"
  rownames(resultado)[9] = "beta8"
  
  ### ### ### ### ### ### ### ### ###  3. ESTIMAÇÃO DO PMSO E ANÁLISE DO R²  ### ### ### ### ### ### ### ### ###

  ### Validacao -  remover uma empresa completamente da base)
  ### loop para retirar a empresa e estimar seu PMSO
  ### Ajustar o modelo e estimar o custo operacional da empresa que foi removida
  ### Calcular R2 preditivo
  ### Realziar validacão cruzada
  ### Calcular R2 -> usar logaritmo do PMSO
  
  ### Loop para calcular todas as empresas:
  ### 1. Criar um vetor contendo o nome de cada empresa presente na base
  ### 2. Pegar o tamanho do vetor
  ### 3. Função que cria um subconjunto da base dedos excluindo a empresa selecionada

  #db1 = filter(db,Concessionaria!= conc[1])
  #db2 = filter(db,Concessionaria == conc[1])
  #db_calc = NULL
  
  # rm(list = x1,x2,x3,x4,x5,x6,x7,x8,
  #    y_tr,t,TABULAR,obj,conc,db_tr,db_v,e1,e2,fcon,i,L,lambda,
  #    N_tr,nconc,x1,faux,fdir,frhs,conc,alpha,betas,solution,errors,outputs,mt,param,j,l)

  
  conc = db$Concessionaria
  conc = conc[!duplicated(conc)]
  nconc = length(conc)
  
  L = 0
  TABULAR = NULL
  #i=1
  for (i in 1:nconc){
    x1 = NULL;x2 = NULL;x3 = NULL; x4 = NULL;x5 = NULL;x6 = NULL;x7 = NULL; x8 = NULL
    db_tr = NULL
    db_v = NULL
    mt = NULL
    N_tr = NULL
    j = NULL
    
    db_tr = filter(db,Concessionaria!= conc[i])
    db_v = filter(db,Concessionaria == conc[i])
    
    # preparação para modelo linear
    y_tr = db_tr$PMSO # y da minimização dos erros
    lambda = 0.5 #parametro paras os erros
    N_tr = nrow(db_tr)
    
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
    outputs   = lpSolve::lp ("min", obj, fcon, fdir, frhs)
    solution = outputs$solution
    errors = solution[1:N_tr ] - solution[(N_tr+1):(2*N_tr )]
    alpha = solution[(2*N_tr )+1] - solution[(2*N_tr)+2]
    
    betas = NULL
    for(l in 1:8){
      betas[l] = solution[(2*N_tr)+(l+2)]
    }
    
    resultado = as.matrix(c(alpha,betas))
    param = as.vector(resultado[-1,])
    mt = with(db_v,data.frame(X1,X2,X3,X4,X5,X6,X7,X8))
    mt  = as.matrix(mt)
  
  
    for(j in 1:nrow(mt)){
      db_v$pmso_val[j] = (mt[j,] %*% param) + alpha
    }
    
    if(L ==0){
      TABULAR = db_v
      L = L+1 
      }else{
        TABULAR = rbind(TABULAR,db_v)
      }
  }
  

  TABULAR$diferenca = TABULAR$pmso_val - TABULAR$PMSO
  TABULAR$dif_per = round(abs((TABULAR$pmso_val/TABULAR$PMSO))-1,2)
  
  View(TABULAR)
  View(db)
  
  
  
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ## Implementacao Marcelo (02/12/2022) - Regressao lpSolve
  # Compara o estimado e o real
  plot(pmso_val ~ PMSO, pch=19, col="blue", ylab="estimated PMSO", 
       xlab="observed PMSO", data = TABULAR)
  abline(a=0, b=1, col="red", pch=19)
  
  # Escala log
  plot(pmso_val ~ PMSO, pch=19, col="blue", log="xy", 
       ylab="estimated PMSO", 
       xlab="observed PMSO", data = TABULAR)
  abline(a=0, b=1, col="red", pch=19)
  
  # R2 preditivo na escala original e na escala logaritmica
  y     <- TABULAR$PMSO
  yfit  <- TABULAR$pmso_val
  SQT   <- sum( (y - mean(y))^2 )
  SQres <- sum( (y - yfit)^2 )
  (1 - SQres/SQT)
  
  # Escala log
  pos   <- yfit > 0 
  y     <- log(TABULAR$PMSO[pos])
  yfit  <- TABULAR$pmso_val[pos]
  SQT   <- sum( (y - mean(y))^2 )
  SQres <- sum( (y - log(yfit))^2 )
  (1 - SQres/SQT)  
  
  
  dt01 <- subset(TABULAR, PMSO >= 359798.5)
  dt02 <- subset(TABULAR, PMSO < 359798.5)
  
  View(dt01)
  
  # Proximos passos: Modelo lpSolve para cada subgrupo
  # e FIM (em 02/12/2022)

    
    
  ### R-squared
  ### Soma dos quadrados da regressao
  ### Soma dos quadrados totais
  
  ### Calculo da media de PMSO para os calculcos do R squared
  
  # y_ = mean(db$PMSO)
  # 
  # ### Calculo dos residuos
  # 
  # ### Preparacao de matriz para prever com o ajuste do modelo de programacao linear
  # mt = with(db,data.frame(X1,X2,X3,X4,X5,X6,X7,X8))
  # mt  = as.matrix(mt)
  # 
  # ### Preparacao do vetor para a multiplicaca
  # ### param = as.vector(resultado[-1,])
  # 
  # 
  # fi = NULL
  # 
  # for(i in 1:nrow(mt)){
  #   fi[i] = (mt[i,] %*% param) + alpha
  #   cat(fi[i])
  #   cat("\n")
  # }
  # 
  # fi

  
  
  
  ### ### ### ### ### ### ### ### ###  4. MODELO GAMA ### ### ### ### ### ### ### ### ###
  
  ### Modelo linear generalizado - Distribuicao Gama - Familia Exponencial  
  ### Variancia = µˆ2 
  ### Função de ligação -> linear
  ### compare.fits | model.comparison | flex.plot
  
  ### Implementação do modelo
  ### 
  modelo_gama = glm(PMSO ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +X8, 
                    family = Gamma,
                    data = db)
  summary(modelo_gama)  
  
  pgama = 1-pchisq(modelo_gama$deviance,modelo_gama$df.residual)
  pgama  

  plot(modelo_gama)    

    

  
  
  