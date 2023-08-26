# Implementacao codigo regressao via otimizacao linear
# Marcelo A Costa
# 11/11/2022

 rm(list = ls())
 
 require(openxlsx)
 require(exploreR)
 require(lpSolve)
 
 dados <- read.xlsx("BaseCompilada_TS14_2022.xlsx",
                    sheet = "DadosEscores")
 
 # Aqui a analise eh para saber qual a variavel mais
 # importante.
 saida <- masslm(dados[,5:13], "PMSO")
 #View(saida)
 
 # Compara com o ajuste OLS (Minimos Quadrados - Regressao)
 plot(PMSO ~ rede.maior.230, data=dados, pch=19, col="blue")
 modelo.lm <- lm(PMSO ~ rede.maior.230, data=dados)
 abline(modelo.lm, lty=2, lwd=2, col="red")
 
 # Ajuste do modelo de regressao via Programacao Linear
 y      <- dados$PMSO
 x      <- dados$rede.maior.230
 N      <- nrow(dados)
 lambda <- 0.5 # Regressao quantilica (mediana : 0.5)
 
 # Definindo o vetor de parametros (regressao linear simples)
 # [e11,...,e1N; e21,...,e2N, alpha1, alpha2, beta]
 
 # Estou assumindo que o processo de otimizacao gera valores
 # positivos (o simplex assume que as variaveis sao positiva)
 # entao para criar uma solucao negativo (como os erros e o
 # intercepto), precisamos colocar duas variaveis: uma com o
 # sinal positivo e a outra com o sinal negativo
 
 # Montando o modelo de Programacao linear
 ## minimize: 
 f.obj <- c(rep(lambda,N), rep(1-lambda,N), 0, 0, 0)
 
 ## subject to
 f.con <- NULL
 
 # No loop sao inseridas as restricoes da equacao linear
 # para cada observacao do banco de dados
 for(cont in 1:nrow(dados)){
   erro1 <- rep(0, nrow(dados)) # Inicializacao do vetor de erro (positivo)
   erro2 <- rep(0, nrow(dados)) # Inicializacao do vetor de erro (negativo)
   
   erro1[cont] <- +1
   erro2[cont] <- -1
   
   f.aux <- c(erro1, erro2, +1, -1, x[cont])

   f.con <- rbind(f.con, f.aux)
   
 }

 f.dir <- rep("=", N)
 f.rhs <- y
 
 ## Gera a solucao
 saida   <- lp ("min", f.obj, f.con, f.dir, f.rhs)
 solucao <- saida$solution
 
 # Para algumas componentes: erro e intercepto existem as
 # 'partes' positivas e negativas. Entao, o valor final eh a
 # diferenca entre essas partes.
 erros <- solucao[1:N] - solucao[(N+1):(2*N)]
 alpha <- solucao[(2*N)+1] - solucao[(2*N)+2]
 beta  <- solucao[(2*N)+3]
 
 # c(alpha, beta)
 
 # Plota no grafico para fazer a comparacao entre o modelo de 
 # regressao linear e o modelo de regressao obtido utilizando
 # o lpSolve.
 abline(a=alpha, b=beta, lwd=2, col="black")


     plot(PMSO ~ X1, data=db, pch=19, col="dark blue", main =  ""); title(main = "PMSO x X1")
    plot(PMSO ~ X2, data=db, pch=19, col="red", main =""); title(main = "PMSO x X2")
    plot(PMSO ~ X3, data=db, pch=19, col="green", main =""); title(main = "PMSO x X3")
    plot(PMSO ~ X4, data=db, pch=19, col="cyan", main =""); title(main = "PMSO x X4")
    plot(PMSO ~ X5, data=db, pch=19, col="grey",main =""); title(main = "PMSO x X5")
    plot(PMSO ~ X6, data=db, pch=19, col="orange",main =""); title(main = "PMSO x X6")
    plot(PMSO ~ X7, data=db, pch=19, col="purple", main =""); title(main = "PMSO x X7")
    plot(PMSO ~ X8, data=db, pch=19, col="black",main ="");
