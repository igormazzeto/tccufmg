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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[1]))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[2]))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[3]))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[4]))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[5]))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[6]))) +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[7])))+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
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
  ggtitle(expression(paste(bold("Modelo linear"))),
          subtitle = expression(paste("PMSO vs ", X[8])))+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold", size = 8),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(margin = margin(0, 0, 0.3, 0,"cm")),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0.5,"cm")),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"))
