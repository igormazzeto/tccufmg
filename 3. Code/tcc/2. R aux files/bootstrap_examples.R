### BOOTSTRAP PROCEDURE INICIALIZATION

set.seed(587358)
B = 10 # Bootstrap resamples
boot_table = NULL # tabela com resultados
Q = 0 # auxiliar
db1 = db #base de dados para bootstrap

for(w in 1:B){
  
  # resample  
  db1 = db1[sample(nrow(db1),replace = TRUE),]
  
  # boot variables
  y_boot = db1$PMSO
  N_boot = nrow(db1)
  tau = 0.5
  
  # reset variables
  
  f.aux_boot = NULL; f.con_boot = NULL; erros_boot = NULL;
  f.dir_boot = NULL; f.obj_boot = NULL; f.dir_boot  = NULL;
  betas_boot = NULL; resultado_boot = NULL; alpha_boot = NULL;
  saida_boot = NULL; solucao_boot;
  
  x1 = NULL;x2 = NULL;x3 = NULL; x4 = NULL;x5 = NULL;
  x6 = NULL;x7 = NULL; x8 = NULL; rbt = NULL;
  
  # assign variables
  
  x1 = db1$X1;x2 = db1$X2;x3 = db1$X3; x4 = db1$X4;
  x5 = db1$X5;x6 = db1$X6;x7 = db1$X7;x8 = db1$X8
  
  # objective function
  f.obj_boot = c(rep(tau,N_boot), rep(1-tau,N_boot),0,0,0,0,0,0,0,0,0,0)
  
  ### Constraints inicialization
  for(cont in 1:nrow(db1)){
    e1_boot = rep(0, nrow(db1)) # Inicializacao do vetor de erro (positivo)
    e2_boot = rep(0, nrow(db1)) # Inicializacao do vetor de erro (negativo)
    e1_boot[cont] = +1
    e2_boot[cont] = -1
    f.aux_boot <- c(e1, e2, +1, -1,x1[cont],x2[cont],x3[cont],x4[cont],x5[cont],x6[cont],x7[cont],x8[cont])
    f.con_boot <- rbind(f.con_boot, f.aux_boot)
  } 
  
  # constraint direction
  f.dir_boot = rep("=", N_boot)
  
  # right side
  f.rhs_boot = y_boot
  
  # Generate solution
  saida_boot = lpSolve::lp ("min", f.obj_boot, f.con_t, f.dir_t, f.rhs_t)
  
  # store solution
  solucao_boot = saida_boot$solution
  
  # Errors, alpha and beta
  
  erros_boot = soluca_boot[1:N_boot] - solucao_boot[(N_boot+1):(2*N_boot)]
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
  