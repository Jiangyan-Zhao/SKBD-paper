Scenario = Dose = matrix(0, nrow = 6, ncol = 5)
target_prob = rep(0.3,6)
MTD = rep(0,6)

Emax_para = function(d, p){
  gamma = ( log(1-p[1])-log(p[1])-log(1-p[2])+log(p[2]) )/
    (log(d[2])-log(d[1]))
  EC50 = exp((log(1-p[1])-log(p[1]))/gamma+log(d[1]))
  return(c(gamma, EC50))
}

s=1
Dose[s, ]=c(5,15,25,35,45)
para=Emax_para(c(5,15),c(0.14,0.45))
gamma=para[1];EC50=para[2]
Scenario[s, ]=Dose[s, ]^gamma/(Dose[s, ]^gamma+EC50^gamma)
MTD[s]=exp(log(EC50)+(log(target_prob[s])-log(1-target_prob[s]))/gamma)

s=2
Dose[s, ]=c(5,10,20,35,60)
para=Emax_para(c(10,20),c(0.14,0.45))
gamma=para[1];EC50=para[2]
Scenario[s, ]=Dose[s, ]^gamma/(Dose[s, ]^gamma+EC50^gamma)
MTD[s]=exp(log(EC50)+(log(target_prob[s])-log(1-target_prob[s]))/gamma)

s=3
Dose[s, ]=c(5,7.5,15,30,60)
para=Emax_para(c(15,30),c(0.2,0.5))
gamma=para[1];EC50=para[2]
Scenario[s, ]=Dose[s, ]^gamma/(Dose[s, ]^gamma+EC50^gamma)
MTD[s]=exp(log(EC50)+(log(target_prob[s])-log(1-target_prob[s]))/gamma)

s=4
Dose[s, ]=c(1,1.5,3,5,10)
para=Emax_para(c(5,10),c(0.2,0.45))
gamma=para[1];EC50=para[2]
Scenario[s, ]=Dose[s, ]^gamma/(Dose[s, ]^gamma+EC50^gamma)
MTD[s]=exp(log(EC50)+(log(target_prob[s])-log(1-target_prob[s]))/gamma)

s=5
Dose[s, ]=c(10,20,30,40,50)
para=Emax_para(c(10,20),c(0.45,0.55))
gamma=para[1];EC50=para[2]
Scenario[s, ]=Dose[s, ]^gamma/(Dose[s, ]^gamma+EC50^gamma)
MTD[s]=exp(log(EC50)+(log(target_prob[s])-log(1-target_prob[s]))/gamma)

s=6
Dose[s, ]=c(5,10,20,35,50)
para=Emax_para(c(35,50),c(0.15,0.2))
gamma=para[1];EC50=para[2]
Scenario[s, ]=Dose[s, ]^gamma/(Dose[s, ]^gamma+EC50^gamma)
MTD[s]=exp(log(EC50)+(log(target_prob[s])-log(1-target_prob[s]))/gamma)

for (s in 1:6) {
  cat( sprintf( "%.2f", Scenario[s, ] ) , sep = " & " ) 
  cat("\n")
  cat( sprintf( "%.2f", Dose[s, ] ) , sep = " & " ) 
  cat("\n")
  cat("\n")
}
