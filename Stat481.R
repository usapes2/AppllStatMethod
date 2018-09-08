# C.I. For Var_1/Var_2
# Independant samples n_1 and n_ 2 from N(mu_1,Sigma_1 ^2 ),N(mu_2,Sigma_2 ^2 )
# W/ sample varians S_1^2, S_2^2
# Measure alpha from 0 to 1 , to the right
# It is reasonable to believe Var_1 = Var_2 if the interval contains 1

Var1_2 <- function(alpha,df_1,df_2,SVar_1,SVar_2){
  
  l<-(1/qf(1-alpha,df1=df_1,df2=df_2))*SVar_1/SVar_2
  r<-qf(1-alpha,df1=df_2,df2=df_1)*SVar_1/SVar_2
  round(c(l,r),2)
}
