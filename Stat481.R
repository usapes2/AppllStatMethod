

# Standardized R.V. z
# X - variable , mu - population mean, sigma^2 - population variance 
SRVZ <- function(x,mu,sigma) {
  (x-mu)/(sqrt(sigma))
}

# C.I for difference in means, when variances are unkown
# Alpha is measured 0 to 1
Mean_diff_u <- function (alpha,n_1,n_2,mu_1,mu_2,var_1,var_2){
  z <- qnorm(alpha/2,lower.tail = FALSE)
  l <- (mu_1 - mu_2 ) - z*sqrt((var_1/n_1)+(var_2/n_2))
  r <- (mu_1 - mu_2 ) + z*sqrt((var_1/n_1)+(var_2/n_2))
  c(l,r)
}

# C.I for variance given sample:size,mean,variance
# Alpha measured 0 to 1
CI_For_Var<-function(alpha,n,SampleVar){
  div1<-qchisq(alpha/2,lower.tail = FALSE,df=n-1)
  div2<-qchisq(1-alpha/2,lower.tail = FALSE,df=n-1)
  l<-(n-1)*SampleVar/div1
  r<-(n-1)*SampleVar/div2
  round(c(l,r),3)
}

# C.I. For Var_1/Var_2
# Independent samples n_1 and n_ 2 from N(mu_1,Sigma_1 ^2 ),N(mu_2,Sigma_2 ^2 )
# W/ sample varians S_1^2, S_2^2
# Measure alpha from 0 to 1 , to the right
# It is reasonable to believe Var_1 = Var_2 if the interval contains 1

Var1_2 <- function(alpha,df_1,df_2,SVar_1,SVar_2){
  
  l<-(1/qf(1-alpha,df1=df_1,df2=df_2))*SVar_1/SVar_2
  r<-qf(1-alpha,df1=df_2,df2=df_1)*SVar_1/SVar_2
  round(c(l,r),2)
}
