setwd("/home/xguo/520server")


################ Method1 MLE & Wald Theory ####################
mle_mu <- function(x) return( mean(x) )
mle_la <- function(x) return( 1 / ( mean(1/x) - 1/mean(x) ) )

ci_mu_1 <- function(x){
  sigma <- sqrt( (mle_mu(x))^3/(length(x)*mle_la(x)) )
  lb <- mle_mu(x) - 1.96*sigma
  ub <- mle_mu(x) + 1.96*sigma
  return(c(lb,ub))
}

ci_la_1 <- function(x){
  sigma <- sqrt( (2*(mle_la(x))^2)/length(x) )
  lb <- mle_la(x) - 1.96*sigma
  ub <- mle_la(x) + 1.96*sigma
  return(c(lb,ub))
}

################ Method2 MLE & Inverse LRT ####################

## lambda 
LR_la <- function(la,x){
  n <- length(x)
  return((sum(1/x) - (n/mle_mu(x)) )*(la - mle_la(x)) - n*log((la)/mle_la(x)))
}


ci_la_2 <- function(x){
  me <- mle_la(x)
  ci <- ci_la_1(x)
  n <- length(x)
  if (n <= 20) b <- 5*(me-ci[1])
  if (n>20 & n <=50) b <- 3*(me-ci[1])
  if (n>50) b <- 2*(me-ci[1])
  st <- max(me-b,0.001)
  ed <- me+b
  XX <- seq(st,ed,0.001)
  LR <- LR_la( XX, x )
  C <- qchisq(0.95,1)
  index1 <- which(LR < C)[1]
  index2 <- which(LR < C)[length(which(LR < C))]
  return(c(XX[index1],XX[index2]))
}

##mu
til_la <- function(mu,x){
  Y <- (mle_mu(x))/(mu^2) - (2/mu) + mean(1/x)
  return(1/Y)
}

LR_mu <- function(mu,x){
  n <- length(x)
  L1 <- (n/2)*log(til_la(mu,x)) - ((til_la(mu,x))/(2*mu^2))*sum(x) +
    (n*til_la(mu,x))/mu - (til_la(mu,x)/2)*sum(1/x)
  L2 <- (n/2)*log(mle_la(x)) + (n*mle_la(x))/(2*mle_mu(x)) - (mle_la(x)/2)*sum(1/x)
  return((-2)*(L1-L2))
}

ci_mu_2 <- function(x){
  me <- mle_mu(x)
  ci <- ci_mu_1(x)
  n <- length(x)
  if (n <= 20) b <- 15*(me-ci[1])
  if (n>20 & n <=50) b <- 10*(me-ci[1])
  if (n>50 & n<=100) b <- 5*(me-ci[1])
  if (n>100 & n<=200) b <- 3*(me-ci[1])
  if(n>200) b <- 2*(me-ci[1])
  st <- max(me-b,0.00001)
  ed <- me+b
  XX <- seq(st,ed,0.001)
  LR <- LR_mu( XX, x )
  C <- qchisq(0.95,1)
  index1 <- which(LR < C)[1]
  index2 <- which(LR < C)[length(which(LR < C))]
  return(c(XX[index1],XX[index2]))
}


################ Method3 MLE & Parametric bootstrap 1 ####################

ci_boot_mu_1 <- function(x,N){
  library(statmod)
  mu <- mle_mu(x)
  la <- mle_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  mu1 <- apply(X,2,mle_mu)
  mu1 <- sort(mu1)
  qmu1 <- mu1[ floor( (N+1)*(1-(0.05/2))) ]
  qmu2 <- mu1[ floor( (N+1)*(0.05/2) ) ]
  lb <- 2*mu-qmu1
  ub <- 2*mu-qmu2
  return(c(lb,ub))
}

ci_boot_la_1 <- function(x,N){
  library(statmod)
  mu <- mle_mu(x)
  la <- mle_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  la1 <- apply(X,2,mle_la)
  la1 <- sort(la1)
  qla1 <- la1[ floor( (N+1)*(1-(0.05/2))) ]
  qla2 <- la1[ floor( (N+1)*(0.05/2) ) ]
  lb <- 2*la-qla1
  ub <- 2*la-qla2
  return(c(lb,ub))
}


################ Method4 MLE & Parametric bootstrap 2 ####################
ci_boot_mu_2 <- function(x,N){
  library(statmod)
  mu <- mle_mu(x)
  la <- mle_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  mu1 <- apply(X,2,mle_mu)
  mu1 <- sort(mu1)
  qmu1 <- mu1[ floor( (N+1)*(1-(0.05/2))) ]
  qmu2 <- mu1[ floor( (N+1)*(0.05/2) ) ]
  lb <- mu^2/qmu1
  ub <- mu^2/qmu2
  return(c(lb,ub))
}

ci_boot_la_2 <- function(x,N){
  library(statmod)
  mu <- mle_mu(x)
  la <- mle_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  la1 <- apply(X,2,mle_la)
  la1 <- sort(la1)
  qla1 <- la1[ floor( (N+1)*(1-(0.05/2))) ]
  qla2 <- la1[ floor( (N+1)*(0.05/2) ) ]
  lb <- la^2/qla1
  ub <- la^2/qla2
  return(c(lb,ub))
}


################ Method5 MLE & Parametric bootstrap 3 ####################
Vmu <- function(x){
  n <- length(x)
  la <- mle_la(x)
  mu <- mle_mu(x)
  return((mu^3)/(n*la))
}

Vla <- function(x){
  n <- length(x)
  la <- mle_la(x)
  return((2*la^2)/n)
}

ci_boot_mu_3 <- function(x,N){
  library(statmod)
  mu <- mle_mu(x)
  la <- mle_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  mu1 <- apply(X,2,mle_mu) - mu
  V1 <- apply(X,2,Vmu)
  mu11 <- sort(mu1/sqrt(V1))
  qmu1 <- mu11[ floor( (N+1)*(1-(0.05/2))) ]
  qmu2 <- mu11[ floor( (N+1)*(0.05/2) ) ]
  lb <- mu-sqrt(Vmu(x))*qmu1
  ub <- mu-sqrt(Vmu(x))*qmu2
  return(c(lb,ub))
}


ci_boot_la_3 <- function(x,N){
  library(statmod)
  mu <- mle_mu(x)
  la <- mle_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  la1 <- apply(X,2,mle_la) - la
  V1 <- apply(X,2,Vla)
  la11 <- sort(la1/sqrt(V1))
  qla1 <- la11[ floor( (N+1)*(1-(0.05/2))) ]
  qla2 <- la11[ floor( (N+1)*(0.05/2) ) ]
  lb <- la-sqrt(Vla(x))*qla1
  ub <- la-sqrt(Vla(x))*qla2
  return(c(lb,ub))
}


################ Method6 MME & Parametric bootstrap 4 ####################
mme_mu <- function(x) return(mean(x))
mme_la <- function(x) return( ((mean(x))^3)/(mean(x^2)-(mean(x))^2) )

ci_boot_mu_4 <- function(x,N){
  library(statmod)
  mu <- mme_mu(x)
  la <- mme_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  mu1 <- apply(X,2,mme_mu)
  mu1 <- sort(mu1)
  qmu1 <- mu1[ floor( (N+1)*(1-(0.05/2))) ]
  qmu2 <- mu1[ floor( (N+1)*(0.05/2) ) ]
  lb <- 2*mu-qmu1
  ub <- 2*mu-qmu2
  return(c(lb,ub))
}


ci_boot_la_4 <- function(x,N){
  library(statmod)
  mu <- mme_mu(x)
  la <- mme_la(x)
  n <- length(x)
  X <- matrix(rinvgauss(n*N,mu,la),ncol = N)
  la1 <- apply(X,2,mme_la)
  la1 <- sort(la1)
  qla1 <- la1[ floor( (N+1)*(1-(0.05/2))) ]
  qla2 <- la1[ floor( (N+1)*(0.05/2) ) ]
  lb <- 2*la-qla1
  ub <- 2*la-qla2
  return(c(lb,ub))
}















cov_rate <- function(M,n = 10, mu = 5,lambda = 2, para = c("mu","lambda")){
  set.seed(520520)
  library(statmod)
  ID <- NULL
  
  if (para == "mu"){
    IG <- matrix(rinvgauss(M*n,mu,lambda), ncol=M)
    CImu <- apply(IG,2,ci_mu)
    ID <- (CImu[1,]<mu)&(CImu[2,]>mu)
  }
  
  if (para == "lambda"){
    IG <- matrix(rinvgauss(M*n,mu,lambda), ncol=M)
    CIla <- apply(IG,2,ci_la)
    ID <- (CIla[1,]<lambda)&(CIla[2,]>lambda)
  }
  
  rate <- mean(ID)
  sd_r <- sqrt(rate*(1-rate)/M)
  ci_rate <- c(rate-1.96*sd_r,rate+1.96*sd_r)
  width <- ci_rate[2] - ci_rate[1]
  result <- list(rate = rate, CI = ci_rate, CIwidth = width)
  return(result)
}


criteria23 <- function(M,n = 10, mu = 5,lambda = 2, para = c("mu","lambda")){
  set.seed(520520)
  library(statmod)
  CI <- NULL
  if (para == "mu"){
    IG <- matrix(rinvgauss(M*n,mu,lambda), ncol=M)
    CI <- t(apply(IG,2,ci_mu))
  }
  
  if (para == "lambda"){
    IG <- matrix(rinvgauss(M*n,mu,lambda), ncol=M)
    CI <- t(apply(IG,2,ci_la))
  }
  
  width <- CI[,2]-CI[,1]
  med_width <- median(width)
  prob <- mean(CI[,1]<0)
  result <- list(medianWidth = med_width, Prob = prob)
  return(result)
}














M0 <- c(100,250,500,1000*c(1:10),
        12000,16000,20000,30000,50000,100000)

ci_mu <- function(x) ci_mu_1(x)
CR1 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR1[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

ci_mu <- function(x) ci_mu_2(x)
CR2 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR2[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

ci_mu <- function(x) ci_boot_mu_1(x,2000)
CR3 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR3[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

ci_mu <- function(x) ci_boot_mu_2(x,2000)
CR4 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR4[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

ci_mu <- function(x) ci_boot_mu_3(x,2000)
CR5 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR5[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

ci_mu <- function(x) ci_boot_mu_4(x,2000)
CR6 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR6[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

library(ggplot2)
d3 <- data.frame(x = rep(sqrt(M0),6), y = c(CR1,CR2,CR3,CR4,CR5,CR6), 
                 z = rep(c("method1","method2","method3","method4","method5","method6"),each=length(M0)))





save(d3,file="d3.RData")