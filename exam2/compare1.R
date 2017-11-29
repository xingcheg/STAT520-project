CFM <- function(method=c(1,2,3,4,5,6), para=c("mu","lambda")){
  if (method == 1){
    ci_mu <- function(x) ci_mu_1(x)
    ci_la <- function(x) ci_la_1(x)
  }
  if (method == 2){
    ci_mu <- function(x) ci_mu_2(x)
    ci_la <- function(x) ci_la_2(x)
  }
  if (method == 3){
    ci_mu <- function(x) ci_boot_mu_1(x,2000)
    ci_la <- function(x) ci_boot_la_1(x,2000)
  }
  if (method == 4){
    ci_mu <- function(x) ci_boot_mu_2(x,2000)
    ci_la <- function(x) ci_boot_la_2(x,2000)
  }
  if (method == 5){
    ci_mu <- function(x) ci_boot_mu_3(x,2000)
    ci_la <- function(x) ci_boot_la_3(x,2000)
  }
  if (method == 6){
    ci_mu <- function(x) ci_boot_mu_4(x,2000)
    ci_la <- function(x) ci_boot_la_4(x,2000)
  }
  
  
  
  
  #############################################
  criteria <- function(M,n = 10, mu = 5,lambda = 2, para = c("mu","lambda")){
    set.seed(520520)
    library(statmod)
    if (para == "mu"){
      IG <- matrix(rinvgauss(M*n,mu,lambda), ncol=M)
      CImu <- apply(IG,2,ci_mu)
      ID <- (CImu[1,]<mu)&(CImu[2,]>mu)
      CI <- t(CImu)
    }
    
    if (para == "lambda"){
      IG <- matrix(rinvgauss(M*n,mu,lambda), ncol=M)
      CIla <- apply(IG,2,ci_la)
      ID <- (CIla[1,]<lambda)&(CIla[2,]>lambda)
      CI <- t(CIla)
    }
    
    rate <- mean(ID)
    width <- CI[,2]-CI[,1]
    med_width <- median(width)
    prob <- mean(CI[,1]<0)
    result <- list(rate = rate,medianWidth = med_width, Prob = prob)
    return(result)
  }
  
  
  
  ##############################################
  
  
  
  
  L <- rep(c(2,4,8,12),each=5)
  NN <- rep(c(10,25,50,100,500),4)
  n <- length(L)
  CR <- NULL
  W <- NULL
  P <- NULL
  for (i in 1:n){
    CT <- criteria(M=10000,n=NN[i],lambda=L[i],para=para)
    CR[i] <-CT[[1]]
    W[i] <- CT[[2]]
    P[i] <- CT[[3]]
  }
  DD <- data.frame(lambda = L,n = NN,
                   cov_rate = CR, median_width = W,
                   prob_out_of_supp = P)
  return(DD)
}


CFM(method = 1, para="mu")



