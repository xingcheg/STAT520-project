ci_mu <- function(x) ci_mu_1(x)
ci_la <- function(x) ci_la_1(x)

##### bootstrap
# ci_mu <- function(x) ci_boot_mu_1(x,2000)
# ci_la <- function(x) ci_boot_la_1(x,2000)
#####

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





