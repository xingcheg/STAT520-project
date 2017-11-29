### 1
ci_mu <- function(x) ci_mu_1(x)
M0 <- c(100,250,500,1000*c(1:10),
        12000,14000,16000,18000,20000,
        25000,30000,40000,50000,100000)
CR1 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR1[i] <- cov_rate(M=M0[i],n=100,mu=5,lambda=2,para="mu")[[1]]
}

CR2 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR2[i] <- cov_rate(M=M0[i],n=100,mu=5,lambda=4,para="mu")[[1]]
}

CR3 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR3[i] <- cov_rate(M=M0[i],n=100,mu=5,lambda=8,para="mu")[[1]]
}

CR4 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR4[i] <- cov_rate(M=M0[i],n=100,mu=5,lambda=12,para="mu")[[1]]
}

library(ggplot2)
d1 <- data.frame(x = rep(sqrt(M0),4), y = c(CR1,CR2,CR3,CR4), 
                 z = rep(c("lambda-2","lambda-4","lambda-8","lambda-12"),each=length(M0)))
ggplot(data = d1) + geom_line(aes(x = x, y = y, colour = z)) + 
  ggtitle("coverage rate of mu vs M (when: n=100, mu=5, method=1)") + 
  theme_bw() +
  ylab("coverage rate") +
  xlab("sqrt(M)")
  



### 2
CR1 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR1[i] <- cov_rate(M=M0[i],n=10,mu=5,lambda=4,para="mu")[[1]]
}

CR2 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR2[i] <- cov_rate(M=M0[i],n=25,mu=5,lambda=4,para="mu")[[1]]
}

CR3 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR3[i] <- cov_rate(M=M0[i],n=50,mu=5,lambda=4,para="mu")[[1]]
}

CR4 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR4[i] <- cov_rate(M=M0[i],n=100,mu=5,lambda=4,para="mu")[[1]]
}

CR5 <- rep(0,length(M0))
for (i in 1:length(M0)){
  CR5[i] <- cov_rate(M=M0[i],n=500,mu=5,lambda=4,para="mu")[[1]]
}

library(ggplot2)
d2 <- data.frame(x = rep(sqrt(M0),5), y = c(CR1,CR2,CR3,CR4,CR5), 
                 z = rep(c("n=10","n=25","n=50","n=100","n=500"),each=length(M0)))
ggplot(data = d2) + geom_line(aes(x = x, y = y, colour = z)) + 
  ggtitle("coverage rate of mu vs M (when: mu=5, lambda=4, method=1)") + 
  theme_bw() +
  ylab("coverage rate") +
  xlab("sqrt(M)")


###3
setwd("/Users/apple/Desktop/ISU 2017 fall/STAT520/exam2")
load("d3.RData")

library(ggplot2)

ggplot(data = d3) + geom_line(aes(x = x, y = y, colour = z)) + 
  ggtitle("coverage rate of mu vs M (when: mu=5, lambda=4, n=10)") + 
  theme_bw() +
  ylab("coverage rate") +
  xlab("sqrt(M)")