library(statmod)
set.seed(123)
N0 <- c(50,100*(1:100))
n <- length(N0)

W11 <- rep(0,n)
y11 <- rinvgauss(10,5,2)
for (i in 1:n){
  Q <- ci_boot_la_1(y11,N0[i])
  W11[i]<- Q[2]-Q[1]
}

W12 <- rep(0,n)
y12 <- rinvgauss(50,5,4)
for (i in 1:n){
  Q <- ci_boot_la_1(y12,N0[i])
  W12[i]<- Q[2]-Q[1]
}

W13 <- rep(0,n)
y13 <- rinvgauss(100,5,8)
for (i in 1:n){
  Q <- ci_boot_la_1(y13,N0[i])
  W13[i]<- Q[2]-Q[1]
}

W14 <- rep(0,n)
y14 <- rinvgauss(500,5,12)
for (i in 1:n){
  Q <- ci_boot_la_1(y14,N0[i])
  W14[i]<- Q[2]-Q[1]
}

W21 <- rep(0,n)
y21 <- rinvgauss(50,5,2)
for (i in 1:n){
  Q <- ci_boot_la_2(y21,N0[i])
  W21[i]<- Q[2]-Q[1]
}

W22 <- rep(0,n)
y22 <- rinvgauss(100,5,4)
for (i in 1:n){
  Q <- ci_boot_la_2(y22,N0[i])
  W22[i]<- Q[2]-Q[1]
}

W23 <- rep(0,n)
y23 <- rinvgauss(500,5,8)
for (i in 1:n){
  Q <- ci_boot_la_2(y23,N0[i])
  W23[i]<- Q[2]-Q[1]
}

W24 <- rep(0,n)
y24 <- rinvgauss(10,5,12)
for (i in 1:n){
  Q <- ci_boot_la_2(y24,N0[i])
  W24[i]<- Q[2]-Q[1]
}

W31 <- rep(0,n)
y31 <- rinvgauss(100,5,2)
for (i in 1:n){
  Q <- ci_boot_la_3(y31,N0[i])
  W31[i]<- Q[2]-Q[1]
}

W32 <- rep(0,n)
y32 <- rinvgauss(500,5,4)
for (i in 1:n){
  Q <- ci_boot_la_3(y32,N0[i])
  W32[i]<- Q[2]-Q[1]
}

W33 <- rep(0,n)
y33 <- rinvgauss(10,5,8)
for (i in 1:n){
  Q <- ci_boot_la_3(y33,N0[i])
  W33[i]<- Q[2]-Q[1]
}

W34 <- rep(0,n)
y34 <- rinvgauss(50,5,12)
for (i in 1:n){
  Q <- ci_boot_la_3(y34,N0[i])
  W34[i]<- Q[2]-Q[1]
}

W41 <- rep(0,n)
y41 <- rinvgauss(500,5,2)
for (i in 1:n){
  Q <- ci_boot_la_4(y41,N0[i])
  W41[i]<- Q[2]-Q[1]
}

W42 <- rep(0,n)
y42 <- rinvgauss(10,5,4)
for (i in 1:n){
  Q <- ci_boot_la_4(y42,N0[i])
  W42[i]<- Q[2]-Q[1]
}

W43 <- rep(0,n)
y43 <- rinvgauss(50,5,8)
for (i in 1:n){
  Q <- ci_boot_la_4(y43,N0[i])
  W43[i]<- Q[2]-Q[1]
}

W44 <- rep(0,n)
y44 <- rinvgauss(100,5,12)
for (i in 1:n){
  Q <- ci_boot_la_4(y44,N0[i])
  W44[i]<- Q[2]-Q[1]
}




par(mfcol=c(4,4))
plot(N0,W11,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 3 for lambda (n=10, lambda=2)")
plot(N0,W12,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 3 for lambda (n=50, lambda=4)")
plot(N0,W13,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 3 for lambda (n=100, lambda=8)")
plot(N0,W14,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 3 for lambda (n=500, lambda=12)")
plot(N0,W21,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 4 for lambda (n=50, lambda=2)")
plot(N0,W22,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 4 for lambda (n=100, lambda=4)")
plot(N0,W23,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 4 for lambda (n=500, lambda=8)")
plot(N0,W24,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 4 for lambda (n=10, lambda=12)")

plot(N0,W31,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 5 for lambda (n=100, lambda=2)")
plot(N0,W32,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 5 for lambda (n=500, lambda=4)")
plot(N0,W33,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 5 for lambda (n=10, lambda=8)")
plot(N0,W34,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 5 for lambda (n=50, lambda=12)")
plot(N0,W41,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 6 for lambda (n=500, lambda=2)")
plot(N0,W42,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 6 for lambda (n=10, lambda=4)")
plot(N0,W43,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 6 for lambda (n=50, lambda=8)")
plot(N0,W44,type = "l", xlab = "N",
     ylab = "CI Width", main = "Method 6 for lambda (n=100, lambda=12)")
par(mfcol=c(1,1))