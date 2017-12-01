######################################################
CR_mu1 <- as.data.frame(
  rbind(as.matrix(CR_mu[,c(1,2,3)]),
                as.matrix(CR_mu[,c(1,2,4)]),
                as.matrix(CR_mu[,c(1,2,5)]),
                as.matrix(CR_mu[,c(1,2,6)]),
                as.matrix(CR_mu[,c(1,2,7)]),
                as.matrix(CR_mu[,c(1,2,8)])) )
 Me <- rep( paste("m",1:6,sep=""), each = 20)
 CR_mu2 <- cbind(CR_mu1,Me) 
 names(CR_mu2) <- c("la","n","cr","method")
 
 
 
 #######################################################
 CR_mu2_la <- CR_mu2
 CR_mu2_la[,1] <- as.factor(CR_mu2_la[,1])
 CR_mu2_la[,2] <- log(CR_mu2_la[,2])
 names(CR_mu2_la) <- c("la","logn","cr","method")
 
 library(ggplot2)
 ggplot(data = CR_mu2_la)+geom_line(aes(x = logn, y = cr, colour = method)) +
   theme_bw() +
   facet_grid(~la) + 
   ggtitle("Coverage rate for mu")
 
 
########################################################### 
 CR_mu2_n <- CR_mu2
 CR_mu2_n[,2] <- as.factor(CR_mu2_n[,2])
 library(ggplot2)
 ggplot(data = CR_mu2_n)+geom_line(aes(x = la, y = cr, colour = method)) +
   theme_bw() +
   facet_grid(~n) + 
   ggtitle("Coverage rate for mu for 5 different n")
 
 
########################################################### 
 MW_mu1 <- as.data.frame(
   rbind(as.matrix(MW_mu[,c(1,2,3)]),
         as.matrix(MW_mu[,c(1,2,4)]),
         as.matrix(MW_mu[,c(1,2,5)]),
         as.matrix(MW_mu[,c(1,2,6)]),
         as.matrix(MW_mu[,c(1,2,7)]),
         as.matrix(MW_mu[,c(1,2,8)])) )

 MW_mu2 <- cbind(MW_mu1,Me) 
 names(MW_mu2) <- c("la","n","mw","method")
 
 #######################################################
 MW_mu2_la <- MW_mu2
 MW_mu2_la[,1] <- as.factor(MW_mu2_la[,1])
 MW_mu2_la[,2] <- log(MW_mu2_la[,2])
 MW_mu2_la[,3] <- log(MW_mu2_la[,3])
 names(MW_mu2_la) <- c("la","log_n","log_mw","method")
 
 library(ggplot2)
 ggplot(data = MW_mu2_la)+geom_line(aes(x = log_n, y = log_mw, colour = method)) +
   theme_bw() +
   facet_grid(~la) + 
   ggtitle("Median width for mu")
 
 
 #######################################################
 MW_mu2_n <- MW_mu2
 MW_mu2_n[,2] <- as.factor(MW_mu2_n[,2])
 MW_mu2_n[,3] <- log(MW_mu2_n[,3])
 names(MW_mu2_n) <- c("la","n","log_mw","method")
 
 library(ggplot2)
 ggplot(data =  MW_mu2_n)+geom_line(aes(x = la, y = log_mw, colour = method)) +
   theme_bw() +
   facet_grid(~n) + 
   ggtitle("Median width for mu for 5 different n")
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
###############################################################
 ######################################################
 CR_la1 <- as.data.frame(
   rbind(as.matrix(CR_la[,c(1,2,3)]),
         as.matrix(CR_la[,c(1,2,4)]),
         as.matrix(CR_la[,c(1,2,5)]),
         as.matrix(CR_la[,c(1,2,6)]),
         as.matrix(CR_la[,c(1,2,7)]),
         as.matrix(CR_la[,c(1,2,8)])) )
 Me <- rep( paste("m",1:6,sep=""), each = 20)
 CR_la2 <- cbind(CR_la1,Me) 
 names(CR_la2) <- c("la","n","cr","method")
 
 
 
 #######################################################
 CR_la2_la <- CR_la2
 CR_la2_la[,1] <- as.factor(CR_la2_la[,1])
 CR_la2_la[,2] <- log(CR_la2_la[,2])
 names(CR_la2_la) <- c("la","logn","cr","method")
 
 library(ggplot2)
 ggplot(data = CR_la2_la)+geom_line(aes(x = logn, y = cr, colour = method)) +
   theme_bw() +
   facet_grid(~la) + 
   ggtitle("Coverage rate for lambda")
 
 
 
 
########################################################### 
 MW_la1 <- as.data.frame(
   rbind(as.matrix(MW_la[,c(1,2,3)]),
         as.matrix(MW_la[,c(1,2,4)]),
         as.matrix(MW_la[,c(1,2,5)]),
         as.matrix(MW_la[,c(1,2,6)]),
         as.matrix(MW_la[,c(1,2,7)]),
         as.matrix(MW_la[,c(1,2,8)])) )
 
 MW_la2 <- cbind(MW_la1,Me) 
 names(MW_la2) <- c("la","n","mw","method")
 
 #######################################################
 MW_la2_la <- MW_la2
 MW_la2_la[,1] <- as.factor(MW_la2_la[,1])
 MW_la2_la[,2] <- log(MW_la2_la[,2])
 MW_la2_la[,3] <- log(MW_la2_la[,3])
 names(MW_la2_la) <- c("la","log_n","log_mw","method")
 
 library(ggplot2)
 ggplot(data = MW_la2_la)+geom_line(aes(x = log_n, y = log_mw, colour = method)) +
   theme_bw() +
   facet_grid(~la) + 
   ggtitle("Median width for lambda")
 