setwd("/Users/apple/Desktop/ISU 2017 fall/STAT520/exam2/520server/compare")
load("output123.RData")
load("output4.RData")
load("output5.RData")
load("output6.RData")


M1 <- output123$M1
M2 <- output123$M2
M3 <- output123$M3
M4 <- output4$M4
M5 <- output5$M5
M6 <- output6$M6

L1 <- output123$L1
L2 <- output123$L2
L3 <- output123$L3
L4 <- output4$L4
L5 <- output5$L5
L6 <- output6$L6

CR_mu <- data.frame(lambda = M1[,1],n = M1[,2],method1 = M1[,3],
                    method2 = M2[,3],method3 = M3[,3],method4 = M4[,3],
                    method5 = M5[,3],method6 = M6[,3])

CR_la <- data.frame(lambda = L1[,1],n = L1[,2],method1 = L1[,3],
                    method2 = L2[,3],method3 = L3[,3],method4 = L4[,3],
                    method5 = L5[,3],method6 = L6[,3])

MW_mu <- data.frame(lambda = M1[,1],n = M1[,2],method1 = M1[,4],
                    method2 = M2[,4],method3 = M3[,4],method4 = M4[,4],
                    method5 = M5[,4],method6 = M6[,4])

MW_la <- data.frame(lambda = L1[,1],n = L1[,2],method1 = L1[,4],
                    method2 = L2[,4],method3 = L3[,4],method4 = L4[,4],
                    method5 = L5[,4],method6 = L6[,4])

OOBP_mu <- data.frame(lambda = M1[,1],n = M1[,2],method1 = M1[,5],
                    method2 = M2[,5],method3 = M3[,5],method4 = M4[,5],
                    method5 = M5[,5],method6 = M6[,5])

OOBP_la <- data.frame(lambda = L1[,1],n = L1[,2],method1 = L1[,5],
                    method2 = L2[,5],method3 = L3[,5],method4 = L4[,5],
                    method5 = L5[,5],method6 = L6[,5])

write.csv(round(CR_mu,4), file="/Users/apple/Desktop/crmu.csv", row.names = FALSE)
write.csv(round(CR_la,4), file="/Users/apple/Desktop/crla.csv", row.names = FALSE)
write.csv(round(MW_mu,4), file="/Users/apple/Desktop/mwmu.csv", row.names = FALSE)
write.csv(round(MW_la,4), file="/Users/apple/Desktop/mwla.csv", row.names = FALSE)
write.csv(round(OOBP_mu,4), file="/Users/apple/Desktop/pmu.csv", row.names = FALSE)
write.csv(round(OOBP_la,4), file="/Users/apple/Desktop/pla.csv", row.names = FALSE)
