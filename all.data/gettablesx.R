dat <- read.csv("ref.csv")
foo <- cbind(sort(unique(dat$phenotype[dat$LH.or.M == "M"])),
             c(sort(unique(dat$phenotype[dat$LH.or.M == "LH"])), 
               rep("",8)))
colnames(foo) <- c("M","LH")
write.csv(foo, "tableSXXX.csv", row.names = F)

