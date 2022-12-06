dat <- read.csv("results.table.csv")[,-1]
starts <- seq(from=1, by=3, length.out=nrow(dat)/3)

desc <- dat$dataset[starts]
additive <- dominance <- epistatic <- rep(0, length(desc))
res <- data.frame(desc, additive, dominance, epistatic)
rm(list=ls()[-c(2,6,7)])


getGoods <- function(tab){
  x <- as.matrix(tab[,-c(1,2)])
  x <- x[,!is.na(x[1,])]
  hits <- x[3,]>.5
  x <- x[,hits, drop = F]
  z <- abs(x[1, ]) - x[2, ]
  x <- x[, z > 0, drop = F]
  
  if(ncol(x)>0){
    additive <- c("Aa", "Ca", "Mea", "Xa", "Ya")      
    dominance <- c("Ad", "Med", "Xd")   
    epistatic <- c("AaXa", "AaXd", "AdXd", "XaXd", "XdXd", "XdCa", "AaYa", 
                   "AaAa", "AaAd", "AaCa", "AdAd", "AdXa", "AdYa", "XaYa", 
                   "YaCa", "XaXa", "XaCa", "AdCa")
    wadd <- which(colnames(x) %in% additive)
    wdom <- which(colnames(x) %in% dominance)
    wepi <- which(colnames(x) %in% epistatic)
    res <- c(sum(abs(x[1,wadd])),
             sum(abs(x[1,wdom])),
             sum(abs(x[1,wepi])))
    res <- res/sum(res)
  }
  if(ncol(x)==0){
    res <- c(NA,NA,NA)
  }
  return(res)
  
  }


for(i in 1:nrow(res)){
  sta <- starts[i]
  sto <- starts[i] + 2
  res[i, 2:4] <- getGoods(dat[sta:sto,])
}





