dat <- read.csv("results.table.csv")[,-1]
starts <- seq(from=1, by=3, length.out=nrow(dat)/3) #where each new dataset starts (each have 3 rows)

desc <- dat$dataset[starts]
additive <- dominance <- epistatic <- rep(0, length(desc))
res <- data.frame(desc, additive, dominance, epistatic)
rm(list=ls()[-c(2,6,7)]) #clean up environ


getGoods <- function(tab){
  x <- as.matrix(tab[,-c(1,2)]) #next 4 lines remove datasets that do not meet 0.5 min.vi 
  x <- x[,!is.na(x[1,])]
  hits <- x[3,]>.5
  x <- x[,hits, drop = F]
  z <- abs(x[1, ]) - x[2, ] #next 2 lines remove datasets where se overlap zero
  x <- x[, z > 0, drop = F]
  
  if(ncol(x)>0){ #if there are cols that meet criteria above...
    additive <- c("Aa", "Ca", "Mea", "Xa", "Ya") #separate cols for add       
    dominance <- c("Ad", "Med", "Xd")   
    epistatic <- c("AaXa", "AaXd", "AdXd", "XaXd", "XdXd", "XdCa", "AaYa", #separate cols for dom
                   "AaAa", "AaAd", "AaCa", "AdAd", "AdXa", "AdYa", "XaYa", 
                   "YaCa", "XaXa", "XaCa", "AdCa") #separate cols for epistatic interactions
    wadd <- which(colnames(x) %in% additive)
    wdom <- which(colnames(x) %in% dominance)
    wepi <- which(colnames(x) %in% epistatic)
    res <- c(sum(abs(x[1,wadd])),
             sum(abs(x[1,wdom])),
             sum(abs(x[1,wepi])))
    res <- res/sum(res) #scale all values between 0-1
  }
  if(ncol(x)==0){ #if there are not cols that meet criteria above...
    res <- c(NA,NA,NA) #replace all with NAs in res
  }
  return(res)
  
  }


for(i in 1:nrow(res)){
  sta <- starts[i]
  sto <- starts[i] + 2
  res[i, 2:4] <- getGoods(dat[sta:sto,])
}





