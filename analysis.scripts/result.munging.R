load("../results/full.run.RData")
ref <- read.csv("../all.data/ref.csv")
###### Pull in all results to single table ######
starts <- seq(from = 1, length.out = length(res), by = 3)
raw.res <- res
res <- as.data.frame(rbind(raw.res[[1]][[1]][,-1], t(raw.res[[1]][[2]])[2,]))
for(i in 2:length(raw.res)){
  cur.res <- raw.res[[i]]
  for(j in 2:ncol(cur.res[[1]])){
    hit <- which(colnames(res) == colnames(cur.res[[1]])[j])
    if(length(hit) > 0){
      res[starts[i], hit] <- cur.res[[1]][1, j]
      res[(starts[i] + 1), hit] <- cur.res[[1]][2, j]
      res[(starts[i] + 2), hit] <- cur.res[[2]][(j-1), 2]
    }else{
      newhit <- ncol(res) + 1
      res[starts[i], newhit] <- cur.res[[1]][1, j]
      res[(starts[i] + 1), newhit] <- cur.res[[1]][2, j]
      res[(starts[i] + 2), newhit] <- cur.res[[2]][(j-1), 2]
      colnames(res)[newhit] <- colnames(cur.res[[1]])[j]
    }
  }
}
for(i in 1:ncol(res)){
  res[,i] <- as.numeric(res[,i])
}
###### Pull in all results to single table ######

###### Remove all non significant results #######
virows <- seq(from=3, by=3, length.out=nrow(res)/3)
for(i in virows){
  for(j in 1:ncol(res)){
    if(!is.na(res[i, j]))
    if(res[i, j] < .5){
      x <- i-2
      res[x:i, j] <- NA
    }
    
  }
}
SErows <- seq(from=2, by=3, length.out=nrow(res)/3)
for(i in SErows){
  for(j in 1:ncol(res)){
    if(!is.na(res[i, j])){
      SE <- abs(res[i, j])
      parest <- abs(res[(i-1), j])
      if(SE > parest){
        x <- i - 1
        z <- i + 1
        res[x:z, j] <- NA
      }
    }
  }
}
###### Remove all non significant results #######

# clean up columns
res <- res[, colSums(res, na.rm=T) != 0]

# function to pool, scale, and pull out add, dom, epi from
# each dataset
getGoods <- function(res, rowval){
    additive <- c("Aa", "Ca", "Mea", "Xa", "Ya","Ma") #separate cols for add       
    dominance <- c("Ad", "Med", "Xd", "Md")   
    epistatic <- c("AaXa", "AaXd", "AdXd", "XaXd", "XdXd", "XdCa", "AaYa", #separate cols for dom
                   "AaAa", "AaAd", "AaCa", "AdAd", "AdXa", "AdYa", "XaYa", 
                   "YaCa", "XaXa", "XaCa", "AdCa", "XaAa", "XaAd", "CaXa", 
                   "CaYa", "CaXd", "AaWa") #separate cols for epistatic interactions
    wadd <- which(colnames(res) %in% additive)
    wdom <- which(colnames(res) %in% dominance)
    wepi <- which(colnames(res) %in% epistatic)
    results <- c(sum(abs(res[rowval,wadd]), na.rm = T),
                 sum(abs(res[rowval,wdom]), na.rm = T),
                 sum(abs(res[rowval,wepi]), na.rm = T))
    results <- results/sum(results) #scale all values between 0-1
    return(results)
}


final.results <- as.data.frame(matrix(NA, nrow(ref), 3))
colnames(final.results) <- c("add", "dom", "epi")
for(i in 1:nrow(final.results)){
  start <- starts[i]
  final.results[i, 1:3] <- getGoods(res, start)
}


# add interesting variables to go with the genetic architectures
final.results$class <- ref$LH.or.M
final.results$kingdom <- ref$plant.or.animal
final.results$trait <- ref$phenotype
final.results$species <- ref$organism
final.results$SCS <- ref$SCS
final.results$divergence <- ref$within.or.between.species
final.results$weighted <- ref$SE.provided.
final.results$method <- ref$data.type..standard..PSU..cmat



write.csv(final.results,"../results/complete.results.csv", row.names = F)




