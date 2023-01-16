load("../results/full.run.RData")

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





dat <- read.csv("results.all.csv")[,-1]
starts <- seq(from=1, by=3, length.out=nrow(dat)/3) #where each new dataset starts (each have 3 rows)

desc <- dat$dataset[starts]
additive <- dominance <- epistatic <- rep(0, length(desc))
res <- data.frame(desc, additive, dominance, epistatic)

rm(list=ls()[-c(2,6,7)]) #clean up environ


for(j in 2:ncol(res$estimates)){ # looping through the genetic effects
  cur.col <- which(colnames(full.table) == colnames(res$estimates)[j]) #which col in full.table matches the col in res$estimates that has the data you want to place
  
  if(length(cur.col) == 0){ # test the length of cur.col if it is zero then designate
    cur.col <- ncol(full.table) + 1  ## the value of cur.col to be the next empty column
    full.table[next.row:(next.row+1), cur.col] <- res$estimates[,j] #fill in the new col with data 
    full.table[(next.row + 2), cur.col] <- res$varimp[(j-1),2]
    colnames(full.table)[cur.col] <- colnames(res$estimates)[j]
  }else{
    full.table[next.row:(next.row+1), cur.col] <- res$estimates[,j] #if cur.col is not 0, fill in col normally w data
    full.table[(next.row + 2), cur.col] <- res$varimp[(j-1),2]
  }
}
}
full.table$dataset <- rep(data.files, each = 3) #add col 'dataset' with names of datasets corresponding to the data
full.table$par <- rep(row.names(full.table)[1:3], times = nrow(full.table)/3) #repeat the first 3 row names (mwa, se, vi) for each dataset (every three rows)
x <- ncol(full.table)
full.table <- full.table[, c((x-1):x, 1:(x-2))] #reorganize the order of the cols



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

write.csv(res, "complete.results.csv")




