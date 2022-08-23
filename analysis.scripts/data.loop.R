#storing results from each LCA run into csv to allow better access to data
####################

library(SAGA2)
chloro <- read.csv("../data/chloroLCA.csv")
res <- LCA(data=chloro, 
           SCS="NSC", parental="calc", env=FALSE,
           max.pars=7, ret.all=F)
full.table <- data.frame(res$estimates[,-1])
full.table[3,] <- res$varimp[,2]
row.names(full.table)[3] <- "vi"


for(i in 1:length(file.names)){ # loop through all your datasets
  biomass <- read.csv("../data/biomassLCA.csv")
  res <- LCA(data=biomass,
              SCS="NSC", parental="calc", env=FALSE,
              max.pars = 7, ret.all = F)
  start.row <- nrow(full.table) + 1
  for(j in 2:ncol(res$estimates)){ # looping through the genetic effects
    cur.col <- which(colnames(full.table) == colnames(res$estimates)[j])
    ## TODO test the length of cur.col if it is zero then designate
    ## the value of cur.col to be the next empty column
    ## create the empty column and then give a column name to the new column too
    ## extra credit if you see the way that you can do that all in one step
    
    full.table[start.row:(start.row+1), cur.col] <- res$estimates[,j]
    full.table[(start.row + 2), cur.col] <- res$varimp[(j-1),2]
  }
}





