#storing results from each LCA run into csv to allow better access to data
####################

data.files <- list.files("../data/")
data.files <- data.files[data.files != "ref.xlsx"]


library(SAGA2)
chloro <- read.csv("../data/chloroLCA.csv")
res <- LCA(data=chloro, 
           SCS="NSC", parental="calc", env=FALSE,
           max.pars=7, ret.all=F)
full.table <- data.frame(res$estimates[,-1]) #next 3 lines establish the basic data frame
full.table[3,] <- res$varimp[,2]
row.names(full.table)[3] <- "vi"


for(i in 1:length(data.files)){ # loop through all datasets in LCA data folder
  biomass <- read.csv("../data/biomassLCA.csv")
  res <- LCA(data=biomass,
              SCS="NSC", parental="calc", env=FALSE,
              max.pars = 7, ret.all = F)
  start.row <- nrow(full.table) + 1 #where in the table the loop should start putting data 
  for(j in 2:ncol(res$estimates)){ # looping through the genetic effects
    cur.col <- which(colnames(full.table) == colnames(res$estimates)[j]) #which col in full.table matches the col in res$estimates that has the data you want to place
    ## TODO test the length of cur.col if it is zero then designate
    if(cur.col == 0){
      cur.col <- ncol(full.table) + 1  ## the value of cur.col to be the next empty column
      colnames(full.table)[cur.col] <- 
    }
    ## create the empty column and then give a column name to the new column too
    ## extra credit if you see the way that you can do that all in one step
    
    full.table[start.row:(start.row+1), cur.col] <- res$estimates[,j]
    full.table[(start.row + 2), cur.col] <- res$varimp[(j-1),2]
  }
}


#after table is completely filled in, add column to the front of table with names of each dataset, every third row


