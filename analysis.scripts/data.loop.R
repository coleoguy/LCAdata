#storing results from each LCA run into dataframe to allow better access to data
####################
library(SAGA2)

data.files <- list.files("../data/") #data in LCAdata rep; all csvs
data.files <- data.files[data.files != "ref.xlsx"] #remove the file with list of references and file with final table
data.files <- data.files[data.files != "results.table.csv"]

# this holds the sex chromosome system for each dataset
# beetles, silene, drosophila, and stickelbacks all XY 
# run toads and plants as NSC
SCS <- c(rep("XY", 4), rep("NSC", 2), rep("XY", 4), rep("NSC", 1), 
         rep("XY", 3), rep("NSC", 12), rep("XY", 9), rep("NSC", 5), 
         rep("XY", 8), rep("NSC", 1), rep("XY", 2), rep("NSC", 10), 
         rep("XY", 1), rep("NSC",4), rep("XY", 3), rep("NSC", 22))

d1 <- read.csv(paste("../data/", data.files[1], sep = "")) # lines 16-22 establish the basic data frame
res <- LCA(data=d1, 
           SCS=SCS[1], parental="calc", env=FALSE,
           max.pars=7, ret.all=F)
full.table <- data.frame(res$estimates[,-1]) 
full.table[3,] <- res$varimp[,2]
row.names(full.table)[3] <- "vi"

for(i in 2:length(data.files)){ # loop through all datasets in LCA data folder
  cur.dat <- read.csv(paste("../data/", data.files[i], sep = ""))
  # check to make sure F for female is not being converted to FALSE
  
  if(cur.dat$sex[1] == FALSE){ #fixing datasets where sex is all female and "F" for female is incorrectly called as "FALSE" 
    cur.dat$sex <- rep("F", nrow(cur.dat))
  }
  max.pars <- nrow(cur.dat) - 1 #changing max pars for each dataset. max 7 pars for the datasets with many rows
  if(max.pars > 7){
    max.pars <- 7
  }
  res <- LCA(data=cur.dat,
              SCS=SCS[i], parental="calc", env=FALSE,
              max.pars = max.pars, ret.all = F)
  next.row <- nrow(full.table) + 1 # where in the table the loop should start putting data 
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
row.names(full.table) <- NULL #remove row names that did not match; changed to just row numbers
write.csv(full.table, "results.table.csv")


