# Jorja Elliot 14 Jan 2023 jorjaelliott@tamu.edu
# First section runs all "standard" data where we know sex of parents and 
# SAGA can calculate a cmatrix. Second section runs all datasets where 
# we are unable to calculate cmatrix automatically becuase of pooled 
# cohorts. Third section runs all datasets where sex of parents is unknown 
# so we can only fit a limited 5 parameter model. All section will make use 
# of the ref file that contains dataset specific info for SCS, cmatrix, etc.
library(SAGA2)
ref <- read.csv("../all.data/ref.csv")
ref <- ref[order(ref$new.file.name),]
res <- list()
maximum.allowed <- 7
data.files <- list.files("../all.data/data/")
## which(data.files != ref$new.file.name)
for(i in 1:length(data.files)){
  cat(paste("analyzing dataset", i, "of", length(data.files), "\r"))
  flush.console()
  cur.dat <- read.csv(paste("../all.data/data/", data.files[i],sep = ""))
  # assuring that F for female is not stored as False 
  if(ncol(cur.dat) > 3){
    if(cur.dat$sex[1] == FALSE){ 
      cur.dat$sex <- rep("F", nrow(cur.dat))
    }
  }
  # max pars max = 7 otherwise 2 minus cohorts
  max.pars <- nrow(cur.dat) - 2 
  if(max.pars > 7) max.pars <- maximum.allowed
  if(ref$data.type..standard..PSU..cmat[i] == "standard"){
    res[[i]] <- LCA(data=cur.dat,
                    SCS=ref[,5][i], parental="calc", env=FALSE,
                    max.pars = max.pars, ret.all = F, messages=F)[c(4,6)]
  }  
  if(ref$data.type..standard..PSU..cmat[i] == "cmat"){
    cname <- ref$cmat..if.assigned.[which(ref$new.file.name == data.files[i])]
    cmat <- as.matrix(read.csv(paste("../all.data/cmat/", cname, sep = "")))
    res[[i]] <- LCA(cur.dat, Cmatrix = cmat, messages=F)[c(4,6)]
  }
  if(ref$data.type..standard..PSU..cmat[i] == "PSU"){
    keep.pars <- c("Aa","Ad","AaAa","AaAd","AdAd")
    res[[i]] <- LCA(data = cur.dat, parental = "calc",env = FALSE, 
                    max.pars = max.pars, ret.all = F, keep.pars = keep.pars,
                    messages = F)[c(4, 6)]
  }
}
save(res, file="../results/full.run.RData")







