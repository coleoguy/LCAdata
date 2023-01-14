# Jorja Elliot 14 Jan 2023 jorjaelliott@tamu.edu
# First section runs all "standard" data where we know sex of parents and 
# SAGA can calculate a cmatrix. Second section runs all datasets where 
# we are unable to calculate cmatrix automatically becuase of pooled 
# cohorts. Third section runs all datasets where sex of parents is unknown 
# so we can only fit a limited 5 parameter model. All section will make use 
# of the ref file that contains dataset specific info for SCS, cmatrix, etc.
library(SAGA2)
ref <- read.csv("../all.data/ref.csv")
res <- list()
maximum.allowed <- 7
######### standard files ######### 
#data in standard folder
data.files <- list.files("../all.data/standard")
for(i in 1:length(data.files)){
  cat(paste("analyzing standard dataset", i, "of", length(data.files), "\r"))
  flush.console()
  cur.dat <- read.csv(paste("../all.data/standard/", data.files[i],sep = ""))
  # assuring that F for female is not stored as False 
  if(cur.dat$sex[1] == FALSE){ 
    cur.dat$sex <- rep("F", nrow(cur.dat))
  }
  # max pars max = 7 otherwise 2 minus cohorts
  max.pars <- nrow(cur.dat) - 2 
  if(max.pars > 7) max.pars <- maximum.allowed
  res[[i]] <- LCA(data=cur.dat,
              SCS=ref[,5][i], parental="calc", env=FALSE,
              max.pars = max.pars, ret.all = F, messages=F)[c(4,6)]
}
######### standard files done ######### 
############# cmat files ##############
data.files <- list.files("../all.data/cmat/data/")
# loop through all datasets in PSU data folder
for(i in 1:length(data.files)){ 
  cat(paste("analyzing cmat dataset", i, "of", length(data.files), "\r"))
  flush.console()
  cur.dat <- read.csv(paste("../all.data/cmat/data/", data.files[i], sep = ""))
  cname <- ref$cmat..if.assigned.[which(ref$new.file.name == data.files[i])]
  cmat <- as.matrix(read.csv(paste("../all.data/cmat/cmats/", cname, sep = "")))
  # max pars max = 7 otherwise 2 minus cohorts
  max.pars <- nrow(cur.dat) - 2 
  if(max.pars > 7) max.pars <- maximum.allowed
  res[[length(res) + 1]] <- LCA(cur.dat, Cmatrix = cmat, messages=F)[c(4,6)]
}
########### cmat files done ###########
############### PSU files #############
data.files <- list.files("../all.data/PSU")
# loop through all datasets in PSU data folder
for(i in 1:length(data.files)){ 
  cat(paste("analyzing PSU dataset", i, "of", length(data.files), "\r"))
  flush.console()
  cur.dat <- read.csv(paste("../all.data/PSU/", data.files[i], sep = ""))
  # assuring that F for female is not stored as False 
  if (cur.dat$sex[1] == FALSE) {
    cur.dat$sex <- rep("F", nrow(cur.dat))
  }
  max.pars <- nrow(cur.dat) - 2 
  keep.pars <- c("Aa","Ad","AaAa","AaAd","AdAd")
  res[[length(res) + 1]] <- LCA(data = cur.dat,
                                parental = "calc", 
                                env = FALSE, 
                                max.pars = max.pars, 
                                ret.all = F,
                                keep.pars = keep.pars,
                                messages = F)[c(4, 6)]
}
############# PSU files done ##########
save.image("~/Desktop/LCA-allresults.RData")







