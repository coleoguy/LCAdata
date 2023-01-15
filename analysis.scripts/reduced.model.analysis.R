#### Performs reduced model analysis (used for PSU datasets) on all data

#### PACKAGES ####
library(SAGA2)

#### READ IN REFERENCE FILE ####
ref <- read.csv("../all.data/ref.csv")

#### DEFINE DATA ####
dat.standard <- list.files("../all.data/standard")
dat.cmat <- list.files("../all.data/cmat/data")
dat.psu <- list.files("../all.data/PSU")
dat.all <- c(dat.standard,dat.cmat,dat.psu)

#### OUTPUT LIST####
res <- list()

#### LOOP THROUGH DATA ####
for(i in 1:length(dat.all)){

  #Print message to track progress
  cat(paste("analyzing dataset", i, "of", length(dat.all), "\r"))
  
  #### READ IN CURRENT DATA ####
  
  #Locate and read in data + cmatrix if necessary
  if(dat.all[i] %in% dat.standard){
    dat.cur <- read.csv(paste0("../all.data/standard/", dat.all[1]))
  } else if(dat.all[i] %in% dat.cmat){
    dat.cur <- read.csv(paste0("../all.data/cmat/data/",dat.all[i]))
    cmat.cur <- as.matrix(read.csv(paste0("../all.data/cmat/cmats/",ref[i,12])))
  } else {
    dat.cur <- read.csv(paste0("../all.data/PSU/", dat.all[i]))
  }
  
  #### CLEAN DATA ####
  
  #Exclude cmat data
  if(!dat.all[i] %in% dat.cmat){
    #Get cohort identifiers in upper case
    dat.cur$cross <- toupper(dat.cur$cross)
  }
  
  #### CHECK FOR F STORED AS FALSE ####
  if("sex" %in% colnames(dat.cur) &&
     !FALSE %in% dat.cur$sex){ 
    dat.cur$sex <- rep("F", nrow(dat.cur))
  }
  
  #### SET MAXIMUM PARAMETERS IN MODEL PARAMETERS TO KEEP####
  
  #Determine maximum parameters
  if(nrow(dat.cur) <= 9){
    max.pars <- nrow(dat.cur) - 2
  } else {
    max.pars <- 5
  }
  
  #Identify reduced model parameters
  keep.pars <- c("Aa","Ad","AaAa","AaAd","AdAd")
  
  #### RUN ANALYSIS ####
  
  if(dat.all[i] %in% dat.standard ||
     dat.all[i] %in% dat.psu){
    
    res[[i]] <- LCA(data = dat.cur,
                    parental = "calc", 
                    env = FALSE,
                    max.pars = max.pars,
                    ret.all = F,
                    keep.pars = keep.pars,
                    messages = F)[c(4, 6)]
  } else {
    
    res[[i]] <- LCA(data = dat.cur,
                    Cmatrix = cmat.cur,
                    parental = "calc", 
                    env = FALSE, 
                    max.pars = max.pars, 
                    ret.all = F,
                    keep.pars = keep.pars,
                    messages = F)[c(4, 6)]
    
  }
}

#### SAVE ANALYSIS ####
save(res, file="../results/reduced.model.run.RData")
