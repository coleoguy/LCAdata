#### Performs reduced model analysis (used for PSU datasets) on all data

#### PACKAGES ####
library(SAGA2)

#### READ IN REFERENCE FILE ####
ref <- read.csv("../all.data/ref.csv")

#### DEFINE DATA ####
dat <- list.files("../all.data/data/")

#### OUTPUT LIST####
res <- list()

#### LOOP THROUGH DATA ####
for(i in 1:length(dat)){

  #Print message to track progress
  cat(paste("analyzing dataset", i, "of", length(dat), "\r"))
  
  #### READ IN CURRENT DATA ####
  
  #Locate and read in data + cmatrix if necessary
  dat.cur <- read.csv(paste("../all.data/data/", dat[i],sep = ""))
  
  #### CLEAN DATA ####
  
  #Exclude cmat data
  if(ref$data.type..standard..PSU..cmat[i] != "cmat"){
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
  if(nrow(dat.cur) <= 7){
    max.pars <- nrow(dat.cur) - 2
  } else {
    max.pars <- 5
  }
  
  #Identify reduced model parameters
  keep.pars <- c("Aa","Ad","AaAa","AaAd","AdAd")
  
  #### RUN ANALYSIS ####
  
  if(ref$data.type..standard..PSU..cmat[i] == "standard" ||
     ref$data.type..standard..PSU..cmat[i] == "PSU"){
    
    #Run LCA
    res[[i]] <- LCA(data = dat.cur,
                    parental = "calc", 
                    env = FALSE,
                    max.pars = max.pars,
                    ret.all = F,
                    keep.pars = keep.pars,
                    messages = F)[c(4, 6)]
  } else {
    
    #Read in cmat
    cmat.cur <- as.matrix(read.csv(paste("../all.data/cmat/", 
                                         ref$cmat..if.assigned.[i], 
                                         sep = "")))
    
    
    #Run LCA
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


