# How I can add data with no extra work for you :-)  - dispersal, pot, etc.
# Show spots in code I changed
# plots axes and N
# folders add depricated and clean out
## tomato
## LCA
## etc.



#storing results from each LCA run into dataframe to allow better access to data
####################
library(SAGA2)

data.files <- list.files("../data/") #data in LCAdata rep; all csvs
data.files <- data.files[data.files != "ref.xlsx"] #remove the file with list of references and file with final table
data.files <- data.files[data.files != "results.table.csv"]

# this holds the sex chromosome system for each dataset
# details for each dataset found in ref file
SCS <- c(rep("NSC", 1), rep("XY", 7), rep("NSC", 3), rep("XY", 4), 
         rep("NSC", 5), rep("XY", 4), rep("NSC", 1), 
         rep("NSC", 2), rep("XY", 1), rep("NSC", 1), rep("XY", 4), 
         rep("NSC", 18), rep("XY", 3), rep("NSC", 1), rep("XY", 9), 
         rep("NSC", 7), rep("XY", 5), rep("NSC", 2), rep("XY", 2), 
         rep("NSC", 1), rep("XY", 2), rep("NSC", 7), rep("XY", 4), 
         rep("NSC", 10), rep("XY", 2), rep("NSC", 1), rep("XY", 1), 
         rep("NSC", 4), rep("XY", 1), rep("NSC", 3), rep("XY", 10), 
         rep("NSC", 1), rep("XY", 2), rep("NSC", 36))

d1 <- read.csv(paste("../data/", data.files[1], sep = "")) # establish the basic data frame
res <- list()
res[[1]] <- LCA(data=d1,
           SCS=SCS[1], parental="calc", env=FALSE,
           max.pars=7, ret.all=F)

for(i in 2:length(data.files)){ # loop through all datasets in LCA data folder
  cur.dat <- read.csv(paste("../data/", data.files[i], sep = ""))
  # check to make sure F for female is not being converted to FALSE
  
  if(cur.dat$sex[1] == FALSE){ #fixing datasets where sex is all female and "F" for female is incorrectly called as "FALSE" 
    cur.dat$sex <- rep("F", nrow(cur.dat))
  }
  max.pars <- nrow(cur.dat) - 2 #changing max pars for each dataset. max 7 pars for the datasets with many rows
  if(max.pars > 7){
    max.pars <- 7
  }
  res[[i]] <- LCA(data=cur.dat,
              SCS=SCS[i], parental="calc", env=FALSE,
              max.pars = max.pars, ret.all = F)

}


#armbruster files
dat <- read.csv("../data.cmat/armbruster/eFL.ME.high.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/eFL.ME.low.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/eFL.wFL.high.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/eFL.wFL.low.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/ON.ME.high.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/ON.ME.low.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/wFL.ON.high.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/armbruster/wFL.ON.low.csv")
cmat <- as.matrix(read.csv("../data.cmat/armbruster/cmat.armbruster.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


#etterson files
#cmat ACE
dat <- read.csv("../data.cmat/etterson/seed.fruit.A.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.ACE.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seed.fruit.C.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.ACE.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seed.fruit.E.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.ACE.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seedwt.A.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.ACE.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seedwt.C.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.ACE.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seedwt.E.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.ACE.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

#cmat BD
dat <- read.csv("../data.cmat/etterson/seed.fruit.B.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.BD.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seed.fruit.D.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.BD.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seedwt.B.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.BD.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seedwt.D.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.BD.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

#cmat F
dat <- read.csv("../data.cmat/etterson/seed.fruit.F.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.F.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("../data.cmat/etterson/seedwt.F.csv")
cmat <- as.matrix(read.csv("../data.cmat/etterson/cmat.etterson.F.csv"))
res[[length(res)+1]] <- LCA(dat, SCS = "NSC", Cmatrix = cmat)


#fox files
dat <- read.csv("../data.cmat/fox/eggdisp.BF.csv")
cmat <- as.matrix(read.csv("../data.cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/fox/eggdisp.cowpeaA.csv")
cmat <- as.matrix(read.csv("../data.cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/fox/eggdisp.cowpeaB.csv")
cmat <- as.matrix(read.csv("../data.cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/fox/eggdisp.cowpeaC.csv")
cmat <- as.matrix(read.csv("../data.cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


#mcclelland files
dat <- read.csv("../data.cmat/mcclelland/salmonL.1-16-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.10-21-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.10-29-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.2-1-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.4-15-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.6-28-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.6-5-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonL.7-24-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.1-16-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.10-21-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.10-29-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.2-1-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.2-26-1.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.4-15-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.6-28-3.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.6-5-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../data.cmat/mcclelland/salmonW.7-24-2.csv")
cmat <- as.matrix(read.csv("../data.cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


#miller files
#cmat sperm
dat <- read.csv("../data.cmat/miller/dros.sperm.csv")
cmat <- as.matrix(read.csv("../data.cmat/miller/cmatrix.sperm.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

#cmat sr
dat <- read.csv("../data.cmat/miller/dros.sr.csv")
cmat <- as.matrix(read.csv("../data.cmat/miller/cmatrix.sr.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


save.image("~/Desktop/LCA-allresults.RData")








modspace <- confsetsize <- c()
for(i in 1:209){
  confsetsize[i] <- length(res[[i]]$best.eqns.w)
  modspace[i] <- length(res[[i]]$daicc)
}
modexp <- data.frame(confsetsize,modspace)
write.csv(modexp,file="modexp.csv")

dat <- read.csv("../results/modexp.csv")[-200,]
uncind <- c()
linenums <-c()
for(i in 1:209){
  if(i != 200){
    linenums[i] <- nrow(res[[i]]$cmatrix)
  }
  xi <- dat$confsetsize[i]/dat$modspace[i]
  minx <- 1/dat$modspace[i]
  maxx <- ceiling(.95*dat$modspace[i])/dat$modspace[i]
  uncind[i] <- (xi-minx)/(maxx-minx)
}

est.res <- read.csv("../results/complete.results.csv")


hist(uncind,breaks=20,xlab="uncertainty index")

dd <- data.frame(uncind, 
                 c("fail", "success")[as.numeric(!is.na(est.res$additive))+1])
colnames(dd) <- c("unc_index", "outcome")


ggplot(dd, aes(x=unc_index, fill=outcome, fill=outcome)) +
  geom_histogram(position="dodge",alpha=.5)+
  theme_bw() + xlab("Uncertainty Index")

lidf <- data.frame(est.res$epistatic, linenums[-200])
colnames(lidf) <- c("propepi","linenums")
lidf <- lidf[complete.cases(lidf),]
ggplot(lidf, aes(x=linenums, y=propepi, col=rgb(1,0,0,.5))) +
  geom_point() +
  theme_bw()
lidf <- lidf[!is.na(lidf$propepi),]
hist(lidf$propepi[lidf$linenums>6])
hist(lidf$propepi[lidf$linenums<=6])


