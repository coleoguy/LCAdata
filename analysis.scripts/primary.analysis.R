##first section should pull data from "standard" folder within all.data
##needs to use [,5] of ref for SCS
##standard files in ref organized a to z

##second section should pull data from "cmat" folder within all.data
##each run needs to use [,5] of ref for SCS

##third section should pull data from "PSU" folder within all.data
##each run needs to use [,5] of ref for SCS
##PSU files in ref organized a to z







####################
library(SAGA2)


#########standard files

data.files <- list.files("../all.data/standard") #data in standard folder
ref <- read.csv("../all.data/ref.csv")

#pull SCS from 5th column in ref file
SCS <- ref[,5]

d1 <- read.csv(paste("../all.data/standard", data.files[1], sep = "")) # establish the basic data frame
res <- list()
res[[1]] <- LCA(data=d1,
           SCS=SCS[1], parental="calc", env=FALSE,
           max.pars=7, ret.all=F)

for(i in 2:length(data.files)){ # loop through all datasets in LCA data folder
  cur.dat <- read.csv(paste("../all.data/standard", data.files[i], sep = ""))
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

#######cmat files


#fox files
dat <- read.csv("../all.data/cmat/fox/eggdisp.BF.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/fox/eggdisp.cowpeaA.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/fox/eggdisp.cowpeaB.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/fox/eggdisp.cowpeaC.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/fox/cmat.fox.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


#mcclelland files
dat <- read.csv("../all.data/cmat/mcclelland/salmonL.1-16-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.10-21-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.10-29-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.2-1-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.4-15-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.6-28-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.6-5-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonL.7-24-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.1-16-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.10-21-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.10-29-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.2-1-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.2-26-1.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.4-15-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.6-28-3.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.6-5-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/mcclelland/salmonW.7-24-2.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/mcclelland/cmat.mcclelland.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


#miller files
#cmat sperm
dat <- read.csv("../all.data/cmat/miller/dros.sperm.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/miller/cmatrix.sperm.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

#cmat sr
dat <- read.csv("../all.data/cmat/miller/dros.sr.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/miller/cmatrix.sr.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)


#starmer files
dat <- read.csv("../all.data/cmat/starmer/ova25.csv")
cmat <- as.matrix(read.csv("../all.data/cmat/starmer/cmat-starmer.csv"))
res[[length(res)+1]] <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("../all.data/cmat/starmer/thorax.len25")
cmat <- as.matrix(read.csv("../all.data/cmat/starmer/cmat-starmer.csv"))
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


