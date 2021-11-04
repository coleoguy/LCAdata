library(SAGA2)
########################## Untransformed Data #######################
###SAMPLE CODE FOR LCA###
beetle.dat <- read.csv("../data/saga.single.val.data.csv")
  res <- LCA(data=beetle.dat,
                   SCS="XY",parental = "calc",env=FALSE,
                   max.pars = 7, ret.all=F)
library(viridis)
plot(res, col.ramp = viridis(100))

barplot(res$best.models[[1]]$coefficients[3:4],
        names=(c("Aa","AaAd")))
#To visualize the model space only works well for low max.pars values
#VisModelSpace(res)
plot(res, min.vi = .55, main = "")

#####

#skin reflectance
skin.data <- read.csv("../data/Skin Reflectance/skin_data.csv")
res <- LCA(data=skin.data, 
             SCS="XY", parental = "calc", env=FALSE,
             max.pars = 7, ret.all=F)
library(viridis)
plot(res, col.ramp = viridis(100))
barplot(res$best.models[[1]]$coefficients[3:4],
        names=(c("Aa", "AaAd")))

library(SAGA2)

#####

#sticklebacks with rF1
stickleback.dat <- read.csv("stickleback_data.csv")
res <- LCA(data = stickleback.dat,
           SCS="XY", parental = "calc", env=TRUE, 
           max.pars = 6, ret.all=F, drop.pars = "Ca")
library(viridis)
plot(res, col.ramp=viridis(100), min.vi = .1)
res$best.models


#sticklebacks without rF1 -- use this one for analysis 
stickleback.dat2 <- read.csv("stickleback_data2.csv")
res <- LCA(data = stickleback.dat2,
           SCS="XY", parental = "calc", env=TRUE, 
           max.pars = 6, ret.all=F, drop.pars = "Ca")
library(viridis)
plot(res, col.ramp=viridis(100))

#####

#skin reflectance 
skin.data <- read.csv("../data/Skin Reflectance/skin_data.csv")
res <- LCA(data = skin.data,
           SCS="XY", parental = "calc", env=TRUE, 
           max.pars = 6, ret.all=F)
plot(res, col.ramp=viridis(100), main="Skin Reflectance")


#####

#seedweight of lima beans
seedweight.data <- read.csv("../data/Seed weight/seedweight.data.csv")
res <- LCA(data= seedweight.data,
               SCS="XY", parental = "calc", 
               max.pars = 5, ret.all=F, env=T)
plot(res, col.ramp=viridis(100), main="Lima Bean Seedweight")

#####

#time from seed to silking in zea mays
silking.data <- read.csv("../data/Seed to silking/silking_data.csv")
res <- LCA(data=silking.data,
           SCS="XY", parental="calc", env=T,
           max.pars =6, ret.all=F)
plot(res, col.ramp=viridis(100), main="Seed to Silking")

#####

#time from seed to shedding pollen in zea mays 
pollen.data <- read.csv("../data/Shedding Pollen/pollen_data.csv")
res <- LCA(data=pollen.data, 
           SCS="XY", parental="calc", env=T, 
           max.pars=6, ret.all=F)
plot(res, col.ramp=viridis(100), main="Seed to Shedding Pollen")