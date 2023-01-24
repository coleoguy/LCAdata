######## plot for LH vs M ########
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
dat <- dat[dat$method %in% c("cmat","standard"),]
#split data by trait type
LH <- sort(rowSums(dat[dat$class == "LH", 2:3]))
M <- sort(rowSums(dat[dat$class == "M", 2:3]))
LHx <- seq(from=0, to=100, length.out=length(LH))
Mx <- seq(from=0, to=100, length.out=length(M))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=LH, x=LHx, col= "#3F4788FF",lwd=3) #LH
lines(y=M, x=Mx, col="#74D055FF",lwd=3) #M
points(y=LH, x=LHx, col="#3F4788FF",pch=16, cex=.9)
points(y=M, x=Mx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("life history (n=", length(LHx),")", sep=""), 
                           paste("morphological (n=", length(Mx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######## plot for LH vs M ########


######### plot for plant vs animal ######### 
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
dat <- dat[dat$divergence == "within",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
plant <- sort(rowSums(dat[dat$kingdom == "plant", 2:3]))
animal <- sort(rowSums(dat[dat$kingdom == "animal", 2:3]))
plantx <- seq(from=0, to=100, length.out=length(plant))
animalx <- seq(from=0, to=100, length.out=length(animal))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=plant, x=plantx, col= "#3F4788FF",lwd=3) #plant
lines(y=animal, x=animalx, col="#74D055FF",lwd=3) #animal
points(y=plant, x=plantx, col="#3F4788FF",pch=16, cex=.9)
points(y=animal, x=animalx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("plant (n=", length(plantx),")", sep=""), 
                           paste("animal (n=", length(animalx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### plot for plant vs animal ######### 


######### plot for within vs between ######### 
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
within <- sort(rowSums(dat[dat$divergence == "within", 2:3]))
between <- sort(rowSums(dat[dat$divergence == "between", 2:3]))
withinx <- seq(from=0, to=100, length.out=length(within))
betweenx <- seq(from=0, to=100, length.out=length(between))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=within, x=withinx, col= "#3F4788FF",lwd=3) #within
lines(y=between, x=betweenx, col="#74D055FF",lwd=3) #between
points(y=within, x=withinx, col="#3F4788FF",pch=16, cex=.9)
points(y=between, x=betweenx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### plot for within vs between ######### 


















trait.table <- as.data.frame(table(dat$trait))
trait.table$prop.epi <- NA
for(i in 1:nrow(trait.table)){
  trait.table$prop.epi[i] <-  mean(dat$epi[dat$trait == trait.table$Var1[i]])
}










