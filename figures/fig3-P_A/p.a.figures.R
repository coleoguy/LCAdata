#plant versus animal figures script
##### figure using only unique combinations of species-phenotype #####
dat <- read.csv("../results/thinned.comp.csv")
#dat <- dat[! is.na(dat$add),]
# get just within species
#dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
plant <- sort(dat[dat$kingdom == "plant", 2])
animal <- sort(dat[dat$kingdom == "animal", 2])
plantx <- seq(from=0, to=100, length.out=length(plant))
animalx <- seq(from=0, to=100, length.out=length(animal))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=plant, x=plantx, col= rgb(0.2, 0.8, 0.4),lwd=3) #plant
lines(y=animal, x=animalx, col=rgb(0.6, 0.2, 1),lwd=3) #animal
points(y=plant, x=plantx, col=rgb(0.2, 0.8, 0.4),pch=16, cex=.9)
points(y=animal, x=animalx, col=rgb(0.6, 0.2, 1),pch=16, cex=.9)
legend("topleft", legend=c(paste("plant (n=", length(plantx),")", sep=""), 
                           paste("animal (n=", length(animalx),")", sep="")), 
       fill=c(rgb(0.2, 0.8, 0.4),rgb(0.6, 0.2, 1)), cex=0.8, bty="n")
##### figure using only unique combinations of species-phenotype #####
