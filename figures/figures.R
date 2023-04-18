######## plot for LH vs M ########
#pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/LH_M.pdf", 
#    width = 5, 
#    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
LH <- sort(dat[dat$class == "LH", 3])
M <- sort(dat[dat$class == "M", 3])
LHx <- seq(from=0, to=100, length.out=length(LH))
Mx <- seq(from=0, to=100, length.out=length(M))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=LH, x=LHx, col= "#3F4788FF",lwd=3) #LH
lines(y=M, x=Mx, col="#74D055FF",lwd=3) #M
points(y=LH, x=LHx, col="#3F4788FF",pch=16, cex=.9)
points(y=M, x=Mx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("life history (n=", length(LHx),")", sep=""), 
                           paste("morphological (n=", length(Mx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
#dev.off()
######## plot for LH vs M ########

######### plot for plant vs animal ######### 
pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/P_A.pdf", 
    width = 5, 
    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
plant <- sort(dat[dat$kingdom == "plant", 3])
animal <- sort(dat[dat$kingdom == "animal", 3])
plantx <- seq(from=0, to=100, length.out=length(plant))
animalx <- seq(from=0, to=100, length.out=length(animal))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=plant, x=plantx, col= "#3F4788FF",lwd=3) #plant
lines(y=animal, x=animalx, col="#74D055FF",lwd=3) #animal
points(y=plant, x=plantx, col="#3F4788FF",pch=16, cex=.9)
points(y=animal, x=animalx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("plant (n=", length(plantx),")", sep=""), 
                           paste("animal (n=", length(animalx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
dev.off()
######### plot for plant vs animal ######### 

######### plot for within vs between ######### 
#pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/W_B.pdf", 
#    width = 5, 
#    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
#dat <- dat[dat$weighted == "Y",]

dat <- dat[dat$species != "Zea mays, Zea diploperennis Iltis",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
within <- sort(dat[dat$divergence == "within", 3])
between <- sort(dat[dat$divergence == "between", 3])
withinx <- seq(from=0, to=100, length.out=length(within))
betweenx <- seq(from=0, to=100, length.out=length(between))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=within, x=withinx, col= "#3F4788FF",lwd=3) #within
lines(y=between, x=betweenx, col="#74D055FF",lwd=3) #between
points(y=within, x=withinx, col="#3F4788FF",pch=16, cex=.9)
points(y=between, x=betweenx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
#dev.off()
######### plot for within vs between ######### 

######### plot for domestic vs wild vs lab #########
pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/D_W.pdf", 
    width = 5, 
    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within or between species
datLH <- dat[dat$class == "LH",]
datM <- dat[dat$class == "M",]
# dat <- dat[dat$method == "standard",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by domestication status
#wildLH <- sort(rowSums(datLH[datLH$domestication == "wild", 2:3])) #wild LH
#domesticLH <- sort(rowSums(datLH[datLH$domestication == "domestic", 2:3])) #domestic LH
labLH <- sort(rowSums(datLH[datLH$domestication == "lab", 2:3])) #lab LH
#wildxLH <- seq(from=0, to=100, length.out=length(wildLH)) #wild LH
#domesticxLH <- seq(from=0, to=100, length.out=length(domesticLH)) #domestic LH
labxLH <- seq(from=0, to=100, length.out=length(labLH)) #lab LH
#wildM <- sort(rowSums(datM[datM$domestication == "wild", 2:3])) #wild M
#domesticM <- sort(rowSums(datM[datM$domestication == "domestic", 2:3])) #domestic M
labM <- sort(rowSums(datM[datM$domestication == "lab", 2:3])) #lab M
#wildxM <- seq(from=0, to=100, length.out=length(wildM)) #wild M
#domesticxM <- seq(from=0, to=100, length.out=length(domesticM)) #domestic M
labxM <- seq(from=0, to=100, length.out=length(labM)) #lab M
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
#lines(y=wildLH, x=wildxLH, col= "#3F4788FF",lwd=3) #wild LH
#lines(y=domesticLH, x=domesticxLH, col="#74D055FF",lwd=3) #domestic LH
lines(y=labLH, x=labxLH, col="#238a8d",lwd=3) #lab LH
#points(y=wildLH, x=wildxLH, col="#3F4788FF",pch=16, cex=.9)
#points(y=domesticLH, x=domesticxLH, col="#74D055FF",pch=16, cex=.9)
#points(y=labLH, x=labxLH, col="#238a8d",pch=16, cex=.9)
#lines(y=wildM, x=wildxM, col= "#3F4788FF",lwd=3, lty=2) #wild M
#lines(y=domesticM, x=domesticxM, col="#74D055FF",lwd=3, lty=2) #domestic M
lines(y=labM, x=labxM, col="#238a8d",lwd=3, lty=2) #lab M
#points(y=wildM, x=wildxM, col="#3F4788FF",pch=16, cex=.9)
#points(y=domesticM, x=domesticxM, col="#74D055FF",pch=16, cex=.9)
#points(y=labM, x=labxM, col="#238a8d",pch=16, cex=.9)
legend("topleft", legend=c(paste("lab LH (n=", length(labxLH),")", sep=""), 
                           paste("lab M (n=", length(labxM),")", sep="")), 
                col="#238a8d", lty=1:2, lwd=2, cex=0.8, bty="n")
#legend("topleft", legend=c(paste("wild LH (n=", length(wildxLH),")", sep=""), 
#                           paste("domestic LH (n=", length(domesticxLH),")", sep=""), 
#                           paste("lab LH (n=", length(labxLH),")", sep=""),
#                           paste("wild M (n=", length(wildxM),")", sep=""), 
#                           paste("domestic M (n=", length(domesticxM),")", sep=""), 
#                           paste("lab M (n=", length(labxM),")", sep="")), 
#       col=c("#3F4788FF", "#74D055FF", "#238a8d", "#3F4788FF", "#74D055FF", "#238a8d"), lty=c(1,1,1,2,2,2), lwd=2, cex=0.8, bty="n")
dev.off()
######### plot for domestic vs wild vs lab ######### 





######### using only unique combo sp+pheno # LH vs M ######### 
dat <- read.csv("../results/thinned.comp.csv")
#dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
LH <- sort(dat[dat$class == "LH", 2])
M <- sort(dat[dat$class == "M", 2])
LHx <- seq(from=0, to=100, length.out=length(LH))
Mx <- seq(from=0, to=100, length.out=length(M))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=LH, x=LHx, col= "#3F4788FF",lwd=3) #LH
lines(y=M, x=Mx, col="#74D055FF",lwd=3) #M
points(y=LH, x=LHx, col="#3F4788FF",pch=16, cex=.9)
points(y=M, x=Mx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("life history (n=", length(LHx),")", sep=""), 
                           paste("morphological (n=", length(Mx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### using only unique combo sp+pheno # LH vs M ######### 

######### using only unique combo sp+pheno # within vs between ######### 
dat <- read.csv("../results/thinned.comp.csv")
#dat <- dat[! is.na(dat$add),]
within <- sort(dat[dat$divergence == "within", 2])
between <- sort(dat[dat$divergence == "between", 2])
withinx <- seq(from=0, to=100, length.out=length(within))
betweenx <- seq(from=0, to=100, length.out=length(between))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=within, x=withinx, col= "#3F4788FF",lwd=3) #within
lines(y=between, x=betweenx, col="#74D055FF",lwd=3) #between
points(y=within, x=withinx, col="#3F4788FF",pch=16, cex=.9)
points(y=between, x=betweenx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### using only unique combo sp+pheno # within vs between ######### 

######### using only unique combo sp+pheno # within vs between animals only #########
dat <- read.csv("../results/thinned.comp.csv")
#dat <- dat[! is.na(dat$add),]
dat <- dat[dat$kingdom == "animal",]
within <- sort(dat[dat$divergence == "within", 2])
between <- sort(dat[dat$divergence == "between", 2])
withinx <- seq(from=0, to=100, length.out=length(within))
betweenx <- seq(from=0, to=100, length.out=length(between))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=within, x=withinx, col= "#3F4788FF",lwd=3) #within
lines(y=between, x=betweenx, col="#74D055FF",lwd=3) #between
points(y=within, x=withinx, col="#3F4788FF",pch=16, cex=.9)
points(y=between, x=betweenx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### using only unique combo sp+pheno # within vs between animals only #########

######### using only unique combo sp+pheno # within vs between plants only #########
dat <- read.csv("../results/thinned.comp.csv")
#dat <- dat[! is.na(dat$add),]
dat <- dat[dat$kingdom == "plant",]
within <- sort(dat[dat$divergence == "within", 2])
between <- sort(dat[dat$divergence == "between", 2])
withinx <- seq(from=0, to=100, length.out=length(within))
betweenx <- seq(from=0, to=100, length.out=length(between))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=within, x=withinx, col= "#3F4788FF",lwd=3) #within
lines(y=between, x=betweenx, col="#74D055FF",lwd=3) #between
points(y=within, x=withinx, col="#3F4788FF",pch=16, cex=.9)
points(y=between, x=betweenx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### using only unique combo sp+pheno # within vs between plants only #########

######### using only unique combo sp+pheno # plant vs animal ######### 
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
lines(y=plant, x=plantx, col= "#3F4788FF",lwd=3) #plant
lines(y=animal, x=animalx, col="#74D055FF",lwd=3) #animal
points(y=plant, x=plantx, col="#3F4788FF",pch=16, cex=.9)
points(y=animal, x=animalx, col="#74D055FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("plant (n=", length(plantx),")", sep=""), 
                           paste("animal (n=", length(animalx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")
######### using only unique combo sp+pheno # plant vs animal #########

