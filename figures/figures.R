######## plot for LH vs M ########
pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/LH_M.pdf", 
    width = 5, 
    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by trait type
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
dev.off()
######## plot for LH vs M ########


######### plot for plant vs animal ######### 
pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/P_A.pdf", 
    width = 5, 
    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
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
dev.off()
######### plot for plant vs animal ######### 


######### plot for within vs between ######### 
pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/W_B.pdf", 
    width = 5, 
    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
dat <- dat[dat$weighted == "Y",]
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
dev.off()
######### plot for within vs between ######### 


######### plot for domestic vs wild vs lab #########
pdf(file = "/Users/jorjaelliott/Desktop/Repositories/LCAdata/figures/D_W.pdf", 
    width = 5, 
    height = 5)
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within or between species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]
# leave out psu datasets
# dat <- dat[dat$method %in% c("cmat","standard"),]
# split data by domestication status
wild <- sort(rowSums(dat[dat$domestication == "wild", 2:3]))
domestic <- sort(rowSums(dat[dat$domestication == "domestic", 2:3]))
lab <- sort(rowSums(dat[dat$domestication == "lab", 2:3]))
wildx <- seq(from=0, to=100, length.out=length(wild))
domesticx <- seq(from=0, to=100, length.out=length(domestic))
labx <- seq(from=0, to=100, length.out=length(lab))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=wild, x=wildx, col= "#3F4788FF",lwd=3) #wild
lines(y=domestic, x=domesticx, col="#74D055FF",lwd=3) #domestic
lines(y=lab, x=labx, col="#238a8d",lwd=3) #lab
points(y=wild, x=wildx, col="#3F4788FF",pch=16, cex=.9)
points(y=domestic, x=domesticx, col="#74D055FF",pch=16, cex=.9)
points(y=lab, x=labx, col="#238a8d",pch=16, cex=.9)
legend("topleft", legend=c(paste("wild (n=", length(wildx),")", sep=""), 
                           paste("domestic (n=", length(domesticx),")", sep=""), 
                           paste("lab (n=", length(labx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF", "#238a8d"), cex=0.8, bty="n")
dev.off()
######### plot for domestic vs wild vs lab ######### 




######### junk plot to see difference in within vs between for various phenotypes ######### 
 # this only works well if there are results for the same phenotype in within AND between divergence
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
dat <- dat[dat$species =="Tribolium castaneum",]

library(ggplot2)
ggplot(dat, aes(, epi)) + geom_point() + theme_bw() + 
    theme(axis.line = element_line(color='black'), 
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())








mean(dat[dat$divergence == "between", 'epi'])
mean(dat[dat$divergence == "within", 'epi'])

nrow(dat[dat$divergence == "between", ])
nrow(dat[dat$divergence == "within", ])





ref <- read.csv("ref.csv")





