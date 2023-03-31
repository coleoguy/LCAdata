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



 
######### junk plot to see difference in within vs between for various phenotypes ######### 
 # this only works well if there are results for the same phenotype in within AND between divergence
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
dat <- dat[dat$trait =="number of fruit",]

library(ggplot2)
ggplot(dat, aes(divergence, epi)) + geom_point() + theme_bw() + 
    theme(axis.line = element_line(color='black'), 
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())

######### plotting diverg x epi under different subsets ######### 

dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# dat <- dat[grep("Zea", dat$species),]
dat <- dat[dat$kingdom == "animal",]
           
dat <- dat[dat$method != "PSU",]


#create binary column of 0 for 0% epi, 1 for >0% epi
# dat$bin <- ifelse(dat$epi > 0, 1, 0)

library(ggplot2)
ggplot(dat, aes(class, epi)) + geom_jitter() + theme_bw() + 
    theme(axis.line = element_line(color='black'), 
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())


mean(dat[dat$divergence == "within", 'epi'])
mean(dat[dat$divergence == "between", 'epi'])

nrow(dat[dat$divergence == "within",])
nrow(dat[dat$divergence == "between",])


######### plotting NA datasets ######### 
# these are the datasets that did not meet initial criteria #
dat <- read.csv("../results/trashthis.csv")
dat <- dat[is.na(dat$add),]

#dat <- dat[dat$method == "cmat",]


#library(ggplot2)
#ggplot(dat, aes(weighted)) + geom_violin() + theme_bw() + 
#    theme(axis.line = element_line(color='black'), 
#          plot.background = element_blank(),
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
#          panel.border = element_blank())




dat <- dat[dat$species > 5,]









