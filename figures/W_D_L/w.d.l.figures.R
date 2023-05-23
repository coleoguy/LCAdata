#domestic versus wild versus lab figures script
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