#Life history versus morphological traits figure script
######## figure using only unique combinations of species-phenotype#######
dat <- read.csv("../results/thinned.comp.csv")
#dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$divergence == "between",]

LH <- sort(dat[dat$class == "LH", 2])
M <- sort(dat[dat$class == "M", 2])
LHx <- seq(from=0, to=100, length.out=length(LH))
Mx <- seq(from=0, to=100, length.out=length(M))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion epistasic")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=LH, x=LHx, col= rgb(0.6, 0.2, 1),lwd=3) #LH
lines(y=M, x=Mx, col=rgb(0.2, 0.8, 0.4),lwd=3) #M
points(y=LH, x=LHx, col=rgb(0.6, 0.2, 1),pch=16, cex=.9)
points(y=M, x=Mx, col=rgb(0.2, 0.8, 0.4),pch=16, cex=.9)
legend("topleft", legend=c(paste("life history (n=", length(LHx),")", sep=""), 
                           paste("morphological (n=", length(Mx),")", sep="")), 
       fill=c(rgb(0.6, 0.2, 1), rgb(0.2, 0.8, 0.4)), cex=0.8, bty="n")
# Exported at 5.5 x 4
######## figure using only unique combinations of species-phenotype#######

