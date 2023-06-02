#within-species versus between-species crosses figures script
##### figure using thinned datasets #####
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
lines(y=within, x=withinx, col= rgb(0.6, 0.2, 1),lwd=3) #within
lines(y=between, x=betweenx, col=rgb(0.2, 0.8, 0.4),lwd=3) #between
points(y=within, x=withinx, col=rgb(0.6, 0.2, 1),pch=16, cex=.9)
points(y=between, x=betweenx, col=rgb(0.2, 0.8, 0.4),pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c(rgb(0.6, 0.2, 1),rgb(0.2, 0.8, 0.4)), cex=0.8, bty="n")
##### figure using thinned datasets #####

##### figure using only animal datasets thinned##### -- saved for talks, not in paper
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
lines(y=within, x=withinx, col= rgb(0.6, 0.2, 1),lwd=3) #within
lines(y=between, x=betweenx, col=rgb(0.2, 0.8, 0.4),lwd=3) #between
points(y=within, x=withinx, col=rgb(0.6, 0.2, 1),pch=16, cex=.9)
points(y=between, x=betweenx, col=rgb(0.2, 0.8, 0.4),pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c(rgb(0.6, 0.2, 1),rgb(0.2, 0.8, 0.4)), cex=0.8, bty="n")
##### figure using only animal datasets #####

##### figure using only plant datasets thinned##### -- saved for talks, not in paper
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
lines(y=within, x=withinx, col= rgb(0.6, 0.2, 1),lwd=3) #within
lines(y=between, x=betweenx, col=rgb(0.2, 0.8, 0.4),lwd=3) #between
points(y=within, x=withinx, col=rgb(0.6, 0.2, 1),pch=16, cex=.9)
points(y=between, x=betweenx, col=rgb(0.2, 0.8, 0.4),pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(withinx),")", sep=""), 
                           paste("between species (n=", length(betweenx),")", sep="")), 
       fill=c(rgb(0.6, 0.2, 1), rgb(0.2, 0.8, 0.4)), cex=0.8, bty="n")
##### figure using only plant datasets thinned#####












