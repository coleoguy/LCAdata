# This script plots standard and cmat datasets fitted with the PSU model against
# the same datasets fitted with the non-PSU model.

#### LOAD IN DATA ####

#PSU reduced dataset
dat.psu <- read.csv("../results/reduced.results.csv")
dat.psu <- dat.psu[! is.na(dat.psu$add),]


#Non-PSU dataset
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]

#### SUBSET TO JUST STANDARD AND CMAT ####

#PSU
dat.psu <- dat.psu[!dat.psu$method %in% "PSU",]

#Non-PSU
dat <- dat[!dat$method %in% "PSU",]

#### GET NON-ADDITIVE SUMS AND X PROPORTIONS ####
psu.sums <- sort(rowSums(dat.psu[,3]))
full.sums <- sort(rowSums(dat[,3]))
psux <- seq(from=0, to=100, length.out=length(psu.sums))
fullx <- seq(from=0, to=100, length.out=length(full.sums))

#### PLOT AND SAVE####
cairo_pdf(paste0("../figures/PSU_Fullplot.pdf"))

plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=psu.sums, x=psux, col= "#3F4788FF",lwd=3) #LH
lines(y=full.sums, x=fullx, col="#74D055FF",lwd=3) #M
points(y=psu.sums, x=psux, col="#3F4788FF",pch=16, cex=.9)
points(y=full.sums, x=fullx, col="#74D055FF",pch=16, cex=.9)
title("Standard and cmat datasets under PSU and Full models")
legend("topleft", legend=c(paste("PSU model (n=", length(psux),")", sep=""), 
                           paste("Full model (n=", length(fullx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")

dev.off()



