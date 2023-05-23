# This script plots standard and cmat datasets fitted with the PSU model against
# the same datasets fitted with the non-PSU model.

#### LOAD IN DATA ####

#PSU reduced dataset
dat.psu <- read.csv("../results/reduced.results.csv")

#Non-PSU dataset
dat <- read.csv("../results/complete.results.csv")

#### SUBSET TO JUST STANDARD AND CMAT ####

#PSU
dat.psu <- dat.psu[!dat.psu$method %in% "PSU",]

#Non-PSU
dat <- dat[!dat$method %in% "PSU",]

#Temporarily convert NAs to 0
dat.psu[which(is.na(dat.psu$add)),c(1,2,3)] <- 0
dat[which(is.na(dat$add)),c(1,2,3)] <- 0

#### CORRELATION SCATTERPLOT ####

cairo_pdf(paste0("../figures/PSU_Fullcor.pdf"),
          width = 5,
          height = 5)

plot(x=jitter(dat.psu$epi,20),
     y=jitter(dat$epi,20),
     xlab = "PSU model proportion epistasis",
     ylab = "Full model proportion epistasis",
     pch=16,
     cex=1,
     col= rgb(0,0,0,0.5))
lines(x=c(0,1), y=c(0,1))
text(0.2, 1, labels="R"^2 ~ "= 0.642")
#legend(0.1, 1, "R"^2 ~ "= 0.642")

dev.off()
dat.psu$epi > 0 & dat$epi ==0
dat.psu$epi == 0 & dat$epi > 0
cor.test(dat.psu$epi,dat$epi)

#Get datasets which are 0 in either PSU or full but not in other
mismatch <- as.data.frame(cbind(c(c(paste0("n=",length(which(dat.psu$epi > 0 & dat$epi ==0))),
                          paste0(dat.psu[which(dat.psu$epi > 0 & dat$epi ==0),5],
                          ", method=",
                          dat.psu[which(dat.psu$epi > 0 & dat$epi ==0),12])),
                            rep(NA,
                                length(which(dat.psu$epi == 0 & dat$epi > 0)) - 
                                  length(which(dat.psu$epi > 0 & dat$epi == 0)))),
                          c(paste0("n=",length(which(dat.psu$epi == 0 & dat$epi > 0))),
                            paste0(dat[which(dat.psu$epi == 0 & dat$epi > 0),5],
                                   ", method=",
                                   dat.psu[which(dat.psu$epi > 0 & dat$epi ==0),12]))))

colnames(mismatch) <- c("normal.0","psu.0")

#Save
write.csv(mismatch,"../results/psu.full.mismatch.csv", row.names = F)

#### GET EPISTASIS SUMS AND X PROPORTIONS ####

#Remove data with no significant models
dat.psu <- dat.psu[-which(dat.psu$add == 0 &
                          dat.psu$dom == 0 &
                          dat.psu$epi == 0),]

dat <- dat[-which(dat$add == 0 &
                  dat$dom == 0 &
                  dat$epi == 0),]

#Get sums and x proportions
psu.sums <- sort(dat.psu[,3])
full.sums <- sort(dat[,3])
psux <- seq(from=0, to=100, length.out=length(psu.sums))
fullx <- seq(from=0, to=100, length.out=length(full.sums))

#### PLOT AND SAVE####

cairo_pdf(paste0("../figures/PSU_Fullplot.pdf"),
          width = 5,
          height = 5)

plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=psu.sums, x=psux, col= "#3F4788FF",lwd=3) #LH
lines(y=full.sums, x=fullx, col="#74D055FF",lwd=3) #M
points(y=psu.sums, x=psux, col="#3F4788FF",pch=16, cex=.9)
points(y=full.sums, x=fullx, col="#74D055FF",pch=16, cex=.9)
#title("Standard and cmat datasets under PSU and Full models")
legend("topleft", legend=c(paste("PSU model (n=", length(psux),")", sep=""), 
                           paste("Full model (n=", length(fullx),")", sep="")), 
       fill=c("#3F4788FF", "#74D055FF"), cex=0.8, bty="n")

dev.off()



