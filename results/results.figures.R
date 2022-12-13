dat <- read.csv("complete.results.csv")
cat <- read.csv("results.category.csv")
dat <- dat[order(dat$epistatic), ]
cat <- cat[complete.cases(dat), ]
dat <- dat[complete.cases(dat), ]



###plot of plant vs animals
plot(0,0,col="white",xlim=c(-1,1),ylim=c(0,160),
     yaxt="n", xaxt="n", xlab="",ylab="")
axis(side=1, at=c(-1,-.5,.5,1), c("100%","50%","50%","100%"))
abline(v=0)
text(x=c(-.5,.5),y=c(160,160), c("epistasic","non-epistatic"))
cols <- c("red","blue")[as.factor(cat$plant.or.animal)]
for(i in 1:152){
  epicomp <- -dat$epistatic[i]
  lines(y=c(i,i), x=c(epicomp, (1+epicomp)), col=cols[i],lwd=3)
}
legend("bottomleft", legend=c("animal", "plant"), 
       fill=c("red", "blue"), cex=0.8)


###plot of LH vs M traits
plot(0,0,col="white",xlim=c(-1,1),ylim=c(0,160),
     yaxt="n", xaxt="n", xlab="",ylab="")
axis(side=1, at=c(-1,-.5,.5,1), c("100%","50%","50%","100%"))
abline(v=0)
text(x=c(-.5,.5),y=c(160,160), c("epistasic","non-epistatic"))
cols <- c("red","blue")[as.factor(cat$LH.or.M)]
for(i in 1:152){
  epicomp <- -dat$epistatic[i]
  lines(y=c(i,i), x=c(epicomp, (1+epicomp)), col=cols[i],lwd=3)
}
legend("bottomleft", legend=c("LH", "M"), 
       fill=c("red", "blue"), cex=0.8)


###plot of within or between species
plot(0,0,col="white",xlim=c(-1,1),ylim=c(0,160),
     yaxt="n", xaxt="n", xlab="",ylab="")
axis(side=1, at=c(-1,-.5,.5,1), c("100%","50%","50%","100%"))
abline(v=0)
text(x=c(-.5,.5),y=c(160,160), c("epistasic","non-epistatic"))
cols <- c("red", "blue")[as.factor(cat$withinor.between.species)]
for(i in 1:152){
  epicomp <- -dat$epistatic[i]
  lines(y=c(i,i), x=c(epicomp, (1+epicomp)), col=cols[i],lwd=3)
}
legend("bottomleft", legend=c("between species", "within species"), 
       fill=c("red", "blue"), cex=0.8)

#######
par(mfcol=c(3,3))

#plot for LH vs M
dat <- read.csv("complete.results.csv")
cat <- read.csv("results.category.csv")
datp <- dat[cat$LH.or.M == "LH",] #split data by trait type
datq <- dat[cat$LH.or.M == "M",]
datp$type <- "LH"
datq$type <- "M"
datp <- datp[order(datp$epistatic), ]
datq <- datq[order(datq$epistatic), ]
dat <- rbind(datp,datq)
dat <- dat[complete.cases(dat), ]

datp <- datp[complete.cases(datp),]
datpx <- seq(from=0, to=100, length.out=nrow(datp))
datq <- datq[complete.cases(datq),]
datqx <- seq(from=0, to=100, length.out=nrow(datq))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="",ylab="")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=datp$epistatic, x=datpx, col= "#74D055FF",lwd=3) #LH
lines(y=datq$epistatic, x=datqx, col="#3F4788FF",lwd=3) #M
points(y=datp$epistatic, x=datpx, col="#74D055FF",pch=16, cex=.9)
points(y=datq$epistatic, x=datqx, col="#3F4788FF",pch=16, cex=.9)
legend("topleft", legend=c("LH", "M"), 
       fill=c("#74D055FF", "#3F4788FF"), cex=0.8, bty="n")



#plot for plants vs animals
dat <- read.csv("complete.results.csv")
cat <- read.csv("results.category.csv")
datp <- dat[cat$plant.or.animal == "plant",] #split data by trait type
datq <- dat[cat$plant.or.animal == "animal",]
datp$type <- "plant"
datq$type <- "animal"
datp <- datp[order(datp$epistatic), ]
datq <- datq[order(datq$epistatic), ]
dat <- rbind(datp,datq)
dat <- dat[complete.cases(dat), ]

datp <- datp[complete.cases(datp),]
datpx <- seq(from=0, to=100, length.out=nrow(datp))
datq <- datq[complete.cases(datq),]
datqx <- seq(from=0, to=100, length.out=nrow(datq))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="",ylab="")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=datp$epistatic, x=datpx, col="#74D055FF",lwd=3) #plant
lines(y=datq$epistatic, x=datqx, col="#3F4788FF",lwd=3) #animal
points(y=datp$epistatic, x=datpx, col="#74D055FF",pch=16, cex=.9)
points(y=datq$epistatic, x=datqx, col="#3F4788FF",pch=16, cex=.9)
legend("topleft", legend=c("plant", "animal"), 
       fill=c("#74D055FF", "#3F4788FF"), cex=0.8, bty="n")


#plot for within vs between species
dat <- read.csv("complete.results.csv")
cat <- read.csv("results.category.csv")
datp <- dat[cat$withinor.between.species == "within",] #split data by trait type
datq <- dat[cat$withinor.between.species == "between",]
datp$type <- "within"
datq$type <- "between"
datp <- datp[order(datp$epistatic), ]
datq <- datq[order(datq$epistatic), ]
dat <- rbind(datp,datq)
dat <- dat[complete.cases(dat), ]

datp <- datp[complete.cases(datp),]
datpx <- seq(from=0, to=100, length.out=nrow(datp))
datq <- datq[complete.cases(datq),]
datqx <- seq(from=0, to=100, length.out=nrow(datq))
plot(0,0,col="white",xlim=c(0,100),ylim=c(0,1),
     xaxt="n", xlab="",ylab="")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=datp$epistatic, x=datpx, col="#74D055FF",lwd=3) #within
lines(y=datq$epistatic, x=datqx, col="#3F4788FF",lwd=3) #between
points(y=datp$epistatic, x=datpx, col="#74D055FF",pch=16, cex=.9)
points(y=datq$epistatic, x=datqx, col="#3F4788FF",pch=16, cex=.9)
legend("topleft", legend=c("within", "between"), 
       fill=c("#74D055FF", "#3F4788FF"), cex=0.8, bty="n")










