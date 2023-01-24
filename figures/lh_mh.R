#plot for LH vs M
dat <- read.csv("../results/complete.results.csv")
dat <- dat[! is.na(dat$add),]
# get just within species
# dat <- dat[dat$divergence == "within",]
# dat <- dat[dat$method %in% c("cmat","standard"),]
#split data by trait type
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


######
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
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=datp$epistatic, x=datpx, col="#74D055FF",lwd=3) #plant
lines(y=datq$epistatic, x=datqx, col="#3F4788FF",lwd=3) #animal
points(y=datp$epistatic, x=datpx, col="#74D055FF",pch=16, cex=.9)
points(y=datq$epistatic, x=datqx, col="#3F4788FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("plant (n=", length(datpx),")", sep=""), 
                           paste("animal (n=", length(datqx),")", sep="")), 
       fill=c("#74D055FF", "#3F4788FF"), cex=0.8, bty="n")

######
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
     xaxt="n", xlab="proportion of datasets analyzed",
     ylab="proportion of trait divergence that is epistasis")
axis(side=1, at=c(0,50,100), c("0%","50%","100%"))
lines(y=datp$epistatic, x=datpx, col="#74D055FF",lwd=3) #within
lines(y=datq$epistatic, x=datqx, col="#3F4788FF",lwd=3) #between
points(y=datp$epistatic, x=datpx, col="#74D055FF",pch=16, cex=.9)
points(y=datq$epistatic, x=datqx, col="#3F4788FF",pch=16, cex=.9)
legend("topleft", legend=c(paste("within species (n=", length(datpx),")", sep=""), 
         paste("between species (n=", length(datqx),")", sep="")), 
fill=c("#74D055FF", "#3F4788FF"), cex=0.8, bty="n")





trait.table <- as.data.frame(table(dat$trait))
trait.table$prop.epi <- NA
for(i in 1:nrow(trait.table)){
  trait.table$prop.epi[i] <-  mean(dat$epi[dat$trait == trait.table$Var1[i]])
}










