dat <- read.csv("complete.results.csv")
cat <- read.csv("results.category.csv")
dat <- dat[order(dat$epistatic), ]
cat <- cat[complete.cases(dat), ]
dat <- dat[complete.cases(dat), ]


#plot of plant vs animals
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




#plot of LH vs M traits
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




#plot of within or between species
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




install.packages("Ternary")
library("Ternary")
Ternary::TernaryApp()
