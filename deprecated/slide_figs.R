library(SAGA2)
  # drosophila melandogaster female development time
#best model plot
dat <- read.csv("../all.data/data/ACO.fem.time.csv")
res1 <- LCA(dat, parental = "calc",env = FALSE, 
                max.pars = 7, ret.all = T, SCS="XY", model.sum=0.3)
plot(res1, cex.main=0.75, main="Best model", min.vi = .3) #13 models

par(mfcol=c(2,1))
barplot(res1$all.models[[160]]$coefficients[3:6],
        names.arg = c("Aa","Ad","Xa","AdAd"),
        ylab="Effect Size",xlab="CGEs",
        ylim=c(-2,18))
abline(h=0)
barplot(res1$all.models[[184]]$coefficients[3:6],
        names.arg = c("Aa","Xa","AdAd","XaXa"),
        ylab="Effect Size",xlab="CGEs",
        ylim=c(-2,18))
abline(h=0)

#model average plot
res2 <- LCA(dat, parental = "calc",env = FALSE, 
           max.pars = 7, ret.all = T, SCS="XY")
plot(res2, cex.main=0.75, main="Model weighted averages") #13 models



VisModelSpace(res)
