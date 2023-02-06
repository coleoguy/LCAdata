library(SAGA2)
  # drosophila melandogaster female development time
#best model plot
dat <- read.csv("ACO.fem.time.csv")
res <- LCA(dat, parental = "calc",env = FALSE, 
                max.pars = 7, ret.all = F, SCS="XY", model.sum=0.3)
plot(res, cex.main=0.75, main="Best model") #13 models



#model average plot
dat <- read.csv("ACO.fem.time.csv")
res <- LCA(dat, parental = "calc",env = FALSE, 
           max.pars = 7, ret.all = F, SCS="XY")
plot(res, cex.main=0.75, main="Model weighted averages") #13 models



VisModelSpace(dat)
