library(SAGA2)
  # drosophila melandogaster female development time
#best model plot
dat <- read.csv("ACO.fem.time.csv")
res <- LCA(dat, parental = "calc",env = FALSE, 
                max.pars = 7, ret.all = F, SCS="XY", model.sum=0.3)
plot(res) #13 models



#model average plot
dat <- read.csv("ACO.fem.time.csv")
res <- LCA(dat, parental = "calc",env = FALSE, 
           max.pars = 7, ret.all = F, SCS="XY")
plot(res) #13 models

