# This script runs LCA and stores results for a folder of input files
library(SAGA2)

results <- list()
mod.nums <- c()

data.files <- list.files("../data/")
enveff <- rep(F, 5)
enveff[5] <- T

for(i in 1:length(data.files)){
  dat <- read.csv(paste("../data/", data.files[i], sep=""))
  lcaest <- LCA(data = dat, env = enveff[i])
  results[[i]] <- as.data.frame(t(lcaest[[4]][,-1]))
  results[[i]]$var.imp <- lcaest[[6]][,2]
  mod.nums[i] <- length(lcaest$best.models)
}

rm(list=ls()[c(-6,-7)])
save.image("../results/full.results2.RData")


simp.plot(results[[5]])

