# This script runs LCA and stores results for a folder of input files
library(SAGA2)

results <- list()
mod.nums <- c()

data.files <- list.files("../data/")

#allows you to make environment false (which is default) for 
#all datasets except dataset 5 (sticklebacks), which has environment = true 
#this same setup would work for xy chromosome systems, if needed
enveff <- rep(F, 5)
enveff[5] <- T

#loop to run LCA on all datasets in data folder 
for(i in 1:length(data.files)){
  dat <- read.csv(paste("../data/", data.files[i], sep="")) 
  lcaest <- LCA(data = dat, env = enveff[i])
  results[[i]] <- as.data.frame(t(lcaest[[4]][,-1]))
  results[[i]]$var.imp <- lcaest[[6]][,2]
  mod.nums[i] <- length(lcaest$best.models)
}

ls() #allows you to see what is in the environment

#removes everything except results and mod.nums from environment
rm(list=ls()[c(-6,-7)])
save.image("../results/full.results2.RData")

#function is in file helpfunctions.R in results folder
simp.plot(results[[5]])

