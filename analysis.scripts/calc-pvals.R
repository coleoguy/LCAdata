res <- read.csv("complete.results.csv")
res <- res[res$divergence == "within",]
LH <- res$epi[res$kingdom == "plant"]
LH <- LH[!is.na(LH)]
M <-  res$epi[res$kingdom == "animal"]
M <- M[!is.na(M)]
obs.diff <- mean(LH) - mean(M)

null.dist <- c()

monte <- res
for(i in 1:10000){
  monte$kingdom <- sample(monte$kingdom)
  LH <- monte$epi[monte$kingdom == "plant"]
  LH <- LH[!is.na(LH)]
  M <-  monte$epi[monte$kingdom == "animal"]
  M <- M[!is.na(M)]
  null.dist[i] <- mean(LH) - mean(M)
}
plot(density(null.dist))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist<=obs.diff)/10000
