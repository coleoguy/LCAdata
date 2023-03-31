res <- read.csv("../results/complete.results.csv")
res <- res[res$weighted=="Y",]
#res <- res[res$divergence == "within",]
wi <- res$epi[res$divergence == "within"]
wi <- wi[!is.na(wi)]
be <-  res$epi[res$divergence == "between"]
be <- be[!is.na(be)]
obs.diff <- mean(be) - mean(wi)

null.dist <- c()

monte <- res
for(i in 1:10000){
  monte$divergence <- sample(monte$divergence)
  wi <- monte$epi[monte$divergence == "within"]
  wi <- wi[!is.na(wi)]
  be <-  monte$epi[monte$divergence == "between"]
  be <- be[!is.na(be)]
  null.dist[i] <- mean(be) - mean(wi)
}
plot(density(null.dist))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist>=obs.diff)/10000
