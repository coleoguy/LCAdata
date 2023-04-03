# Heath Blackmon
# This script performs permutation tests

res <- read.csv("../results/complete.results.csv")
res <- res[!is.na(res$epi),]

# Life history
lh <- res$epi[res$class == "LH"]
mo <- res$epi[res$class == "M"]
# hyp lh should be larger
obs.diff <- mean(lh) - mean(mo)
null.dist <- c()
for(i in 1:10000){
  keys <- sample(res$class) == "LH"
  null.dist[i] <- mean(res$epi[keys]) - 
    mean(res$epi[!keys])
}
plot(density(null.dist),xlim=c(-.1,.11))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist>=obs.diff)/10000


# Divergence
res2 <- res
res2 <- res2[res2$species != "Zea mays, Zea diploperennis Iltis",]
bw <- res2$epi[res2$divergence == "between"]
wi <- res2$epi[res2$divergence == "within"]
# hyp bw should be larger
obs.diff <- mean(bw) - mean(wi)
null.dist <- c()
for(i in 1:10000){
  keys <- sample(res2$divergence) == "between"
  null.dist[i] <- mean(res2$epi[keys]) - 
    mean(res2$epi[!keys])
}
plot(density(null.dist),xlim=c(-.1,.11))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist>=obs.diff)/10000

# Clade
res2 <- res[res$divergence == "within",]
pl <- res2$epi[res2$kingdom == "plant"]
an <-  res2$epi[res2$kingdom == "animal"]
obs.diff <- mean(pl) - mean(an)
null.dist <- c()
for(i in 1:10000){
  keys <- sample(res2$kingdom) == "plant"
  null.dist[i] <- mean(res2$epi[keys]) - 
    mean(res2$epi[!keys])
}
plot(density(null.dist))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist>=obs.diff)/10000