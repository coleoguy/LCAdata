### THIS IS THE PLACE TO START RUNNNING THE CODE
# Alternative approaches to tests of significance for these hard questions

res <- read.csv("../results/complete.results.csv")
res <- res[!is.na(res$epi),]
combos <- paste(res$species, "-", res$trait, sep="")
unique <- unique(combos)

for(i in 1:length(unique)){
  hit <-which(combos == unique[i])
  if(i == 1){
    new.res <- res[hit[1],c(3, 6,7, 12)]
  }else{
    new.res[nrow(new.res) + 1, 1:4] <- res[hit[1],c(3, 6,7, 12)]
  }
  new.res[i, 1] <- mean(res$epi[hit])
  new.res$combo[i] <- unique[i]
}
rm(res, hit, i, unique, combos)
#write.csv(new.res, "../results/thinned.comp.csv")

## TODO START HERE CODING JORJA - I BELIEVE IN YOU!!!
res2 <- new.res
# Life history
lh <- res2$epi[res2$class == "LH"]
mo <- res2$epi[res2$class == "M"]
# hyp lh should be larger
obs.diff <- mean(lh) - mean(mo)
null.dist <- c()
for(i in 1:10000){
  keys <- sample(res2$class) == "LH"
  null.dist[i] <- mean(res2$epi[keys]) - 
    mean(res2$epi[!keys])
}
plot(density(null.dist))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist>=obs.diff)/10000
table(res2$divergence[res2$class=="LH"])
42/(42+202)
49/(49+195)
table(res2$divergence[res2$class=="M"])

# Divergence
res2 <- res2[res2$kingdom == "plant",]
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
table(res2$class[res2$divergence == "between"])
table(res2$class[res2$divergence == "within"])
42/(42+49)
202/(202+195)

# Clade
#res2 <- res2[res2$divergence == "between",]
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
table(res2$class[res2$kingdom=="plant"])
table(res2$class[res2$kingdom=="animal"])











# Divergence
res2 <- res
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
plot(density(null.dist))
abline(v=obs.diff,lwd=2,col="darkred")
sum(null.dist>=obs.diff)/10000

















#probably trashable below this:
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
