#script to plot density figs with pvals for three primary comparisons 
#(LH vs M, Plant vs Animal, Within vs Between)

res <- read.csv("../results/complete.results.csv")
res <- res[!is.na(res$epi),]
combos <- paste(res$species, "-", res$trait, sep="")
unique <- unique(combos)

for(i in 1:length(unique)){
  hit <-which(combos == unique[i])
  if(i == 1){
    new.res <- res[hit[1],c(3, 6,7, 12,4)]
  }else{
    new.res[nrow(new.res) + 1, 1:5] <- res[hit[1],c(3,6,7,12,4)]
  }
  new.res[i, 1] <- mean(res$epi[hit])
  new.res$combo[i] <- unique[i]
}
rm(res, hit, i, unique, combos)
#write.csv(new.res, "../results/thinned.comp.csv")
res2 <- new.res

###### Life history
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
plot(density(null.dist), main="", xlab="mean difference in epistatic contribution (LH-M)", ylab="density")
polygon(density(null.dist), col=rgb(0.6, 0.2, 1, 0.1))

abline(v=obs.diff,lwd=2,col=rgb(0.6, 0.2, 1))

###### Divergence
#res2 <- res2[res2$kingdom == "plant",]
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
plot(density(null.dist),xlim=c(-.1,.11), main="", xlab="mean difference in epistatic contribution (B-W)", ylab="density")
polygon(density(null.dist), col=rgb(0.6, 0.2, 1, 0.1))
abline(v=obs.diff,lwd=2,col=rgb(0.6, 0.2, 1))


###### Clade
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
plot(density(null.dist), main="", xlab="mean difference in epistatic contribution", ylab="density")
polygon(density(null.dist), col=rgb(0.6, 0.2, 1, 0.1))
abline(v=obs.diff,lwd=2,col=rgb(0.6, 0.2, 1))


