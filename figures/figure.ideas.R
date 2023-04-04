library(vcd)
library(viridis)
library(Ternary)
res <- read.csv("../results/complete.results.csv")
res <- res[!is.na(res$epi),]
res <- res[order(res$epi),]


cats <- as.factor(round(res$add * 9)+1)
library(beeswarm)
beeswarm(res$epi~cats, pch=16, cex=.3,corral="random", col=rgb(.5,0,0,.5))


cols <- plasma(101, alpha=.5)[1 + round(100 * res$add)]
plot(sort(res$add), col=rgb(0,.5,0,.15), pch=16)
points(sort(res$epi), col=rgb(0,0,.5,.15), pch=16)
points(sort(res$dom), col=rgb(.5,0,0,.15), pch=16)

hist(res[,1])

##### ternary plots #####

cols <- plasma(7, alpha=.5)[c(2,6)][as.factor(res$kingdom)]
#pchs <- c(15,16)[as.factor(res$kingdom)]
ternaryplot(res[,1:3],
            col=cols,
            cex=.45,
            pch=16, main="", dimnames=c("additive", "dominance", "epistatic"), 
            dimnames_position="corner", grid=F)

#just another option
TernaryPlot(alab = "Additive", blab = "Dominance", clab = "Epistatic", main="")
TernaryPoints(res[,1:3], col = cols, cex = .45, pch=pchs)


round(res$add * 9)
library(ggplot2)
library(ggridges)
ggplot(res, aes(x = epi, y = cats)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(res, aes(y=cats, x=epi,  fill=cats)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=10) +
  theme_ridges()
