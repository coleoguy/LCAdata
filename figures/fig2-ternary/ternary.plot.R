library(Ternary)
#original ternary plot using all results (NOT THINNE) and not parsing by class type - WORKS
res <- read.csv("../../results/complete.results.csv")
res <- res[!is.na(res$epi),]
res <- res[order(res$epi),]
res[1298,1:3] <- c(1,0,0)
res[1299,1:3] <- c(0,1,0)
res[1300,1:3] <- c(0,0,1)
cols <- c(rep(rgb(0.6, 0.2, 1, 0.5), 1297), rep(rgb(0.2, 0.8, 0.4), 3))
TernaryPlot(alab = "", blab = "", clab = "", main="", grid.lines=4, grid.minor.col="grey95", grid.col="grey80")
TernaryPoints(res[,1:3], col = cols, cex = 1, pch=16)

