#storing results from each LCA run into csv to allow better access to data
####################


chloro <- read.csv("../data/chloroLCA.csv")
res <- LCA(data=chloro, 
           SCS="NSC", parental="calc", env=FALSE,
           max.pars=7, ret.all=F)
foo <- data.frame(res$estimates[,-1])
foo[3,] <- res$varimp[,2]
row.names(foo)[3] <- "vi"


biomass <- read.csv("../data/biomassLCA.csv")
res2 <- LCA(data=biomass,
            SCS="NSC", parental="calc", env=FALSE,
            max.pars = 7, ret.all = F)

