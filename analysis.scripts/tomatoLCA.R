#LCA for tomato data
library(SAGA2)
library(viridis)

########################## Untransformed Data #######################
###SAMPLE CODE FOR LCA###
beetle.dat <- read.csv("../data/saga.single.val.data.csv")
res <- LCA(data=beetle.dat,
           SCS="XY",parental = "calc",env=FALSE,
           max.pars = 7, ret.all=F)
library(viridis)
plot(res, col.ramp = viridis(100))

barplot(res$best.models[[1]]$coefficients[3:4],
        names=(c("Aa","AaAd")))
#To visualize the model space only works well for low max.pars values
#VisModelSpace(res)
plot(res, min.vi = .55, main = "")
######################################################################

#chloro
chloro <- read.csv("../data/chloroLCA.csv")
res <- LCA(data=chloro, 
           SCS="NSC", parental="calc", env=FALSE,
           max.pars=7, ret.all=F)
plot(res, main="chlorophyll content", col.ramp=viridis(100))
## - autosomal additive


#biomass ---- error in plot, changed min.vi to .4 instead of .5 
biomass <- read.csv("../data/biomassLCA.csv")
res2 <- LCA(data=biomass,
           SCS="NSC", parental="calc", env=FALSE,
           max.pars = 7, ret.all = F)
plot(res2, min.vi = .4, main="biomass", col.ramp = viridis(100)) 
## - epistatic interaction: autosomal additive by autosomal additive


#germ time
germ <- read.csv("../data/germLCA.csv")
res3 <- LCA(data=germ,
            SCS="NSC", parental="calc", env=FALSE,
            max.pars=7, ret.all=F)
plot(res3, main="germination time", col.ramp=viridis(100))
## - epistatic interaction: autosomal additive by autosomal dominance


#flower time
flower <- read.csv("../data/flowertimeLCA.csv")
res4 <- LCA(data=flower, 
            SCS="NSC", parental="calc", env=FALSE,
            max.pars=7, ret.all=F)
plot(res4, main="time to flowering", col.ramp=viridis(100))
## + autosomal additive





#####################
#test new batch of tomatoes for applicable genetic effects, using FAKE DATA
test.tomato <- read.csv("test.tomato.csv")
res <- LCA(data=test.tomato,
           SCS="NSC", parental="calc", env=FALSE,
           max.pars=7, ret.all=F)
plot(res, col.ramp=viridis(100))

