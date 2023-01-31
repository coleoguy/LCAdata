dat <- read.csv("../results/complete.results.csv")
trait.table <- as.data.frame(table(dat$trait))
trait.table$prop.epi <- NA
for(i in 1:nrow(trait.table)){
  trait.table$prop.epi[i] <-  mean(dat$epi[dat$trait == trait.table$Var1[i]])
}
