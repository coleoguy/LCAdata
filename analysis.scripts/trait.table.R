dat <- read.csv("trait.table.results.csv")
dat <- dat[!is.na(dat$add),] #have to remove NAs before loop or traits where some rows are NA will not calculate prop.epi
trait.table <- as.data.frame(table(dat$trait))
trait.table$prop.epi <- NA
for(i in 1:nrow(trait.table)){
  trait.table$prop.epi[i] <-  mean(dat$epi[dat$trait == trait.table$Var1[i]])
}
trait.table <- trait.table[trait.table$Freq > 3,]
write.csv(trait.table, "trait.table.csv")
