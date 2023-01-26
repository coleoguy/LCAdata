load("../results/full.run.RData")
ref <- read.csv("../all.data/ref.csv")
procLCA <- function(x){
  mcomp <- rep(0, 3)
  est <- matrix(as.numeric(unlist(x[[1]])),nrow=nrow(x[[1]]))
  colnames(est) <- colnames(x[[1]])
  est <- est[, -1]
  est[1, ] <- abs(est[1,])
  magcheck <- est[1, ] > est[2, ]
  vicheck <- as.numeric(x[[2]][, 2]) >= .5
  cge <- colnames(est)[magcheck & vicheck]
  cge.est <- est[1, ][magcheck & vicheck]
  additive <- c("Aa", "Ca", "Mea", "Xa", "Ya","Ma")    
  dominance <- c("Ad", "Med", "Xd", "Md")   
  epistatic <- c("AaXa", "AaXd", "AdXd", "XaXd", "XdXd", "XdCa", "AaYa",
                 "AaAa", "AaAd", "AaCa", "AdAd", "AdXa", "AdYa", "XaYa", 
                 "YaCa", "XaXa", "XaCa", "AdCa", "XaAa", "XaAd", "CaXa", 
                 "CaYa", "CaXd", "AaWa") 
  mcomp[1] <- sum(cge.est[cge %in% additive])
  mcomp[2] <- sum(cge.est[cge %in% dominance])
  mcomp[3] <- sum(cge.est[cge %in% epistatic])
  names(mcomp) <- c("add","dom", "epi")
  mcomp <- mcomp/sum(mcomp)
  return(mcomp)
}
final.results <- as.data.frame(matrix(NA, 1,4))
colnames(final.results) <- c("add","dom","epi","file")
for(i in 1:length(res)){
  final.results[i, 1:3] <- procLCA(res[[i]])
  final.results[i, 4] <- names(res)[i]
}
# add interesting variables to go with the genetic architectures
final.results$refile <- ref$new.file.name
final.results$class <- ref$LH.or.M
final.results$kingdom <- ref$plant.or.animal
final.results$domestication <- ref$domestication
final.results$trait <- ref$phenotype
final.results$species <- ref$organism
final.results$SCS <- ref$SCS
final.results$divergence <- ref$withinor.between.species
final.results$weighted <- ref$SE.provided.
final.results$method <- ref$data.type..standard..PSU..cmat
write.csv(final.results,"../results/complete.results.csv", row.names = F)




