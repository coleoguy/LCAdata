# script for complete_cohorts vs reduced_cohorts

# dat is complete_cohorts_results.csv + reduced_cohorts_results.csv 
# + complete_cohorts_results.csv2 + reduced_cohorts_results.csv2 (put together manually)
dat <- read.csv("../../results/all.cohort.results.csv")
dat <- replace(dat, is.na(dat), 0)

library(beeswarm)
beeswarm(dat$epi~dat$cohorts,
         method = "hex",
         pch=16,
         corral = "gutter",
         xlab = "cohort",
         ylab = "Proportion epistatic",
         col="black")
mucomp <- mean(dat$epi[dat$cohorts=="complete"])
mured <- mean(dat$epi[dat$cohorts=="reduced"])
lines(x=c(.8,1.2), y=rep(mucomp, 2), lwd=3, col=rgb(0.6, 0.2, 1))
lines(x=c(1.8,2.2), y=rep(mured, 2), lwd=3, col=rgb(0.6, 0.2, 1))
# Perform t-test
t_test <- t.test(dat$epi ~ dat$cohorts)

# Extract p-value
p_value <- t_test$p.value

# Print the p-value
print(p_value)    
