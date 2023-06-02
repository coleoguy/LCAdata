#t-test for epistatic contribution for trait divergence for complete cohorts vs reduced cohorts 

complete <- read.csv("../results/cohorts_complete.all.csv")
reduced <- read.csv("../results/cohorts_reduced.all.csv")

t.test(x=complete$epi, y=reduced$epi)


#t-test for epistatic contrinubtion in full model set vs reduced model set

reduced.mod <- read.csv("../results/reduced.results.csv")
complete.mod <- read.csv("../results/complete.results.csv")

t.test(x=complete.mod$epi, y=reduced.mod$epi)
