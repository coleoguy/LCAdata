# script for complete_cohorts vs reduced_cohorts

# dat is complete_cohorts_results.csv + reduced_cohorts_results.csv 
# + complete_cohorts_results.csv2 + reduced_cohorts_results.csv2 (put together manually)
dat <- read.csv("../results/all.cohort.results.csv")
dat <- replace(dat, is.na(dat), 0)

# ggplot version of figure  
library(ggplot2)
ggplot(dat, aes(cohorts, epi)) + geom_jitter(width = 0.2) + theme_bw() + 
  theme(axis.line = element_line(color='black'), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(vjust=3)) +
  stat_summary(fun = "mean", fun.min = "mean", fun.max="mean", geom = "crossbar",
               width = .5, color = "red") + xlab("") + 
  ylab("Proportion of trait divergence that is epistatic")


#beeswarm version of figure
library(beeswarm)
beeswarm(dat$epi~dat$cohorts,
         method = "hex",
         pch=16,
         corral = "gutter")


      