#script for complete_cohorts vs reduced_cohorts

dat <- read.csv("../results/all.cohort.results.csv")
ggplot(dat, aes(cohorts, epi)) + geom_jitter(width = 0.2) + theme_bw() + 
  theme(axis.line = element_line(color='black'), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +  
  stat_summary(
          geom = "point",
          fun = "mean",
          col = "red",
          size = 3,
          shape = 19,
          fill = "red"
        ) + labs(y = "Proportion of trait divergence that is epistatic", x="")
