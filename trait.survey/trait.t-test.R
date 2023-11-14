dat <- read.csv("./trait.survey/fitsurvey_sort.csv")

group_M <- dat[dat$trait_type == 'M', ]
group_LH <- dat[dat$trait_type == 'LH', ]

t_test_result <- t.test(group_M$mean_fitness_value, group_LH$mean_fitness_value)

print(t_test_result)
