Calc-pvals.R: this script calculates the p-values and generates figures for the permutation tests completed on each primary analysis (LH-M, P-A, W-B)

Primary.analysis.R: this script runs LCA on all files

Result.munging.R: this script takes the results from the primary.analysis.R and organizes the genetic effects into additive, dominance, and epistatic components.

red_cohorts_reformed.R: this script runs LCA on the number of offspring deformed T. castaneum datasets under a reduced cohort set (P1, P2, F1, BC1, BC2).

red_cohorts_offspring.R: this script runs LCA on the number of offspring T. castaneum datasets under a reduced cohort set (P1, P2, F1, BC1, BC2).

Reduced.medel.analysis.R: this script runs the datasets that have parental sex known under both a complete and reduced model set. 

reduced.result.munging.R: this script takes the results from the reduced.model.run.RData and munges them into the three genetic effects.  

t_tests.R: this script runs a t test on two analyses: complete vs reduced number of cohorts in T. castaneum and full model set vs reduced model set analysis. 

trait.table.R: this script creates a table of the phenotypes observed in this study, with their frequency and mean epistatic contribution to trait divergence across all datasets where that phenotype was observed. 