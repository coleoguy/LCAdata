complete.results.csv: This file contains all results from the LCA on all 1606 files

thinned.comp.csv: This file contains the LCA results averaged across all datasets for a specific species-phenotype combination. This file includes one result for each unique species-phenotype combination.

full.run.RData: This file contains the full analysis of all datasets and is used
as the base file for "result.munging.R"

reduced.model.run.RData: This file reruns standard model datasets under the PSU
settings.

complete.results.csv: This file has the results from full run parsed into
additive, dominance, and epistatic and includes lots of variables from
the ref file.

all.cohort.results.csv: This file has the combined results from running LCA on both reduced and complete cohort sets for number of offspring in Tribolium castaneum. This file is all four cohort result files put together (complete_cohorts_results.csv, complete_cohorts_results2.csv, reduced_cohorts_results.csv, reduced_cohorts_results2.csv).

complete_cohorts_results.csv: This file holds the results from running LCA on the number of offspring in T. castaneum under a complete cohort set.

complete_cohorts_results2.csv: This file holds the results from running LCA on the number of offspring deformed in T. castaneum under a complete cohort set.

reduced_cohorts_results.csv: This file holds the results from running LCA on the number of offspring in T. castaneum under a reduced cohort set (P1, P2, F1, BC1, BC2).

reduced_cohorts_results2.csv: This file holds the results from running LCA on the number of offspring deformed in T. castaneum under a reduced cohort set (P1, P2, F1, BC1, BC2).

trait.table.csv: This file contains the results of the trait table script, providing the phenotype, its frequency, and average epistatic contribution for each unique phenotype. 
