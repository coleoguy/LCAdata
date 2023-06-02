complete.results.csv: This file contains all results from the LCA on all 1606 files

thinned.comp.csv: This file contains the LCA results averaged across all datasets for a specific species-phenotype combination. This file includes one result for each unique species-phenotype combination.

full.run.RData: This file contains the full analysis of all datasets and is used
as the base file for "result.munging.R"

reduced.model.run.RData: This file reruns standard model datasets under the PSU
settings.

Reduced.results.csv: this file has the results for running all standard model datasets under the reduced model set setting. 

complete.results.csv: This file has the results from full run parsed into
additive, dominance, and epistatic and includes lots of variables from
the ref file.

all.cohort.results.csv: This file has the combined results from running LCA on both reduced and complete cohort sets for number of offspring in Tribolium castaneum. This file is all four cohort result files put together (complete_cohorts_results.csv, complete_cohorts_results2.csv, reduced_cohorts_results.csv, reduced_cohorts_results2.csv).

cohorts_complete.all.csv: this file has the results of running LCA on all T. Castaneum datasets with phenotype offspring number and offspring number deformed under the full number of cohorts. 

cohorts_reduced.all.csv: this file has the results of running LCA on all T. Castaneum datasets with phenotype offspring number and offspring number deformed under the reduced number of cohorts (P1, P2, F1, BC1, BC2). 

trait.table.csv: This file contains the results of the trait table script, providing the phenotype, its frequency, and average epistatic contribution for each unique phenotype. 
