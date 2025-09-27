# Data

# Files 

`database_analysis_summary.csv` -  summary data for community level, based on species alien/native status for each plot and scale; produced by [01_prepare_species_data](../R/01_prepare_data/01_prepare_species_data.R)

`non-native_matrix.csv` -  species composition matrix for alien species for each plot at 10 and 100 m2; produced by [01_prepare_species_data](../R/01_prepare_data/01_prepare_species_data.R)

`non-native_trait_matrix.csv`-  matrix of alien species with their traits; produced by [02_prepare_matrix](../R/01_prepare_data/02_prepare_matrix.R)

`header_data_prepared.csv`- selected predictor variables; produced by [04_prepare_header_data](../R/01_prepare_data/04_prepare_header_data.R)


`Clima_PC.csv`- Climate gradient form principal component analysis; produced in Buzhdygan et.al 2025 Ecology and Evolution https://doi.org/10.1002/ece3.70941

`alien_dataset_all.csv`- number, proportion and presence of alien species groups, natives and total species richness, merged with the environmental data; produced by [03_summary_statistics](../R/05_summary_stats/03_summary_statistics.R)

`model_results.rds`- Results of binomial GLMM models testing the effects of native species richness and environmental drivers on proportions of alien species for all scales. produced by [01_run_all_binomial_models.R](../R/02_run_models/01_run_all_binomial_models.R)

`model_results_summary.csv`- Summarized results of binomial GLMM models testing the effects of native species richness and environmental drivers on proportions of alien species for all scales. produced by [01_run_all_binomial_models.R](../R/02_run_models/01_run_all_binomial_models.R)
