################
# CONSTRUCTION #
################

# Project: Masters' Thesis in Economics
# Title: Economic impacts of green steel
# Author: Clara Rabelo Caiafa Pereira

scenario_SA_list <- c("SA0", "SA1")

# Construction

for(k in 1: length(scenario_SA_list)){
  scenario_SA <- scenario_SA_list[k]
  resultsdatapath <- paste("~/MSc Economics/Thesis/results/Construction_", scenario_SA, ".rds", sep="")
  assign(paste("results_construction_", scenario_SA, sep=""), as.data.frame(readRDS(resultsdatapath)))
}
df_construction_list <- list(results_construction_SA0, results_construction_SA1)

data_construction <- df_construction_list %>% reduce(full_join)
