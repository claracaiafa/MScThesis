###############
#  OPERATION  #
###############

scenario_list <- c("imports", "rel.current", "rel.emulation")
scenario_label_list <- c("Imp", "Rel.Current", "Rel.Emulation")
scenario_SA_list <- c("SA0","SA1", "SA2", "SA3")


for (i in 1:length(scenario_label_list)) {
  for(k in 1: length(scenario_SA_list)){
  scenario_label <- scenario_label_list[i]
  scenario_SA <- scenario_SA_list[k]
  resultsdatapath <- paste("~/MSc Economics/Thesis/results/",scenario_label, "_", scenario_SA, ".rds", sep="")
  assign(paste("results_", scenario_label, "_", scenario_SA, sep=""), as.data.frame(readRDS(resultsdatapath)))
  }
} 

df_list <- list(results_Imp_SA0, results_Rel.Current_SA0, results_Rel.Emulation_SA0,
                results_Imp_SA1, results_Rel.Current_SA1, results_Rel.Emulation_SA1,
                results_Imp_SA2, results_Rel.Current_SA2, results_Rel.Emulation_SA2,
                results_Imp_SA3, results_Rel.Current_SA3, results_Rel.Emulation_SA3)

data_operation <- df_list %>%  reduce(full_join)