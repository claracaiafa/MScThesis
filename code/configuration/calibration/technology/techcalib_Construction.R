############################
## TECH CALIB CONSTRUCTION #
############################
for (c in 1:length(scenario_label)) {
F_Scenario_path <- paste("~/MSc Economics/Thesis/data_input/tech_calib/", project_phase, "/F_", scenario_label[c],"_", scenario_SA, ".csv", sep="")

assign(paste0("F_", scenario_label[c]), as.data.frame(read.csv(F_Scenario_path)))}
