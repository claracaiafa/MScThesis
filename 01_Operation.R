###############
#  OPERATION  #
###############

# 1) Prepare environment
rm(list = ls())
options(scipen = 14)
source(file.path("code","packages.R"))

project_phase <- c("Operation")
scenario_list <- c("imports", "rel.current", "rel.emulation")
scenario_label_list <- c("Imp", "Rel.Current", "Rel.Emulation")
scenario_SA_list <- c("SA0", "SA1", "SA2", "SA3")
baseline <- c("BF-BOF")
baseline_SA <- c("BA0")


# 2) Run scenarios

for(i in 1:length(scenario_list)){
  for(s in 1: length(scenario_SA_list)){
  # 1. Define scenario
  project_scenario <- scenario_list[i] 
  scenario_label <- scenario_label_list[i]
  scenario_SA <- scenario_SA_list[s]
  
  # 2. Load configuration file
  source(file.path("code","configuration", paste0("config_",project_phase,".R")))
  
  # 3. Model run
  source(file.path("code","model", paste0("00_RunModel_", project_phase, ".R")))} 
}
