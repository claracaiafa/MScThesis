################
# CONSTRUCTION #
################

# Project: Masters' Thesis in Economics
# Title: Economic impacts of green steel
# Author: Clara Rabelo Caiafa Pereira

# 1) Prepare environment
rm(list = ls())
options(scipen = 14)
source(file.path("code","packages.R"))

project_phase <- c("Construction")
scenario_label <- c("BR", "NL", "BRImpShare", "NLImpShare", "CNIN", "OtherEU", "RoW", "USMCA")
scenario_SA_list <- c("SA0", "SA1")
baseline <- c("BF-BOF")
baseline_SA <- c("BA0")
technology <- c("GH2", "GE", "RE")

# 2) Run scenarios

for(i in 1:length(scenario_SA_list)){
  # 1. Define scenario
  scenario_SA <- scenario_SA_list[i] 
  
  # 2. Load configuration file
  source(file.path("code","configuration","config_construction.R"))
  
  # 3. Model run
  source(file.path("code","model", paste0("00_RunModel_", project_phase, ".R")))} 