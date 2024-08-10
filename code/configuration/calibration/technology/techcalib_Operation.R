##############
# TECH CALIB #
##############

# 1. Load cost data with full information and then work out a final vector aggregated by sector

Cost_data <- read.csv(file.path("data_input", "tech_calib", project_phase, paste(scenario_label,"_", scenario_SA,".csv", sep="")))

#2. Obtain Ac_PP

Scenario_PP <- Cost_data %>% group_by(iRowAgg) %>% 
  summarise("EUR" = sum(EUR.tsteel_2019)*Mton_baseline)

# 3. Load green hydrogen coefficients
REGH2 <- read.csv("data_input/tech_calib/Operation/REGH2.csv")
 



