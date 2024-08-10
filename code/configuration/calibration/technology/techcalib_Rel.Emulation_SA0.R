##############
# TECH CALIB #
##############

# 1. Load cost data with full information and then work out a final vector aggregated by sector

Imp_cost_data <- read.csv(file.path("data_input", "tech_calib", paste(scenario_label,"_", scenario_SA,".csv", sep="")))

#2. Obtain Ac_PP

Scenario_PP <- Imp_cost_data %>% group_by(iRowAgg) %>% 
  summarise("EUR" = sum(EUR.tsteel_2019)*Mton_baseline)

# 3. Load green hydrogen coefficients
REGH2 <- read.csv("data_input/tech_calib/REGH2.csv")
 



