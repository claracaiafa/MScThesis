##############
# TECH CALIB #
##############

# 1. Load cost data with full information and then work out a final vector aggregated by sector

BFBOF_cost_data <- read.csv(file.path("data_input", "tech_calib", paste(baseline,"_", baseline_SA,".csv", sep="")))

# 2. Check with company data

BFBOF_cost_data %>% filter(Self.produced == "No") %>%
  summarise(sum(EUR.tsteel_2019))

BFBOF_cost_data %>% filter(Self.produced == "No") %>%
  summarise(sum(Ac_PP))

## NOTE: apparently no risk of overestimating costs of raw materials

# 3. Obtain Ac in PP
BFBOF_Ac_PP <- BFBOF_cost_data %>% select(iRowAgg, Ac_PP)
Maintenance_Ac_PP <- data.frame("iRowAgg"= "C26T33",
                                "Ac_PP"= mean(Maintc1819,Maintc1920))
BFBOF_Ac_PP <- BFBOF_Ac_PP %>% full_join(Maintenance_Ac_PP)
sum(BFBOF_Ac_PP$Ac_PP)

  