########################
#     RUN MODEL        #
########################


#1) find new output levels
I <- diag(n)
L_Scenario <- solve(I-A_Scenario)

X_Scenario = L_Scenario %*% F_Scenario

X_Direct = A_Scenario %*% F_Scenario

Mt_Scenario <- X_Scenario["BR_GS",1]/CostperTon_Scenario +X_Scenario["NL_C24_TS",1]/CostperTon_Scenario

#2) Construct data set for saving

Results_Scenario = data.frame("ProjectPhase" = project_phase,
                              "Scenario" = scenario_label, 
                              "SA" = scenario_SA,
                              "PriceTon" = CostperTon_Scenario,
                              "Mt" = Mt_Scenario,
                              "icioiRowAgg" = rownames(X_Scenario),
                              "X_Baseline" = X_baseline,
                              "X_Scenario"= X_Scenario,
                              "X_Direct" = X_Direct,
                              "X_Indirect" = X_Scenario - X_Direct,
                              "VA_Baseline" = VA_baseline,
                              "VAc_Scenario" = t(VAc_Scenario))

Results_Scenario <- Results_Scenario %>% mutate("VA_Scenario" = VAc_Scenario*X_Scenario)
Results_Scenario <- Results_Scenario %>% mutate("X_D_Abs" = X_Scenario-X_baseline)
Results_Scenario <- Results_Scenario %>% mutate("X_D_Rel" = X_D_Abs/X_baseline)
Results_Scenario <- Results_Scenario %>% mutate("VA_D_Abs" = VA_Scenario-VA_baseline)
Results_Scenario <- Results_Scenario %>% mutate("VA_D_Rel" =VA_D_Abs/VA_baseline)

Results_Scenario <- Results_Scenario %>%  left_join(SecRegList)


Results_Scenario <- Results_Scenario %>% group_by(Region) %>% mutate("X_Baseline_Region" = sum(X_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(Region) %>% mutate("X_Scenario_Region" = sum(X_Scenario, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(Region) %>% mutate("VA_Baseline_Region" = sum(VA_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(Region) %>% mutate("VA_Scenario_Region" = sum(VA_Scenario, na.rm = TRUE))

Results_Scenario <- Results_Scenario %>% group_by(iRow) %>% mutate("X_Baseline_iRow" = sum(X_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(iRow) %>% mutate("X_Scenario_iRow" = sum(X_Scenario, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(iRow) %>% mutate("VA_Baseline_iRow" = sum(VA_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(iRow) %>% mutate("VA_Scenario_iRow" = sum(VA_Scenario, na.rm = TRUE))

Results_Scenario <- Results_Scenario %>% ungroup()

# 3) Save results
saveRDS(Results_Scenario, file= file.path("results", paste0(scenario_label, "_", scenario_SA, ".rds")))
