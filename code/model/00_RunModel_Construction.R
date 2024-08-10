########################
#     RUN MODEL        #
########################


F <- matrix(data = NA, nrow = n, ncol = 1)


for (c in 1:length(scenario_label)) {  

assign("F_Scenario", data.frame(F_list[c]))

 for (t in 2:(length(technology)+1)) {
f <- F_Scenario[,t]
f <- as.matrix(f)

X_Scenario = L_baseline %*% f
X_Direct = A_baseline %*% f

Results_Tech_Scenario = data.frame("ProjectPhase" = project_phase,
                              "Scenario" = scenario_label[c], 
                              "SA" = scenario_SA,
                              "icioiRowAgg" = rownames(X_Scenario),
                              "Technology" = paste(colnames(F_Scenario[t])),
                              "X_Baseline" = X_baseline,
                              "X_D_Abs"= X_Scenario,
                              "X_D_Direct" = X_Direct,
                              "X_D_Indirect" = X_Scenario - X_Direct,
                              "VA_Baseline" = VA_baseline,
                              "VAc_Baseline"= VAc_baseline,
                              "VA_D_Abs" = VAc_baseline*X_Scenario
                              )
  
assign(paste("results", colnames(F_Scenario[t]), sep = "."), Results_Tech_Scenario)

 }

df_list <- list(results.GH2, results.GS, results.RE)

Results_Scenario <- df_list %>%  reduce(full_join)

assign(paste("results", scenario_label[c], sep = "."), Results_Scenario)

}

df_list <- list(results.BR,results.BRImpShare, results.NL, results.NLImpShare, results.CNIN, results.OtherEU, results.RoW, results.USMCA)

Results_Scenario <- df_list %>% reduce(full_join)


Results_Scenario <- Results_Scenario %>% mutate("X_D_Rel" = X_D_Abs/X_Baseline)
Results_Scenario <- Results_Scenario %>% mutate("VA_D_Rel" =VA_D_Abs/VA_Baseline)

Results_Scenario <- Results_Scenario %>%  left_join(SecRegList)

Results_Scenario <- Results_Scenario %>% group_by(Region, Scenario) %>% mutate("X_Baseline_Region" = sum(X_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(Region, Scenario) %>% mutate("X_D_Region" = sum(X_D_Abs, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(Region, Scenario) %>% mutate("VA_Baseline_Region" = sum(VA_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(Region, Scenario) %>% mutate("VA_D_Region" = sum(VA_D_Abs, na.rm = TRUE))

Results_Scenario <- Results_Scenario %>% group_by(iRow, Scenario) %>% mutate("X_Baseline_iRow" = sum(X_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(iRow, Scenario) %>% mutate("X_D_iRow" = sum(X_D_Abs, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(iRow, Scenario) %>% mutate("VA_Baseline_iRow" = sum(VA_Baseline, na.rm = TRUE))
Results_Scenario <- Results_Scenario %>% group_by(iRow, Scenario) %>% mutate("VA_D_iRow" = sum(VA_D_Abs, na.rm = TRUE))

Results_Scenario <- Results_Scenario %>% ungroup()

# 3) Save results
saveRDS(Results_Scenario, file= file.path("results", paste0(project_phase, "_", scenario_SA,".rds")))
