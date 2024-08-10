
#1) Obtain column of coefficients for steel (A, VAc)
# Adjust coefficients
Scenario_IO <- Scenario_PP %>% left_join(UBP_FIGIO)
Scenario_IO$UBP_FIGIO[Scenario_IO$iRowAgg == "GH2"]<- Scenario_IO$UBP_FIGIO[Scenario_IO$iRowAgg == "C20"]
Scenario_IO$UBP_FIGIO[Scenario_IO$iRowAgg == "H"]<- 1

# FROM PURCHASING PRICES TO IO ALLOCATION
Scenario_IO <- Scenario_IO  %>% mutate("Scenario_IO" = EUR*UBP_FIGIO)
Scenario_G <- data.frame("iRowAgg" = "G",
                    "Scenario_IO" = sum(Scenario_IO$EUR) - sum(Scenario_IO$Scenario_IO))
Scenario_IO <- Scenario_IO %>%  full_join(Scenario_G)

# SHOCK VECTOR
Shock <- Scenario_IO %>% full_join(BFBOF_IO)
Shock$Scenario_IO[Shock$iRowAgg == "C26T33"] <- Shock$BFBOF_IO[Shock$iRowAgg == "C26T33"]

Shock <- Shock %>%  full_join(Column_Build_Final)

Shock$BFBOF_IO[is.na(Shock$BFBOF_IO)] <- 0  
Shock$Scenario_IO[is.na(Shock$Scenario_IO)] <- 0  
Shock$Val_Final[Shock$iRowAgg == "C24_TS"] <-  Shock$Val_Final[Shock$iRowAgg == "C24"] 

Shock <- Shock %>% filter(iRowAgg != "C24") %>% 
  mutate("Val_Shock" = Val_Final - BFBOF_IO + Scenario_IO)

X_Shock <- sum(Shock$Val_Shock)+VA_baseline["NL_C24_TS"]

Shock <- Shock %>% mutate("Ac_IO_Shock" = Val_Shock/X_Shock)

# FROM IO TO MRIO
Shock <- Shock %>%  select(iRowAgg, Val_Shock, Ac_IO_Shock)
Shock$iRowAgg[Shock$iRowAgg == "C24_TS"] <- "C24" 

Shock_MRIO <- left_join(impshare, Shock)
Shock_MRIO <- Shock_MRIO %>% mutate("Ac_MRIO_Shock" = impc*Ac_IO_Shock)

# Prepare to merge to A
Shock_MRIO <- Shock_MRIO %>%  select(icioiRowAgg, Ac_MRIO_Shock) %>% drop_na()
Shock_MRIO$icioiRowAgg[Shock_MRIO$icioiRowAgg == "NL_C24"]<- "NL_C24_TS"
Shock_MRIO <- left_join(baseline_IO, Shock_MRIO)
Shock_MRIO$Ac_MRIO_Shock[Shock_MRIO$icioiRowAgg == "NL_C24_Other"] <- 0

# 2) Define new matrixes
A_Scenario <- A_baseline
A_Scenario [,"NL_C24_TS"] <- Shock_MRIO$Ac_MRIO_Shock[1:n]
A_Scenario [, "BR_RE"] <- REGH2$BR_RE[1:n]
A_Scenario [, "BR_GH2"] <- REGH2$BR_GH2[1:n] 
  
irow <- matrix(data = 1, nrow = 1, ncol=n)
VAc_Scenario <- irow - colSums(A_Scenario)
colnames(VAc_Scenario) = colnames(A_Scenario)
VAc_Scenario[,"BR_GS"] <- NA

F_Scenario <- F_baseline

CostperTon_Scenario <- X_Shock/Mton_baseline
