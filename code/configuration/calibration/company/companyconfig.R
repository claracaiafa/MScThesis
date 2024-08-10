### Company data ###

Mton <- 6.7 # Million tonnes of crude steel production (World Steel Association, 2020)
NetTurnover1819 <- 4484 # million euros in 2018-2019 FY (Tata Steel, 2020)
NetTurnover1920 <- 3925 # million euros in 2019-2020 FY (Tata Steel, 2020)
XperTon_TS <- mean(NetTurnover1819,NetTurnover1920)/Mton

# All data below comes from Tata Steel (2020), for FY18-19 and FY19-20
Profit1819 <- 242
Profit1920 <- -105
Employment1819 <- 759
Employment1920 <- 754

OperatingCosts1819 <- 4188 # total, million euros
RawMaterials1819 <- 2087
Maintenance1819 <- 323
ExternalCharges1819 <- 529
OtherOpItems1819 <- 351

OperatingCosts1920 <- 4041 # total, million euros
RawMaterials1920 <- 2009
Maintenance1920 <- 306
ExternalCharges1920 <- 461
OtherOpItems1920 <- 325

RMc1819 <- RawMaterials1819/OperatingCosts1819
RMc1920 <- RawMaterials1920/OperatingCosts1920

Maintc1819 <- Maintenance1819/OperatingCosts1819
Maintc1920 <- Maintenance1920/OperatingCosts1920

OtherZc1819 <- (ExternalCharges1819+OtherOpItems1819)/OperatingCosts1819
OtherZc1920 <- (ExternalCharges1920+OtherOpItems1920)/OperatingCosts1920

CompanyCoeff <- data.frame("CostItem" = c("RawMaterials", "Maintenance", 
                                          "OtherIntermediaries", "TotalIntermediaries"),
                           "CompanyCoeff2018-2019" = c(RMc1819, Maintc1819, 
                                                       OtherZc1819, RMc1819+Maintc1819+OtherZc1819),
                           "CompanyCoeff2019-2020" = c(RMc1920, Maintc1920, 
                                                       OtherZc1920, RMc1920+Maintc1920+OtherZc1920))
CostperTon_TS <- mean(c(OperatingCosts1819,OperatingCosts1920))/Mton
RMperTon_TS <- mean(c(RawMaterials1819, RawMaterials1920))/Mton
MeanTurnover <- mean(c(NetTurnover1819,NetTurnover1920))
IntmPurch <- mean(c(RMc1819+Maintc1819+OtherZc1819,RMc1920+Maintc1920+OtherZc1920))* MeanTurnover
VAc_TS <- 1- mean(c(RMc1819+Maintc1819+OtherZc1819,RMc1920+Maintc1920+OtherZc1920))
