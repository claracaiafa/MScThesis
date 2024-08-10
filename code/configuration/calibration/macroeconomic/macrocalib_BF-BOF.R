###############
# MACRO CALIB #
###############

# 1. Load the IO table, extended, but empty. Load vector of NL_C24 too, separately
Z <- read.csv(file.path("data_input", "macro_calib", baseline, "Z.csv"))
VA <- read.csv(file.path("data_input", "macro_calib", baseline, "VA.csv"))
F <- read.csv(file.path("data_input", "macro_calib", baseline, "F.csv"))
IO <- read.csv(file.path("data_input", "macro_calib", baseline, "IO.csv"))
impshare <-  read.csv(file.path("data_input", "macro_calib","FIGARO_2019", paste("NL_C24_imp_shares_", baseline,".csv", sep="" )))
SecRegList <- read.csv(file.path("data_input", "macro_calib","SectorRegionList.csv"))

NL_C24_col <- read.csv(file.path("data_input", "macro_calib","FIGARO_2019", "NL_C24_col.csv"))
NL_C24_row <- read.csv(file.path("data_input", "macro_calib","FIGARO_2019", "NL_C24_row.csv"))
sum(NL_C24_row$NL_C24, na.rm = TRUE)
sum(NL_C24_col$NL_C24)

ProdSectors <- Z[,1]
VASectors <- VA[,1]
AllSectors <- IO[,1]
FSectors <- colnames(F[-1])
n <- length(ProdSectors)

# 2. Define import coefficients. (vector)

impshare <- impshare %>% 
  pivot_longer(cols = 2:8, names_to = "region", values_to = "impc")

impshare <- impshare %>% mutate("icioiRowAgg" = paste(region,iRowAgg, sep = "_"))
impshare %>%  group_by(iRowAgg) %>% summarise(sum(impc))

iRowSectors <- unique(impshare$iRowAgg)
Regions <- unique(impshare$region)
iRowSectorsVA <- c("B2A3G", "D1", "D21X31" , "D29X39","OP_NRES","OP_RES" )
iRowSectorsZ <- iRowSectors[!(iRowSectors%in%iRowSectorsVA)]
  
# 3. From Use BP to FIGARO IO
# load transformation table from Use in PURCHASERS Prices to FIGARO IO coefficients
UBP_FIGIO <-  read.csv(file.path("data_input", "macro_calib","FIGARO_2019", "UBP_FIGIO.csv"))

# Transform coefficients from tech calib to make it coherent with FIGARO IO
BFBOF_Ac_IO <- BFBOF_Ac_PP %>% 
  group_by(iRowAgg) %>% 
  summarise("Ac_PP" = sum(Ac_PP))

BFBOF_Ac_IO <- left_join(BFBOF_Ac_IO,UBP_FIGIO)
BFBOF_Ac_IO <- BFBOF_Ac_IO %>% mutate("Ac_IO"= Ac_PP*UBP_FIGIO)

# Add the "left overs" to sector G
Ac_IO_G <- data.frame("iRowAgg" = "G",
                      "Ac_IO" = sum(BFBOF_Ac_IO$Ac_PP)-sum(BFBOF_Ac_IO$Ac_IO))
BFBOF_Ac_IO <- BFBOF_Ac_IO %>%  select(iRowAgg, Ac_IO)
BFBOF_Ac_IO <- BFBOF_Ac_IO %>% full_join(Ac_IO_G)

# 4. Obtain Columns for C24_TS and C24_Other
Column_Build <- NL_C24_col %>% group_by(iRowAgg) %>% 
  mutate(NL_C24_Agg = sum(NL_C24)) %>% 
  mutate(NL_C24_Ac_Agg = sum(NL_C24_Ac)) %>% 
  select(iRowAgg, NL_C24_Ac_Agg, NL_C24_Agg) %>% 
  unique()

Column_Build <- Column_Build %>% 
  filter(iRowAgg %in% iRowSectorsZ)

C24_TS_share <- MeanTurnover/sum(NL_C24_col$NL_C24)

Column_Build <- Column_Build %>% 
  mutate("Value_Share" = NL_C24_Agg*C24_TS_share)

Column_Build <- Column_Build %>% left_join(BFBOF_Ac_IO)

Column_Build$Ac_IO[Column_Build$iRowAgg == "C24"] <- BFBOF_Ac_IO$Ac_IO[BFBOF_Ac_IO$iRowAgg == "C24_TS"]

Column_Build <- Column_Build %>% 
  mutate("MeanTurnover" = MeanTurnover)

Column_Build <- Column_Build %>% 
  mutate("Val_MF" = Ac_IO*MeanTurnover)

Column_Build_Est <- Column_Build %>% filter(!is.na(Val_MF))
Column_Build_Left <- Column_Build %>% filter(is.na(Val_MF))

Column_Build_Left <- Column_Build_Left %>% 
  ungroup() %>% 
  mutate("Total_Left" = sum(NL_C24_Agg))

Column_Build_Left <- Column_Build_Left %>% 
  mutate("Ac_Val_Share_Left" = NL_C24_Agg/sum(NL_C24_Agg))

Val_Est_Total <- sum(Column_Build_Est$Val_MF)

Val_Left_Total <- IntmPurch - Val_Est_Total

Column_Build_Left <- Column_Build_Left %>% 
  mutate("Val_Share_Left" = Ac_Val_Share_Left*Val_Left_Total)

Column_Build_Final <- data.frame("iRowAgg"= Column_Build$iRowAgg,
                                 Val_Final = NA)
  
Column_Build_Final$Val_Final[Column_Build_Final$iRowAgg %in% Column_Build_Est$iRowAgg] <- Column_Build_Est$Val_MF
Column_Build_Final$Val_Final[Column_Build_Final$iRowAgg %in% Column_Build_Left$iRowAgg] <- Column_Build_Left$Val_Share_Left

sum(Column_Build_Final$Val_Final) == IntmPurch

Column_Build_Final <- Column_Build_Final %>% 
  mutate("Ac_IO_Final" = Val_Final/MeanTurnover) 

NL_C24_TS_VAc <- 1-sum(Column_Build_Final$Ac_IO_Final)

# 5. Go from single region coefficients to multirregional using import shares
Ac_MRIO <- left_join(impshare, Column_Build_Final)
Ac_MRIO <- Ac_MRIO %>% mutate("Ac_MRIO" = impc*Ac_IO_Final)
Ac_MRIO <- drop_na(Ac_MRIO)

round(sum(Column_Build_Final$Ac_IO_Final), digits = 6) == round(sum(Ac_MRIO$Ac_MRIO), digits = 6)

Ac_MRIO <- Ac_MRIO %>% 
  mutate("NL_C24_TS" = Ac_MRIO*MeanTurnover)

# 6. Obtain column for TS and Other

NL_C24_TS_col <- Ac_MRIO %>% select(icioiRowAgg, NL_C24_TS) 

NL_C24_col<- NL_C24_col %>% left_join(NL_C24_TS_col)

NL_C24_col$NL_C24_TS[NL_C24_col$iRowAgg == "D1"] <- mean(c(Employment1819, Employment1920))
NL_C24_col$NL_C24_TS[NL_C24_col$iRowAgg == "D21X31"] <- C24_TS_share * NL_C24_col$NL_C24[NL_C24_col$iRowAgg == "D21X31"]
NL_C24_col$NL_C24_TS[NL_C24_col$iRowAgg == "D29X39"] <- C24_TS_share * NL_C24_col$NL_C24[NL_C24_col$iRowAgg == "D29X39"]
NL_C24_col$NL_C24_TS[NL_C24_col$iRowAgg == "OP_RES"] <- C24_TS_share * NL_C24_col$NL_C24[NL_C24_col$iRowAgg == "OP_RES"]
NL_C24_col$NL_C24_TS[NL_C24_col$iRowAgg == "OP_NRES"] <- C24_TS_share * NL_C24_col$NL_C24[NL_C24_col$iRowAgg == "OP_NRES"]
NL_C24_col$NL_C24_TS[NL_C24_col$iRowAgg == "B2A3G"] <- NL_C24_TS_VAc*MeanTurnover - sum(NL_C24_col$NL_C24_TS[NL_C24_col$region == "W2"], na.rm = TRUE)

round(sum(NL_C24_col$NL_C24_TS)- MeanTurnover, digits = 6)

NL_C24_col <- NL_C24_col %>% mutate("NL_C24_Other" = NL_C24 - NL_C24_TS)

# 7. Obtain rows for TS and Other
# need to remove the own consumption and then recalculate coefficients
x_NL_C24 <- sum(NL_C24_row$NL_C24, na.rm= TRUE)
x_NL_C24_TS <- sum(NL_C24_col$NL_C24_TS)
x_NL_C24_Other <- sum(NL_C24_col$NL_C24_Other)

OwnPurch_TS <- NL_C24_col$NL_C24_TS[NL_C24_col$icioiRowAgg == "NL_C24"]
OwnPurch_Other <- NL_C24_col$NL_C24_Other[NL_C24_col$icioiRowAgg == "NL_C24"]
OwnPurch_Total <- OwnPurch_TS + OwnPurch_Other

x_NL_C24_ExOwnPur <- x_NL_C24 - OwnPurch_Total
x_NL_C24_TS_ExOwnPur <- x_NL_C24_TS - OwnPurch_TS
x_NL_C24_Other_ExOwnPur <- x_NL_C24_Other - OwnPurch_Other

# define row coefficients excluding own purchases

NL_C24_row <- NL_C24_row %>% mutate("Ac_ExOwnPur" = NL_C24/x_NL_C24_ExOwnPur) %>% 
  mutate("NL_C24_TS" = Ac_ExOwnPur*x_NL_C24_TS_ExOwnPur) %>% 
  mutate("NL_C24_Other" = Ac_ExOwnPur*x_NL_C24_Other_ExOwnPur)

NL_C24_row$NL_C24_TS[NL_C24_row$icioiRowAgg == "NL_C24_TS"] <- OwnPurch_TS
NL_C24_row$NL_C24_TS[NL_C24_row$icioiRowAgg == "NL_C24_Other"] <- 0
NL_C24_row$NL_C24_Other[NL_C24_row$icioiRowAgg == "NL_C24_TS"] <-0
NL_C24_row$NL_C24_Other[NL_C24_row$icioiRowAgg == "NL_C24_Other"] <- OwnPurch_Other


# 8. Insert in the IO table
NL_C24_Disag <- NL_C24_col %>% select(icioiRowAgg, NL_C24_TS, NL_C24_Other)

Insert_Col <- data.frame("icioiRowAgg" = AllSectors)
Insert_Col <- Insert_Col %>% left_join(NL_C24_Disag)

IO$NL_C24_TS <- 0
IO$NL_C24_Other <- 0
IO[IO$icioiRowAgg == "NL_C24_TS", -1]<- 0
IO[IO$icioiRowAgg == "NL_C24_Other", -1]<- 0

IO$NL_C24_TS <- Insert_Col$NL_C24_TS
IO$NL_C24_Other <- Insert_Col$NL_C24_Other

Insert_Row <- NL_C24_row %>% select(icioiRowAgg, NL_C24_TS, NL_C24_Other)

IO[IO$icioiRowAgg == "NL_C24_TS",-1 ]<- t(Insert_Row$NL_C24_TS)
IO[IO$icioiRowAgg == "NL_C24_Other",-1 ]<- t(Insert_Row$NL_C24_Other)

# Check that output from productive sectors matches from both column and row sums
X_Col <- colSums(IO[,2:(n+1)])
X_Row <- rowSums(IO[1:n,-1])

sum(round(X_Col - X_Row, digits = 6) == 0)==n


# 9. Define Baseline Values
X_baseline <- X_Col
VA_baseline <- colSums(IO[IO$icioiRowAgg %in% VASectors, 2:(n+1)])
VAc_baseline <- VA_baseline/X_baseline

Z_baseline <- IO %>% filter(icioiRowAgg %in% ProdSectors)
Z_baseline <- Z_baseline %>% select(2:(n+1))

Z_baseline <- data.matrix(Z_baseline)
X_baseline <- data.matrix(X_baseline)
X_baseline <- as.vector(X_baseline)

A_baseline = Z_baseline %*% diag(1/X_baseline)
colnames(A_baseline) = colnames(IO[2:(n+1)])
rownames(A_baseline) = IO$icioiRowAgg[1:n]
A_baseline[,"BR_RE"] <- 0
A_baseline[,"BR_GH2"] <- 0
A_baseline[,"BR_GS"] <- 0

F_baseline <- IO [IO$icioiRowAgg %in% ProdSectors,FSectors]
F_baseline <- rowSums(F_baseline)
F_baseline <- data.matrix(F_baseline)
F_baseline <- as.vector(F_baseline)

Mton_baseline <- x_NL_C24_TS/XperTon_TS
(XperTon_TS - CostperTon_TS)*Mton_baseline

baseline_IO <- IO %>% select(icioiRowAgg, NL_C24_TS)
baseline_IO <- baseline_IO %>%  left_join(SecRegList)

I <- diag(n)
L_baseline = solve(I-A_baseline)

Multipliers_baseline <- colSums(L_baseline)

BFBOF_IO <- BFBOF_Ac_IO %>% mutate("BFBOF_IO" = Ac_IO*x_NL_C24_TS)
