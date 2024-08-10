rm(list = ls())
source("code/packages.R")

#############
# LOAD DATA #
#############
IO <- read.csv("datasets/FIGAROE/matrix_FIGARO-e-ic-io-D_ind-by-ind_millionEUR_2015.csv")
IO_pxp <- read.csv("datasets/FIGAROE/matrix_FIGARO-e-ic-io-B_prod-by-prod_millionEUR_2015.csv")
use <- read.csv("datasets/FIGAROE/flatfile_FIGARO-e-ic-use_millionEUR_2015.csv")
cbridge <- read.xlsx("bridge_tables/bridge_cpa_from_BC3_to_FIDELIO4_H2PROJECT.xlsx")
cbridge <- gather(cbridge, "cagg", "value", CPA_A01:W2_OP_RES, na.rm = TRUE)

use <- use %>%  
    left_join(cbridge, by = "rowPi")

use_C24_A <- use %>%  filter(icuseCol == "BR_C24_A" | icuseCol == "NL_C24_A") %>%  
  group_by(cagg, icuseCol) %>% 
  summarise(obsValue = sum(obsValue))

use_C24_A <- use_C24_A %>% pivot_wider(names_from = icuseCol, values_from = obsValue)

x_BR_C24_A = sum(use_C24_A$BR_C24_A)
x_NL_C24_A = sum(use_C24_A$NL_C24_A)
write.xlsx(use_C24_A, "C24_A_BR_NL.xlsx")

##
BR_C24 <- data.frame(IO[,1],IO[, grepl("BR_C24", substr(names(IO),1,6))]) 
NL_C24 <- data.frame(IO[,1],IO[, grepl("NL_C24", substr(names(IO),1,6))]) 
X_BR_C24 <- colSums(BR_C24[,-1])
X_NL_C24 <- colSums(NL_C24[,-1])

X_BR_C24[1]/sum(X_BR_C24)              
X_NL_C24[1]/sum(X_NL_C24)    
X_NL_C24/sum(X_NL_C24)
X_BR_C24/sum(X_BR_C24)
 
BR_C24 <- BR_C24 %>% mutate("C24_A_c"= BR_C24$BR_C24_A/X_BR_C24[1])
NL_C24 <- NL_C24 %>% mutate("C24_A_c"= NL_C24$NL_C24_A/X_NL_C24[1])

##
BR_C24_pxp <- data.frame(IO_pxp[,1],IO_pxp[, grepl("BR_CPA_C24", substr(names(IO_pxp),1,10))]) 
NL_C24_pxp <- data.frame(IO_pxp[,1],IO_pxp[, grepl("NL_CPA_C24", substr(names(IO_pxp),1,10))]) 
X_BR_C24_pxp <- colSums(BR_C24_pxp[,-1])
X_NL_C24_pxp <- colSums(NL_C24_pxp[,-1])

X_BR_C24[1]/sum(X_BR_C24)              
X_NL_C24[1]/sum(X_NL_C24)    
X_NL_C24/sum(X_NL_C24)
X_BR_C24/sum(X_BR_C24)

BR_C24 <- BR_C24 %>% mutate("C24_A_c"= BR_C24$BR_C24_A/X_BR_C24[1])
NL_C24 <- NL_C24 %>% mutate("C24_A_c"= NL_C24$NL_C24_A/X_NL_C24[1])
