##########################
# IO TABLES IN 5 REGIONS #
##########################

rm(list = ls())
source("code/packages.R")


#############
# LOAD DATA #
#############
#IO <- read.csv("datasets/FIGAROE/matrix_FIGARO-e-ic-io-D_ind-by-ind_millionEUR_2015.csv")

#IO_flat <- IO %>% pivot_longer(cols = 2:8327, names_to = "icioiCol")
#IO_flat <- IO_flat %>%  
 # mutate(refArea = paste(substr(IO_flat$icioiRow, 1,2))) %>% 
  #mutate(counterpartArea = paste(substr(IO_flat$icioiCol,1,2))) %>% 
  #mutate(rowPi = paste(substr(IO_flat$icioiRow, 4,nchar(IO_flat$icioiRow)))) %>% 
  #mutate(colPi = paste(substr(IO_flat$icioiCol, 4,nchar(IO_flat$icioiCol)))) 


IO_flat <- read.csv("datasets/FIGAROE/flatfile_FIGARO-e-ic-io-D_ind-by-ind_millionEUR_2015.csv")
ibc <- read.xlsx("bridge_tables/bridge_from_BC3_industries_col.xlsx")
ibc <- gather(ibc, "colIiAgg", "value", A:U, na.rm = TRUE)

ibr <- read.xlsx("bridge_tables/bridge_from_BC3_industries_row.xlsx")
ibr <- gather(ibr, "rowIiAgg", "value", A:OP_RES, na.rm = TRUE)

ibc <- ibc %>%  select(colIi, colIiAgg)
ibr <- ibr %>%  select(rowIi, rowIiAgg)

regb <- read.xlsx("bridge_tables/bridge_regions.xlsx")


#############
# AGGREGATE #
#############

# Add new columns
IO_flat_agg <- IO_flat %>% 
  left_join(regb, by = c("refArea", "counterpartArea")) %>% 
  left_join(ibc, by = "colIi") %>% 
  left_join (ibr, by = "rowIi")

IO_flat_agg %>% filter(is.na(IO_flat_agg$refAreaAgg)) %>% View()

# Prepare dataset to pivot wider
IO_flat_agg <- IO_flat_agg %>% 
  mutate("icioiRow" = paste(refAreaAgg, rowIiAgg, sep="_")) %>% 
  mutate("icioiCol" = paste(counterpartAreaAgg, colIiAgg, sep="_"))

# Group by new aggregation level and sum values
IO_flat_agg <- IO_flat_agg %>% 
  group_by(icioiRow,icioiCol) %>%
  summarise(sum(obsValue))

IO_flat_agg <- IO_flat_agg %>% 
  ungroup() %>% 
  select(icioiRow, icioiCol, `sum(obsValue)`)

# Pivot wider
IO_matrix_agg <- IO_flat_agg %>% pivot_wider(names_from =icioiCol, values_from =  `sum(obsValue)`)
