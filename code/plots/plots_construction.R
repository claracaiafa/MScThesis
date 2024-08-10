#################################################
#               VISUALIZATION                   #
#################################################

data <- data_construction

### 1. Define colour palettes ####

Palette <- c("#4DAF4A", "#E41A1C", "#FF7F00","#377EB8", "#FFFF33", "#984EA3", "#11A579")
Palette2 <- c("chartreuse4","brown3", "darkorange", "blue4", "yellow", "cornflowerblue")

longPalette <- c("#80BA5A", "#f97b72", "#DDCC77", "#117733", "#332288",
                 "#AA4499", "#44AA99", "#999933","#6699CC", "#882255",
                 "#661100", "#88CCEE","#CC6677",
                 "#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", 
                 "#008695", "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99")


loadfonts()

labels <- read.csv("~/MSc Economics/Thesis/visualization/sectorlabels.csv")
labels <- labels %>% mutate("SectorLabel" = paste(iRow, SectorName, sep = ": "))

data <- data %>% full_join(labels)

techlabels <- data.frame("Technology" = c("RE", "GH2", "GS"),
                         "Plant"= c("Renewable Electricity", "Green hydrogen", "Green steel"))

data <- data %>% full_join(techlabels)

#### 2. Create folder #####labels### 2. Create folder ####

date <- Sys.Date()
date <- format(date, format = "%Y-%m-%d")

visualization_folder <- paste(date)

if(!file.exists(visualization_folder)){ 
  dir.create(file.path("visualization", visualization_folder))} 

visualization_path <- paste("~/MSc Economics/Thesis/visualization/", visualization_folder, sep = "")


##### 3. Results by sector and region ####

Table.Region <- data %>% 
  group_by(Region, Scenario, SA) %>% 
  mutate("X_D_Region_Rel" = (X_D_Region)/X_Baseline_Region) %>% 
  mutate("VA_D_Region_Rel" = (VA_D_Region)/VA_Baseline_Region) 

Table.Region <- Table.Region %>% 
  select(Region, Scenario, SA, X_D_Region, X_D_Region_Rel, VA_D_Region, VA_D_Region_Rel) %>% 
  unique()

write.xlsx(Table.Region, file = paste0(visualization_path, "/Construction_Table.Region.xlsx"))

data %>% 
  ggplot()+
  geom_col(aes(x=Region, y= (X_D_Abs), fill = SectorLabel), position = "stack")+
  geom_hline(yintercept = 0)+
  labs(title = "",
       y = "Absolute change from 2019 (in millions of EUR/Mt of green steel)",
       x= "Region",
       caption = "")+
  facet_wrap(Scenario ~ ., ncol = 4)+
  coord_flip()+
  scale_fill_manual(values = longPalette, labels = function(x) str_wrap(x, width = 35))+
  guides(fill=guide_legend(ncol=3,byrow=FALSE))+
  theme(
    text = element_text(family = "CM Roman", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold",  family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, family = "CM Roman"),
    plot.caption = element_text(hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 14, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14, family = "CM Roman"),
    strip.text = element_text(face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 14, family = "CM Roman"),
    legend.position = "bottom"
  )

data %>% 
  ggplot()+
  geom_col(aes(x=Plant, y= (X_D_Abs), fill = Region), position = "stack")+
  geom_hline(yintercept = 0)+
  labs(title = "",
       y = "Absolute change from 2019 (EUR mi/Mts)",
       x= "Technology",
       caption = "")+
  facet_wrap(Scenario ~ ., ncol = 4)+
  coord_flip()+
  scale_fill_manual(values = longPalette)+
  guides(fill=guide_legend(nrow=1,byrow=FALSE))+
  theme(
    text = element_text(family = "CM Roman", size = 15),
    plot.subtitle = element_text(hjust = 0.5, family = "CM Roman"),
    plot.caption = element_text(hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, family = "CM Roman"),
    strip.text = element_text(face = "bold", family = "CM Roman"),
    legend.text = element_text(family = "CM Roman"),
    legend.position = "top"
  )

####### figure on quantity of Mt

OPMT <- data_operation %>% 
  select(Scenario, SA, Mt, PriceTon,icioiRowAgg, X_Scenario, X_Direct, X_Indirect, VA_Baseline, VA_Scenario) %>%
  filter(icioiRowAgg == "BR_GS" | icioiRowAgg == "NL_C24_TS")

OPMT$VA_Scenario[is.na(OPMT$VA_Scenario)] <- 0

VADisc <- read.csv("~/MSc Economics/Thesis/visualization/2024-08-01/constructionVA_discounted.csv")
VADisc <- VADisc %>% mutate("Scenario_label" = paste(Scenario, OpSA, ConSA, sep="."))


a <- VADisc %>% 
  ggplot()+
  geom_col(aes(x=Scenario_label, y= TotalVADisc/10^3, fill = Scenario), position = "stack")+
  geom_text(aes(x=Scenario_label, y= (TotalVADisc/10^3)+10, label = paste(round(TotalVADisc/10^3))), position = "stack")+ 
  labs(title = "a) Total impact on value added until 2050, discounted",
       y = "Billion EUR",
       x= "Scenario",
       caption = "")+
  scale_fill_manual(values = longPalette)+
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, by = 20))+
  theme(
    text = element_text(family = "CM Roman", size = 12),
    plot.title = element_text(hjust = 0, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 12, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 12, family = "CM Roman"),
    legend.position = "none"
  )


a

b <- VADisc %>% 
  ggplot()+
  geom_col(aes(x=Scenario_label, y= MtTotal, fill = Scenario), position = "stack")+
  geom_text(aes(x=Scenario_label, y= MtTotal+50, label = paste(MtTotal)), position = "stack")+
  labs(title = "b) Mt of green steel capacity (H-DRI-EAF)",
       y = "Total Mt",
       x= "Scenario",
       caption = "")+
  scale_fill_manual(values = longPalette)+
    theme(
    text = element_text(family = "CM Roman", size = 12),
    plot.title = element_text(hjust = 0, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 11, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "none"
  )


gridExtra::grid.arrange(a,b, ncol=1)
install.packages("patchwork")
library(patchwork)


VADisc %>% 
  ggplot()+
  geom_col(aes(x=Scenario_label, y= TotalVADisc/10^3, fill = Scenario), position = "stack")+
  geom_text(aes(x=Scenario_label, y= (TotalVADisc/10^3)+5, label = paste(MtTotal)), position = "stack")+
  geom_hline(yintercept = 0)+
  labs(title = "Total impact on value added and total Mts of steel until 2050",
       y = "Billion EUR",
       x= "Scenario",
       caption = "")+
  scale_fill_manual(values = longPalette)+
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, by = 20))+
  theme(
    text = element_text(family = "CM Roman", size = 12),
    plot.title = element_text(hjust = 0, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 12, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 12, family = "CM Roman"),
    legend.position = "none"
  )

