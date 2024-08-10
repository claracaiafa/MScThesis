#################################################
#               VISUALIZATION                   #
#################################################

data <- data_operation

### 1. Define colour palettes ####

Palette <- c("#4DAF4A", "#E41A1C", "#FF7F00","#377EB8", "#FFFF33", "#984EA3")
Palette2 <- c("chartreuse4","brown3", "darkorange", "blue4", "yellow", "cornflowerblue")

longPalette <- c("#80BA5A", "#f97b72", "#DDCC77","#A5AA99" , "#332288",
                 "#AA4499", "#44AA99", "#999933","#6699CC", "#882255",
                 "#661100", "#88CCEE","#CC6677",
                 "#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", 
                 "#008695", "#CF1C90","#117733")


loadfonts()


labels <- read.csv("~/MSc Economics/Thesis/visualization/sectorlabels.csv")
labels <- labels %>% mutate("SectorLabel" = paste(iRow, SectorName, sep = ": "))

data <- data %>% full_join(labels)

#### 2. Create folder ####

date <- Sys.Date()
date <- format(date, format = "%Y-%m-%d")

visualization_folder <- paste(date)

if(!file.exists(visualization_folder)){ 
  dir.create(file.path("visualization", visualization_folder))} 

visualization_path <- paste("~/MSc Economics/Thesis/visualization/", visualization_folder, sep = "")

#### 3. Results by region ####
a <- data %>% 
ggplot()+
  geom_col(aes(x= Scenario, y = X_Scenario_Region - X_Baseline_Region, fill = Region), position = "stack")+
  geom_hline(yintercept = 0)+
  facet_wrap(SA ~.)+
  scale_fill_manual(values = Palette2)+
  labs(title = "Absolute change in output",
       y = " millions of EUR")+
  theme(text = element_text(size = 12, family = "CM Roman"),
        plot.title = element_text(hjust = 0.0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(colour = "lightgrey"),
        axis.text.x = element_text (angle = 45, vjust = 1, hjust= 1, size =12),
        axis.text.y = element_text (angle = 0, vjust = 0.5, hjust= 1, size = 12),
        strip.text = element_text(face= "bold"),
        legend.position = "right")

b <- data %>% 
  ggplot()+
  geom_col(aes(x= Region, y = (X_Scenario_Region - X_Baseline_Region)/X_Baseline_Region, fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Percentage change in output",
       y = "Percentage points")+
  facet_wrap(SA ~.)+
  scale_y_continuous(labels = scales::percent)+
  theme(text = element_text(size = 12, family = "CM Roman"),
        plot.title = element_text(hjust = 0.0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(colour = "lightgrey"),
        axis.text.x = element_text (angle = 45, vjust = 1, hjust= 1, size =12),
        axis.text.y = element_text (angle = 0, vjust = 0.5, hjust= 1, size = 12),
        strip.text = element_text(face= "bold"),
        legend.position = "right")

c <- data %>% 
  ggplot()+
  geom_col(aes(x= Scenario, y = VA_Scenario_Region - VA_Baseline_Region, fill = Region), position = "stack")+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = Palette2)+
  labs(title = "Absolute change in Value Added",
       y = " millions of EUR")+
  facet_wrap(SA ~.)+
  theme(text = element_text(size = 12, family = "CM Roman"),
        plot.title = element_text(hjust = 0.0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(colour = "lightgrey"),
        axis.text.x = element_text (angle = 45, vjust =1, hjust= 1, size =12),
        axis.text.y = element_text (angle = 0, vjust = 0.5, hjust= 1, size = 12),
        strip.text = element_text(face= "bold"),
        legend.position = "right")

d <- data %>% 
  ggplot()+
  geom_col(aes(x= Region, y = (VA_Scenario_Region - VA_Baseline_Region)/VA_Baseline_Region, fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Percentage change in Value Added",
       y = "Percentage points")+
  facet_wrap(SA ~.)+
  scale_y_continuous(labels = scales::percent)+
  theme(text = element_text(size = 12, family = "CM Roman"),
        plot.title = element_text(hjust = 0.0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(colour = "lightgrey"),
        axis.text.x = element_text (angle = 45, vjust = 1, hjust= 1, size =12),
        axis.text.y = element_text (angle = 0, vjust = 0.5, hjust= 1, size = 12),
        strip.text = element_text(face= "bold"),
        legend.position = "right")

gridExtra::grid.arrange(a,c,b,d, ncol = 2, nrow=2)

##### 4. Results by sector and region ####

# manually insert the relative changes for sectors where output/VA becomes zero (rel change NaN or Inf)
data$X_D_Rel[data$icioiRowAgg == "BR_RE"] <- data$X_D_Abs[data$icioiRowAgg == "BR_RE"]/ data$X_Baseline[data$icioiRowAgg == "BR_D"]
data$VA_D_Rel[data$icioiRowAgg == "BR_RE"] <- data$VA_D_Abs[data$icioiRowAgg == "BR_RE"]/ data$VA_Baseline[data$icioiRowAgg == "BR_D"]

data$X_D_Rel[data$icioiRowAgg == "BR_GH2"] <- data$X_D_Abs[data$icioiRowAgg == "BR_GH2"]/ data$X_Baseline[data$icioiRowAgg == "BR_C20"]
data$VA_D_Rel[data$icioiRowAgg == "BR_GH2"] <- data$VA_D_Abs[data$icioiRowAgg == "BR_GH2"]/ data$VA_Baseline[data$icioiRowAgg == "BR_C20"]

data$X_D_Rel[data$icioiRowAgg == "BR_GS"] <- data$X_D_Abs[data$icioiRowAgg == "BR_GS"]/ data$X_Baseline[data$icioiRowAgg == "BR_C24"]
data$VA_D_Rel[data$icioiRowAgg == "BR_GS"] <- data$VA_D_Abs[data$icioiRowAgg == "BR_GS"]/ data$VA_Baseline[data$icioiRowAgg == "BR_C24"]

data$VA_Scenario[is.na(data$VA_Scenario)] <- 0

data$VA_D_Rel[data$icioiRowAgg == "NL_C24_TS"] <- (data$VA_Scenario[data$icioiRowAgg == "NL_C24_TS"]- data$VA_Baseline[data$icioiRowAgg == "NL_C24_TS"])/ data$VA_Baseline[data$icioiRowAgg == "NL_C24_TS"]
data$VA_D_Abs[data$icioiRowAgg == "NL_C24_TS"] <- (data$VA_Scenario[data$icioiRowAgg == "NL_C24_TS"]- data$VA_Baseline[data$icioiRowAgg == "NL_C24_TS"])

## Output
data %>% 
  ggplot()+
  geom_col(aes(x=iRow, y= (X_Scenario - X_Baseline), fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "",
       y = "Absolute change from 2019 (in millions of EUR)",
       x= "Sector",
       caption = "")+
  facet_grid(SA ~ Region, scales = "free")+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 12, family = "CM Roman"),
    legend.position = "top"
  )

data %>% 
  ggplot()+
  geom_col(aes(x=iRow, y= (X_D_Rel), fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Relative impact on output by sector",
       y = "Relative change from 2019 (%)",
       x= "Sector",
       caption = "Note: for new industries (RE, GH2, and GS), relative change compared to the existing equivalent industry (D, C20, and C24, respectively)")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(SA ~ Region, scales = "free", ncol = 6)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "top"
  )


## Value added
data %>% 
  ggplot()+
  geom_col(aes(x=iRow, y= (VA_D_Abs), fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Absolute impact on value added by sector",
       y = "Absolute change from 2019 (in millions of EUR)",
       x= "Sector",
       caption = "Note different scales")+
  facet_wrap(Region ~ SA, scales = "free", ncol = 6)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.2, 0.2,0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "top"
  )

data %>% 
  ggplot()+
  geom_col(aes(x=iRow, y= (VA_D_Rel), fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "",
       y = "Relative change from 2019 (%)",
       x= "Sector",
       caption = "")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(Region ~ SA, scales = "free", ncol = 6)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 12, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 12, family = "CM Roman"),
    legend.position = "top"
  )


data %>% 
  ggplot()+
  geom_col(aes(x=iRow, y= (VA_Scenario_iRow - VA_Baseline_iRow), fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Absolute impact in value added by sector",
       y = "Absolute change from 2019",
       x= "Sector",
       caption = "")+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 12, family = "CM Roman"),
    legend.position = "top"
  )z

Table.Region <- data %>% 
  group_by(Region, Scenario, SA) %>% 
  mutate("X_D_Region" = X_Scenario_Region - X_Baseline_Region) %>% 
  mutate("X_D_Region_Rel" = (X_Scenario_Region - X_Baseline_Region)/X_Baseline_Region) %>% 
  mutate("VA_D_Region" = VA_Scenario_Region - VA_Baseline_Region) %>% 
  mutate("VA_D_Region_Rel" = (VA_Scenario_Region - VA_Baseline_Region)/VA_Baseline_Region) 

Table.Region <- Table.Region %>% 
  select(Region, Scenario, SA, X_D_Region, X_D_Region_Rel, VA_D_Region, VA_D_Region_Rel) %>% 
  unique()

write.xlsx(Table.Region, file = paste0(visualization_path, "/Operation_Table.Region.xlsx"))



data %>% 
  filter(Scenario == "Imp") %>%
  ggplot()+
  geom_col(aes(x=Region, y= (X_Scenario - X_Baseline), fill = iRow), position = "stack")+
  geom_hline(yintercept = 0)+
  labs(title = "Absolute impact on output by sector",
       y = "Absolute change from 2019 (in millions of EUR)",
       x= "Sector",
       caption = "Note different scales")+
  facet_grid(. ~ SA, scales = "free")+
  scale_fill_manual(values = longPalette)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "right"
  )

data %>% 
  ggplot()+
  geom_col(aes(x=Region, y= (X_Scenario_Region - X_Baseline_Region)/X_Baseline_Region, fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Absolute impact on output by sector",
       y = "Absolute change from 2019 (in millions of EUR)",
       x= "Sector",
       caption = "Note different scales")+
  facet_grid(Scenario ~ SA, scales = "free")+
  scale_fill_manual(values = longPalette)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "right"
  )

data %>% 
  ggplot()+
  geom_col(aes(x=Region, y= (VA_Scenario_Region - VA_Baseline_Region)/VA_Baseline_Region, fill = Scenario), position = "dodge")+
  geom_hline(yintercept = 0)+
  labs(title = "Absolute impact on value added by region",
       y = "Absolute change from 2019 (in millions of EUR)",
       x= "Region",
       caption = "Note different scales")+
  facet_grid(SA~ Scenario)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = longPalette)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "right"
  )


data %>% 
  ggplot()+
  geom_col(aes(x=Scenario, y= (VA_Scenario - VA_Baseline), fill = iRow), position = "stack")+
  geom_hline(yintercept = 0)+
  labs(title = "Region",
       y = "Absolute change from 2019 (in millions of EUR)",
       x= "Scenario",
       caption = "")+
  facet_grid(SA~ Region, scales = "free")+
  scale_fill_manual(values = longPalette)+
  guides(fill=guide_legend(nrow=2,byrow=FALSE))+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "bottom"
  )

a <- data %>% 
  ggplot()+
  geom_col(aes(x=Scenario, y= (VA_Scenario_Region - VA_Baseline_Region), fill = Scenario))+
  geom_hline(yintercept = 0)+
  labs(title = "Region",
       y = "millions of EUR",
      caption = "Note different scales")+
  facet_grid(SA~ Region, scales = "free")+
  scale_fill_manual(values = longPalette)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "none"
  )

b <- data %>% 
  ggplot()+
  geom_col(aes(x=Scenario, y= (VA_Scenario_Region - VA_Baseline_Region)/VA_Baseline_Region, fill = Scenario))+
  geom_hline(yintercept = 0)+
  labs(title = "Region",
       y = "Percentage change from 2019",
       x= "",
       caption = "")+
  facet_grid(SA~ Region, scales = "free")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = longPalette)+
  coord_flip()+
  theme(
    text = element_text(family = "CM Roman"),
    plot.title = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "CM Roman"),
    plot.caption = element_text(size = 10, hjust = 0.5, family = "CM Roman"),
    plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), "cm"),
    axis.text.y = element_text(size = 12, family = "CM Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, family = "CM Roman"),
    strip.text = element_text(size = 12, face = "bold", family = "CM Roman"),
    legend.text = element_text(size = 11, family = "CM Roman"),
    legend.position = "none"
  )

a

gridExtra::grid.arrange(a,b)
