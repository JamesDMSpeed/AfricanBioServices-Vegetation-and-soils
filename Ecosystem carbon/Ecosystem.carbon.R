#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/DW.Block.csv",head=T)
Soil.C <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.Carbon.Block.csv", head=T)
Soil.texture <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.texture.Min_Hor.csv",head=T)

library(tidyr)
library(plyr)
library(dplyr)

# Fixing the data for further processing 
Tree.carbon$Region <- as.character(Tree.carbon$Region)
Tree.carbon$Region[Tree.carbon$Region == "SNP handejega"] <- "Handajega"

SoilAHor.carbon <- Soil.C %>%
  filter(Horizon== "A-hor")

SoilMinHor.carbon <- Soil.C %>%
  filter(Horizon=="Min-hor")

colnames(Herbaceous.carbon)[colnames(Herbaceous.carbon) == "C.kg_m2"] <- "HerbC.kg_m2"
colnames(SoilAHor.carbon)[colnames(SoilAHor.carbon) == "C.kg_m2"] <- "SoilAC.kg_m2"
colnames(SoilAHor.carbon)[colnames(SoilAHor.carbon) == "SE.C.kg_m2"] <- "SE.SoilAC.kg_m2"
colnames(SoilMinHor.carbon)[colnames(SoilMinHor.carbon) == "C.kg_m2"] <- "SoilMC.kg_m2"
colnames(SoilMinHor.carbon)[colnames(SoilMinHor.carbon) == "SE.C.kg_m2"] <- "SE.SoilMC.kg_m2"

names(SoilAHor.carbon)
Soil.carbon <- merge(SoilAHor.carbon[,c(2,3,5,13,6,11:12,7,8)],SoilMinHor.carbon[,c(5,7,8)],all.x = TRUE,by="Block.ID")

#Relevel 
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous.carbon$Region<- factor(Herbaceous.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Deadwood.carbon$Region<- factor(Deadwood.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil.carbon$Region<- factor(Soil.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# Merge the datasets 
names(Soil.carbon)
names(Tree.carbon)
names(Soil.texture)
names(Deadwood.carbon)
Ecosystem.carbon1 <- merge(Tree.carbon[,c(18,2,4,3,6,8,7,14:17)],Herbaceous.carbon[,c(3,11)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon1 <- merge(Ecosystem.carbon1, Soil.texture[,c(5:8)],all.x=TRUE, by="Block.ID")

Ecosystem.carbon2 <- merge(Soil.carbon,Deadwood.carbon[,c(4:6)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2 <- merge(Ecosystem.carbon2,Soil.texture[,c(5:8)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2 <- merge(Ecosystem.Carbon2,Ecosystem.Carbon1[,c(1,8,10,11)],all.x = TRUE,by="Block.ID")

names(Ecosystem.Carbon1)
names(Ecosystem.Carbon2)
Ecosystem.CHerbTree <- Ecosystem.Carbon1[,c(1:7,13:15,8,10,11,9,12)]
Ecosystem.CSoilDW <- Ecosystem.Carbon2[,c(1:7,14:19,8,10,12)]
SE.Ecosystem.CSoilDW <- Ecosystem.carbon2[,c(1:3,9,11,13)]

# Make the data into a long format instead of a wide
data_long.CTreeHerb <- gather(Ecosystem.CHerbTree, Carbon.pool,C.amount, TreeC.kg_m2:HerbC.kg_m2,factor_key=TRUE)

data_long.CSoilDW <- gather(Ecosystem.CSoilDW, Carbon.pool,C.amount, SoilAC.kg_m2:DWC.kg_m2,factor_key=TRUE)

SE.data_long.CSoilDW <- gather(SE.Ecosystem.CSoilDW, Carbon.pool,C.amount, SE.SoilAC.kg_m2:SE.DWC.kg_m2,factor_key=TRUE)

EcosystemC.SoilDW<- cbind(data_long.CSoilDW,SE.data_long.CSoilDW[5])
names(EcosystemC.SoilDW)
names(data_long.CTreeHerb)
colnames(EcosystemC.SoilDW) <- c("Block.ID","Region","Vilde.block","landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency.2000_2017","Clay.pip.per","Silt.pip.per","Sand.pip.per","Total.basal.area_m2","TreeBM.kg_m2", "No.trees_m2","Carbon.pool","C.amount","SE.C.amount")

EcosystemC.SoilDW <- EcosystemC.SoilDW[
  order(EcosystemC.SoilDW[,1], EcosystemC.SoilDW[,2] ),
  ]

data_long.CTreeHerb <- data_long.CTreeHerb[
  order(data_long.CTreeHerb[,1], data_long.CTreeHerb[,3] ),
  ]

# Creating one long dataset with all data
data_long.CTreeHerb$SE.C.amount <- c(NA*56)
names(data_long.CTreeHerb)
names(EcosystemC.SoilDW)
Ecosystem.Carbon <- bind_rows(data_long.CTreeHerb, EcosystemC.SoilDW, id=NULL)

Ecosystem.Carbon <- Ecosystem.Carbon[
  order(Ecosystem.Carbon[,1], Ecosystem.Carbon[,3] ),
  ]

write.csv(Ecosystem.Carbon,file="Ecosystem carbon/Ecosystem.Carbon.csv")
write.csv(EcosystemC.SoilDW,file="Ecosystem carbon/Soil.DW.Carbon.Block.csv")
write.csv(data_long.CTreeHerb,file="Ecosystem carbon/Tree.Herb.Carbon.Block.csv")


### Exploring the data at Region level ####
Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)
Soil <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.Region.csv",header=T)

Woody$Region <- as.factor(c("Makao","Maswa", "Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
names(Woody)
Woody <- Woody[,c(1,2,14,11:13,3:10)]

#Relevel 
Woody$Region<- factor(Woody$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil$Region<- factor(Soil$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Merge the three datasets in two steps 
Ecosystem.C1 <- merge(Woody[,c(2:6,7:14)],Herbaceous[,c(2:4)],all.x = TRUE,by="Region")
Ecosystem.C2 <- merge(Ecosystem.C1,Deadwood[2:4],all.x = TRUE,by="Region")
Ecosystem.C <- merge(Ecosystem.C2,Soil[2:16],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1:5,18,8,10,12,19,21,23,6,14,16,25,27)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,18,7,15,17,26,28)]

# Make the data into a long format instead of a wide
library(tidyr)
library(plyr)

data_long.C <- gather(EcosystemCarbon, Carbon.pool,C.amount, TreeC.kg_m2:CMinHor,factor_key=TRUE)
data_long.CSE <- gather(EcosystemCarbonSE, Carbon.poolSE,C.amountSE, SE.TreeC.kg_m2:SE.CMinHor,factor_key=TRUE)

Tot.EcosystemCarbon <- cbind(data_long.C,data_long.CSE[3])

Tot.EcosystemCarbon <- arrange(Tot.EcosystemCarbon,Region)
colnames(Tot.EcosystemCarbon)[colnames(Tot.EcosystemCarbon) == "Landuse.x"] <- "Landuse"
Tot.EcosystemCarbon$Climate <- as.factor(c("Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Int-Dry","Int-Dry","Int-Dry","Int-Dry","Int-Dry","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet"))

write.csv(Tot.EcosystemCarbon,file="Ecosystem carbon/Tot.Ecosystem.Carbon.Region.csv")
### Ploting Ecosystem Carbon  #### 

Region.Eco.C <- read.csv("Ecosystem carbon/Tot.Ecosystem.Carbon.Region.csv", head=T)
Block.Eco.C <- read.csv("Ecosystem carbon/Ecosystem.Carbon.csv", head=T)

levels(Block.Eco.C$Carbon.pool)

# Rename the Carbon pool names 
levels(Block.Eco.C$Carbon.pool)[levels(Block.Eco.C$Carbon.pool)=="TreeC.kg_m2"] <- "WoodyC"
levels(Block.Eco.C$Carbon.pool)[levels(Block.Eco.C$Carbon.pool)=="HerbC.kg_m2"] <- "HerbC"
levels(Block.Eco.C$Carbon.pool)[levels(Block.Eco.C$Carbon.pool)=="DWC.kg_m2"] <- "DWC"
levels(Block.Eco.C$Carbon.pool)[levels(Block.Eco.C$Carbon.pool)=="SoilAC.kg_m2"] <- "SoilCAHor"
levels(Block.Eco.C$Carbon.pool)[levels(Block.Eco.C$Carbon.pool)=="SoilMC.kg_m2"] <- "SoilCMinHor"

Block.Eco.C$Carbon.pool<- factor(Block.Eco.C$Carbon.pool, levels = c("WoodyC","HerbC", "DWC","SoilCAHor","SoilCMinHor"))
Block.Eco.C$Region<- factor(Block.Eco.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

library(ggplot2)
library(dplyr)

# Plots per region of ECOSYSTEM CARBON

# Legend titeles
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

#### TOT ECOSYSTEM C ####
# Point

# To get different shape of point: scale_shape_manual(values=c(1,15))
EcosystemC.plot1 <- ggplot(data = Block.Eco.C, aes(x = Region,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.plot1 + xlab("Region") +  ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")")))  + 
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_point(size = 3, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=.4,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4"))

#ggsave("Ecosystem carbon/Figures/EcoC.plot.png",
#       width= 25, height = 15,units ="cm",bg ="transparent",
#       dpi = 600, limitsize = TRUE)

#ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))

# Bar - dodge 
EcosystemC.bar1 <- ggplot(data = Block.Eco.C, aes(x=Region,y=C.amount,ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, fill=Carbon.pool))

EcosystemC.bar1 + 
  geom_errorbar(width=0.6,lwd=0.5,position=position_dodge(width=0.9),show.legend=F) +
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + xlab("Region") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  scale_fill_manual(breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

# Bar - stacked 
EcosystemC.bar2 <- ggplot(data = Block.Eco.C, aes(x=Region,y=C.amount,ymin=C.amount-SE.C.amount, ymax=C.amount+SE.C.amount, fill=factor(Carbon.pool)))

EcosystemC.bar2 + 
  geom_bar(stat="identity", position="stack",width = 0.7,na.rm=T) +
  theme_bw() + Lines_gone  + 
  xlab("Region") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  scale_fill_manual(breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

ggsave("Ecosystem carbon/Figures/EcoC.Stacked.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#+ geom_errorbar(position="dodge",width=.2,show.legend=F, na.rm=T)

# MAP and ECOSYSTEM CARBON
# Seems to be a positive relationship between MAP and C Except for Maswa which is quite hight in tree C even though it is considered a dry region. 
EcosystemC.MAP <- ggplot(data = Block.Eco.C, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.MAP + xlab(expression(paste("MAP (mm", yr^-1,")")))+ ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_errorbar(stat = "identity",width=25,lwd=1.1,show.legend=F, na.rm=T) + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/EcoC.MAP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Soil texture and ECOSYSTEM CARBON

# 1. Clay 
# Clay seems to be affecting ecosystem C positive, except for with Handajega (low clay, but loads of aboveground C) 

EcosystemC.Soil.clay<- ggplot(data = Block.Eco.C, aes(x = Clay.pip.per,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Soil.clay  + xlab("Clay (%)") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Clay.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# 2. Sand
# Negative relationship 
EcosystemC.Soil.sand<- ggplot(data = Block.Eco.C, aes(x = Sand.pip.per,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Soil.sand  + xlab("Sand (%)") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Sand.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Fire and Ecosystem Carbon 
# year of last fire 
EcosystemC.Fire<- ggplot(data = Block.Eco.C, aes(x = Last.fire_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Fire  + xlab("Year of last fire") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=2,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Year.of.last.fire.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Fire freq
EcosystemC.Fire<- ggplot(data = Block.Eco.C, aes(x = Fire_frequency.2000_2017,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Fire  + xlab("Fire frequency") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=1.5,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Fire.freq.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#### Trees and ECOSYSTEM CARBON - maybe not so relevant #### 
# Tre basal area and ECOSYSTEM CARBON

EcosystemC.basal.area <- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBasalA_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.basal.area  + xlab(expression(paste("Tree basal area (per ", m^-2,")"))) + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")")))  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.01,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Number of trees and ECOSYSTEM CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
EcosystemC.No.trees<- ggplot(data = Total.ecosystem.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.No.trees  + xlab("Number of trees (m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC"),values=c("dimgray","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone

# Tree Biomass and ECOSYSTEM CARBON

EcosystemC.treeBM<- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBM_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.treeBM  + xlab("Tree Biomass (g/m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)   + 
  geom_errorbar(stat = "identity",width=35,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC"),values=c("dimgray","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone

#### SOIL CARBON #### 

# Tre basal area and SOIL CARBON

# Creating a table with soil carbon 
Soil.carbon <- Block.Eco.C %>%
  filter(Carbon.pool == "SoilCAHor" | Carbon.pool== "SoilCMinHor")
Soil.carbon <- arrange(Soil.carbon,Region)

names(Soil.carbon)

SoilC.basal.area <- ggplot(data = Soil.carbon, aes(x = Total.basal.area_m2,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

SoilC.basal.area  + xlab("Tree basal area") + ylab(expression(paste("Carbon pool (kg", m^-2,")")))  + 
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_errorbar(stat = "identity",width=0.01,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/SoilC.TreeBA.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Number of trees and SOIL CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
SoilC.No.trees<- ggplot(data = Soil.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

SoilC.No.trees  + xlab("Number of trees (m2)") + ylab("Soil Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Tree Biomass and SOIL CARBON

SoilC.treeBM<- ggplot(data = Soil.carbon, aes(x = TreeBM_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

SoilC.treeBM  + xlab("Tree Biomass (g/m2)") + ylab("Soil Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)   + 
  geom_errorbar(stat = "identity",width=35,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) +
  theme_bw() + Lines_gone

### Aggregate TOTAL CARBON per region ####

EcosystemC <- aggregate(C.amount~Region, data=Total.ecosystem.carbon,sum)
EcosystemCSE <- aggregate(C.amountSE~Region, data=Total.ecosystem.carbon,sum)
No.trees_m2 <- aggregate(No.trees_m2~Region, data=Total.ecosystem.carbon,median)
TreeBM_m2 <- aggregate(TreeBM_m2~Region, data=Total.ecosystem.carbon,median)
Ecosystem.Carbon <- cbind(EcosystemC,EcosystemCSE[2],No.trees_m2[2],TreeBM_m2[2])
colnames(Ecosystem.Carbon) <- c("Region","C.amount","SE.C.Amount","No.trees_m2","TreeBM_m2") # Maswa extreamly high in C and woody cover
Ecosystem.Carbon$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

# Seperate per land-use 
Wild.EcosystemC <- Ecosystem.Carbon[Ecosystem.Carbon$Landuse!="Pasture",]# Only wild regions
Wild.EcosystemC <- droplevels(Wild.AbovegroundC)
Pasture.EcosystemC <- Ecosystem.Carbon[Ecosystem.Carbon$Landuse!="Wild",]# Only pasture regions
Pasture.EcosystemC <- droplevels(Pasture.AbovegroundC)

# Number of trees and ECOSYSTEM CARBON
# Point
Tree.no.plot <- ggplot(data=Ecosystem.Carbon, aes(x=No.trees_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

Tree.no.plot + xlab("Number of Trees") + ylab("Ecosystem Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat="identity",width=.001,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

# Biomass of trees and ECOSYSTEM CARBON

# Point
BM.plot <- ggplot(data=Ecosystem.Carbon, aes(x=TreeBM_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

BM.plot + xlab("Tree Biomass (g/m2)") + ylab("Ecosystem Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(width=35,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

#???
# Wild plot
Woody.cover.plot.wild <- ggplot(data=Wild.AbovegroundC, aes(x=Woody.Cover,y=C.amount, colour="Wild"))

Woody.cover.plot.wild + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Wild"),values=c("forestgreen"))

# Pasture plot
Woody.cover.plot.pasture <- ggplot(data=Pasture.AbovegroundC, aes(x=Woody.Cover,y=C.amount,colour="Pasture"))

Woody.cover.plot.pasture + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Pasture"),values=c("chocolate2"))

### Correlation between variables #### 

# Want to do the analysis at block size 
Soil.properties <- read.csv("Ecosystem carbon/Soil.data/Soil.properties.csv", head=T) # 28 obs
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T) #27 obs, missing one in Seronera 

# Using DataCombine to insert a new row in Tree data 
names(Soil.properties)
names(Tree.carbon)

Correlation <- cbind(Soil.properties,Tree.carbon[9:12])
Correlation <- na.omit(Correlation)
Correlation <- droplevels(Correlation)
str(Correlation)

Correlation$Total.basal.area_m2<- as.numeric(Correlation$Total.basal.area_m2)
Correlation$No.trees_m2 <- as.numeric(Correlation$No.trees_m2 )
# RUN THIS CODE FIRST (FROM STU)

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

# Then select the variables to use in the pair function with panel.cor

names(Correlation)
MyVar<-c("MAP","Clay","Last.fire", "Fire.freq","No.trees_m2","Total.basal.area_m2")

# Want to get these two in one matrix. 
pairs(Correlation[,MyVar],lower.panel = panel.cor)

# If I want these values in a table:
Red.Ecosystem.C <- Total.ecosystem.carbon[,c(4:7,13,17)] # first select the collums I want to use 
library("Hmisc")
Mycor <- rcorr(as.matrix(Red.Ecosystem.C), type="pearson") # Use the pearson correlation (r-value)
MycorR <- as.data.frame(round(Mycor$r, digits=3))
MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

# ANOVA on LANDUSE to look for correlation, is the variance bigger between than within? 
par(mfrow=c(1,1))
plot(TreeBasalA_m2~Landuse.x, data= Total.ecosystem.carbon)
plot(No.trees_m2~Landuse.x, data= Total.ecosystem.carbon)

# Tree basal area and landuse 1. per Region 
Tree.basal.area <- lm(TreeBasalA_m2~factor(Landuse), data= Total.ecosystem.carbon)
anova(Tree.basal.area) # Look at the F- value, quite high, and the P value for the F-test is significant. 
summary(Tree.basal.area) # Not look at the P value here - because this is for the t-test!! Look at the Adjusted R-squared value (0.3597), and the difference between the estimates: Wild= 0.0087, Pasture = 0.0087+0.068 = 0.076, quite a difference -> use this if plotting the values.. 
par(mfrow=c(2,2))
plot(Tree.basal.area)
TukeyHSD(aov(Tree.basal.area)) # Post Hoc test to look at the difference between the variables, not important when I only have to variables, I get the same information from summary.. 

tapply(Total.ecosystem.carbon$TreeBasalA_m2, Total.ecosystem.carbon$Landuse, mean)

# Testing if correlated per block.. 
Tree.BA <- lm(Total.basal.area_m2~factor(landuse), data=Tree.carbon)
summary(Tree.BA) # Still a correlation, but less significant, lower r-value.. 

Tree.no <- lm(No.trees_m2~factor(landuse),data=Tree.carbon)
summary(Tree.no) # Quite the same as at regional level actually! 

# Number of trees and landuse 
Number.of.trees <- lm(No.trees_m2~factor(Landuse), data= Total.ecosystem.carbon)
anova(Number.of.trees)
summary(Number.of.trees)
par(mfrow=c(2,2))
plot(Number.of.trees)
TukeyHSD(aov(Number.of.trees))

# Landuse and correlation with other variables 
MAP <- lm(MAP.mm_yr~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(MAP)
Clay <- lm(Clay~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(Clay)
Last.fire <- lm(Last_fire.yr~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(Last.fire)
Fire.freq <- lm(Fire_frequency.2000_2017~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(Fire.freq)


         