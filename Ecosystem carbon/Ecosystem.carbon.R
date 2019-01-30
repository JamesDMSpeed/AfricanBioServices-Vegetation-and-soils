#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/Herbaceous.data/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/DW.Block.csv",head=T)
Soil.C <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.Carbon.Block.csv", head=T)
Soil.texture <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.texture.Min_Hor.csv",head=T)
Tree.size <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.size.csv",head=T)

library(tidyr)
library(plyr)
library(dplyr)

# Fixing the data for further processing 
# Soil texture 
levels(Soil.texture$Region)
Soil.texture$Region<- factor(Soil.texture$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Soil.texture <- Soil.texture[,c(1:8)]
Soil.texture <- droplevels(Soil.texture)
Soil.texture$Class <- c("SaClLo","SaLo","SaClLo","SaClLo","Cl","Cl","Cl","ClLo","Cl","SaCl","ClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaLo","SaLo","SaClLo","SaClLo","SaClLo","ClLo","Cl","ClLo","Cl","Cl","ClLo","ClLo","Cl") 

# Tree data
Tree.carbon$Region <- as.character(Tree.carbon$Region)

Tree.size.small <- Tree.size %>%
  filter(Size== "small")

Tree.size.large <- Tree.size %>%
  filter(Size=="large")

Tree.size <- merge(Tree.size.small[,c(2:4,6)],Tree.size.large[,c(4,6)],all.x = TRUE,by="Block.ID")
colnames(Tree.size) <- c("Block.ID","Region","Block","No.small.trees","No.large.trees")

# Soil data
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

Tree.size$Region<- factor(Tree.size$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# Merge the datasets 
names(Soil.carbon)
names(Tree.carbon)
names(Soil.texture)
names(Deadwood.carbon)
names(Tree.size)
Ecosystem.Carbon1a <- merge(Tree.carbon[,c(18,2,4,3,6,8,7,14:17)],Herbaceous.carbon[,c(3,11)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon1b <- merge(Ecosystem.Carbon1a, Soil.texture[,c(5:9)],all.x=TRUE, by="Block.ID")
Ecosystem.Carbon1c <- merge(Ecosystem.Carbon1b,Tree.size[,c(1,4:5)],all.x=TRUE, by="Block.ID")

Ecosystem.Carbon2a <- merge(Soil.carbon,Deadwood.carbon[,c(4:6)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2b <- merge(Ecosystem.Carbon2a,Soil.texture[,c(5:9)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2c <- merge(Ecosystem.Carbon2b,Ecosystem.Carbon1c[,c(1,8,10,11)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2d <- merge(Ecosystem.Carbon2c,Tree.size[,c(1,4:5)],all.x=TRUE, by="Block.ID")

names(Ecosystem.Carbon1c)
names(Ecosystem.Carbon2d)
Ecosystem.CHerbTree <- Ecosystem.Carbon1c[,c(1:7,13:16,8,10,11,17,18,9,12)]
Ecosystem.CSoilDW <- Ecosystem.Carbon2d[,c(1:7,14:22,8,10,12)]
SE.Ecosystem.CSoilDW <- Ecosystem.Carbon2d[,c(1:3,9,11,13)]

#### Creating a DF for the poster ####

# Belowground C 

Belowground.C <- Ecosystem.CSoilDW
Belowground.C$MAP <- round(Belowground.C$MAP, digits=2)

Belowground.C$MAP[Belowground.C$MAP=="717.36"] <- "736.10"
Belowground.C$MAP[Belowground.C$MAP=="754.84"] <- "736.10"
Belowground.C$MAP[Belowground.C$MAP=="736.10"] <- "704.07"
Belowground.C$MAP[Belowground.C$MAP=="672.04"] <- "704.07"
Belowground.C$MAP[Belowground.C$MAP=="1295.06"] <- "1287.16"
Belowground.C$MAP[Belowground.C$MAP=="1279.26"] <- "1287.16"

Deadwood.C <- Belowground.C[,c(1:5,19)]

Belowground.C$BelowgroundC <- Belowground.C$SoilAC.kg_m2 + Belowground.C$SoilMC.kg_m2
# Checking for significans of Clay, MAP and Lanuse 
summary(lm(BelowgroundC~Clay.pip.per, data=Belowground.C))
summary(lm(BelowgroundC~MAP:Clay.pip.per, data=Belowground.C))
summary(lm(BelowgroundC~Landuse:Clay.pip.per, data=Belowground.C))

SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BelowgroundC <- cbind(aggregate(BelowgroundC~Region+MAP+Landuse,mean,data=Belowground.C),
                      aggregate(BelowgroundC~Region+MAP+Landuse,SE,data=Belowground.C)[4])
colnames(BelowgroundC) <- c("Region","MAP.mm_yr","landuse","BelowgroundC","SE.BelowgroundC")

# Abovground C 
Aboveground.C <- Ecosystem.CHerbTree
names(Aboveground.C)
Aboveground.C$MAP.mm_yr <- round(Aboveground.C$MAP.mm_yr, digits=2)

Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="717.36"] <- "736.10"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="754.84"] <- "736.10"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="736.10"] <- "704.07"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="672.04"] <- "704.07"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="1295.06"] <- "1287.16"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="1279.26"] <- "1287.16"

# Creating a new collumn with all of aboveground C 
Aboveground.C$AbovegroundC <- rowSums(Aboveground.C[,c("HerbC.kg_m2", "TreeC.kg_m2")], na.rm=TRUE)
Aboveground.C$AbovegroundC <- Aboveground.C$AbovegroundC + Deadwood.C$DWC.kg_m2

AbovegroundC <- cbind(aggregate(AbovegroundC~Region+MAP.mm_yr+landuse,mean,data=Aboveground.C),
                          aggregate(AbovegroundC~Region+MAP.mm_yr+landuse,SE,data=Aboveground.C)[4])
colnames(AbovegroundC) <- c("Region","MAP.mm_yr","landuse","AbovegroundC","SE.AbovegroundC")

Above_below.C <- cbind(AbovegroundC,BelowgroundC[,c(4,5)])

AboveBelow <- Above_below.C[,c(1:4,6)]
SE.AboveBelow <- Above_below.C[,c(1:3,5,7)]

AboveBelow <- gather(AboveBelow, Carbon.pool,C.amount, AbovegroundC,BelowgroundC,factor_key=TRUE)
SE.AboveBelow <- gather(SE.AboveBelow, Carbon.pool,SE.C.amount, SE.AbovegroundC,SE.BelowgroundC,factor_key=TRUE)
AboveBelow <- cbind(AboveBelow,SE.AboveBelow[5])

write.csv(AboveBelow,file="Ecosystem carbon/Above_Below.C.csv")

#### Make the data into a long format instead of a wide ####
data_long.CTreeHerb <- gather(Ecosystem.CHerbTree, Carbon.pool,C.amount, TreeC.kg_m2:HerbC.kg_m2,factor_key=TRUE)

data_long.CSoilDW <- gather(Ecosystem.CSoilDW, Carbon.pool,C.amount, SoilAC.kg_m2:DWC.kg_m2,factor_key=TRUE)

SE.data_long.CSoilDW <- gather(SE.Ecosystem.CSoilDW, Carbon.pool,C.amount, SE.SoilAC.kg_m2:SE.DWC.kg_m2,factor_key=TRUE)

EcosystemC.SoilDW<- cbind(data_long.CSoilDW,SE.data_long.CSoilDW[5])
names(EcosystemC.SoilDW)
names(data_long.CTreeHerb)
colnames(EcosystemC.SoilDW) <- c("Block.ID","Region","Vilde.block","landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency.2000_2017","Clay.pip.per","Silt.pip.per","Sand.pip.per","Class","Total.basal.area_m2","TreeBM.kg_m2", "No.trees_m2","No.small.trees","No.large.trees","Carbon.pool","C.amount","SE.C.amount")

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

#### Plotting for poster ####
library(ggplot2)
library(dplyr)

legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

Above_Below.C <- read.csv("Ecosystem carbon/Above_Below.C.csv",head=T)

# Remove Seronera from my dataset 
Above_Below.C <- Above_Below.C %>%
  filter(Region != "Seronera")
Above_Below.C <- droplevels(Above_Below.C)

Above_Below.C$Region<- factor(Above_Below.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Park Nyigoti","Ikorongo"))

Above_Below.C <- Above_Below.C[
  order(Above_Below.C[,2]),
  ]

Above <- Above_Below.C %>%
  filter(Carbon.pool=="AbovegroundC")
Below <- Above_Below.C %>%
  filter(Carbon.pool=="BelowgroundC")

# Plot per MAP per Carbon pool 
Above.plot<- ggplot(data = Above, aes(x=MAP.mm_yr,y=C.amount,group=landuse,fill=landuse))

Above.plot + 
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + 
  xlab("MAP (mm/year)") + ylab(expression(paste("Carbon (kg", m^-2,")"))) +
  scale_fill_manual(breaks = c("Pasture","Wild"),values=c("darkolivegreen","darkgoldenrod")) 

Below<- ggplot(data = Below, aes(x=MAP.mm_yr,y=C.amount,group=landuse,fill=landuse))

Below + 
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + 
  xlab("MAP (mm/year)") + ylab(expression(paste("Carbon (kg", m^-2,")"))) +
  scale_fill_manual(breaks = c("Pasture","Wild"),values=c("darkolivegreen","darkgoldenrod")) 

# MAP per total ecosystem carbon rather plotted in power point  

# Add a collumn with number of large trees 
Above_Below.C$No.Large.trees <- c(3,3,10,10,1,1,8,8,10,10,7,7)

# Plot no.large trees and carbon as a barplot 
Above_Below.C$No.Large.trees <- as.factor(Above_Below.C$No.Large.trees)

Above_Below.Trees<- ggplot(data = Above_Below.C, aes(x=No.Large.trees,y=C.amount,group=Carbon.pool,fill=Carbon.pool,colour=landuse))

Above_Below.Trees + 
  facet_wrap(~landuse) +
  geom_bar(stat="identity", position="stack",na.rm=T) + 
  theme_bw() + Lines_gone  + 
  xlab("# Large Trees") + ylab(expression(paste("Carbon (kg", m^-2,")"))) +
  scale_fill_manual(breaks = c("Pasture","Wild"),values=c("salmon4","burlywood4")) 


 ### Ploting Ecosystem Carbon  #### 

Region.Eco.C <- read.csv("Ecosystem carbon/Tot.Ecosystem.Carbon.Region.csv", head=T)
Block.Eco.C <- read.csv("Ecosystem carbon/Ecosystem.Carbon.csv", head=T)

levels(Block.Eco.C$Carbon.pool)

# Rename the Carbon pool names 
Block.Eco.C$Carbon.pool<- factor(Block.Eco.C$Carbon.pool, levels = c("TreeC.kg_m2","HerbC.kg_m2", "DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))
Block.Eco.C$Region<- factor(Block.Eco.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")

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
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4"))

ggsave("Ecosystem carbon/Figures/EcoC.plot.png",
      width= 25, height = 15,units ="cm",bg ="transparent",
      dpi = 600, limitsize = TRUE)

#ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))

# Bar - dodge 
EcosystemC.bar1 <- ggplot(data = Block.Eco.C, aes(x=Region,y=C.amount,ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, fill=Carbon.pool))

EcosystemC.bar1 + 
  geom_errorbar(width=0.6,lwd=0.5,position=position_dodge(width=0.9),show.legend=F) +
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + xlab("Region") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  scale_fill_manual(breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

# Bar - stacked per Region 
EcosystemC.bar2 <- ggplot(data = Block.Eco.C, aes(x=Region,y=C.amount,ymin=C.amount-SE.C.amount, ymax=C.amount+SE.C.amount, fill=factor(Carbon.pool)))

EcosystemC.bar2 + 
  geom_bar(stat="identity", position="stack",width = 0.7,na.rm=T) +
  theme_bw() + Lines_gone  + 
  xlab("Region") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  scale_fill_manual(breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

ggsave("Ecosystem carbon/Figures/EcoC.Stacked.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Bar - stacked per Landuse 
EcosystemC.bar3 <- ggplot(data = Block.Eco.C, aes(x=landuse,y=C.amount,ymin=C.amount-SE.C.amount, ymax=C.amount+SE.C.amount, fill=factor(Carbon.pool)))

EcosystemC.bar3 + 
  geom_bar(stat="identity", position="stack",width = 0.5,na.rm=T) +
  theme_bw() + Lines_gone  +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank() 
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=16)
        ,axis.title=element_text(size=18)
        ,legend.text=element_text(size=16)
        ,legend.title=element_text(size=18)
        ,strip.background = element_blank()
        ,strip.text.x =element_text(size=18)) +
  theme(axis.line = element_line(color = 'black')) + 
  scale_y_continuous(labels = function(y) y / 10) +
  xlab("Land-Use") + ylab(expression(paste("Carbon (kg ", m^-2,")"))) + 
  scale_fill_manual(legend_titleCarbon,breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

ggsave("Ecosystem carbon/Figures/EcoC.Landuse.png",
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
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/EcoC.MAP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Soil texture and ECOSYSTEM CARBON

# 1. Clay 
# Clay seems to be affecting ecosystem C positive, except for with Handajega (low clay, but loads of aboveground C) 

EcosystemC.Soil.clay<- ggplot(data = Block.Eco.C, aes(x = Clay.pip.per,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Soil.clay  + xlab("Clay (%)") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Clay.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# 2. Sand
# Negative relationship 

EcosystemC.Soil.sand<- ggplot(data = Block.Eco.C, aes(x = Sand.pip.per,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Soil.sand  + xlab("Sand (%)") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free") + 
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) +
  theme_bw() + Lines_gone 

# Experimenting with a line of MAP in the plot.. 
# Rain <- aggregate(MAP.mm_yr~Region+Block.ID+Sand.pip.per,mean,data=Block.Eco.C)
# plot(MAP.mm_yr~Sand.pip.per,data=Rain)
#   scale_x_continuous(expand=c(0,0), limits = c(0, 70)) + 
#     scale_y_continuous(limits = c(0, 8), expand=c(0,0)) + 
#     scale_y_continuous(sec.axis = sec_axis(~ . *150, name = "Precipitation (mm)")) + 
#     geom_line(data= Rain,aes(y=MAP.mm_yr/150, colour= "MAP.mm_yr"))
#   nap2<-nap2+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
# nap2<-nap2+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)

ggsave("Ecosystem carbon/Figures/EcoC.Sand.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

summary(lm(C.amount~Sand.pip.per+ Sand.pip.per:MAP.mm_yr,data=Herbaceous))

# Fire and Ecosystem Carbon 
# year of last fire 
EcosystemC.Fire<- ggplot(data = Block.Eco.C, aes(x = Last.fire_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Fire  + xlab("Year of last fire") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=2,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Year.of.last.fire.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

summary(lm(C.amount~Clay.pip.per,data=Soil.min))

# Fire freq
EcosystemC.Fire<- ggplot(data = Block.Eco.C, aes(x = Fire_frequency.2000_2017,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Fire  + xlab("Fire frequency") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=1.5,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
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
  filter(Carbon.pool == "Soil A-horizon" | Carbon.pool== "Soil Min-horizon")
Soil.carbon <- arrange(Soil.carbon,Region)
Soil.carbon <- droplevels(Soil.carbon)

Soil.min<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil Min-horizon")
Soil.A.hor<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil A-horizon")

summary(lm(C.amount~landuse:Clay.pip.per,data=Soil.A.hor))

Tree.size.data <- gather(Soil.carbon, Tree.size,Count, No.small.trees:No.large.trees,factor_key=TRUE)

Tree.size.data <- Tree.size.data[
  order(Tree.size.data[,2], Tree.size.data[,3] ),
  ]

levels(Tree.size.data$Tree.size) <- c("Small trees","Large trees")
levels(Tree.size.data$Carbon.pool) <- c("Soil A-horizon","Soil Min-horizon")
levels(Soil.carbon$Carbon.pool) <- c("Soil A-horizon","Soil Min-horizon")

# Total tree biomass
SoilC.tree.biomass <- ggplot(data = Soil.carbon, aes(x = TreeBM.kg_m2,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

SoilC.tree.biomass  + xlab(expression(paste("Tree Biomass (kg", m^-2,")"))) + ylab(expression(paste("Carbon pool (kg", m^-2,")")))  + 
  facet_wrap(~Carbon.pool,ncol=1,scales = "free") +
  geom_errorbar(stat = "identity",width=0.05,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Soil A-horizon","Soil Min-horizon"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/SoilC.TreeBM.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Size of trees 
Tree.size.block<- Tree.size.data %>%
  filter(Carbon.pool== "Soil A-horizon")

Small.trees<- Tree.size.block %>%
  filter(Tree.size== "Small trees")
Large.trees<- Tree.size.block %>%
  filter(Tree.size== "Large trees")

large <- aggregate(Count~Region,sum,data=Large.trees)
small <- aggregate(Count~Region,sum,data=Small.trees)
plot(Count~Region,data=small)

# Small trees vs large trees 
SoiiC.Tree.Size <- ggplot(data = Tree.size.data, aes(x=Count,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, group= Tree.size, shape= landuse))

SoilC.Tree.Size +
  xlab("# Trees") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) + 
  facet_wrap(~Tree.size + Carbon.pool, ncol=2,scales='fixed') + 
  geom_errorbar(stat = "identity",width=1,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Soil A-horizon","Soil Min-horizon"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/SoilC.TreeSize.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Number of trees and SOIL CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
SoilC.No.trees<- ggplot(data = Soil.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

SoilC.No.trees  + xlab("Number of trees (m2)") + ylab("Soil Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
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
Ecosystem.Carbon <- read.csv(file="Ecosystem Carbon/Ecosystem.Carbon.csv", head=T)
names(Ecosystem.Carbon)

Variables <- Ecosystem.Carbon %>%
  filter(Carbon.pool=="HerbC.kg_m2")

str(Ecosystem.Carbon)

# Check the outliars 
# 1. An outliar in TreeBM 
plot(TreeBM.kg_m2~Last.fire_yr,data=Variables)
identify(Variables$TreeBM.kg_m2~Variables$Last.fire_yr) 
# 16 - All these outliars due to the gigant tree in Handajega 
Variables.red1 <- Variables[-c(16),] # Try to run it without this block 
# the Fire.freq is not correlated 0.4 with tree.BM anymore, and MAP-small trees is 0.5 instead of 0.4

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

names(Variables)
MyVar<-c("MAP.mm_yr","Clay.pip.per","Sand.pip.per","Last.fire_yr", "Fire_frequency.2000_2017","No.small.trees","No.large.trees","TreeBM.kg_m2")

# Want to get these two in one matrix. 
pairs(Variables[,MyVar],lower.panel = panel.cor)

# If I want these values in a table:
Variables2 <- Variables[,c(6:9,11,13,15,16)] # first select the collums I want to use 
library("Hmisc")
Mycor <- rcorr(as.matrix(Variables2), type="pearson") # Use the pearson correlation (r-value)
MycorR <- as.data.frame(round(Mycor$r, digits=3))
MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

# ANOVA on LANDUSE to look for correlation, is the variance bigger between than within? 
par(mfrow=c(1,1))
plot(TreeBM.kg_m2~landuse, data= Variables)
plot(No.trees_m2~landuse, data= Variables)

# Tree basal area and landuse 1. per Region 
Tree.basal.area <- lm(TreeBM.kg_m2~factor(landuse), data= Variables)
anova(Tree.basal.area) # Look at the F- value, quite high, and the P value for the F-test is significant. 
summary(Tree.basal.area) # Not look at the P value here - because this is for the t-test!! Look at the Adjusted R-squared value (0.3597), and the difference between the estimates: Wild= 0.0087, Pasture = 0.0087+0.068 = 0.076, quite a difference -> use this if plotting the values.. 
par(mfrow=c(2,2))
plot(Tree.basal.area)
TukeyHSD(aov(Tree.basal.area)) # Post Hoc test to look at the difference between the variables, not important when I only have to variables, I get the same information from summary.. 

tapply(Variables$TreeBM.kg_m2, Variables$landuse, mean)

# Other variables
MAP <- lm(MAP.mm_yr~factor(landuse), data= Variables)
summary(MAP)
Clay <- lm(Clay.pip.per~factor(landuse), data= Variables)
summary(Clay)
Sand <- lm(Sand.pip.per~factor(landuse), data= Variables)
summary(Clay)
Last.fire <- lm(Last.fire_yr~factor(landuse), data= Variables)
summary(Last.fire)
Fire.freq <- lm(Fire_frequency.2000_2017~factor(landuse), data= Variables)
summary(Fire.freq)
Small.trees <- lm(No.small.trees~factor(landuse), data= Variables)
summary(Small.trees)
Large.trees <- lm(No.large.trees~factor(landuse), data= Variables)
summary(Large.trees)


         
### Data modelling/ analysis ####

library(nlme)
library(lme4)
library(glmmADMB) 
library(piecewiseSEM)
library(MuMIn) # to make "model.sel()" of different models 

# About mixed effect models (nlme package)
# REML = restricted maximum likelihood estimation 
# Fixed effects influence the mean of y, while Random effects influence the variance of y 
# Use REML= F when looking at the fixed effects 
# Use REML = T when looking at the random effects, and the parameter estimates
# I have region and block.ID as random effects 
# Choose the model with the smallest AIC value 

Block.Eco.C$Block.ID <- as.factor(Block.Eco.C$Block.ID)
str(Block.Eco.C)
Soil.min<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil Min-horizon")
Soil.Ahor<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil A-horizon")
DW<- Block.Eco.C %>%
  filter(Carbon.pool== "Dead wood")
Woody<- Block.Eco.C %>%
  filter(Carbon.pool== "Woody")
Herbaceous<- Block.Eco.C %>%
  filter(Carbon.pool== "Herbaceous")

# Testing my first hypothesis 1 where I think MAP has an effect on C, however, this relationship can be mediated by soil texture. 

# Soil mineral horizon 
Soil.min.H11<-lmer(C.amount~ MAP.mm_yr + (1|Region), data = Soil.min, REML=F)
Soil.min.H12<-lmer(C.amount~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.min, REML=F)
Soil.min.H13<-lmer(C.amount~ MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.min, REML=F)
Soil.min.H14<-lmer(C.amount~ Sand.pip.per + (1|Region), data = Soil.min, REML=F)

model.sel(Soil.min.H11,Soil.min.H12,Soil.min.H13,Soil.min.H14)
# Look at delta AIC - the first is 0 (the best model), and the change to the next indicates if I should continue using it or not. If it is between 1 and 2, continue with second model as well. Here delta mod H14 = 0.59 (bring along). 

Soil.min.H12<-lmer(C.amount~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.min, REML=T)
Soil.min.H14<-lmer(C.amount~ Sand.pip.per + (1|Region), data = Soil.min, REML=T)

summary(Soil.min.H12)
summary(Soil.min.H14)

# Assumption 1: Within group errors are independent, and identically normally distributed, mean= 0. And they are independent of the random effect. 

hist(resid(Soil.min.H12)) # A bit sqewed towards right, more on the positive side - could consider log.transforming the data 
hist(resid(Soil.min.H14)) # "more normally distributed" 
plot(Soil.min.H12,factor(Region)~resid(.),abline=0) # Makao is strange Block 4 has a very low carbon amount (0.8 kg). 
plot(Soil.min.H14,factor(Region)~resid(.),abline=0)

# Another way to show the same. 
plot(Soil.min.H12,resid(.)~fitted(.)|factor(Region)) # Distribution of residuals within each group. 

#Assumption 2: Among groups, random effects are normally distributed (Regions). 
# BLUPS (Best Linear Unbiased Predictors)
ranef(Soil.min.H12, drop=TRUE)
blup <- c(-0.14196792,-0.03692530,-0.21005201,-0.06268585,0.13150659,0.04134352,0.27878097) 
hist(blup)

anova(Soil.min.H12) # Due to high F value: MAP:Sand is very strong. MAP almost 2 
anova(Soil.min.H14) 

# Run a post hoc test 
drop1(Soil.min.H12,test="Chi") 
# Single term deletions
# 
# Model:
#   C.amount ~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1 | Region)
# Df    AIC    LRT  Pr(Chi)   
# <none>                    53.370                   
# MAP.mm_yr:Sand.pip.per  1 59.764 8.3941 0.003764 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

drop1(Soil.min.H14,test="Chi")
# Single term deletions
# 
# Model:
#   C.amount ~ Sand.pip.per + (1 | Region)
# Df    AIC    LRT Pr(Chi)  
# <none>          54.952                 
# Sand.pip.per  1 58.045 5.0931 0.02402 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 

# Soil A-horizon: The best model is taking MAP and the relationship between MAP:Sand into account####
Soil.A.H11<-lmer(C.amount~ MAP.mm_yr + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H12<-lmer(C.amount~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H13<-lmer(C.amount~ MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H14<-lmer(C.amount~ Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H15<-lmer(C.amount~ Sand.pip.per + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)

model.sel(Soil.A.H11,Soil.A.H12,Soil.A.H13,Soil.A.H14,Soil.A.H15)
# AIC: H14 < H15 < H11 < H12 < H13 
drop1(Soil.A.H15, test="Chi")

Soil.A.H15<-lmer(C.amount~ Sand.pip.per + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=T)

summary(Soil.A.H15)
hist(resid(Soil.A.H15)) # normally distriuted - a bit sqewed towards right 
anova(Soil.A.H15, Soil.A.H14) # Due to high F value: Sand is very strong, MAP:Sand is 1.6 
AIC(Soil.A.H15) # AIC: 30.42954

# Testing second hypothesis H2 where I ask about the effect of land use and woody plant cover. 
Soil.min.H21<-lmer(C.amount~ landuse + (1|Region), data = Soil.min, REML=F)
Soil.min.H22<-lmer(C.amount~ No.small.trees + (1|Region), data = Soil.min, REML=F)
Soil.min.H23<-lmer(C.amount~ landuse + No.small.trees + (1|Region), data = Soil.min, REML=F)
Soil.min.H24<-lmer(C.amount~ landuse + No.small.trees + landuse:No.small.trees + (1|Region), data = Soil.min, REML=F)
Soil.min.H25<-lmer(C.amount~ landuse:No.small.trees + (1|Region), data = Soil.min, REML=F)

model.sel(Soil.min.H21,Soil.min.H22,Soil.min.H23,Soil.min.H24,Soil.min.H25) # AIC: H22<H25<H23<H24<H21
anova(Soil.min.H22) # No.small.trees F: 0.13
anova(Soil.min.H25) # landuse:No.small.trees F: 0.91

Soil.min.H25<-lmer(C.amount~ landuse:No.small.trees + (1|Region), data = Soil.min, REML=T)
Soil.min.H22<-lmer(C.amount~ No.small.trees + (1|Region), data = Soil.min, REML=T)

summary(Soil.min.H25)
summary(Soil.min.H22)
hist(resid(Soil.min.H25))
hist(resid(Soil.min.H22))
AIC(Soil.min.H25) # 73.558
AIC(Soil.min.H22) # 69.6068




# Run a full model of all my fixed factors ####
names(Soil.Ahor)
Soil.A_horizon<-lmer(C.amount~ MAP.mm_yr + Sand.pip.per + Last.fire_yr + landuse + TreeBM.kg_m2 + No.large.trees + landuse:TreeBM.kg_m2 + landuse:No.large.trees + landuse:Last.fire_yr + landuse:Sand.pip.per + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)

summary(Soil.A_horizon)
drop1(Soil.A_horizon,test="Chi")

Soil.A_horizon1 <- update(Soil.A_horizon, .~. -landuse:TreeBM.kg_m2)
anova(Soil.A_horizon,Soil.A_horizon1) # using simpler model because the p-value is larger than 0.05?? (no difference between the two models)

Soil.A_horizon2 <- update(Soil.A_horizon1, .~. -landuse:No.large.trees)
anova(Soil.A_horizon1,Soil.A_horizon2) #Remove "landuse:No.large.trees"
