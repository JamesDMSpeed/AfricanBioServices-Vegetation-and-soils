#### PACKAGES #### 
library(tidyr)
library(plyr)
library(dplyr)
library("Hmisc") # For the correlation plot 
library(ggplot2)
library(lattice) # xy.plot

library(nlme)
library(lme4)
library(glmmADMB) 
library(piecewiseSEM) # SEM
library(MuMIn) # to make "model.sel()" of different models 
library(emmeans) # estimated marginal means --> look at this for three ways interactions

#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/Herbaceous.data/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/DW.Block.csv",head=T)
Soil.C <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.Carbon.Block.csv", head=T)
Soil.texture <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.texture.Tot_Hor.csv",head=T)
Tree.size <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.size.csv",head=T)
Belowground <- read.csv(file="Ecosystem Carbon/Soil.data/Belowground.Carbon.csv",head=T)
Tree.BM.N.non <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.BM.N.non.csv",head=T)

# Fixing the data for further processing 
# Soil texture 
levels(Soil.texture$Region)
Soil.texture$Region<- factor(Soil.texture$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Soil.texture <-Soil.texture[,c(1:3,5:8)]
Soil.texture <- na.omit(Soil.texture)
Soil.texture <- droplevels(Soil.texture)

Soil.texture$Class <- c("SaClLo","SaLo","SaClLo","SaClLo","Cl","Cl","Cl","ClLo","SaCl","SaCl","ClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaLo","SaLo","SaLo","ClLo","Cl","ClLo","Cl", "Cl","ClLo","ClLo","Cl") 

# Tree data
Tree.carbon$Region <- as.character(Tree.carbon$Region)

Tree.size.small <- Tree.size %>%
  filter(Size== "small")

Tree.size.large <- Tree.size %>%
  filter(Size=="large")

Tree.size.small.N <- Tree.size.small %>%
  filter(N.non== "N")

Tree.size.small.non <- Tree.size.small %>%
  filter(N.non== "non")

Tree.size.large.N <- Tree.size.large %>%
  filter(N.non=="N")

Tree.size.large.non <- Tree.size.large %>%
  filter(N.non=="non")

ID <- Soil.texture[,c(1,3)]
colnames(ID) <- c("area","block")
Tree.size.small.N <- full_join(ID,Tree.size.small.N)
Tree.size.small.non <- full_join(ID,Tree.size.small.non)
Tree.size.large.N <- full_join(ID,Tree.size.large.N)
Tree.size.large.non <- full_join(ID,Tree.size.large.non)

Tree.size.S <- merge(Tree.size.small.N[,c(1,2,7)],Tree.size.small.non[,c(1,2,7)],all.x = TRUE,by=c("area","block"))
colnames(Tree.size.S) <- c("Region","Block","Small.N","Small.N.non")
Tree.size.L <- merge(Tree.size.large.N[,c(1,2,7)],Tree.size.large.non[,c(1,2,7)],all.x = TRUE,by=c("area","block"))
colnames(Tree.size.L) <- c("Region","Block","Large.N","Large.N.non")
Tree.size <- cbind(Tree.size.S,Tree.size.L[c(3,4)])
Tree.size$Region<- factor(Tree.size$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Tree.size <- Tree.size[
  order(Tree.size[,1],Tree.size[,2]),
  ]

Tree.size$Block.ID <- as.numeric(1:28)
Trees.N.non <- cbind(Tree.size,Tree.BM.N.non[c(6:9)])

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

Nitrogen <- aggregate(mean.N.kg_m2.x~Region + Block, mean,data=Belowground)
Soil.carbon <- merge(Soil.carbon,Nitrogen[,c(1:3)],all.x = TRUE,by=c("Region","Block"))

Soil.carbon <- Soil.carbon[
  order(Soil.carbon[,3]),
  ]

#Relevel 
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous.carbon$Region<- factor(Herbaceous.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Deadwood.carbon$Region<- factor(Deadwood.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil.carbon$Region<- factor(Soil.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Trees.N.non$Region<- factor(Tree.size$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# Merge the datasets 
names(Soil.carbon)
names(Tree.carbon)
names(Soil.texture)
names(Deadwood.carbon)
names(Trees.N.non)
Ecosystem.Carbon1a <- merge(Tree.carbon[,c(22,2,3,4,6,8,7,15,18:21)],Herbaceous.carbon[,c(3,11)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon1b <- merge(Ecosystem.Carbon1a, Soil.texture[,c(4:8)],all.x=TRUE, by="Block.ID")
Ecosystem.Carbon1c <- merge(Ecosystem.Carbon1b,Trees.N.non[,c(1,7,3:6,8:11)],all.x=TRUE, by="Block.ID")
Ecosystem.Carbon1c <- merge(Ecosystem.Carbon1c,Soil.carbon[,c(3,12)],all.x=TRUE, by="Block.ID")

Ecosystem.Carbon2a <- merge(Soil.carbon,Deadwood.carbon[,c(4:6)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2b <- merge(Ecosystem.Carbon2a,Soil.texture[,c(4:8)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2c <- merge(Ecosystem.Carbon2b,Ecosystem.Carbon1c[,c(1,8,10:12)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2d <- merge(Ecosystem.Carbon2c,Trees.N.non[,c(1,7,3:6,8:11)],all.x=TRUE, by="Block.ID")

names(Ecosystem.Carbon1c)
names(Ecosystem.Carbon2d)
Ecosystem.CHerbTree <- Ecosystem.Carbon1c[,c(1:7,14:17,27,8,10:12,19:26,9,13)]
Ecosystem.CSoilDW <- Ecosystem.Carbon2d[,c(1:7,15:18,12,19:22,24:31,8,10,13)]
SE.Ecosystem.CSoilDW <- Ecosystem.Carbon2d[,c(1:3,9,11,14)]

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
names(Belowground.C)
Deadwood.C <- Belowground.C[,c(1:5,26)]

Belowground.C$BelowgroundC <- Belowground.C$SoilAC.kg_m2 + Belowground.C$SoilMC.kg_m2
# Checking for significans of Clay, MAP and Lanuse 
summary(lm(BelowgroundC~Clay.pip.per, data=Belowground.C))
summary(lm(BelowgroundC~MAP:Clay.pip.per, data=Belowground.C))
summary(lm(BelowgroundC~Landuse:Clay.pip.per, data=Belowground.C))

SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BelowgroundC <- cbind(aggregate(BelowgroundC~Region.x+MAP+Landuse,mean,data=Belowground.C),
                      aggregate(BelowgroundC~Region.x+MAP+Landuse,SE,data=Belowground.C)[4])
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

AbovegroundC <- cbind(aggregate(AbovegroundC~Region.x+MAP.mm_yr+landuse,mean,data=Aboveground.C),
                          aggregate(AbovegroundC~Region.x+MAP.mm_yr+landuse,SE,data=Aboveground.C)[4])
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
colnames(EcosystemC.SoilDW) <- c("Block.ID","Region.x","Vilde.block","landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency.2000_2017","Clay.pip.per","Silt.pip.per","Sand.pip.per","Class","mean.N.kg_m2","Total.basal.area_m2","TreeBM.kg_m2", "No.trees_m2","Shrubbiness","Small.N","Small.N.non","Large.N","Large.N.non" , "BM.Large.N.m2", "BM.Large.non.m2","BM.Small.N.m2","BM.Small.non.m2","Carbon.pool","C.amount","SE.C.amount")

EcosystemC.SoilDW <- EcosystemC.SoilDW[
  order(EcosystemC.SoilDW[,1], EcosystemC.SoilDW[,2] ),
  ]

data_long.CTreeHerb <- data_long.CTreeHerb[
  order(data_long.CTreeHerb[,1], data_long.CTreeHerb[,2] ),
  ]

# Creating one long dataset with all data
data_long.CTreeHerb$SE.C.amount <- c(NA*56)
names(data_long.CTreeHerb)
str(data_long.CTreeHerb)
names(EcosystemC.SoilDW)
str(EcosystemC.SoilDW)
Ecosystem.Carbon <- bind_rows(data_long.CTreeHerb, EcosystemC.SoilDW, id=NULL)

Ecosystem.Carbon <- Ecosystem.Carbon[
  order(Ecosystem.Carbon[,1], Ecosystem.Carbon[,2] ),
  ]

names(Ecosystem.Carbon)
Ecosystem.Carbon$Mean.N.kg_m2 <- rowSums(Ecosystem.Carbon[,c("mean.N.kg_m2.x","mean.N.kg_m2")], na.rm=TRUE)
Ecosystem.Carbon <- Ecosystem.Carbon[,c(1:11,13:27,29)]

# Make NA values for Tree variables to 0 
Ecosystem.Carbon$Total.basal.area_m2[is.na(Ecosystem.Carbon$Total.basal.area_m2)]<- 0
Ecosystem.Carbon$TreeBM.kg_m2[is.na(Ecosystem.Carbon$TreeBM.kg_m2)]<- 0
Ecosystem.Carbon$No.trees_m2[is.na(Ecosystem.Carbon$No.trees_m2)]<- 0
Ecosystem.Carbon$Shrubbiness[is.na(Ecosystem.Carbon$Shrubbiness)]<- 0
Ecosystem.Carbon$Small.N[is.na(Ecosystem.Carbon$Small.N)]<- 0
Ecosystem.Carbon$Small.N.non[is.na(Ecosystem.Carbon$Small.N.non)]<- 0
Ecosystem.Carbon$Large.N[is.na(Ecosystem.Carbon$Large.N)]<- 0
Ecosystem.Carbon$Large.N.non[is.na(Ecosystem.Carbon$Large.N.non)]<- 0
Ecosystem.Carbon$BM.Large.N.m2[is.na(Ecosystem.Carbon$BM.Large.N.m2)]<- 0
Ecosystem.Carbon$BM.Large.non.m2[is.na(Ecosystem.Carbon$BM.Large.non.m2)]<- 0
Ecosystem.Carbon$BM.Small.N.m2[is.na(Ecosystem.Carbon$BM.Small.N.m2)]<- 0
Ecosystem.Carbon$BM.Small.non.m2[is.na(Ecosystem.Carbon$BM.Small.non.m2)]<- 0
Ecosystem.Carbon$C.amount[is.na(Ecosystem.Carbon$C.amount)]<- 0

write.csv(Ecosystem.Carbon,file="Ecosystem carbon/Ecosystem.Carbon.csv")
write.csv(EcosystemC.SoilDW,file="Ecosystem carbon/Soil.DW.Carbon.Block.csv")
write.csv(data_long.CTreeHerb,file="Ecosystem carbon/Tree.Herb.Carbon.Block.csv")

### Ploting Ecosystem Carbon  #### 
Block.Eco.C <- read.csv("Ecosystem carbon/Ecosystem.Carbon.csv", head=T)
Belowground.full <- read.csv("Ecosystem carbon/Soil.data/Belowground.Carbon.csv", head=T)
Above_Below.C <- read.csv("Ecosystem carbon/Above_Below.C.csv",head=T)

# housekeeping 
str(Belowground.full)
Belowground.full$Fire_frequency.2000_2017.x <- as.numeric(Belowground.full$Fire_frequency.2000_2017.x)

Block.Eco.C$Carbon.pool<- factor(Block.Eco.C$Carbon.pool, levels = c("TreeC.kg_m2","HerbC.kg_m2", "DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))
levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")
Block.Eco.C$Region.x<- factor(Block.Eco.C$Region.x, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Belowground.full$Region<- factor(Belowground.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# Climate variable 
# Belowground full 
Belowground.full$MAP.mm_yr <- round(Belowground.full$MAP.mm_yr,digits = 2)
Belowground.full$climate <- as.factor(Belowground.full$MAP.mm_yr)
levels(Belowground.full$climate) <- c("Dry", "Dry" ,"Dry","IntDry", "IntWet", "Wet", "Wet")
Belowground.full$fMAP<-as.factor(Belowground.full$MAP.mm_yr)
levels(Belowground.full$fMAP) <- c("1295", "1279",  "1135", "856",  "755",  "717" ,"672")
levels(Belowground.full$landuse) <- c("Pasture","Wild")

# Block eco carbon 
Block.Eco.C$MAP.mm_yr <- round(Block.Eco.C$MAP.mm_yr,digits = 2)
Block.Eco.C$climate <- as.factor(Block.Eco.C$MAP.mm_yr)
levels(Block.Eco.C$climate) <- c("Dry", "Dry" ,"Dry","IntDry", "IntWet", "Wet", "Wet")

# Adding shrubbyness to my belowground data set 
Soil.min<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil Min-horizon")
Shrubbiness <- Soil.min[,c(2:4,16)]
colnames(Shrubbiness)[3] <- "Block"
colnames(Shrubbiness)[2] <- "Region"
Belowground.full <- full_join(Belowground.full,Shrubbiness)
Belowground.full$fShrubbiness <- as.factor(round(Belowground.full$Shrubbiness, digits = 0))
# Adding C:N ratio
Belowground.full$CN <- Belowground.full$tot.C.kg_m2 / Belowground.full$tot.N.kg_m2

# saving the datasets
write.csv(Belowground.full,file="Ecosystem carbon/Soil.data/Belowground.Carbon.csv")
write.csv(Block.Eco.C,file="Ecosystem carbon/Ecosystem.Carbon.csv")

# Look at shrubbiness and Phosphorous
Soil.properties <- read.csv("Ecosystem carbon/Soil.data/Soil.Properties.csv", head=T)
Soil.properties <- full_join(Soil.properties,Shrubbiness)
names(Soil.properties)
summary(lm(Shrubbiness~P,data=Soil.properties))

par(mfrow=c(1,1))
plot(Belowground.full$tot.C.kg_m2~Belowground.full$Shrubbiness)
plot(Shrubbiness~mean.N.kg_m2.y,data=Belowground.full)
plot(TreeBM.kg_m2~Sand,data=Belowground.full)
plot(tot.C.kg_m2~N.trees,data=Belowground.full)
plot(tot.C.kg_m2~Livestock.bm,data=Belowground.full)
plot(tot.C.kg_m2~livestock,data=Belowground.full)
plot(tot.C.kg_m2~log(BM.N.trees.m2),data=Belowground.full)
plot(tot.N.kg_m2~log(BM.N.trees.m2),data=Belowground.full)
plot(tot.C.kg_m2~Sand,data=Belowground.full)
plot(tot.N.kg_m2~log(BM.Large.N.m2),data=Belowground.full)
plot(tot.C.kg_m2~log(BM.Large.N.m2),data=Belowground.full)
plot(tot.C.kg_m2~climate,data=Belowground.full)
plot(tot.N.kg_m2~MAP.mm_yr,data=Belowground.full)
plot(tot.C.kg_m2~MAP.mm_yr,data=Belowground.full)
plot(CN~MAP.mm_yr, data=Belowground.full)
plot(CN~climate,data=Belowground.full)
plot(tot.N.kg_m2~livestock, data=Belowground.full, na.rm=T)
names(Belowground.full)

#### TOT ECOSYSTEM C ####

# Legend titeles
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

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
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=12,color="black")
        ,axis.title.x=element_text(size=12,color="black")
        ,axis.text.x=element_text(size=11,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm") # - because we want them inwards. 
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size=12,margin = margin(.5,.5,.5,.5, "mm"),hjust = .02)
        ,strip.text.y = element_blank()
        ,panel.spacing = unit(.1, "lines")
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=12)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm")) +
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

# Texture class 
EcosystemC.class<- ggplot(data = Block.Eco.C, aes(x = Class,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool))

EcosystemC.class  + xlab("Texture Class") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free") + 
  geom_boxplot(size=0.5)  + 
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

summary(lm(C.amount~landuse:Sand.pip.per,data=Soil.A.hor))
plot(C.amount~Class, data=Soil.A.hor)
summary(lm(C.amount~Class, data=Soil.A.hor))

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

# Soil C and Texture class 
A.hor.class<- ggplot(data = Soil.A.hor, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount,colour=Carbon.pool))

A.hor.class  + xlab("Texture Class") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Class,scales = "free") + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Soil A-horizon"),values=c("salmon4")) +
  theme_bw() + Lines_gone 

#### Plotting for thesis ####
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"
TitleRain<-"Rainfall (mm/year)"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

# Plotting C and N 

CN.plot <- ggplot(data = Belowground.full, aes(x = tot.N.kg_m2,y = tot.C.kg_m2,group = fMAP, colour= fMAP,shape= landuse))

CN.plot + xlab(expression(paste("Soil Nitrogen (kg", m^-2,")"))) +  ylab(expression(paste("Soil Carbon (kg", m^-2,")"))) +
  geom_point(size = 3,stroke=2, na.rm=T)+
  scale_color_manual(TitleRain,breaks = c("1295", "1279",  "1135", "856",  "755",  "717" ,"672"),values=c("#6460D9","#8495E9","#74DEDA", "#9EF5C1", "#F9EE5E","#F9EE5E","#E9F882")) + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  theme_bw() +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="white")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank() 
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.line.y = element_line(color="black", size = .5) 
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(5,5,5,5), "mm")
        ,axis.text.x = element_text(size=15,color="black")
        ,axis.text.y = element_text(size=15,color="black")
        ,axis.title.y=element_text(size=20,color="black")
        ,axis.title.x=element_text(size=20,color="black")
        ,legend.text=element_text(size=15)
        ,legend.title=element_text(size=15)
        ,legend.position = c(0.15,0.6)
        ,strip.text.x = element_text(size = 0.5, hjust=0.1,colour = "black")
        ,strip.background = element_rect(fill="transparent",colour=NA))

ggsave("Ecosystem carbon/Figures/CN.MAP.png",
       width= 20, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Plotting Fire and soil C 

Fire.MAP.plot <- ggplot(data = Belowground.full, aes(x = Fire_frequency.2000_2017.x, y = tot.C.kg_m2,group = fMAP, colour= fMAP,shape= landuse))

Fire.MAP.plot + xlab("Fire frequency (2000-2017)") +  ylab(expression(paste("Soil Carbon (kg", m^-2,")"))) +
  geom_point(size = 3,stroke=2, na.rm=T)+
  scale_color_manual(TitleRain,breaks = c("1295", "1279",  "1135", "856",  "755",  "717" ,"672"),values=c("#6460D9","#8495E9","#74DEDA", "#9EF5C1", "#F9EE5E","#F9EE5E","#E9F882")) + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  theme_bw() +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="white")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank() 
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.line.y = element_line(color="black", size = .5) 
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(5,5,5,5), "mm")
        ,axis.text.x = element_text(size=15,color="black")
        ,axis.text.y = element_text(size=15,color="black")
        ,axis.title.y=element_text(size=20,color="black")
        ,axis.title.x=element_text(size=20,color="black")
        ,legend.text=element_text(size=15)
        ,legend.title=element_text(size=15)
        ,legend.position = c(0.92,0.65)
        ,strip.text.x = element_text(size = 0.5, hjust=0.1,colour = "black")
        ,strip.background = element_rect(fill="transparent",colour=NA))

ggsave("Ecosystem carbon/Figures/Fire.MAP.png",
       width= 20, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#### Sketch fitted values
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE

#A:Specify covariate values for predictions
str(Belowground.full)
MyData <- expand.grid(Climate.kat=levels(Belowground.full$climate.kat),
                      Fire = seq(min(Belowground.full$Fire_frequency.2000_2017.x), max(Belowground.full$Fire_frequency.2000_2017.x), length = 25))

#B. Create X matrix with expand.grid
X <- model.matrix(~ Climate.kat+Fire_frequency.2000_2017.x+Climate.kat:Fire_frequency.2000_2017.x, data = MyData)
head(X)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(Nitlmer2)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X %*% vcov(Nitlmer2) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[3]<-"Com.N.conc"

#### Plot observed data versus prediction

# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(NutsO,aes(x=rain.sum, y=Com.N.conc))
RN<-RN+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F) # geom_ribbon gives you a error around the line. 
RN<-RN+geom_line(data=MyData,colour="red",alpha=.9,lwd=2,show.legend=F) # line of best fit - use this in the thesis - the predicted pattern based on the model. 
RN<-RN+geom_point(stats="identity",colour="grey50",fill="grey50",size=2.5)
RN<-RN+facet_wrap(~landuse, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(0,530), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0,0))
RN<-RN+scale_y_continuous(expand=c(0,0))
RN<-RN+ylab("Nitrogen concentration (%)")+xlab("Rainfall (mm)") # Adding x and ylabs to plot
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
RN<-RN+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1) 
RN




### Correlation of variables (numeric) #### 

# Want to do the analysis at block size 
Ecosystem.Carbon <- read.csv(file="Ecosystem Carbon/Ecosystem.Carbon.csv", head=T)
names(Ecosystem.Carbon)

Variables <- Ecosystem.Carbon %>%
  filter(Carbon.pool=="HerbC.kg_m2")

str(Ecosystem.Carbon)

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
names(Belowground.full)
str(Belowground.full)
names(Belowground.C)
Belowground.C$CN <- Belowground.C$tot.C.kg_m2 / Belowground.C$tot.N.kg_m2
MyVar1<-c("MAP.mm_yr","Sand","Fire_frequency.2000_2017","Shrubbiness","tot.N.kg_m2","CN")

MyVar2<-c("MAP.mm_yr","Sand.pip.per","Fire_frequency.2000_2017","Shrubbiness","tot.N.kg_m2","TreeBM.N","TreeBM.kg_m2","tot.C.kg_m2")

names(Soil.Ahor)
MyVar3<-c("CMAP.mm_yr","CSand","CFire_frequency.2000_2017","CShrubbiness","Ctot.N.kg_m2","CTreeBM.N","CTreeBM.kg_m2","Herb.C")

# Want to get these two in one matrix. 
pairs(Belowground.full[,MyVar1],lower.panel = panel.cor)
pairs(Belowground.C[,MyVar2],lower.panel = panel.cor)
pairs(Soil.Ahor[,MyVar3],lower.panel = panel.cor)
# If I want these values in a table:
Variables2 <- Belowground.C[,c(5,7,10,13,15,27,36,40)]
Variables1 <- Belowground.full[,c(10:12,15,16,22,24,26,41,43,46)] # first select the collums I want to use 
Mycor <- rcorr(as.matrix(Variables2), type="pearson") # Use the pearson correlation (r-value)
MycorR <- as.data.frame(round(Mycor$r, digits=3))
MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

#### LANDUSE and correlation ####
# Trees 
plot(TreeBM.kg_m2~landuse, data= Block.Eco.C) # COVARYING 
plot(Shrubbiness~landuse,data=Block.Eco.C) # not covarying 
plot(No.trees_m2~landuse, data= Block.Eco.C) # not covarying
plot(No.small.trees~landuse, data= Block.Eco.C) # not covarying 
plot(No.large.trees~landuse, data= Block.Eco.C)# not covarying 
plot(Small.N~landuse,data=Block.Eco.C) # not covarying
plot(Small.N.non~landuse,data=Block.Eco.C)# not covarying
plot(Large.N~landuse,data=Block.Eco.C)# not covarying
plot(Large.N.non~landuse,data=Block.Eco.C)# not covarying
plot(BM.Small.N.m2~landuse,data=Block.Eco.C)# not covarying
plot(BM.Small.non.m2~landuse,data=Block.Eco.C) # COVARYING, larger in P 
plot(BM.Large.N.m2~landuse,data=Block.Eco.C) # COVARYING, larger in W
plot(BM.Large.non.m2~landuse,data=Block.Eco.C) # COVARYING, larger in W
plot(TreeBM.N~landuse,data=Block.Eco.C) # not covarying 
plot(N.trees~landuse,data=Belowground.full) # not covarying 
plot(non.N.trees~landuse,data=Belowground.full) # not covarying 
plot(BM.N.trees.m2~landuse,data=Belowground.full) # not covarying 
plot(BM.non.N.trees.m2~landuse,data=Belowground.full) # not covarying 

# Soil properties 
plot(Sand.pip.per~landuse, data= Block.Eco.C) # not covarying
plot(mean.N.kg_m2.x~landuse,data=Belowground.full) # not covarying
# Site traits 
plot(MAP.mm_yr~landuse, data= Block.Eco.C) # not covarying
plot(Last.fire_yr~landuse, data= Block.Eco.C) # not covarying
plot(Fire_frequency.2000_2017~landuse, data= Block.Eco.C) # not covarying 
plot(livestock~landuse,data=Belowground.full) # COVARYING
plot(wild~landuse,data=Belowground.full) # not covarying 

plot(livestock~Region,data=Belowground.full)
plot(tot.C.kg_m2~Region,data=Belowground.full)
plot(tot.N.kg_m2~Region,data=Belowground.full)
plot(Clay~Region,data=Belowground.full)

par(mfrow=c(1,1))
levels(Belowground.full$Region)
plot(No.large.trees~Region, data= Block.Eco.C)
plot(C.amount ~ Region, data=Soil.A.hor)
N.trees <- aggregate(N.trees ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(N.trees ~landuse, sum,data=N.trees)
# landuse N.trees
# 1 Pasture      41
# 2    Wild      59
non.N.trees <- aggregate(non.N.trees ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(non.N.trees ~landuse, sum,data=non.N.trees)
# landuse non.N.trees
# 1 Pasture          36
# 2    Wild          30
No.trees_m2 <- aggregate(No.trees_m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(No.trees_m2 ~landuse, mean,data=No.trees_m2)
# landuse No.trees_m2
# 1 Pasture 0.007291667
# 2    Wild 0.002373333
BM.N.trees.m2 <- aggregate(BM.N.trees.m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(BM.N.trees.m2 ~landuse, mean,data=BM.N.trees.m2)
# landuse BM.N.trees.m2
# 1 Pasture    0.01182283
# 2    Wild    0.16568178
BM.non.N.trees.m2 <- aggregate(BM.non.N.trees.m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(BM.non.N.trees.m2 ~landuse, mean,data=BM.non.N.trees.m2)
# landuse BM.non.N.trees.m2
# 1 Pasture       0.006182991
# 2    Wild       0.040260107
TreeBM.kg_m2 <- aggregate(TreeBM.kg_m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(TreeBM.kg_m2 ~landuse, mean,data=TreeBM.kg_m2)
# landuse TreeBM.kg_m2
# 1 Pasture   0.02363048
# 2    Wild   0.21517161
### Data modelling/ analysis ####

Block.Eco.C <- read.csv("Ecosystem carbon/Ecosystem.Carbon.csv", head=T)
Belowground.full <- read.csv("Ecosystem carbon/Soil.data/Belowground.Carbon.csv", head=T)

Block.Eco.C$Region.x<- factor(Block.Eco.C$Region.x, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Belowground.full$Region<- factor(Belowground.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# About mixed effect models (nlme package)
# REML = restricted maximum likelihood estimation 
# Fixed effects influence the mean of y, while Random effects influence the variance of y 
# Use REML= F when looking at the fixed effects, maximum likelihood  - use this first when I want to figure out which model to use - when comparing models, then
# Use REML = T when looking at the random effects, and the parameter estimates - when I have found the model I want to use! 
# I have region and block.ID as random effects 
# Choose the model with the smallest AIC value 

# Creating dataframes for aboveground C and belowground C on block scale 

str(Block.Eco.C)
names(Block.Eco.C)
Block.Eco.C$TreeBM.N <- Block.Eco.C$BM.Large.N.m2 + Block.Eco.C$BM.Small.N.m2

# scaling some of the variables in Block.Eco.C 
Block.Eco.C$CMAP.mm_yr <- as.numeric(scale(Block.Eco.C$MAP.mm_yr))
Block.Eco.C$CSand <- as.numeric(scale(Block.Eco.C$Sand.pip.per))
Block.Eco.C$CFire_frequency.2000_2017 <- as.numeric(scale(Block.Eco.C$Fire_frequency.2000_2017))
Block.Eco.C$Cmean.N.kg_m2 <- as.numeric(scale(Block.Eco.C$Mean.N.kg_m2))
Block.Eco.C$CShrubbiness <- as.numeric(scale(Block.Eco.C$Shrubbiness))
Block.Eco.C$CTreeBM.kg_m2 <- as.numeric(scale(Block.Eco.C$TreeBM.kg_m2))
Block.Eco.C$CTreeBM.N <- as.numeric(scale(Block.Eco.C$TreeBM.N))

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

Belowground.C <- cbind(Soil.min[c(3:25,27,29:38)],Soil.Ahor[c(27)])
colnames(Belowground.C)[24] <- "Soil.MinHor" 
colnames(Belowground.C)[35] <- "Soil.AHor"
tot.N.kg_m2 <- aggregate(tot.N.kg_m2 ~ Region + Block.ID, mean, data=Belowground.full)
Belowground.C <- cbind(Belowground.C,tot.N.kg_m2[3])
Belowground.C$tot.C.kg_m2 <- Belowground.C$Soil.MinHor + Belowground.C$Soil.AHor

names(DW)
Aboveground.C <- cbind(DW[c(3:25,27,29:38)],Woody[c(27)],Herbaceous[c(27)])
colnames(Aboveground.C)[24] <- "DW"
colnames(Aboveground.C)[35] <- "Woody"
colnames(Aboveground.C)[36] <- "Herbaceous"
Aboveground.C <- cbind(Aboveground.C,tot.N.kg_m2[3])
Aboveground.C$tot.C.kg_m2 <- Aboveground.C$DW + Aboveground.C$Woody + Aboveground.C$Herbaceous

Soil.min <- cbind(Soil.min,tot.N.kg_m2[3])
Soil.Ahor <- cbind(Soil.Ahor,tot.N.kg_m2[3])
Herbaceous <- cbind(Herbaceous,tot.N.kg_m2[3])
Woody <- cbind(Woody,tot.N.kg_m2[3])
DW<- cbind(DW,tot.N.kg_m2[3])

names(Belowground.full)
summary(Belowground.full)
Belowground.full$CN <- Belowground.full$tot.C.kg_m2/Belowground.full$tot.N.kg_m2
max(Belowground.full$CN)
plot(CN~Region,data=Belowground.full)
# scaling tot.C to be able to compare estimates.. 
Belowground.C$Ctot.C.kg_m2 <- as.numeric(scale(Belowground.C$tot.C.kg_m2)) 
Aboveground.C$Ctot.C.kg_m2 <- as.numeric(scale(Aboveground.C$DW)) + as.numeric(scale(Aboveground.C$Herbaceous)) + as.numeric(scale(Aboveground.C$Woody))
Soil.min$Ctot.C.kg_m2 <- as.numeric(scale(Soil.min$C.amount)) 
Soil.Ahor$Ctot.C.kg_m2 <- as.numeric(scale(Soil.Ahor$C.amount))
Herbaceous$Ctot.C.kg_m2 <- as.numeric(scale(Herbaceous$C.amount)) 
DW$Ctot.C.kg_m2 <- as.numeric(scale(DW$C.amount)) 
Woody$Ctot.C.kg_m2 <- as.numeric(scale(Woody$C.amount)) 
Aboveground.C$Ctot.N.kg_m2 <- as.numeric(scale(Aboveground.C$tot.N.kg_m2))
Belowground.C$Ctot.N.kg_m2 <- as.numeric(scale(Belowground.C$tot.N.kg_m2))
Soil.min$Ctot.N.kg_m2 <- as.numeric(scale(Soil.min$tot.N.kg_m2))
Soil.Ahor$Ctot.N.kg_m2 <- as.numeric(scale(Soil.Ahor$tot.N.kg_m2))
Herbaceous$Ctot.N.kg_m2 <- as.numeric(scale(Herbaceous$tot.N.kg_m2))
DW$Ctot.N.kg_m2 <- as.numeric(scale(DW$tot.N.kg_m2))
Woody$Ctot.N.kg_m2 <- as.numeric(scale(Woody$tot.N.kg_m2))
Aboveground.C$CLast.fire_yr <- as.numeric(scale(Aboveground.C$Last.fire_yr))
# scaling some of the variables in belowground.full
Belowground.full$CMAP.mm_yr <- as.numeric(scale(Belowground.full$MAP.mm_yr))
Belowground.full$CSand <- as.numeric(scale(Belowground.full$Sand))
Belowground.full$CFire_frequency.2000_2017 <- as.numeric(scale(Belowground.full$Fire_frequency.2000_2017.x))
Belowground.full$Ctot.N.kg_m2 <- as.numeric(scale(Belowground.full$tot.N.kg_m2))
Belowground.full$CShrubbiness <- as.numeric(scale(Belowground.full$Shrubbiness))
Belowground.full$CTreeBM.kg_m2 <- as.numeric(scale(Belowground.full$TreeBM.kg_m2))
Belowground.full$Ctot.C.kg_m2 <- as.numeric(scale(Belowground.full$tot.C.kg_m2)) 
#### BELOWGROUND CARBON #### 
# Look at belowground C first (A + Min)
# Run a full model of all my fixed factors interactions with belowground C 
# James: modelaverages - mumin, dredge, averaging - you get a table.. 
#  To compare to aboveground ++ --> Then start to use the "Block.Eco.C" instead..? 
# 1. BLOCK SCALE  ####
summary(Belowground.C)#NA in fire variables
#Remove row with NA
Belowground.CnoNA<-Belowground.C[!is.na(Belowground.C$CFire_frequency.2000_2017),]
# 1. Global model for Belowground C univariate variables ####
Belowground.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 +  Ctot.N.kg_m2 + CTreeBM.kg_m2 + CSand + CShrubbiness + CTreeBM.N + 
                          (1|Region.x), data = Belowground.CnoNA, REML=F,
                        na.action=na.fail)
summary(Belowground.block)
drop1(Belowground.block,test="Chisq")
anova(Belowground.block)
AIC(Belowground.block) #22.29482
# With interactions
Belowground.block.update<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + #landuse + 
                                 CFire_frequency.2000_2017 +  Ctot.N.kg_m2 + CTreeBM.kg_m2 + #CSand + CShrubbiness + CTreeBM.N + 
                                 CTreeBM.kg_m2:CFire_frequency.2000_2017 + 
                                 Ctot.N.kg_m2:CMAP.mm_yr +
                          (1|Region.x), data = Belowground.CnoNA, REML=F,
                        na.action=na.fail)
summary(Belowground.block.update)
drop1(Belowground.block.update,test="Chisq")
anova(Belowground.block.update)
AIC(Belowground.block.update) #17.86313

# Model averaging: All possible models between null and global
modsetbelowB<-dredge(Belowground.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse) & !(CSand & Ctot.N.kg_m2 ))

modselbelowB<-model.sel(modsetbelowB) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowB<-model.avg(modselbelowB)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowB)#Importance of each variable  
summary(modavgbelowB)#Estimated coefficients given weighting

# Add interactions + remove ns univariate variables + get estimates REML=T
B1<-lmer(Ctot.C.kg_m2~CMAP.mm_yr + CFire_frequency.2000_2017 + Ctot.N.kg_m2 + CTreeBM.kg_m2 + 
           Ctot.N.kg_m2:CMAP.mm_yr + 
           #CTreeBM.kg_m2:CMAP.mm_yr + CTreeBM.kg_m2:Ctot.N.kg_m2 + CFire_frequency.2000_2017:Ctot.N.kg_m2 + 
           CTreeBM.kg_m2:CFire_frequency.2000_2017 + 
           (1|Region.x), data = Belowground.CnoNA, REML=F)
summary(B1)
drop1(B1,test="Chisq") # MAP:N is significant, Fire:TreeBM: 0.07, drop ns, now Fire:TreeBM also significant. 
AIC(B1) # 22.56718, drop ns interactions, AIC: 17.86313! 

# Alternative model 
B2 <-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + Ctot.N.kg_m2 + Ctot.N.kg_m2:CMAP.mm_yr + 
              (1|Region.x), data = Belowground.CnoNA, REML=F)
drop1(B2,test="Chisq")
AIC(B2) #23.7962 - not better.. 

# Remove interactions one by one  
B1a <- update(B1, .~. - CTreeBM.kg_m2:CFire_frequency.2000_2017 )  
B1b <- update(B1, .~. - Ctot.N.kg_m2:CMAP.mm_yr) 
B1c <- update(B1a, .~. - Ctot.N.kg_m2:CMAP.mm_yr)
anova(B1,B1a) # significant TreeBM:Fire 0.03152 *
anova(B1,B1b) # significant N:MAP 0.02296 *
anova(B1a,B1c)

# Removing N from model to look at sand! 
Belowground.block.red<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 +  #Ctot.N.kg_m2 + 
                          CTreeBM.kg_m2 + CSand + CShrubbiness + CTreeBM.N +
                          (1|Region.x), data = Belowground.CnoNA, REML=F,
                        na.action=na.fail)
summary(Belowground.block.red)
drop1(Belowground.block.red,test="Chisq")
anova(Belowground.block.red)
AIC(Belowground.block.red) #55.41308

# Model averaging: All possible models between null and global
modsetbelowB.red<-dredge(Belowground.block.red,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse))
modselbelowB.red<-model.sel(modsetbelowB.red) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowB.red<-model.avg(modselbelowB.red)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowB.red)#Importance of each variable
summary(modavgbelowB.red)#Estimated coefficients given weighting

# Updated model with interactions - no significant.. 
B1.red<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 +
                              CTreeBM.kg_m2 + CSand + 
               CMAP.mm_yr:CFire_frequency.2000_2017 + #CMAP.mm_yr:CTreeBM.kg_m2 + CMAP.mm_yr:CSand +
              # CFire_frequency.2000_2017:CTreeBM.kg_m2 + CFire_frequency.2000_2017:CTreeBM.kg_m2 + CTreeBM.kg_m2:CSand +
                              (1|Region.x), data = Belowground.CnoNA, REML=F,
                            na.action=na.fail)
summary(B1.red)
drop1(B1.red,test="Chisq")
anova(B1.red)
AIC(B1.red) #50.99106, removing TreeBM: 52.52395

# 2. Global model for A-hor C univariate variables ####
names(Soil.Ahor)
Soil.Ahor <- cbind(Soil.Ahor,Herbaceous[40])
colnames(Soil.Ahor)[42] <- "Herb.C"
Soil.Ahor.CnoNA<-Soil.Ahor[!is.na(Soil.Ahor$CFire_frequency.2000_2017),]
Ahor.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness + Herb.C + CTreeBM.N + CMAP.mm_yr:CSand + 
                          (1|Region.x),data = Soil.Ahor.CnoNA, REML=F,
                        na.action=na.fail)

summary(Ahor.block)
drop1(Ahor.block,test="Chisq")  
anova(Ahor.block)
AIC(Ahor.block) #65.20516

# Model averaging: All possible models between null and global
modsetbelowA<-dredge(Ahor.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse)&!(CSand & Herb.C)&!(Herb.C & CTreeBM.N))
modselbelowA<-model.sel(modsetbelowA) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA<-model.avg(modselbelowA)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA)#Importance of each variable
write.table(importance(modavgbelowA),file="Ecosystem carbon/importanceAhor.txt")
summary(modavgbelowA)#Estimated coefficients given weighting
summary(modavgbelowA)$coefmat.full # Full average 
write.table(summary(modavgbelowA)$coefmat.subset, file="Ecosystem carbon/con.avg.Ahor.txt") # conditional average - I will try first with this.. 

# 3. Global model for Mineral hor C univariate variables ####
Soil.min.CnoNA<-Soil.min[!is.na(Soil.min$CFire_frequency.2000_2017),]
Min.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness + CTreeBM.N + CMAP.mm_yr:CSand +
                          (1|Region.x),data = Soil.min.CnoNA, REML=F,
                        na.action=na.fail)

summary(Min.block)
drop1(Min.block,test="Chisq") # MAP,Fire,N,TreeBM
anova(Min.block)
AIC(Min.block) #59.16717
names(Belowground.C)

# Model averaging: All possible models between null and global
modsetbelowM<-dredge(Min.block,trace = TRUE, rank = "AICc", REML = FALSE,
                    subset=!(CTreeBM.kg_m2 & landuse))
modselbelowM<-model.sel(modsetbelowM) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowM<-model.avg(modselbelowM)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowM)#Importance of each variable
write.table(importance(modavgbelowM),file="Ecosystem carbon/importanceMinhor.txt")
summary(modavgbelowM)#Estimated coefficients given weighting
write.table(summary(modavgbelowM)$coefmat.subset, file="Ecosystem carbon/con.avg.MinHor.txt")

# simple comparison of A and min 
par(mfrow=c(1,2))
plot(Ctot.C.kg_m2~CMAP.mm_yr, data=Soil.min)
plot(Ctot.C.kg_m2~CMAP.mm_yr, data=Soil.Ahor)
plot(Ctot.C.kg_m2~CFire_frequency.2000_2017, data=Soil.min)
plot(Ctot.C.kg_m2~CFire_frequency.2000_2017, data=Soil.Ahor)
plot(Ctot.C.kg_m2~CTreeBM.kg_m2, data=Soil.min)
plot(Ctot.C.kg_m2~CTreeBM.kg_m2, data=Soil.Ahor)
plot(Ctot.C.kg_m2~CSand, data=Soil.min)
plot(Ctot.C.kg_m2~CSand, data=Soil.Ahor)

# 4. Global model for Herbs ####
Herbaceous.CnoNA<-Herbaceous[!is.na(Herbaceous$CFire_frequency.2000_2017),]
Herbaceous.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness + CTreeBM.N + CMAP.mm_yr:CSand + 
                   (1|Region.x),data = Herbaceous.CnoNA, REML=F,
                 na.action=na.fail)

summary(Herbaceous.block)
drop1(Herbaceous.block,test="Chisq")  
anova(Herbaceous.block)
AIC(Herbaceous.block) #44.56924

# Model averaging: All possible models between null and global
modsetaboveH<-dredge(Herbaceous.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse))
modselaboveH<-model.sel(modsetaboveH) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveH<-model.avg(modselaboveH)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveH)#Importance of each variable
write.table(importance(modavgaboveH),file="Ecosystem carbon/importanceaboveH.txt")
summary(modavgaboveH)#Estimated coefficients given weighting
write.table(summary(modavgaboveH)$coefmat.subset, file="Ecosystem carbon/con.avg.H.txt") 

# 5. Global model for DW #### 
DW.CnoNA<-DW[!is.na(DW$CFire_frequency.2000_2017),]
DW.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness + CTreeBM.N + CMAP.mm_yr:CSand + 
                   (1|Region.x),data = Soil.Ahor.CnoNA, REML=F,
                 na.action=na.fail)

summary(DW.block)
drop1(DW.block,test="Chisq")  
anova(DW.block)
AIC(DW.block) #65.25896

# Model averaging: All possible models between null and global
modsetaboveDW<-dredge(DW.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse))
modselaboveDW<-model.sel(modsetaboveDW) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveDW<-model.avg(modselaboveDW)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveDW)#Importance of each variable
write.table(importance(modavgaboveDW),file="Ecosystem carbon/importanceaboveDW.txt")
#importance$Variable <- c("MAP","Tree biomass (N.fix)","Sand","Fire frequency","Shrubbiness","Land-use","Tree biomass","MAP:Sand")
summary(modavgaboveDW)#Estimated coefficients given weighting
write.table(summary(modavgaboveDW)$coefmat.subset, file="Ecosystem carbon/con.avg.DW.txt") 
# 6. Global model for Woody #### 
Woody.CnoNA<-Woody[!is.na(Woody$CFire_frequency.2000_2017),]
Woody.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CSand + CMAP.mm_yr:CSand + 
                   (1|Region.x),data = Woody.CnoNA, REML=F,
                 na.action=na.fail)

summary(Woody.block)
drop1(Woody.block,test="Chisq")  
anova(Woody.block)
AIC(Woody.block) #69.92378

# Model averaging: All possible models between null and global
modsetaboveW<-dredge(Woody.block,trace = TRUE, rank = "AICc", REML = FALSE)
modselaboveW<-model.sel(modsetaboveW) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveW<-model.avg(modselaboveW)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveW)#Importance of each variable
write.table(importance(modavgaboveW),file="Ecosystem carbon/importanceaboveW.txt")
#importance$Variable <- c("MAP","Tree biomass (N.fix)","Sand","Fire frequency","Shrubbiness","Land-use","Tree biomass","MAP:Sand")
summary(modavgaboveW)#Estimated coefficients given weighting
write.table(summary(modavgaboveW)$coefmat.subset, file="Ecosystem carbon/con.avg.W.txt") 

# 7. Global model for Aboveground C ####
summary(Aboveground.C)#NA in fire variables
#Remove row with NA
Aboveground.CnoNA<-Aboveground.C[!is.na(Aboveground.C$CFire_frequency.2000_2017),]
# Add interactions from below.. and remove the least important variables. did not affect the best models..  
Aboveground.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + 
                          landuse + CFire_frequency.2000_2017 + Ctot.N.kg_m2 + 
                          CTreeBM.kg_m2 + CSand + CShrubbiness + 
                          CTreeBM.N + (1|Region.x),data = Aboveground.CnoNA, REML=F, na.action=na.fail)

summary(Aboveground.block)
drop1(Aboveground.block,test="Chisq") # TreeBM, TreeBM.N, landuse, sand.. 
anova(Aboveground.block)
AIC(Aboveground.block) #88.76742

# Model averaging: All possible models between null and global
modsetabove<-dredge(Aboveground.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse) & !(CSand & Ctot.N.kg_m2))
modselabove<-model.sel(modsetabove) #Model selection table giving AIC, deltaAIC and weighting
modavgabove<-model.avg(modselabove)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgabove)#Importance of each variable
summary(modavgabove)#Estimated coefficients given weighting

# IF i remove treeBM.. 
AB1<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + 
                          landuse + CFire_frequency.2000_2017 + Ctot.N.kg_m2 + 
                        + CSand + CShrubbiness + 
                         (1|Region.x),data = Aboveground.CnoNA, REML=F, na.action=na.fail)

summary(AB1)
drop1(AB1,test="Chisq") # landuse, MAP... 
anova(AB1)
AIC(AB1) #112.927

# Model averaging: All possible models between null and global
modsetAB<-dredge(AB1,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & Ctot.N.kg_m2))
modselAB<-model.sel(modsetAB) #Model selection table giving AIC, deltaAIC and weighting
modavgAB<-model.avg(modsetAB)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgAB)#Importance of each variable
summary(modavgAB)#Estimated coefficients given weighting

# 8. From belowground.full fine scale ####

# Model averaging 
summary(Belowground.full.CnoNA)
Belowground.full.CnoNA<-Belowground.full[!is.na(Belowground.full$CFire_frequency.2000_2017),]

Belowground <-lmer(Ctot.C.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 +  Ctot.N.kg_m2 + CTreeBM.kg_m2 + CSand + CShrubbiness + (1|Region), data = Belowground.full.CnoNA, REML=F, na.action=na.fail)

modsetbelow.full<-dredge(Belowground,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse) & !(CSand & Ctot.N.kg_m2 ))
modselbelow.full<-model.sel(modsetbelow.full) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.full<-model.avg(modselbelow.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.full)#Importance of each variable
summary(modavgbelow.full)#Estimated coefficients given weighting

B.full<-lmer(Ctot.C.kg_m2 ~ CMAP.mm_yr + #landuse + 
               CFire_frequency.2000_2017 +  Ctot.N.kg_m2 + CTreeBM.kg_m2 + #CSand 
              # CShrubbiness + 
               (1|Region),data = Belowground.full.CnoNA, REML=F, na.action=na.fail)

summary(B.full)
drop1(B.full,test="Chisq")
anova(B.full)
AIC(B.full) #105.904

# Add interactions: 
B1a <- update(B1, .~. +CMAP.mm_yr:CSand) # ns 
anova(B1a,B1)
AIC(B1a) #50.82971
B1b <- update(B1, .~. +tot.N.kg_m2:CMAP.mm_yr ) # ns 
anova(B1b,B1)
AIC(B1b) #50.75608
B1c <- update(B1, .~. + CMAP.mm_yr:Shrubbiness) # ns 
anova(B1c,B1)
AIC(B1c) #50.79712
B1d <- update(B1, .~. + TreeBM.kg_m2:CFire_frequency.2000_2017.x) # ns 
anova(B1d,B1)
AIC(B1d) #50.3881

# Removing N 
Belowground.red <-lmer(Ctot.C.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 +  #Ctot.N.kg_m2 + 
                     CTreeBM.kg_m2 + CSand + CShrubbiness + (1|Region),
                   data = Belowground.full.CnoNA, REML=T, na.action=na.fail)

modsetbelow.red<-dredge(Belowground.red,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse))
modselbelow.red<-model.sel(modsetbelow.red) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.red<-model.avg(modselbelow.red)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.red)#Importance of each variable
summary(modavgbelow.red)#Estimated coefficients given weighting

BR1 <-lmer(Ctot.C.kg_m2 ~ #CMAP.mm_yr + landuse +
             CFire_frequency.2000_2017 + #CTreeBM.kg_m2 + 
             #CSand + 
             #CShrubbiness + 
             (1|Region),data = Belowground.full.CnoNA, REML=T, na.action=na.fail)

summary(BR1)
drop1(BR1,test="Chisq")
anova(BR1)
AIC(BR1) #241.7772, only fire: 234.0106, fire+ sand: 236.9274, fire+ shrubs: 238.6526

# FULL model selection without N first to look at interactions. 
Belowground.tot<-lmer(tot.C.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2  + CSand + CShrubbiness + #tot.N.kg_m2 + 
                        landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017  +  landuse:CSand + landuse:CShrubbiness + #tot.N.kg_m2:landuse + 
                        CMAP.mm_yr:CSand +  CTreeBM.kg_m2:CSand + #tot.N.kg_m2:CSand + 
                        CFire_frequency.2000_2017:CSand +  CShrubbiness:CSand +
                        CTreeBM.kg_m2:CMAP.mm_yr +  #tot.N.kg_m2:CMAP.mm_yr + 
                        CFire_frequency.2000_2017:CMAP.mm_yr + Shrubbiness:CMAP.mm_yr +
                        #TreeBM.kg_m2:tot.N.kg_m2 +  CFire_frequency.2000_2017:tot.N.kg_m2 + 
                        CTreeBM.kg_m2:CFire_frequency.2000_2017 +#BM.N.trees.m2: tot.N.kg_m2 + Shrubbiness:CFire_frequency.2000_2017 + Shrubbiness:tot.N.kg_m2 + 
                        (1|Region), data = Belowground.full.CnoNA, REML=F)

summary(Belowground.tot)
drop1(Belowground.tot,test="Chisq")
anova(Belowground.tot)
AIC(Belowground.tot) #286.6673

# Reduce model: 
summary(Belowground.full)
Belowground.red<-lmer(tot.C.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2  +  CSand + CShrubbiness + 
                        landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017  + landuse:CSand + landuse:CShrubbiness + #tot.N.kg_m2:landuse + 
                        #CMAP.mm_yr:CSand +  CTreeBM.kg_m2:CSand + tot.N.kg_m2:CSand + 
                        #CFire_frequency.2000_2017:CSand +  CShrubbiness:CSand +
                        #CTreeBM.kg_m2:CMAP.mm_yr +  #tot.N.kg_m2:CMAP.mm_yr + 
                        #CFire_frequency.2000_2017:CMAP.mm_yr + Shrubbiness:CMAP.mm_yr +
                        #TreeBM.kg_m2:tot.N.kg_m2 +  CFire_frequency.2000_2017:tot.N.kg_m2 + 
                        #CTreeBM.kg_m2:CFire_frequency.2000_2017 +#BM.N.trees.m2: tot.N.kg_m2 + Shrubbiness:CFire_frequency.2000_2017 + Shrubbiness:tot.N.kg_m2 + 
                        (1|Region), data = Belowground.full.CnoNA, REML=F, na.action=na.fail)
summary(Belowground.red)
drop1(Belowground.red,test="Chisq") 
anova(Belowground.red) 
AIC(Belowground.red) # 286.6673, new AIC better: 277.4945

BF1<-lmer(tot.C.kg_m2 ~ -1 + landuse +  CShrubbiness +landuse:CShrubbiness + 
                          (1|Region), data = Belowground.full.CnoNA, REML=F, na.action=na.fail)
summary(BF1)
drop1(BF1,test="Chisq") 
anova(BF1) 
AIC(BF1) #291.1328

# distribution of residuals 
E2 <- resid(Belowground.red, type ="pearson") 
F2 <- fitted(Belowground.red)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # None of the fitted values <0
abline(h = 0, lty = 2, col = 1)  # Good

modsetbelow.tot.red<-dredge(Belowground.red,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse))
modselbelow.tot.red<-model.sel(modsetbelow.tot.red) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.tot.red<-model.avg(modsetbelow.tot.red)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.tot.red)#Importance of each variable
summary(modavgbelow.tot.red)#Estimated coefficients given weighting

# Exploring alternative models 

# Adding livestock
names(Belowground.full)
Belowground.red <- Belowground.full[,c(3:7,10:13,16:18,22:28,31,45,47,49:56)]
Belowground.red <- Belowground.red[Belowground.red$Region!="Ikorongo",]
Belowground.red <- Belowground.red[Belowground.red$Region!="Park Nyigoti",]
Belowground.red <- Belowground.red[Belowground.red$Region!="Seronera",]
summary(Belowground.red)
Belowground.red <- na.omit(Belowground.red)
Belowground.red <- droplevels(Belowground.red)
# Belowground.red[is.na(Belowground.red)] <- 0

Belowground2<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + total.dung + CFire_frequency.2000_2017 + #Ctot.N.kg_m2 + 
                     CTreeBM.kg_m2  + CSand + CShrubbiness +
                     #livestock:CMAP.mm_yr + livestock:CFire_frequency.2000_2017  + tot.N.kg_m2:livestock + livestock:CSand + livestock:Shrubbiness +
                     #CMAP.mm_yr:CSand +  TreeBM.kg_m2:CSand + livestock:CSand + tot.N.kg_m2:CSand + CFire_frequency.2000_2017.x:CSand + Shrubbiness:CSand +
                     #TreeBM.kg_m2:CMAP.mm_yr +  tot.N.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017.x:CMAP.mm_yr +  Shrubbiness:CMAP.mm_yr +
                     #TreeBM.kg_m2:tot.N.kg_m2 +  CFire_frequency.2000_2017.x:tot.N.kg_m2 + TreeBM.kg_m2:CFire_frequency.2000_2017.x + Shrubbiness:CFire_frequency.2000_2017.x + Shrubbiness:tot.N.kg_m2 + 
                     (1|Region), data = Belowground.red, REML=F)
AIC(Belowground2) #46.6168
summary(Belowground2)
drop1(Belowground2,test="Chisq")

# Reduce model to what is significant
Belowground2.red<-lmer(tot.C.kg_m2~ livestock + tot.N.kg_m2 + Shrubbiness +
                         livestock:tot.N.kg_m2 + 
                         livestock:Shrubbiness  +
                         (1|Region), data = Belowground.red, REML=T)
AIC(Belowground2.red) #119.3093
summary(Belowground2.red)
drop1(Belowground2.red,test="Chisq") # ns 

# Only Nitrogen and MAP 
N.MAP<-lmer(tot.C.kg_m2~ CMAP.mm_yr + tot.N.kg_m2 + CMAP.mm_yr:tot.N.kg_m2 + (1|Region), data = Belowground.full, REML=T)
AIC(N.MAP) # 125.4687 better.. 
N.MAPa<- update(N.MAP, .~. -tot.N.kg_m2:MAP.mm_yr)
N.MAPb<- update(N.MAPa, .~. -tot.N.kg_m2)
N.MAPc<- update(N.MAPa, .~. -MAP.mm_yr)

anova(N.MAP,N.MAPa)
anova(N.MAPa,N.MAPb)
anova(N.MAPa,N.MAPc)
#                  Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#Belowground2red3   7 122.34 141.37 -54.169   108.34 26.892      1  2.152e-07 *** # the interaction is significant 
#Belowground2red3a  6 147.23 163.54  -67.615   135.23 158.64      1  < 2.2e-16 *** # Nitrogen is significant
#Belowground2red3a  6 147.23 163.54 -67.615   135.23 5.1323      1    0.02348 * # MAP is significant (but nitrogen more significant)

# Plotting the data 
# Estimated values for plotting C ~ Fire:MAP 
Fire.MAP<-lmer(tot.C.kg_m2~ CFire_frequency.2000_2017 + climate.kat + CFire_frequency.2000_2017:climate.kat + (1|Region), data = Belowground.full, REML=T)
summary(Fire.MAP)
AIC(Fire.MAP)
drop1(Fire.MAP,test="Chisq")

#interaction plot
Belowground.full$fMAP <- as.factor(Belowground.full$MAP.mm_yr)
xyplot(tot.C.kg_m2~tot.N.kg_m2|fMAP,data=Belowground.full)
xyplot(tot.C.kg_m2~Shrubbiness|fMAP,data=Belowground.full)
xyplot(tot.C.kg_m2~Sand|fMAP,data=Belowground.full)
xyplot(tot.C.kg_m2~Shrubbiness|climate,data=Belowground.full)
xyplot(tot.C.kg_m2~TreeBM.kg_m2|as.factor(Fire_frequency.2000_2017.x),data=Belowground.full)
xyplot(tot.C.kg_m2~Fire_frequency.2000_2017.x|climate,data=Belowground.full)
xyplot(tot.C.kg_m2~Fire_frequency.2000_2017.x|landuse,data=Belowground.full)
xyplot(tot.C.kg_m2~Shrubbiness|landuse,data=Belowground.full)
plot(tot.C.kg_m2~Sand, data=Belowground.full)
plot(tot.C.kg_m2~Fire_frequency.2000_2017.x,data=Belowground.full)
plot(tot.C.kg_m2~TreeBM.kg_m2,data=Belowground.full)

with(Belowground.full, {interaction.plot(tot.N.kg_m2,MAP.mm_yr,tot.C.kg_m2,
                                         xlab = "Nitrogen",
                                         ylab = " Carbon",
                                         fun=mean)})

Belowground.full$fMAP <- as.factor(Belowground.full$MAP.mm_yr)
xyplot(tot.C.kg_m2~tot.N.kg_m2|fMAP,data=Belowground.full)

# CN 

CNBelowground.tot<-lmer(CN~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017.x +TreeBM.kg_m2 + BM.N.trees.m2 + CSand + Shrubbiness +
                          landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017.x + landuse:CSand + landuse:BM.N.trees.m2 + landuse:Shrubbiness +
                          CMAP.mm_yr:CSand +  TreeBM.kg_m2:CSand + landuse:CSand +  CFire_frequency.2000_2017.x:CSand + BM.N.trees.m2:CSand + Shrubbiness:CSand +
                          TreeBM.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017.x:CMAP.mm_yr + BM.N.trees.m2:CMAP.mm_yr + Shrubbiness:CMAP.mm_yr + 
                          TreeBM.kg_m2:CFire_frequency.2000_2017.x + Shrubbiness:CFire_frequency.2000_2017.x +
                          (1|Region), data = Belowground.full, REML=T)
summary(CNBelowground.tot)
drop1(CNBelowground.tot,test="Chisq")
AIC(CNBelowground.tot) #353.3435

CNBelowground.red<-lmer(CN~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017.x +TreeBM.kg_m2 + 
                          CSand + Shrubbiness +
                          landuse:CFire_frequency.2000_2017.x + 
                          CMAP.mm_yr:CSand +  
                          TreeBM.kg_m2:CSand + 
                          Shrubbiness:CSand +
                          Shrubbiness:CMAP.mm_yr + 
                          TreeBM.kg_m2:CFire_frequency.2000_2017.x + 
                          Shrubbiness:CFire_frequency.2000_2017.x +
                          (1|Region), data = Belowground.full, REML=T)
summary(CNBelowground.red)
drop1(CNBelowground.red,test="Chisq")
AIC(CNBelowground.red) # 378.0542 worse.. 

CNBelowground.red2<-lmer(CN~ CMAP.mm_yr + CFire_frequency.2000_2017.x +TreeBM.kg_m2 + Shrubbiness +
                           Shrubbiness:CMAP.mm_yr + 
                           TreeBM.kg_m2:CFire_frequency.2000_2017.x +
                           (1|Region), data = Belowground.full, REML=T)
summary(CNBelowground.red2)
drop1(CNBelowground.red2,test="Chisq")
# Df    AIC    LRT  Pr(Chi)   
# <none>                                      351.85                   
# CMAP.mm_yr:Shrubbiness                    1 352.90 3.0542 0.080528 . 
# CFire_frequency.2000_2017.x:TreeBM.kg_m2  1 356.56 6.7181 0.009544 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
AIC(CNBelowground.red2) # 364.0556

CNBelowground<-lmer(CN~ #CFire_frequency.2000_2017.x + 
                      TreeBM.kg_m2 + CFire_frequency.2000_2017.x:TreeBM.kg_m2 + 
                      (1|Region), data = Belowground.full, REML=T)
summary(CNBelowground)
drop1(CNBelowground,test="Chisq")
AIC(CNBelowground) # 360.9615, remove Fire, AIC sligtly better: 360.2276

# PLOT Importance #### 
importance.Ahor<- read.table("Ecosystem carbon/importanceAhor.txt")
importance.MinHor<- read.table("Ecosystem carbon/importanceMinHor.txt")
importance.H<- read.table("Ecosystem carbon/importanceaboveH.txt")
importance.DW<- read.table("Ecosystem carbon/importanceaboveDW.txt")
importance.W<- read.table("Ecosystem carbon/importanceaboveW.txt")

# I now have all my importance variables 
colnames(importance.Ahor)<-'Ahor'
colnames(importance.MinHor)<-'MinHor'
colnames(importance.H)<-'Herbs'
colnames(importance.DW)<-'DW'
colnames(importance.W)<-'Woody'

rownames(importance.Ahor)<-rownames(importance.Ahor)[order(rownames(importance.Ahor))]
rownames(importance.MinHor)<-rownames(importance.MinHor)[order(rownames(importance.MinHor))]
rownames(importance.H)<-rownames(importance.H)[order(rownames(importance.H))]
rownames(importance.DW)<-rownames(importance.DW)[order(rownames(importance.DW))]
rownames(importance.W)<-rownames(importance.W)[order(rownames(importance.W))]

rownames(importance.Ahor) <- (c("Fire frequency","MAP","MAP:Sand","Sand","Shrubbiness","Tree biomass","Biomass N.fix trees","Herbaceous biomass","Land-use"))

rownames(importance.MinHor) <- (c("Fire frequency","MAP","MAP:Sand","Sand","Shrubbiness","Tree biomass","Biomass N.fix trees","Land-use"))

rownames(importance.H) <- (c("Fire frequency","MAP","MAP:Sand","Sand","Shrubbiness","Tree biomass","Biomass N.fix trees","Land-use"))

rownames(importance.DW) <- (c("Fire frequency","MAP","MAP:Sand","Sand","Shrubbiness","Tree biomass","Biomass N.fix trees","Land-use"))

rownames(importance.W) <- (c("Fire frequency","MAP","MAP:Sand","Sand","Land-use"))

# Colours 
# MAP: deepskyblue4
# Tree bm: darkolivegreen4
# N-fix tree bm: darkolivegreen3
# Fire: darkorange2
# Shrubbiness: darkseagreen4
# Landuse: goldenrod3
# Sand: darkgray
# MAP:Sand: cyan4
# Herb biomass: forestgreen
par(mar=c(5,1,1,7))

Legend.text <- c("MAP","Biomass N.fix trees","Sand","Shrubbiness","Fire frequency","Herbaceous biomass","Land-use","Tree biomass","MAP:Sand")
# Plot A-hor
barplot(t(as.matrix(importance.Ahor)), horiz=T,las=1,xlab='Relative variable importance',main='Soil A-horizon Carbon',cex.main = 1,axisname=F,col=c("cyan4",'darkolivegreen4','goldenrod3',"forestgreen","darkorange2","darkseagreen4","darkgray","darkolivegreen3","deepskyblue4"),beside=T, legend=rownames(importance.Ahor), args.legend=list(y=nrow(importance.Ahor)+9,x=+0.32))

# Plot Min-hor
barplot(t(as.matrix(importance.Min)), horiz=T,las=1,xlab='Relative variable importance',main='Soil Mineral-horizon Carbon',cex.main = 1,axisname=F,col=c('darkolivegreen3','darkseagreen4','darkolivegreen4',"goldenrod3","cyan4","darkorange2","deepskyblue4","darkgray"),beside=T)

# Plot Herb 
barplot(t(as.matrix(importance.Herb)), horiz=T,las=1,xlab='Relative variable importance',main='Herbaceous Carbon',cex.main = 1,axisname=F,col=c('cyan4','goldenrod3','deepskyblue4',"darkseagreen4","darkgray","darkolivegreen4","darkorange2","darkolivegreen3"),beside=T)

# Plot DW 
barplot(t(as.matrix(importance.DW)), horiz=T,las=1,xlab='Relative variable importance',main='Dead Wood Carbon',col=c('cyan4','darkolivegreen4','goldenrod3',"darkseagreen4","darkorange2","darkgray","darkolivegreen3","deepskyblue4"),beside=T)

#Plot Woody
barplot(t(as.matrix(importance.Woody)), horiz=T,las=1,xlab='Relative variable importance',main='Woody Carbon',cex.main = 1,axisname=F,col=c('goldenrod3','cyan4','deepskyblue4',"darkgray","darkorange2"),beside=T)

# PLOT variable coefficients from model averages ####
con.avg.Ahor<- read.table("Ecosystem carbon/con.avg.Ahor.txt")
con.avg.MinHor<- read.table("Ecosystem carbon/con.avg.MinHor.txt")
con.avg.H<- read.table("Ecosystem carbon/con.avg.H.txt")
con.avg.DW<- read.table("Ecosystem carbon/con.avg.DW.txt")
con.avg.W<- read.table("Ecosystem carbon/con.avg.W.txt")

#Reorder rows
con.avg.Ahor<-con.avg.Ahor[order(rownames(con.avg.Ahor)),]
con.avg.MinHor<-con.avg.MinHor[order(rownames(con.avg.MinHor)),]
con.avg.H<-con.avg.H[order(rownames(con.avg.H)),]
con.avg.DW<-con.avg.DW[order(rownames(con.avg.DW)),]
con.avg.W<-con.avg.W[order(rownames(con.avg.W)),]

# Separate into SE and Estimates 

#macplot_est<-rbind(Ahor=con.avg.Ahor[,1],Minhor=con.avg.MinHor[,1],Herb=con.avg.H[,1],DW=con.avg.DW[,1])
#colnames(macplot_est)<-rownames(fullcoefsr1)
#macplot_est<-macplot_est[,c(1,5,7,2,8,9,6,2,3)]
#colnames(macplot_est)<-c('Intercept','Predator diversity (R)','NDVI','Habitat heterogeneity','Topographic heterogeneity','Winter minimum temperature','Ice-free history','Region:Eur vs. Arc','Region: NA vs. Arc')
#macplot_se<-rbind(SR=fullcoefsr1[,2],PD=fullcoefpd1[,2],FD=fullcoeffd1[,2],FDPD=fullcoeffdpd1[,2])
#macplot_se<-macplot_se[,c(1,5,7,2,8,9,6,2,3)]
#macplot_p<-rbind(SR=fullcoefsr1[,5],PD=fullcoefpd1[,5],FD=fullcoeffd1[,5],FDPD=fullcoeffdpd1[,5])
#macplot_p<-macplot_p[,c(1,5,7,2,8,9,6,2,3)]
#macplot_p[macplot_p>=0.05]<-1
#macplot_p[macplot_p<0.05]<-16

barplot(t(as.matrix(importance.Ahor)), horiz=T,las=1,xlab='Relative variable importance',main='Soil A-horizon Carbon',cex.main = 1,axisname=F,col=c("cyan4",'darkolivegreen4','goldenrod3',"forestgreen","darkorange2","darkseagreen4","darkgray","darkolivegreen3","deepskyblue4"),beside=T, legend=rownames(importance.Ahor), args.legend=list(y=nrow(importance.Ahor)+9,x=+0.32))

b1<-barplot(t(as.matrix(con.avg.Ahor[,2])),horiz=T,las=1,xlab='Model averaged coefficients',beside=T, points(con.avg.Ahor[,2]),pch=con.avg.Ahor[,2],col=c('black','orange','blue','pink4')) 

arrows(macplot_est[,2:ncol(macplot_est)]+1.96*macplot_se[,2:ncol(macplot_est)],b1,
       macplot_est[,2:ncol(macplot_est)]-1.96*macplot_se[,2:ncol(macplot_est)],b1,
       angle=90,length=0.05,code=3,col=colsImp)#,col=c('black','orange','blue','pink4'))
abline(v=0,lty=2)
legend('topr',pch=16,col=rev(colsImp),legend=rev(c('Species','Phylogenetic','Functional','Functional:Phylogenetic')),title='Diversity',cex=0.7)
legend('topl',pch=c(1,16),col=colsImp[4],c('P>=0.05','P<0.05'),cex=0.7,title='Significance')


x11(12,6)
tiff('S:\\DISENTANGLE\\WP3\\ArcticHerbivoreFDPD\\PhylogeneticFunctionalAnalysisPicanteR\\VarImpModAvgCoef_apr18.tif',width = 8,height=5,units='in',res=150,pointsize=8)
par(mfrow=c(1,2))
par(mar=c(5,13,1,1))
#Imp
#bI<-barplot(cbind(t(as.matrix(impdivsortx)),rep(NA,times=4)), horiz=T,las=1,xlab='Relative variable importance',col=colsImp#,col=c('darkred','red','pink4',grey(0.8))
#        ,beside=T,legend.text=T,args.legend=list(cex=0.9,'topr',title='Diversity',legend=rev(c('Species','Phylogenetic','Functional','Functional divergence'))))
bI<-barplot(cbind(t(as.matrix(impdivsortx)),rep(NA,times=4)), horiz=T,las=1,xlab='Relative variable importance',col=colsImp#,col=c('darkred','red','pink4',grey(0.8))
            ,beside=T,legend.text=T,args.legend=list(cex=0.9,'topr',title='Diversity',legend=rev(c('Species','Phylogenetic','Functional','Functional divergence'))),
            names.arg=c('Predator diversity (R)','Vegetation productivity \n (NDVI)','Habitat heterogeneity','Topographic heterogeneity','Climatic severity \n (winter minimum temperature)',
                        'Landscape history \n (time since glaciation)','Zoogeographic region (F)',''))
mtext('(a)',side=3,adj=0)
par(mar=c(5,2,1,1))
par(xpd=T)
bI1<-barplot(cbind(t(as.matrix(impdivsortx)),rep(NA,times=4)), horiz=T,beside=T,xlim=c(-0.58,0.5),col=F,border=F,
             xlab='Model averaged coefficients',las=1,names.arg=rep(NA,times=8))#,names.arg=colnames(macplot_est[,2:ncol(macplot_est)]))
#names.arg=c(rep(NA,times=6),colnames(macplot_est[,2:ncol(macplot_est)])[7:8]))
points(macplot_est[,2:ncol(macplot_est)],bI1,pch=macplot_p[,2:ncol(macplot_p)],col=colsImp,lwd=2,cex=1.5) #col=c('black','orange','blue','pink4')) 
arrows(macplot_est[,2:ncol(macplot_est)]+1.96*macplot_se[,2:ncol(macplot_est)],bI1,
       macplot_est[,2:ncol(macplot_est)]-1.96*macplot_se[,2:ncol(macplot_est)],bI1,
       angle=90,length=0.05,code=3,col=colsImp)#,col=c('black','orange','blue','pink4'))
#legend('topr',pch=c(1,16),col=colsImp[4],c('P>=0.05','P<0.05'),title='Significance',pt.cex=1.5,cex=0.9)
text(-0.47,colMeans(bI1),c(rep(NA,times=6),'Zoogeographic region \n (Eurasian vs. Arctico-Siberian)','Zoogeographic region \n (N. American vs Arctico-Siberian)'),cex=0.8)
axis(side=1)
title(xlab='Model averaged coefficients')
axis(side=2,pos=0,outer=F,lwd.ticks=NA,labels=F,lty=2)
mtext('(b)',side=3,adj=0)
dev.off()





#### Plot observed data versus prediction ####
# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(Belowground.full,aes(x=Fire_frequency.2000_2017.x, y=tot.C.kg_m2))
RN<-RN+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData,aes(ymin=SeUp, ymax=SeLo),colour="red",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_ribbon(data=MyData2,aes(ymin=SeUp, ymax=SeLo),fill="green4",colour="green4",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData2,aes(ymin=SeUp, ymax=SeLo),colour="green4",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(stats="identity",colour="grey50",fill="grey50",size=2.5)
RN<-RN+facet_wrap(~landuse, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(0,530), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0,0))
RN<-RN+scale_y_continuous(expand=c(0,0))
RN<-RN+ylab("Nitrogen concentration (%)")+xlab("Rainfall (mm)") # Adding x and ylabs to plot
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
RN<-RN+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1) 
RN

#### Check the relationship between the factors Landuse, climat(dry-int-wet), climate.kat (Dry vs Wet) and texture ####
str(Belowground.full)
plot(tot.C.kg_m2~climate,data = Belowground.full)

# 1. landuse climate belowground, A-hor, Min-hor
# Belowground
Climate.C <- lmer(tot.C.kg_m2~ landuse + climate + landuse:climate + (1|Region/Block), data = Belowground.full, REML=T)
drop1(Climate.C,test="Chisq")
AIC(Climate.C) # 294.0345

xyplot(tot.C.kg_m2~ Shrubbiness|climate,data=Belowground.full)

ref_grid(Climate.C)
test <- emmeans(Climate.C,~landuse:climate)
pairs(test)
plot(test)
#A-hor 
Climate.A_hor<- lmer(AhorC.kg_m2~ landuse:climate + (1|Region/Block), data = Belowground.full, REML=T)
xyplot(AhorC.kg_m2~landuse|climate,data=Belowground.full)

ref_grid(Climate.A_hor)
test2 <- emmeans(Climate.A_hor,~landuse:climate)
pairs(test2)
plot(test2)
#A-hor 
Climate.Min_hor<- lmer(MinC.kg_m2~ landuse:climate + (1|Region/Block), data = Belowground.full, REML=T)
xyplot(MinC.kg_m2~landuse|climate,data=Belowground.full)

ref_grid(Climate.Min_hor)
test3 <- emmeans(Climate.Min_hor,~landuse:climate)
pairs(test3)
plot(test3)
# With texture three ways 
Climate.texture <- lmer(tot.C.kg_m2~Class.x:climate + (1|Region), data = Belowground.full, REML=T)

xyplot(tot.C.kg_m2~Sand|climate,data=Belowground.full)
anova(Climate.texture)
drop1(Climate.texture,test="Chi") # Significant 

ref_grid(Climate.texture)
test2 <- emmeans(Climate.texture,~Class.x:climate)
pairs(test2)
plot(test2)

# With shrubbiness 
Climate.shrubbiness <- lmer(tot.C.kg_m2~fShrubbiness:climate + (1|Region), data = Belowground.full, REML=T)

xyplot(tot.C.kg_m2~fShrubbiness|climate,data=Belowground.full)
anova(Climate.shrubbiness)
drop1(Climate.shrubbiness,test="Chi")

ref_grid(Climate.shrubbiness)
test2 <- emmeans(Climate.shrubbiness,~fShrubbiness:climate)
pairs(test2)
plot(test2)

summary(lm(tot.C.kg_m2~Shrubbiness,data=Belowground.full))
plot(tot.C.kg_m2~Fire_frequency.2000_2017.x,data=Belowground.full)

# With fire 
Fire.climate <- lmer(tot.C.kg_m2~CFire_frequency.2000_2017:climate.kat + (1|Region), data = Belowground.full, REML=T)

xyplot(tot.C.kg_m2~CFire_frequency.2000_2017|climate.kat,data=Belowground.full)
anova(Fire.climate)

#### Hypothesis 1 ####
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

# Soil A-horizon: The best model is taking MAP and the relationship between MAP:Sand into account
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

#### Hypothesis 2 ####
# Testing second hypothesis H2 where I ask about the effect of land use and woody plant cover. 
Soil.min$BM.N.trees.m2 <- Soil.min$BM.Large.N.m2+Soil.min$BM.Small.N.m2
plot(C.amount~log(BM.N.trees.m2),data=Soil.min)

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

# per pool. 
names(Soil.Ahor)
Soil.A_horizon<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.Ahor, REML=T)

drop1(Soil.A_horizon,test="Chi")

Soil.Min_horizon<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.min, REML=T)

drop1(Soil.Min_horizon,test="Chi")

Woody.mod<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Woody, REML=T)

drop1(Woody.mod,test="Chi")

Herbacaous.mod<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Herbaceous, REML=T)

drop1(Herbacaous.mod,test="Chi")

DW.mod<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = DW, REML=T)

drop1(DW.mod,test="Chi")

Soil.A_horizon1 <- update(Soil.A_horizon, .~. -landuse:TreeBM.kg_m2)
anova(Soil.A_horizon,Soil.A_horizon1) # using simpler model because the p-value is larger than 0.05?? (no difference between the two models)

Soil.A_horizon2 <- update(Soil.A_horizon1, .~. -landuse:No.large.trees)
anova(Soil.A_horizon1,Soil.A_horizon2) #Remove "landuse:No.large.trees"

#### SEM model #### 

library(MuMIn)
library(piecewiseSEM)
vignette('piecewiseSEM') # too look at the package 

# Variation for each model component
# Testing different models 
#psem
# na.omit? 
Modlist <-   psem(
  lme(tot.C.kg_m2~CMAP.mm_yr + CFire_frequency.2000_2017.x + tot.N.kg_m2 +CSand + Shrubbiness,random= ~ 1|Region/Block,na.action=na.omit, data=Belowground.full),
  lme(Shrubbiness~CFire_frequency.2000_2017.x + CMAP.mm_yr,random= ~ 1|Region/Block,na.action=na.omit, data=Belowground.full),
  lme(tot.N.kg_m2~ CSand,random= ~ 1|Region/Block,na.action=na.omit, data=Belowground.full),
  lme(CFire_frequency.2000_2017.x~ CMAP.mm_yr,random= ~ 1|Region/Block,na.action=na.omit, data=Belowground.full))
# %~~% between correlated error - telling R to not care about the correlation between these variables. 
# MySummary <- summary(modell)
# save(MySummary, file="")
sem.fit(Modlist,Belowground.full) # Error: no 'nobs' method is available

rsquared(Modlist) # new version of sem.model.fits
# Marginal R2= proportion of variance explained by the fixed factor
# Conditional R2= Proportion of variance explained by the fixed factor and the random factor 

# Response   family     link method     Marginal Conditional
# 1 tot.C.kg_m2 gaussian identity   none 2.022436e-02   0.7121275
# 2 tot.C.kg_m2 gaussian identity   none 2.290810e-01   0.9346487
# 3 tot.C.kg_m2 gaussian identity   none 1.299114e-01   0.6023577
# 4 tot.C.kg_m2 gaussian identity   none 5.934348e-01   0.9173345
# 5 tot.C.kg_m2 gaussian identity   none 8.788994e-05   0.7287494
# 6 tot.C.kg_m2 gaussian identity   none 2.193277e-02   0.7160153
