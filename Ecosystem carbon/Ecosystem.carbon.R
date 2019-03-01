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
MyVar1<-c("MAP.mm_yr","Sand","Fire_frequency.2000_2017.x","Shrubbiness","tot.N.kg_m2","CN")

MyVar2<-c("BM.non.N.trees.m2","BM.N.trees.m2","mean.N.kg_m2.x","Shrubbiness")

# Want to get these two in one matrix. 
pairs(Belowground.full[,MyVar1],lower.panel = panel.cor)

# If I want these values in a table:
Variables2 <- Belowground.full[,c(10:12,15,16,22,24,26,41,43,46)] # first select the collums I want to use 
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
# Use REML= F when looking at the fixed effects 
# Use REML = T when looking at the random effects, and the parameter estimates
# I have region and block.ID as random effects 
# Choose the model with the smallest AIC value 

# Creating dataframes for aboveground C and belowground C on block scale 

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

Belowground.C <- cbind(Soil.min[c(3:25,27,29,30)],Soil.Ahor[c(27)])
colnames(Belowground.C)[24] <- "Soil.MinHor" 
colnames(Belowground.C)[27] <- "Soil.AHor"
Belowground.C$tot.C.kg_m2 <- Belowground.C$Soil.MinHor + Belowground.C$Soil.AHor
tot.N.kg_m2 <- aggregate(tot.N.kg_m2 ~ Region + Block.ID, mean, data=Belowground.full)
Belowground.C <- cbind(Belowground.C,tot.N.kg_m2[3])

names(DW)
Aboveground.C <- cbind(DW[c(3:25,27,29,30)],Woody[c(27)],Herbaceous[c(27)])
colnames(Aboveground.C)[24] <- "DW"
colnames(Aboveground.C)[27] <- "Woody"
colnames(Aboveground.C)[28] <- "Herbaceous"
Aboveground.C$tot.C.kg_m2 <- Aboveground.C$DW + Aboveground.C$Woody + Aboveground.C$Herbaceous
Aboveground.C <- cbind(Aboveground.C,tot.N.kg_m2[3])

names(Belowground.full)
# scaling some of the variables in belowground.C 
Belowground.C$CMAP.mm_yr <- as.numeric(scale(Belowground.C$MAP.mm_yr))
Belowground.C$CSand <- as.numeric(scale(Belowground.C$Sand.pip.per))
Belowground.C$CFire_frequency.2000_2017 <- as.numeric(scale(Belowground.C$Fire_frequency.2000_2017))
Belowground.C$Cmean.N.kg_m2 <- as.numeric(scale(Belowground.C$Mean.N.kg_m2))
Belowground.C$CShrubbiness <- as.numeric(scale(Belowground.C$Shrubbiness))
Belowground.C$CTreeBM.kg_m2 <- as.numeric(scale(Belowground.C$TreeBM.kg_m2))
Belowground.C$Ctot.N.kg_m2 <- as.numeric(scale(Belowground.C$tot.N.kg_m2))

# scaling some of the variables in Aboveground.C 
Aboveground.C$CMAP.mm_yr <- as.numeric(scale(Aboveground.C$MAP.mm_yr))
Aboveground.C$CSand <- as.numeric(scale(Aboveground.C$Sand.pip.per))
Aboveground.C$CFire_frequency.2000_2017 <- as.numeric(scale(Aboveground.C$Fire_frequency.2000_2017))
Aboveground.C$Cmean.N.kg_m2 <- as.numeric(scale(Aboveground.C$Mean.N.kg_m2))
Aboveground.C$CShrubbiness <- as.numeric(scale(Aboveground.C$Shrubbiness))
Aboveground.C$CTreeBM.kg_m2 <- as.numeric(scale(Aboveground.C$TreeBM.kg_m2))
Aboveground.C$Ctot.N.kg_m2 <- as.numeric(scale(Aboveground.C$tot.N.kg_m2))

# scaling some of the variables in belowground.full
Belowground.full$CMAP.mm_yr <- as.numeric(scale(Belowground.full$MAP.mm_yr))
Belowground.full$CSand <- as.numeric(scale(Belowground.full$Sand))
Belowground.full$CFire_frequency.2000_2017 <- as.numeric(scale(Belowground.full$Fire_frequency.2000_2017.x))
Belowground.full$Ctot.N.kg_m2 <- as.numeric(scale(Belowground.full$tot.N.kg_m2))
Belowground.full$CShrubbiness <- as.numeric(scale(Belowground.full$Shrubbiness))
Belowground.full$CTreeBM.kg_m2 <- as.numeric(scale(Belowground.full$TreeBM.kg_m2))

#### BELOWGROUND CARBON #### 
# Look at belowground C first (A + Min)
# Run a full model of all my fixed factors interactions with belowground C 
# James: modelaverages - mumin, dredge, averaging - you get a table.. 
#  To compare to aboveground ++ --> Then start to use the "Block.Eco.C" instead..? 

# 1. BLOCK SCALE  ####
# Global model for Belowground C 
NABelowground.C <- na.omit(Belowground.C)
NABelowground.C<-droplevels(NABelowground.C)

# No interactions
B1<-lmer(tot.C.kg_m2~CMAP.mm_yr + #landuse + 
           CFire_frequency.2000_2017 + Ctot.N.kg_m2 + CTreeBM.kg_m2 + CSand + #CShrubbiness + 
           (1|Region.x), data = NABelowground.C, REML=T)
summary(B1)
drop1(B1,test="Chisq") # signifiant: MAP,Fire,N,treeBM,Sand
AIC(B1) # 54.38128, drop ns, AIC: 46.67971

# Add interactions - no interactions seems to be significant here. 
B1a <- update(B1, .~. +CMAP.mm_yr:CSand) # ns 
anova(B1a,B1)
AIC(B1a) #50.82971
B1b <- update(B1, .~. +CTreeBM.kg_m2:CSand ) # ns 
anova(B1b,B1)
AIC(B1b) #50.75608
B1c <- update(B1, .~. + CTreeBM.kg_m2:CMAP.mm_yr) # ns 
anova(B1c,B1)
AIC(B1c) #50.79712
B1d <- update(B1, .~. + CTreeBM.kg_m2:Ctot.N.kg_m2) # ns 
anova(B1d,B1)
AIC(B1d) #50.3881
B1e <- update(B1, .~. + CFire_frequency.2000_2017:Ctot.N.kg_m2) # ns 
anova(B1e,B1)
AIC(B1e) #50.70199
B1f <- update(B1, .~. + CTreeBM.kg_m2:CFire_frequency.2000_2017) # ns 
anova(B1f,B1)
AIC(B1f) #51.66514

# Global model for Belowground C 
Belowground.block<-lmer(tot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + Ctot.N.kg_m2 + CTreeBM.kg_m2 + CSand + CShrubbiness +
                          landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017  + landuse:Ctot.N.kg_m2: + landuse:CSand  + landuse:CShrubbiness +
                          CMAP.mm_yr:CSand +  CTreeBM.kg_m2:CSand +  Ctot.N.kg_m2:CSand + CFire_frequency.2000_2017:CSand +  CShrubbiness:CSand +
                          CTreeBM.kg_m2:CMAP.mm_yr +  Ctot.N.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017:CMAP.mm_yr +  CShrubbiness:CMAP.mm_yr +
                          CTreeBM.kg_m2:Ctot.N.kg_m2 +  CFire_frequency.2000_2017:Ctot.N.kg_m2 + CTreeBM.kg_m2:CFire_frequency.2000_2017 + CShrubbiness:CFire_frequency.2000_2017 + CShrubbiness:Ctot.N.kg_m2 + (1|Region.x),data = Belowground.C, REML=T)

summary(Belowground.block)
drop1(Belowground.block,test="Chisq") # a lot of warninings.. 
anova(Belowground.block)
AIC(Belowground.block) #85.71602

# Model averaging: All possible models between null and global
modset<-dredge(Belowground.block,trace = TRUE, rank = "AICc", REML = FALSE)
modsel<-model.sel(modset) #Model selection table giving AIC, deltaAIC and weighting
modavg<-model.avg(modsel)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgglm)#Importance of each variable
summary(modavgglm)#Estimated coefficients given weighting

# Global model for Aboveground C 
Aboveground.block<-lmer(tot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + Ctot.N.kg_m2 + CTreeBM.kg_m2 + CSand + CShrubbiness +
                          landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017  + landuse:Ctot.N.kg_m2: + landuse:CSand  + landuse:CShrubbiness +
                          CMAP.mm_yr:CSand +  CTreeBM.kg_m2:CSand +  Ctot.N.kg_m2:CSand + CFire_frequency.2000_2017:CSand +  CShrubbiness:CSand +
                          CTreeBM.kg_m2:CMAP.mm_yr +  Ctot.N.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017:CMAP.mm_yr +  CShrubbiness:CMAP.mm_yr +
                          CTreeBM.kg_m2:Ctot.N.kg_m2 +  CFire_frequency.2000_2017:Ctot.N.kg_m2 + CTreeBM.kg_m2:CFire_frequency.2000_2017 + CShrubbiness:CFire_frequency.2000_2017 + CShrubbiness:Ctot.N.kg_m2 + (1|Region.x),data = Aboveground.C, REML=T)

summary(Aboveground.block)
drop1(Aboveground.block,test="Chisq")
anova(Aboveground.block)
AIC(Aboveground.block) #84.44392

# Reduce the model 
Aboveground.block.red<-lmer(tot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + Ctot.N.kg_m2 + CTreeBM.kg_m2 + CSand + #CShrubbiness +
                          #landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017  + landuse:Ctot.N.kg_m2: + landuse:CSand  + landuse:CShrubbiness +
                          CMAP.mm_yr:CSand +  #CTreeBM.kg_m2:CSand +  Ctot.N.kg_m2:CSand + CFire_frequency.2000_2017:CSand +  CShrubbiness:CSand +
                          #CTreeBM.kg_m2:CMAP.mm_yr +  Ctot.N.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017:CMAP.mm_yr +  CShrubbiness:CMAP.mm_yr +
                          #CTreeBM.kg_m2:Ctot.N.kg_m2 +  
                          CFire_frequency.2000_2017:Ctot.N.kg_m2 + CTreeBM.kg_m2:CFire_frequency.2000_2017 + #CShrubbiness:CFire_frequency.2000_2017 + CShrubbiness:Ctot.N.kg_m2 + 
                          (1|Region.x),data = Aboveground.C, REML=T)
summary(Aboveground.block.red)
drop1(Aboveground.block.red,test="Chisq") # nothing significant 
anova(Aboveground.block.red)
AIC(Aboveground.block.red) #10.57838


# 2. From belowground.full fine scale ####
Belowground.tot<-lmer(tot.C.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + tot.N.kg_m2 + TreeBM.kg_m2 + BM.N.trees.m2 + CSand + Shrubbiness +
                        landuse:CMAP.mm_yr + landuse:CFire_frequency.2000_2017  + tot.N.kg_m2:landuse + landuse:CSand + landuse:BM.N.trees.m2 + landuse:Shrubbiness +
                        CMAP.mm_yr:CSand +  TreeBM.kg_m2:CSand + landuse:CSand + tot.N.kg_m2:CSand + CFire_frequency.2000_2017:CSand + BM.N.trees.m2:CSand + Shrubbiness:CSand +
                        TreeBM.kg_m2:CMAP.mm_yr +  tot.N.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017:CMAP.mm_yr + BM.N.trees.m2:CMAP.mm_yr + Shrubbiness:CMAP.mm_yr +
                        TreeBM.kg_m2:tot.N.kg_m2 +  CFire_frequency.2000_2017:tot.N.kg_m2 + TreeBM.kg_m2:CFire_frequency.2000_2017 +BM.N.trees.m2: tot.N.kg_m2 + Shrubbiness:CFire_frequency.2000_2017 + Shrubbiness:tot.N.kg_m2 + 
                        (1|Region), data = Belowground.full, REML=T)

summary(Belowground.tot)
drop1(Belowground.tot,test="Chisq")
anova(Belowground.tot)
AIC(Belowground.tot) #152.5925

# Reduce the model to what was significant in drop1 
Belowground.tot.red<-lmer(tot.C.kg_m2~ CMAP.mm_yr + #landuse + 
                            CFire_frequency.2000_2017.x + tot.N.kg_m2 + TreeBM.kg_m2 + #BM.N.trees.m2 + 
                            CSand + Shrubbiness +
                            # landuse:MAP.mm_yr + landuse:Fire_frequency.2000_2017.x  + tot.N.kg_m2:landuse + landuse:Sand + landuse:BM.N.trees.m2 + 
                            CMAP.mm_yr:CSand +  #TreeBM.kg_m2:Sand + landuse:Sand + tot.N.kg_m2:Sand + Fire_frequency.2000_2017.x:Sand + BM.N.trees.m2:Sand +  TreeBM.kg_m2:MAP.mm_yr + 
                            tot.N.kg_m2:CMAP.mm_yr + #Fire_frequency.2000_2017.x:MAP.mm_yr + BM.N.trees.m2:MAP.mm_yr + 
                            CMAP.mm_yr:Shrubbiness +
                            #TreeBM.kg_m2:tot.N.kg_m2 +  
                            CFire_frequency.2000_2017.x:tot.N.kg_m2 + TreeBM.kg_m2:CFire_frequency.2000_2017.x + #BM.N.trees.m2: tot.N.kg_m2 + 
                            (1|Region), data = Belowground.full, REML=T)

summary(Belowground.tot.red)
drop1(Belowground.tot.red,test="Chisq") 
anova(Belowground.tot.red) # Nitrogen really high - something going on here.. 
AIC(Belowground.tot.red) # 144.4792 - better

# Reduce the model further.. 
Belowground.tot.red2<-lmer(tot.C.kg_m2~ CMAP.mm_yr +
                             CFire_frequency.2000_2017.x + tot.N.kg_m2 + TreeBM.kg_m2 + 
                             CSand + 
                             Shrubbiness + 
                             CMAP.mm_yr:CSand + 
                             tot.N.kg_m2:CMAP.mm_yr + 
                             CMAP.mm_yr:Shrubbiness
                           #CFire_frequency.2000_2017.x:tot.N.kg_m2 
                           + TreeBM.kg_m2:CFire_frequency.2000_2017.x + 
                             (1|Region), data = Belowground.full, REML=T)

summary(Belowground.tot.red2)
drop1(Belowground.tot.red2,test="Chisq") 
anova(Belowground.tot.red2) 
AIC(Belowground.tot.red2) # 144.4798 - not better.. Remove MAP:Sand, and the model AIC: 139.2077

# Removing singular terms to look at their p-value. 
Test1<-lmer(tot.C.kg_m2~ CMAP.mm_yr +
              CFire_frequency.2000_2017.x + tot.N.kg_m2 + TreeBM.kg_m2 + 
              CSand + 
              Shrubbiness + 
              CMAP.mm_yr:CSand + 
              tot.N.kg_m2:CMAP.mm_yr + 
              CMAP.mm_yr:Shrubbiness
            + TreeBM.kg_m2:CFire_frequency.2000_2017.x + 
              (1|Region), data = Belowground.full, REML=T)
# terms one by one.. 
Test1a<- update(Test1, .~. -CMAP.mm_yr:CSand)
Test1b<- update(Test1, .~. -CMAP.mm_yr)
Test1c<- update(Test1, .~. -CSand)
Test1d<- update(Test1, .~. -tot.N.kg_m2:CMAP.mm_yr)
Test1e<- update(Test1, .~. -CMAP.mm_yr:Shrubbiness)
Test1f<- update(Test1, .~. -Shrubbiness)
Test1g<- update(Test1, .~. -TreeBM.kg_m2:CFire_frequency.2000_2017.x)
Test1h<- update(Test1, .~. -CFire_frequency.2000_2017.x)

anova(Test1,Test1a) # - MAP:SAND . 
anova(Test1,Test1b) # - MAP *
anova(Test1,Test1c) # - Sand *
anova(Test1,Test1d) # - N:MAP ***
anova(Test1,Test1e) # - MAP:SHrubbiness **
anova(Test1,Test1f) # - Shrubbiness * 
anova(Test1,Test1g) # - TreeBM:Fire *
anova(Test1,Test1h) # - Fire ***

# distribution of residuals 
E2 <- resid(Belowground.tot.red2, type ="pearson") 
F2 <- fitted(Belowground.tot.red2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # None of the fitted values <0
abline(h = 0, lty = 2, col = 1)  # Good

# Exploring alternative models 

# Adding livestock
names(Belowground.red)
Belowground.red <- Belowground.full[,c(1:12,15,21:26,30,44:52)]
Belowground.red <- Belowground.red[Belowground.red$Region!="Ikorongo",]
Belowground.red <- Belowground.red[Belowground.red$Region!="Park Nyigoti",]
Belowground.red <- na.omit(Belowground.red)
Belowground.red <- droplevels(Belowground.red)
# Belowground.red[is.na(Belowground.red)] <- 0

Belowground2<-lmer(tot.C.kg_m2~ CMAP.mm_yr + livestock + CFire_frequency.2000_2017.x + tot.N.kg_m2 + TreeBM.kg_m2  + CSand + Shrubbiness +
                     livestock:CMAP.mm_yr + livestock:CFire_frequency.2000_2017.x  + tot.N.kg_m2:livestock + livestock:CSand + livestock:Shrubbiness +
                     CMAP.mm_yr:CSand +  TreeBM.kg_m2:CSand + livestock:CSand + tot.N.kg_m2:CSand + CFire_frequency.2000_2017.x:CSand + Shrubbiness:CSand +
                     TreeBM.kg_m2:CMAP.mm_yr +  tot.N.kg_m2:CMAP.mm_yr + CFire_frequency.2000_2017.x:CMAP.mm_yr +  Shrubbiness:CMAP.mm_yr +
                     TreeBM.kg_m2:tot.N.kg_m2 +  CFire_frequency.2000_2017.x:tot.N.kg_m2 + TreeBM.kg_m2:CFire_frequency.2000_2017.x + Shrubbiness:CFire_frequency.2000_2017.x + Shrubbiness:tot.N.kg_m2 + 
                     (1|Region), data = Belowground.red, REML=T)
AIC(Belowground2) #144.4665
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
