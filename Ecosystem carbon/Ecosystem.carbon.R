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

names(Tree.carbon)

# Fixing the data for further processing 
# Soil texture 
levels(Soil.texture$Region)
Soil.texture$Region<- factor(Soil.texture$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Soil.texture <-Soil.texture[,c(1:3,5:8)]
Soil.texture <- na.omit(Soil.texture)
Soil.texture <- droplevels(Soil.texture)

Soil.texture$Class <- c("SaClLo","SaLo","SaClLo","SaClLo","Cl","Cl","Cl","ClLo","SaCl","SaCl","ClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaLo","SaLo","SaLo","ClLo","Cl","ClLo","Cl", "Cl","ClLo","ClLo","Cl") 

# Per Region
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Soil.tex.reg <- cbind(
  aggregate(Sand.pip.per~Region,mean, data=Soil.texture),
  aggregate(Sand.pip.per~Region,SE, data=Soil.texture)[2])

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
Ecosystem.Carbon1a <- merge(Tree.carbon[,c(23,2,3,4,6,8,7,15,18:20,22)],Herbaceous.carbon[,c(3,11)],all.x = TRUE,by="Block.ID")
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

#### Creating a DF for the poster/Thesis ####
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

# Mean above and below 
Below <- Above_Below.C %>%
  filter(Above_Below.C$Carbon.pool=="BelowgroundC")
Above <- Above_Below.C %>%
  filter(Above_Below.C$Carbon.pool=="AbovegroundC")
mean(Below$C.amount) # 4.4
SE(Below$C.amount) # 0.417
mean(Above$C.amount) # 0.11
SE(Above$C.amount) # 0.044

# Properties per sampling region
Properties <- Block.Eco.C %>%
  filter(Block.Eco.C$Carbon.pool=="Woody")
names(Properties)
Properties.reg <- cbind(aggregate(Fire_frequency.2000_2017~Region.x, mean, data=Properties),
                        aggregate(Last.fire_yr~Region.x, mean, data=Properties)[2],
                        aggregate(Shrubbiness~Region.x, mean, data=Properties)[2]
                        )

#### Make the data into a long format instead of a wide ####
data_long.CTreeHerb <- gather(Ecosystem.CHerbTree, Carbon.pool,C.amount, TreeC.kg_m2:HerbC.kg_m2,factor_key=TRUE)

data_long.CSoilDW <- gather(Ecosystem.CSoilDW, Carbon.pool,C.amount, SoilAC.kg_m2:DWC.kg_m2,factor_key=TRUE)

SE.data_long.CSoilDW <- gather(SE.Ecosystem.CSoilDW, Carbon.pool,C.amount, SE.SoilAC.kg_m2:SE.DWC.kg_m2,factor_key=TRUE)

EcosystemC.SoilDW<- cbind(data_long.CSoilDW,SE.data_long.CSoilDW[5])
names(EcosystemC.SoilDW)
names(data_long.CTreeHerb)
colnames(EcosystemC.SoilDW) <- c("Block.ID","Region.x","Vilde.block","landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency.2000_2017","Clay.pip.per","Silt.pip.per","Sand.pip.per","Class","mean.N.kg_m2","Total.basal.area_m2","TreeBM.kg_m2", "No.trees_m2","Shrubbiness2","Small.N","Small.N.non","Large.N","Large.N.non" , "BM.Large.N.m2", "BM.Large.non.m2","BM.Small.N.m2","BM.Small.non.m2","Carbon.pool","C.amount","SE.C.amount")

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
Ecosystem.Carbon$Shrubbiness2[is.na(Ecosystem.Carbon$Shrubbiness2)]<- 0
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
Block.Eco.C <- Block.Eco.C[-c(76:80),]

# Create a df for carbon per landuse. 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
landuse.C <- cbind(
  aggregate(C.amount~Carbon.pool + landuse, mean, data= Block.Eco.C),
  aggregate(C.amount~Carbon.pool + landuse, SE, data= Block.Eco.C)[3])
colnames(landuse.C)[4] <- "SE.C.amount"
landuse.C$Carbon.pool<- factor(landuse.C$Carbon.pool, levels = c("Herbaceous","Dead wood","Woody","Soil A-horizon","Soil Min-horizon"))
landuse.C$landuse<- factor(landuse.C$landuse, levels = c("Pasture","Wild"))
levels(landuse.C$landuse)<- c("UPA","PA")
landuse.C$Main.pool <- c("Aboveground","Aboveground","Aboveground","Belowground","Belowground")

aggregate(C.amount~Main.pool + landuse,sum, data=landuse.C)
names(Block.Eco.C)
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
names(Belowground.full)
Belowground.full <- full_join(Belowground.full,Shrubbiness)
#Belowground.full$fShrubbiness <- as.factor(round(Belowground.full$Shrubbiness, digits = 0))
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
plot(tot.C.kg_m2~Class.x,data=Belowground.full)
plot(tot.C.kg_m2~Region,data=Belowground.full)
names(Belowground.full)
Sand <- aggregate(Sand~Region, mean, data=Belowground.full)
Clay <- aggregate(Clay~Region, mean, data=Belowground.full)
C <- aggregate(tot.C.kg_m2~Region, mean, data=Belowground.full)

#### TOT ECOSYSTEM C ####

# Legend titeles
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

# Point

# Per carbon pool 
# Transform the data
trans <- function(x){pmin(x,0.20) + 0.05*pmax(x-1,0)}
yticks <- c(0,0.05,0.10,0.15,1,2,3)

#Transform the data onto the display scale
landuse.C$mean_t <- trans(landuse.C$C.amount)
landuse.C$sd_up_t <- trans(landuse.C$C.amount + landuse.C$SE.C.amount)
landuse.C$sd_low_t <- pmax(trans(landuse.C$C.amount - landuse.C$SE.C.amount))

Carbon.pool <- ggplot(data = landuse.C, aes(x = Carbon.pool,y = mean_t, shape = landuse, colour= Carbon.pool))

Carbon.pool + xlab("Carbon pool") +  ylab(expression(paste("Carbon (kg", m^-2,")")))  + 
  geom_errorbar(aes(ymin=sd_low_t,ymax=sd_up_t),stat = "identity",width=.4,lwd=1.1,show.legend=F, position=position_dodge(width=0.4)) +
  geom_point(fill="white",size = 6, stroke=1.5,show.legend=T, position=position_dodge(width=0.4))  +
  scale_y_continuous(breaks=trans(yticks),labels=c(0,0.05,0.10,0.15,0.20,0.25,0.30),limits = c(0,NA))  +
  scale_shape_manual(legend_titleLAND,values=c(21,22)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Herbaceous","Dead wood","Woody","Soil A-horizon","Soil Min-horizon"),values=c("forestgreen","darkgoldenrod","darkolivegreen","salmon4","burlywood4")) +
  theme(rect = element_rect(fill ="transparent")
      ,panel.background=element_rect(fill="transparent")
      ,plot.background=element_rect(fill="transparent",colour=NA)
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.grid.major.x = element_blank()
      ,panel.grid.major.y = element_blank()
      ,axis.text=element_text(size=16,color="black")
      ,axis.title.y=element_text(size=16,color="black")
      ,axis.title.x=element_text(size=16,color="black")
      ,axis.text.x=element_text(size=14,color="black",
                                margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,axis.ticks.length=unit(-1.5, "mm") # - because we want them inwards. 
      ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,axis.line.y = element_line(color="black", size = .5)
      ,axis.line.x = element_line(color="black", size = .5)
      ,plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")
      ,strip.background = element_rect(fill="transparent",colour=NA)
      ,strip.text.x = element_text(size=14,margin = margin(.5,.5,.5,.5, "mm"),hjust = .02)
      ,strip.text.y = element_blank()
      ,panel.spacing = unit(.1, "lines")
      ,legend.text=element_text(size=14)
      ,legend.title=element_text(size=15)
      ,legend.position = "right"
      ,legend.justification = "top"
      ,legend.direction="vertical"
      ,legend.key.width = unit(1.2,"cm"))

ggsave("Ecosystem carbon/Figures/EcoC.Landuse.png",
       width= 30, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Same plot with facet wrap 
names(landuse.C)
Carbon.pool <- ggplot(data = landuse.C, aes(x = Carbon.pool,y = C.amount, shape = landuse, colour= Carbon.pool))

Carbon.pool + xlab("Carbon pool") +  ylab(expression(paste("Carbon (kg ", m^-2,")")))  + 
  facet_wrap(~Main.pool,scales="free") +
  geom_errorbar(aes(ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount),stat = "identity",width=.4,lwd=1.1,show.legend=F, position=position_dodge(width=0.4)) +
  geom_point(fill="white",size = 6, stroke=1.5,show.legend=T, position=position_dodge(width=0.4))  +
  scale_shape_manual(legend_titleLAND,values=c(21,22)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Herbaceous","Dead wood","Woody","Soil A-horizon","Soil Min-horizon"),values=c("forestgreen","darkgoldenrod","darkolivegreen","salmon4","burlywood4")) +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.text=element_text(size=16,color="black")
        ,axis.title.y=element_text(size=16,color="black")
        ,axis.title.x=element_text(size=16,color="black")
        ,axis.text.x=element_text(size=14,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm") # - because we want them inwards. 
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size=14,margin = margin(.5,.5,.5,.5, "mm"),hjust = .02)
        ,strip.text.y = element_blank()
        ,panel.spacing = unit(.1, "lines")
        ,legend.text=element_text(size=14)
        ,legend.title=element_text(size=15)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm"))

ggsave("Ecosystem carbon/Figures/EcoC.Landuse2.png",
       width= 30, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)


# To get different shape of point: scale_shape_manual(values=c(1,15))
EcosystemC.plot1 <- ggplot(data = Block.Eco.C, aes(x = Region.x,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, group = Carbon.pool, colour= Carbon.pool))

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

# Plotting C and Sand + MAP
names(Belowground.full2)
CN.plot <- ggplot(data = Belowground.full2, aes(x = MAP.mm_yr,y = MinC.kg_m2,group = landuse))

CN.plot + xlab("MAP") +  ylab(expression(paste("Soil Carbon (kg", m^-2,")"))) +
  facet_wrap(~landuse)+
  geom_point(size = 3,stroke=2, na.rm=T)+
  #scale_color_manual(TitleRain,breaks = c("1295", "1279",  "1135", "856",  "755",  "717" ,"672"),values=c("#6460D9","#8495E9","#74DEDA", "#9EF5C1", "#F9EE5E","#F9EE5E","#E9F882")) + 
  theme_bw() #+
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

Tree.var<-c("TreeBM.kg_m2","No.trees_m2","Total.basal.area_m2","Shrubbiness","TreeBM.N","tot.N.kg_m2")

names(Soil.Ahor)
ModelVar<-c("CMAP.mm_yr","CSand","CFire_frequency.2000_2017","CShrubbiness","CTreeBM.kg_m2","Herbaceous","Ctot.N.kg_m2")

names(Belowground.full.CnoNA)
Model.var.full<-c("MAP.mm_yr","Sand","Clay","Silt","Fire_frequency.2000_2017.x","Shrubbiness","TreeBM.kg_m2","Herbaceous","AhorN.kg_m2","MinN.kg_m2","AhorC.kg_m2","MinC.kg_m2","Last_fire.yr")

names(Total.Eco.C.CnoNA2)
Model.var.sub<-c("MAP.mm_yr","Sand.pip.per","Clay.pip.per","Fire_frequency.2000_2017","Shrubbiness2","Woody","Herbaceous","AhorN.kg_m2","MinN.kg_m2","Soil.Ahor","Soil.min","Last.fire_yr", "livestock", "wild", "Termite.effect")

CandN<-c("Ctot.C.kg_m2","Ctot.N.kg_m2")

# Want to get these two in one matrix. 
pairs(Soil.Ahor[,Tree.var],lower.panel = panel.cor)
pairs(Soil.Ahor[,ModelVar],lower.panel = panel.cor)
pairs(Belowground.full.CnoNA[,Model.var.full],lower.panel = panel.cor)
pairs(Total.Eco.C.CnoNA2[,Model.var.sub],lower.panel = panel.cor)
# If I want these values in a table:
Model.var.FULL <- Belowground.full.CnoNA[,c(14,28,26,27,12,13,51,32,64,16,22,21,15)]
Model.var.Herb <- Belowground.full.CnoNA[,c(14,28,26,12,13,51,32,64,16,22,21,15)]
Model.var.SUB <- Total.Eco.C.CnoNA2[,c(5,10,8,7,15,35,34,41,42,33,24,6,44,46,47)]
CandN.var <- Belowground.full[,c(53,56)] 
Tree.var <- Soil.Ahor[,c(14:17,31,39)]

MycorFULL <- rcorr(as.matrix(Model.var.FULL), type="pearson") # Use the pearson correlation (r-value)
MycorHERB <- rcorr(as.matrix(Model.var.Herb), type="pearson") # Use the pearson correlation (r-value)
MycorSUB <- rcorr(as.matrix(Model.var.SUB), type="pearson") # Use the pearson correlation (r-value)
MycorFULL <- as.data.frame(round(MycorFULL$r, digits=3))
MycorHERB <- as.data.frame(round(MycorHERB$r, digits=3))
MycorSUB <- as.data.frame(round(MycorSUB$r, digits=3))
write.csv(MycorFULL, file= "Ecosystem carbon/VariableCorrelationFULL.csv")
write.csv(MycorHERB, file= "Ecosystem carbon/VariableCorrelationHERB.csv")
write.csv(MycorSUB, file= "Ecosystem carbon/VariableCorrelationSUB.csv")

MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

#### LANDUSE and correlation ####
# Trees 
plot(TreeBM.kg_m2~landuse, data= Block.Eco.C) # COVARYING 
plot(Shrubbiness2~landuse,data=Block.Eco.C) # not covarying 
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
plot(livestock~Region,data=Belowground.full) # COVARYING
plot(wild~landuse,data=Belowground.full) # not covarying 

plot(livestock~Region,data=Belowground.full)
plot(tot.C.kg_m2~Region,data=Belowground.full)
plot(tot.N.kg_m2~Region,data=Belowground.full)
plot(Clay~Region,data=Belowground.full)
plot(Herb.C~landuse, data=Soil.Ahor)

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
names(Belowground.full)

Block.Eco.C$Carbon.pool <- factor(Block.Eco.C$Carbon.pool, levels=c("TreeC.kg_m2","HerbC.kg_m2","DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))

levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")

Livestock.dung<- aggregate(livestock~Region + Block.ID, mean, data=Belowground.full)
Wild.dung<- aggregate(wild~Region + Block.ID, mean, data=Belowground.full)
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
Block.Eco.C$CShrubbiness2 <- as.numeric(scale(Block.Eco.C$Shrubbiness2))
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
names(Soil.min)
Belowground.C <- cbind(Soil.min[c(2:24,26,29:36)],Soil.Ahor[c(26)])
colnames(Belowground.C)[24] <- "Soil.MinHor" 
colnames(Belowground.C)[33] <- "Soil.AHor"
tot.N.kg_m2 <- aggregate(tot.N.kg_m2 ~ Region + Block.ID, mean, data=Belowground.full)
Belowground.C <- cbind(Belowground.C,tot.N.kg_m2[3])
Belowground.C$tot.C.kg_m2 <- Belowground.C$Soil.MinHor + Belowground.C$Soil.AHor

names(DW)
Aboveground.C <- cbind(DW[c(2:24,26,29:36)],Woody[c(26)],Herbaceous[c(26)])
colnames(Aboveground.C)[24] <- "DW"
colnames(Aboveground.C)[33] <- "Woody"
colnames(Aboveground.C)[34] <- "Herbaceous"
Aboveground.C <- cbind(Aboveground.C,tot.N.kg_m2[3])
Aboveground.C$tot.C.kg_m2 <- Aboveground.C$DW + Aboveground.C$Woody + Aboveground.C$Herbaceous

Soil.min <- cbind(Soil.min,tot.N.kg_m2[3])
Soil.Ahor <- cbind(Soil.Ahor,tot.N.kg_m2[3])
Herbaceous <- cbind(Herbaceous,tot.N.kg_m2[3])
Woody <- cbind(Woody,tot.N.kg_m2[3])
DW<- cbind(DW,tot.N.kg_m2[3])

Total.Eco.C <- cbind(Soil.min[c(2:24,26,29:36)],Soil.Ahor[c(26)],Herbaceous[c(26)],Woody[c(26)],DW[c(26)])
colnames(Total.Eco.C)[24] <- "Soil.min"
colnames(Total.Eco.C)[33] <- "Soil.Ahor"
colnames(Total.Eco.C)[34] <- "Herbaceous"
colnames(Total.Eco.C)[35] <- "Woody"
colnames(Total.Eco.C)[36] <- "DW"
Total.Eco.C$tot.C.kg_m2 <- Total.Eco.C$Soil.min + Total.Eco.C$Soil.Ahor + Total.Eco.C$Herbaceous + Total.Eco.C$Woody + Total.Eco.C$DW
Total.Eco.C <- cbind(Total.Eco.C, tot.N.kg_m2[3])

names(Belowground.full)
summary(Belowground.full)
Belowground.full$CN <- Belowground.full$tot.C.kg_m2/Belowground.full$tot.N.kg_m2
max(Belowground.full$CN)
Total.Eco.C$Herbaceous[Total.Eco.C$Herbaceous==0] <- NA
# scaling tot.C to be able to compare estimates.. 
Belowground.C$Ctot.C.kg_m2 <- as.numeric(scale(Belowground.C$tot.C.kg_m2)) 
Aboveground.C$Ctot.C.kg_m2 <- as.numeric(scale(Aboveground.C$DW)) + as.numeric(scale(Aboveground.C$Herbaceous)) + as.numeric(scale(Aboveground.C$Woody))
Soil.min$Ctot.C.kg_m2 <- as.numeric(scale(Soil.min$C.amount)) 
Soil.Ahor$Ctot.C.kg_m2 <- as.numeric(scale(Soil.Ahor$C.amount))
Herbaceous$C.amount[Herbaceous$C.amount==0] <- NA
Herbaceous$Ctot.C.kg_m2 <- as.numeric(scale(Herbaceous$C.amount)) 
DW$Ctot.C.kg_m2 <- as.numeric(scale(DW$C.amount)) 
Woody$Ctot.C.kg_m2 <- as.numeric(scale(Woody$C.amount)) 
Total.Eco.C$Ctot.C.kg_m2 <- as.numeric(scale(Total.Eco.C$tot.C.kg_m2)) 
Total.Eco.C$Ctot.N.kg_m2 <- as.numeric(scale(Total.Eco.C$tot.N.kg_m2)) 
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
Belowground.full$CLast_fire.yr <- as.numeric(scale(Belowground.full$Last_fire.yr))
Belowground.full$Ctot.N.kg_m2 <- as.numeric(scale(Belowground.full$tot.N.kg_m2))
Belowground.full$CShrubbiness2 <- as.numeric(scale(Belowground.full$Shrubbiness2))
Belowground.full$CTreeBM.kg_m2 <- as.numeric(scale(Belowground.full$TreeBM.kg_m2))
Belowground.full$Ctot.C.kg_m2 <- as.numeric(scale(Belowground.full$tot.C.kg_m2)) 
Belowground.full$Clivestock <- as.numeric(scale(Belowground.full$livestock)) 
Belowground.full$Cwild <- as.numeric(scale(Belowground.full$wild)) 
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
?model.sel
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
names(Herbaceous)
Soil.Ahor <- cbind(Soil.Ahor,Herbaceous[26])
colnames(Soil.Ahor)[40] <- "Herb.C"
Soil.Ahor$CHerb.C <- as.numeric(scale(Soil.Ahor$Herb.C))
summary(Soil.Ahor)
Soil.Ahor.CnoNA<-Soil.Ahor[!is.na(Soil.Ahor$Herb.C),]
Soil.Ahor.CnoNA <-  Soil.Ahor.CnoNA[(-16),]
Soil.Ahor.CnoNA <-  Soil.Ahor.CnoNA[(-16),]
Ahor.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness2 + CHerb.C #+ CTreeBM.N 
                 + CMAP.mm_yr:CSand + landuse:CMAP.mm_yr + landuse:CSand + 
                          (1|Region.x),data = Soil.Ahor.CnoNA, REML=F,
                        na.action=na.fail)

summary(Ahor.block)
drop1(Ahor.block,test="Chisq")  
anova(Ahor.block)
AIC(Ahor.block) #62.22761

# Model averaging: All possible models between null and global
modsetbelowA<-dredge(Ahor.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse)&!(CSand & CHerb.C))
modselbelowA<-model.sel(modsetbelowA) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA<-model.avg(modselbelowA)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA)#Importance of each variable
write.table(importance(modavgbelowA),file="Ecosystem carbon/importanceAhor.txt")
summary(modavgbelowA)#Estimated coefficients given weighting
summary(modavgbelowA)$coefmat.full # Full average 
write.table(summary(modavgbelowA)$coefmat.subset, file="Ecosystem carbon/ConAvgAhor.txt") # conditional average - I will try first with this.. 

# With livestock, wild dung and termites 
Livestock.dung <- Livestock.dung[c(2,3)]
Wild.dung <- Wild.dung[c(2,3)]
Soil.Ahor2 <- left_join(Soil.Ahor,Livestock.dung,by="Block.ID",drop=F)
Soil.Ahor2 <- left_join(Soil.Ahor2,Wild.dung,by="Block.ID",drop=F)
Soil.Ahor2 <- left_join(Soil.Ahor2,Termites,by="Block.ID",drop=F)
Soil.Ahor2.CnoNA<-Soil.Ahor2[!is.na(Soil.Ahor2$livestock),]
Soil.Ahor2.CnoNA$Clivestock <- as.numeric(scale(Soil.Ahor2.CnoNA$livestock))
Soil.Ahor2.CnoNA$Cwild <- as.numeric(scale(Soil.Ahor2.CnoNA$wild))
Soil.Ahor2.CnoNA$CTermites <- as.numeric(scale(Soil.Ahor2.CnoNA$Termite.effect))
Soil.Ahor2.CnoNA <- Soil.Ahor2.CnoNA[(-16),]
Soil.Ahor2.CnoNA <- droplevels(Soil.Ahor2.CnoNA)

Ahor2.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 + 
                    CTreeBM.kg_m2 + CSand + CShrubbiness2 + CHerb.C + Clivestock + Cwild + CTermites
                  + #CMAP.mm_yr:CSand + 
                    (1|Region.x),data = Soil.Ahor2.CnoNA, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelowA2<-dredge(Ahor2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & CHerb.C)&!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites)& !(CMAP.mm_yr & CShrubbiness2)& !(Cwild & CShrubbiness2))
modselbelowA2<-model.sel(modsetbelowA2) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA2<-model.avg(modselbelowA2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA2)#Importance of each variable
write.table(importance(modavgbelowA2),file="Ecosystem carbon/importanceAhorDung.txt")
summary(modavgbelowA2)#Estimated coefficients given weighting
confint.Ahor.dung <- confint(modavgbelowA2)
coef.Ahor.dung <- summary(modavgbelowA2)$coefmat.subset
Ahor.dung <- cbind(coef.Ahor.dung, confint.Ahor.dung)
write.table(Ahor.dung, file="Ecosystem carbon/ConAvgAhordung.txt") # conditional average - I will try first with this.. 

# 3. Global model for Mineral hor C univariate variables ####
Soil.min <- cbind(Soil.min,Herbaceous[26])
colnames(Soil.min)[40] <- "Herb.C"
Soil.min$CHerb.C <- as.numeric(scale(Soil.min$Herb.C))
Soil.min.CnoNA<-Soil.min[!is.na(Soil.min$CFire_frequency.2000_2017),]
Soil.min.CnoNA <- Soil.min.CnoNA[(-16),]
Min.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness + CHerb.C + #CTreeBM.N + 
                  CMAP.mm_yr:CSand + landuse:CMAP.mm_yr + landuse:CSand + 
                          (1|Region.x),data = Soil.min.CnoNA, REML=F,
                        na.action=na.fail)

summary(Min.block)
drop1(Min.block,test="Chisq") # MAP,Fire,N,TreeBM
anova(Min.block)
AIC(Min.block) #57.77696
plot(Ctot.C.kg_m2~Region.x,data=Soil.min.CnoNA)
Soil.min.CnoNA2 <- Soil.min.CnoNA[-16,]

# Model averaging: All possible models between null and global
modsetbelowM<-dredge(Min.block,trace = TRUE, rank = "AICc", REML = FALSE,
                    subset=!(CTreeBM.kg_m2 & landuse)&!(CSand & CHerb.C))
modselbelowM<-model.sel(modsetbelowM) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowM<-model.avg(modselbelowM)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowM)#Importance of each variable
write.table(importance(modavgbelowM),file="Ecosystem carbon/importanceMinhor.txt")
summary(modavgbelowM)#Estimated coefficients given weighting
write.table(summary(modavgbelowM)$coefmat.subset, file="Ecosystem carbon/ConAvgMinHor.txt")

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

# With livestock, wild dung and termites 
Soil.min2 <- left_join(Soil.min,Livestock.dung,by="Block.ID",drop=F)
Soil.min2 <- left_join(Soil.min2,Wild.dung,by="Block.ID",drop=F)
Soil.min2 <- left_join(Soil.min2,Termites,by="Block.ID",drop=F)
Soil.min2.CnoNA<-Soil.min2[!is.na(Soil.min2$livestock),]
Soil.min2.CnoNA$Clivestock <- as.numeric(scale(Soil.min2.CnoNA$livestock))
Soil.min2.CnoNA$Cwild <- as.numeric(scale(Soil.min2.CnoNA$wild))
Soil.min2.CnoNA$CTermites <- as.numeric(scale(Soil.min2.CnoNA$Termite.effect))
Soil.min2.CnoNA <- Soil.min2.CnoNA[(-16),]
Soil.min2.CnoNA <- droplevels(Soil.min2.CnoNA)

Min.block2<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 + 
                  CTreeBM.kg_m2 + CSand + CShrubbiness2 + CHerb.C + Clivestock + Cwild + CTermites 
                + CMAP.mm_yr:CSand + 
                  (1|Region.x),data = Soil.min2.CnoNA, REML=F,
                na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelowM2<-dredge(Min.block2,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & CHerb.C)&!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites)& !(CMAP.mm_yr & CShrubbiness2)& !(Cwild & CShrubbiness2))
modselbelowM2<-model.sel(modsetbelowM2) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowM2<-model.avg(modselbelowM2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowM2)#Importance of each variable
write.table(importance(modavgbelowM2),file="Ecosystem carbon/importanceMinhor.dung.txt")
summary(modavgbelowM2)#Estimated coefficients given weighting
confint.Minhor.dung <- confint(modavgbelowM2)
coef.Minhor.dung <- summary(modavgbelowM2)$coefmat.subset
Minhor.dung <- cbind(coef.Minhor.dung, confint.Minhor.dung)
write.table(Minhor.dung, file="Ecosystem carbon/ConAvgMinHordung.txt")

# 4. Global model for Herbs ####
names(Herbaceous.CnoNA)
summary(lmer(C.amount~Fire_frequency.2000_2017 + (1|Region.x), data=Herbaceous.CnoNA))
plot(C.amount~Fire_frequency.2000_2017, data=Herbaceous.CnoNA)
abline(a=0.035862, b=-0.004594)

Herbaceous.CnoNA<-Herbaceous[!is.na(Herbaceous$Ctot.C.kg_m2),]
Herbaceous.CnoNA <- Herbaceous.CnoNA[(-20),]
Herbaceous.CnoNA <- Herbaceous.CnoNA[(-16),]
Herbaceous.CnoNA <- droplevels(Herbaceous.CnoNA)
summary(Herbaceous.CnoNA)
Herbaceous.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness2 + Ctot.N.kg_m2 + 
                         #CMAP.mm_yr:CSand + 
                         landuse:CMAP.mm_yr + landuse:CSand +
                   (1|Region.x),data = Herbaceous.CnoNA, REML=F, 
                   na.action=na.fail)

summary(Herbaceous.block)
drop1(Herbaceous.block,test="Chisq")  
anova(Herbaceous.block)
AIC(Herbaceous.block) #52.15059

# Model averaging: All possible models between null and global
modsetaboveH<-dredge(Herbaceous.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse) & !(Ctot.N.kg_m2 & CSand) & !(CMAP.mm_yr & CShrubbiness2))
modselaboveH<-model.sel(modsetaboveH) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveH<-model.avg(modselaboveH)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveH)#Importance of each variable
write.table(importance(modavgaboveH),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgaboveH)
coef.Herb <- summary(modavgaboveH)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 

# With livestock, wild dung and termites, not anything special.
Livestock.dung <- Livestock.dung[c(2,3)]
Wild.dung <- Wild.dung[c(2,3)]
Herbaceous2 <- left_join(Herbaceous,Livestock.dung,by="Block.ID",drop=F)
Herbaceous2 <- left_join(Herbaceous2,Wild.dung,by="Block.ID",drop=F)
Herbaceous2 <- left_join(Herbaceous2,Termites,by="Block.ID",drop=F)
Herbaceous2.CnoNA<-Herbaceous2[!is.na(Herbaceous2$livestock),]
Herbaceous2.CnoNA$Clivestock <- as.numeric(scale(Herbaceous2.CnoNA$livestock))
Herbaceous2.CnoNA$Cwild <- as.numeric(scale(Herbaceous2.CnoNA$wild))
Herbaceous2.CnoNA$CTermites <- as.numeric(scale(Herbaceous2.CnoNA$Termite.effect.x))
Herbaceous2.CnoNA <- Herbaceous2.CnoNA[(-16),]
Herbaceous2.CnoNA <- droplevels(Herbaceous2.CnoNA)

Herb2.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 + 
                    CTreeBM.kg_m2 + CSand + CShrubbiness2 + Clivestock + Cwild + CTermites
                  + CMAP.mm_yr:CSand + 
                    (1|Region.x),data = Herbaceous2.CnoNA, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetHerb2<-dredge(Herb2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites)& !(CMAP.mm_yr & CShrubbiness2)& !(Cwild & CShrubbiness2))
modselHerb2<-model.sel(modsetHerb2) #Model selection table giving AIC, deltaAIC and weighting
modavgHerb2<-model.avg(modselHerb2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgHerb2)#Importance of each variable
#write.table(importance(modavgHerb2),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgHerb2)
coef.Herb <- summary(modavgHerb2)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
#write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 

# 5. Global model for DW #### 
DW.CnoNA<-DW[!is.na(DW$CFire_frequency.2000_2017),]
DW.CnoNA <- DW.CnoNA[(-16),]
DW.block<-lmer(Ctot.C.kg_m2~ #CMAP.mm_yr + CSand + Ctot.N.kg_m2
                 landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2  + CShrubbiness2 +  #CTreeBM.N + 
                 #CMAP.mm_yr:CSand + landuse:CMAP.mm_yr + landuse:CSand +
                   (1|Region.x),data = DW.CnoNA, REML=F,
                 na.action=na.fail)

summary(DW.block)
drop1(DW.block,test="Chisq")  
anova(DW.block)
AIC(DW.block) #92.05196

# Model averaging: All possible models between null and global
modsetaboveDW<-dredge(DW.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse))
# & !(Ctot.N.kg_m2 & CSand))
modselaboveDW<-model.sel(modsetaboveDW) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveDW<-model.avg(modselaboveDW)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveDW)#Importance of each variable
write.table(importance(modavgaboveDW),file="Ecosystem carbon/importanceaboveDW.txt")
#importance$Variable <- c("MAP","Tree biomass (N.fix)","Sand","Fire frequency","Shrubbiness","Land-use","Tree biomass","MAP:Sand")
confint.DW <- confint(modavgaboveDW)
coef.DW <- summary(modavgaboveDW)$coefmat.subset
DW <- cbind(coef.DW, confint.DW)
summary(modavgaboveDW)#Estimated coefficients given weighting
write.table(DW, file="Ecosystem carbon/ConAvgDW.txt") 

# With livestock, wild dung and termites, not anything special.
DW2 <- left_join(DW,Livestock.dung,by="Block.ID",drop=F)
DW2 <- left_join(DW2,Wild.dung,by="Block.ID",drop=F)
DW2 <- left_join(DW2,Termites,by="Block.ID",drop=F)
DW2.CnoNA<-Herbaceous2[!is.na(Herbaceous2$livestock),]
DW2.CnoNA$Clivestock <- as.numeric(scale(DW2.CnoNA$livestock))
DW2.CnoNA$Cwild <- as.numeric(scale(DW2.CnoNA$wild))
DW2.CnoNA$CTermites <- as.numeric(scale(DW2.CnoNA$Termite.effect.x))
DW2.CnoNA <- DW2.CnoNA[(-16),]
DW2.CnoNA <- droplevels(DW2.CnoNA)

DW2.block<-lmer(Ctot.C.kg_m2~ CFire_frequency.2000_2017 + 
                    CTreeBM.kg_m2 + CShrubbiness2 + Clivestock + Cwild + CTermites +
                    (1|Region.x),data = DW2.CnoNA, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetDW2<-dredge(DW2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(Cwild & CTermites)& !(Cwild & CShrubbiness2))
modselDW2<-model.sel(modsetDW2) #Model selection table giving AIC, deltaAIC and weighting
modavgDW2<-model.avg(modselDW2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgDW2)#Importance of each variable
write.table(importance(modavgDW2),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgDW2)
coef.Herb <- summary(modavgDW2)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 
# 6. Global model for Woody #### 
Woody.CnoNA<-Woody[!is.na(Woody$CFire_frequency.2000_2017),]
Woody.CnoNA2<-Woody[!is.na(Woody$CFire_frequency.2000_2017),]
# Remove Handajega outlier 
Woody.CnoNA <- Woody.CnoNA[-16,]
plot(TreeBM.kg_m2~landuse,data=Woody.CnoNA)
Woody.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CSand + Ctot.N.kg_m2 + #CMAP.mm_yr:CSand + 
                    landuse:CMAP.mm_yr + landuse:CSand +
                   (1|Region.x),data = Woody.CnoNA2, REML=F,
                 na.action=na.fail)

summary(Woody.block)
drop1(Woody.block,test="Chisq")  
anova(Woody.block)
AIC(Woody.block) #75.208
# Model averaging: All possible models between null and global
modsetaboveW<-dredge(Woody.block,trace = TRUE, rank = "AICc", REML = FALSE, subset= !(Ctot.N.kg_m2 & CSand))
modselaboveW<-model.sel(modsetaboveW) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveW<-model.avg(modselaboveW)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveW)#Importance of each variable
write.table(importance(modavgaboveW),file="Ecosystem carbon/importanceaboveW.outl.txt")
#importance$Variable <- c("MAP","Tree biomass (N.fix)","Sand","Fire frequency","Shrubbiness","Land-use","Tree biomass","MAP:Sand")
summary(modavgaboveW)#Estimated coefficients given weighting
confint.woody <- confint(modavgaboveW)
coef.woody <- summary(modavgaboveW)$coefmat.subset
woody <- cbind(coef.woody, confint.woody)
write.table(woody, file="Ecosystem carbon/ConAvgW.outl.txt") 

# With livestock, wild dung, and termites
Woody2 <- left_join(Woody,Livestock.dung,by="Block.ID",drop=F)
Woody2 <- left_join(Woody2,Wild.dung,by="Block.ID",drop=F)
Woody2 <- left_join(Woody2,Termites,by="Block.ID",drop=F)
Woody2.CnoNA<-Woody2[!is.na(Woody2$livestock),]
Woody2.CnoNA$Clivestock <- as.numeric(scale(Woody2.CnoNA$livestock))
Woody2.CnoNA$Cwild <- as.numeric(scale(Woody2.CnoNA$wild))
Woody2.CnoNA$CTermites <- as.numeric(scale(Woody2.CnoNA$Termite.effect))
Woody2.CnoNA <- Woody2.CnoNA[(-16),]
Woody2.CnoNA <- droplevels(Woody2.CnoNA)

Woody2.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 + 
                    CSand + Clivestock + Cwild + CTermites + #CMAP.mm_yr:CSand +
                    (1|Region.x),data = Woody2.CnoNA, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetWoody2<-dredge(Woody2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites))
modselWoody2<-model.sel(modsetWoody2) #Model selection table giving AIC, deltaAIC and weighting
modavgWoody2<-model.avg(modselWoody2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgWoody2)#Importance of each variable
write.table(importance(modavgWoody2),file="Ecosystem carbon/importanceWoody2.txt")
#Estimated coefficients given weighting
confint.Woody <- confint(modavgWoody2)
coef.Woody <- summary(modavgWoody2)$coefmat.subset
Woody <- cbind(coef.Woody, confint.Woody)
write.table(Woody, file="Ecosystem carbon/ConAvgWoody2.txt") 

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

# 8. Global model for Total Ecosystem C ####
Total.Eco.C.CnoNA<-Total.Eco.C[!is.na(Total.Eco.C$CFire_frequency.2000_2017),]
Total.Eco.C.CnoNA <- Total.Eco.C.CnoNA[(-16),]
Tot.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CShrubbiness + 
                  CMAP.mm_yr:CSand + landuse:CMAP.mm_yr + landuse:CSand + 
                  (1|Region.x),data = Total.Eco.C.CnoNA, REML=F,
                na.action=na.fail)

summary(Tot.block)
drop1(Tot.block,test="Chisq") # MAP,Fire,N,TreeBM
anova(Tot.block)
AIC(Tot.block) #57.6387

# Model averaging: All possible models between null and global
modsettot<-dredge(Tot.block,trace = TRUE, rank = "AICc", REML = FALSE,
                     subset=!(CTreeBM.kg_m2 & landuse))
modseltot<-model.sel(modsettot) #Model selection table giving AIC, deltaAIC and weighting
modavgtot<-model.avg(modseltot)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgtot)#Importance of each variable
write.table(importance(modavgtot),file="Ecosystem carbon/importancetot.txt")
summary(modavgtot)#Estimated coefficients given weighting
write.table(summary(modavgtot)$coefmat.subset, file="Ecosystem carbon/con.avg.tot.txt")

# 9. From belowground.full fine scale ####

# Model averaging 
# Add Herbaceous biomass, dung and termites. 
names(Aboveground.C)
Above <- Aboveground.C[,c(1,24,33,34)]
Above$Herbaceous[Above$Herbaceous==0] <- NA
# Checking herbaceous biomass from Stu.
#Above$HerbaceousStu <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.111,0.066,0.109,0.139,0.417,0.347,0.457,0.334)
#Above$Herbaceous.test <- rowSums(Above[,c("Herbaceous","HerbaceousStu")], na.rm=TRUE)
Belowground.full <- left_join(Belowground.full,Above, by="Block.ID")
Belowground.full$Herbaceous[Belowground.full$Herbaceous==0] <- NA
Belowground.full$CHerb.C <- as.numeric(scale(Belowground.full$Herbaceous))
#Belowground.full$CHerb.test <- as.numeric(scale(Belowground.full$Herbaceous.test))
# Add termites 
Termites <- read.csv("Termites/RecalTermEff_Wetseason_Block.csv", head=T)
Termites <- Termites[Termites$Landuse!="Agriculture",]
Termites <- Termites[c(4,6,7)]
Termites <- Termites[Termites$Site!="Seronera",]
Termites <- droplevels(Termites)
Termites$Site<- factor(Termites$Site, levels = c("Makao","Maswa","Mwantimba","Handajega"))
Termites <- Termites[order(Termites[,1]), ]
Termites$Block.ID <- as.numeric(1:16)
Termites <- Termites[c(3,4)]
Belowground.full <- left_join(Belowground.full,Termites, by= "Block.ID")
Belowground.full2 <- Belowground.full[-c(77,78,79,80),]
Belowground.full2 <- Belowground.full2[-c(61,62,63,64),]
Belowground.full$CTermites <- as.numeric(scale(Belowground.full$Termite.effect))
Belowground.full.CnoNA<-Belowground.full[!is.na(Belowground.full$Herbaceous),]
Belowground.full.CnoNA <- Belowground.full.CnoNA[-c(77,78,79,80),]
Belowground.full.CnoNA <- Belowground.full.CnoNA[-c(61,62,63,64),]
Belowground.full.CnoNA <- droplevels(Belowground.full.CnoNA)
Belowground.full.CnoNA2<-Belowground.full.CnoNA[!is.na(Belowground.full.CnoNA$livestock),]
names(Belowground.full.CnoNA)
summary(Belowground.full.CnoNA)

# A horizon 
Belowground.Ahor <-lmer(AhorC.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017  + CTreeBM.kg_m2 + CSand + CShrubbiness2 + #CHerb.C + 
                          #CMAP.mm_yr:CSand + 
                          landuse:CMAP.mm_yr + landuse:CSand + 
                     (1|Region/Block.ID), data = Belowground.full2, REML=F, na.action=na.fail)

modsetbelow.Ahor<-dredge(Belowground.Ahor,trace = TRUE, rank = "AICc", REML = FALSE,subset=!(CTreeBM.kg_m2 & landuse)&!(CMAP.mm_yr & CShrubbiness2))
# For the model with herbs: subset=!(CTreeBM.kg_m2 & landuse)&!(CSand & CFire_frequency.2000_2017)&!(CMAP.mm_yr & CHerb.C))
modselbelow.Ahor<-model.sel(modsetbelow.Ahor) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Ahor<-model.avg(modselbelow.Ahor)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Ahor)
write.table(importance(modavgbelow.Ahor), file="Ecosystem carbon/importanceAhorFull.txt")#Importance of each variable
summary(modavgbelow.Ahor)#Estimated coefficients given weighting
confint.Ahor <- confint(modavgbelow.Ahor)
coef.Ahor <- summary(modavgbelow.Ahor)$coefmat.subset
Ahor <- cbind(coef.Ahor, confint.Ahor)
write.table(Ahor, file="Ecosystem carbon/ConAvgAhorFull.txt")

# A hor with dung and termites NOT this I use 
Belowground.Ahor.sub <-lmer(AhorC.kg_m2 ~ CMAP.mm_yr + CFire_frequency.2000_2017  + CTreeBM.kg_m2 + CSand + CShrubbiness2 + CHerb.C + Clivestock + Cwild + CTermites
                        #+ CMAP.mm_yr:CSand 
                        + Clivestock:CMAP.mm_yr +
                          (1|Region/Block.ID), data = Belowground.full.CnoNA2, REML=F, na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelow.Ahor.sub<-dredge(Belowground.Ahor.sub,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & CHerb.C)&!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites) & !(CMAP.mm_yr & CShrubbiness2)) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Ahor.sub<-model.avg(modsetbelow.Ahor.sub)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Ahor.sub)
write.table(importance(modavgbelow.Ahor), file="Ecosystem carbon/importanceAhorDung.txt")#Importance of each variable
summary(modavgbelow.Ahor.sub)#Estimated coefficients given weighting
confint.Ahor <- confint(modavgbelow.Ahor)
coef.Ahor <- summary(modavgbelow.Ahor)$coefmat.subset
Ahor <- cbind(coef.Ahor, confint.Ahor)
write.table(Ahor, file="Ecosystem carbon/ConAvgAhorDung.txt")

# Mineral horizon
Belowground.Minhor <-lmer(MinC.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017  + CTreeBM.kg_m2 + CSand + CShrubbiness2 + #CHerb.C + 
                            #CMAP.mm_yr:CSand + 
                            landuse:CMAP.mm_yr + landuse:CSand +  (1|Region/Block.ID), data = Belowground.full2, REML=F, na.action=na.fail)

modsetbelow.Minhor<-dredge(Belowground.Minhor,trace = TRUE, rank = "AICc", REML = FALSE,subset=!(CTreeBM.kg_m2 & landuse)&!(CMAP.mm_yr & CShrubbiness2)) 
#For herbaceous: subset=!(CTreeBM.kg_m2 & landuse)&!(CSand & CFire_frequency.2000_2017)&!(CMAP.mm_yr & CHerb.C))
modselbelow.Minhor<-model.sel(modsetbelow.Minhor) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Minhor<-model.avg(modselbelow.Minhor)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Minhor)#Importance of each variable
write.table(importance(modavgbelow.Minhor), file="Ecosystem carbon/importanceMinHorFull.txt")#
summary(modavgbelow.Minhor)#Estimated coefficients given weighting
confint.Minhor <- confint(modavgbelow.Minhor)
coef.Minhor <- summary(modavgbelow.Minhor)$coefmat.subset
Minhor <- cbind(coef.Minhor, confint.Minhor)
write.table(Minhor, file="Ecosystem carbon/ConAvgMinhorFull.txt")

plot(MinC.kg_m2~MAP.mm_yr, data=Belowground.full2)
?plot
plot(MinC.kg_m2~landuse, data=Belowground.full2)
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

#interaction plot
Belowground.full2$fMAP <- as.factor(Belowground.full2$MAP.mm_yr)
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

# PLOT above and below C ####

# Block level landuse 
names(Total.Eco.C)
Total.Eco.C$Region.x<- factor(Total.Eco.C$Region.x, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
levels(Total.Eco.C$landuse)<- c("UPA","PA")

par(mar=c(5,5,1,1))
plot(tot.C.kg_m2~landuse, data=Total.Eco.C, ylab=(expression(paste("Ecosystem Carbon (kg", m^-2,")"))), xlab = "Land-use")

plot(Soil.min~Region.x,data=Total.Eco.C)
plot(Soil.Ahor~Region.x,data=Total.Eco.C)
# Aggrigate per region: 
names(Total.Eco.C)
landuseSand <- Total.Eco.C[c(1,4,10,44,46)]
# Total soil data 
total.soil.data <- read.csv("Ecosystem carbon/Soil.data/Total.soil.data.csv", head=T)
names(total.soil.data)
total.soil.data$Region<- factor(total.soil.data$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
total.soil.data <- droplevels(total.soil.data)
total.soil.data <- left_join(total.soil.data,landuseSand, by="Block.ID")
total.soil.data <- total.soil.data[total.soil.data$Horizon!="O-hor",] 
AHor <- total.soil.data[total.soil.data$Horizon=="A-hor",]
# Plot A-hor carbon against livestock and wild 
names(AHor)
par(mfrow=c(1,2))
plot(C.kg_m2.scaled~livestock, data=AHor)
plot(C.kg_m2.scaled~wild, data=AHor)

# Aggrigate to get A and min horizon varlues for carbon
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
AHorizon <- total.soil.data[total.soil.data$Horizon=="A-hor",]
MinHorizon <- total.soil.data[total.soil.data$Horizon=="Min-hor",]
A.hor <- aggregate(C.kg_m2.scaled~Region,mean,data=AHorizon)
SE.A.hor <- aggregate(C.kg_m2.scaled~Region,SE,data=AHorizon)
Min.hor <- aggregate(C.kg_m2.scaled~Region,mean,data=MinHorizon)
SE.Min.hor <- aggregate(C.kg_m2.scaled~Region,SE,data=MinHorizon)
plot(Tot.C.per~Region,data=AHorizon)
plot(Tot.C.per~Region,data=MinHorizon)

SandMAP.mod <- (lmer(C.kg_m2.scaled~ Sand.pip.per + MAP.mm_yr + Sand.pip.per:MAP.mm_yr +  (1|Region/Block.ID), data=total.soil.data, REML=T))
drop1(SandMAP.mod,test="Chisq")
AIC(SandMAP.mod) # -553.8972
summary(SandMAP.mod)

# Remove interactions one by one  
SandMAP.moda <- update(SandMAP.mod, .~. - Sand.pip.per:MAP.mm_yr )  
SandMAP.modb <- update(SandMAP.moda, .~. - Sand.pip.per) 
SandMAP.modc <- update(SandMAP.moda, .~. - MAP.mm_yr)
anova(SandMAP.mod,SandMAP.moda) # non sign interacion 
anova(SandMAP.moda,SandMAP.modb) # sand sign! 
anova(SandMAP.moda,SandMAP.modc) # non sign MAP 

#             Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
#remove interaction (MAP:SAND)   7 -553.90 -530.02 283.95  -567.90 0.5695      1     0.4504
#remove sand  6 -555.33 -534.86 283.66  -567.33 5.094      1    0.02401 *
#remove MAP  6 -555.33 -534.86 283.66  -567.33 1.3836      1     0.2395

Horizon.mod <- (lmer(C.kg_m2.scaled~Horizon + landuse + Sand.pip.per + MAP.mm_yr + Sand.pip.per:MAP.mm_yr + (1|Region/Block), data=total.soil.data, REML=T))
drop1(Horizon.mod,test="Chisq")
AIC(Horizon.mod)
summary(Horizon.mod)

xyplot(C.kg_m2.scaled~ Horizon|landuse,data=total.soil.data)

ref_grid(Horizon.mod)
test <- emmeans(Horizon.mod,~Horizon:landuse)
pairs(test)
plot(test)

# Plot Livestock and wild against A-hor carbon #### 
plot(Total.Eco.C.CnoNA2$Soil.Ahor~ Total.Eco.C.CnoNA2$livestock)
plot(AhorC.kg_m2~ livestock, data= Belowground.full.CnoNA2)
plot(AhorC.kg_m2~ wild, data=Belowground.full.CnoNA2)
plot(AhorC.kg_m2~ ratio, data=Belowground.full.CnoNA2)
plot(AhorC.kg_m2~ total.dung, data=Belowground.full.CnoNA2)
Belowground.full.CnoNA2$ratio <- Belowground.full.CnoNA2$Clivestock/Belowground.full.CnoNA2$Cwild
Belowground.full.CnoNA2$total.dung <- Belowground.full.CnoNA2$livestock+Belowground.full.CnoNA2$wild

# Creating a variable for livestock dung per m2 
names(Total.Eco.C.CnoNA2)
Total.Eco.C.CnoNA2$livestock_m2 <- Total.Eco.C.CnoNA2$livestock/200
Total.Eco.C.CnoNA2$wild_m2 <- Total.Eco.C.CnoNA2$wild/200

summary(lm(AhorC.kg_m2~ratio, data=Belowground.full.CnoNA2))
Total.Eco.C.CnoNA2$total.dung <- Total.Eco.C.CnoNA2$livestock+Total.Eco.C.CnoNA2$wild

DungC <- lme(Soil.Ahor~ livestock + wild, random= ~ 1|Region.x,na.action=na.fail, method= "REML",data=Total.Eco.C.CnoNA2)
#DungC2 <- lme(Soil.Ahor~ total.dung, random= ~ 1|Region.x,na.action=na.fail, method= "REML",data=Total.Eco.C.CnoNA2)
summary(DungC)
#summary(DungC2)

# Prediction lines: 
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE
str(Belowground.full.CnoNA2)

#A:Specify covariate values for predictions
MyData <- expand.grid(livestock = seq(min(Total.Eco.C.CnoNA2$livestock), max(Total.Eco.C.CnoNA2$livestock)),
wild = seq(min(Total.Eco.C.CnoNA2$wild), max(Total.Eco.C.CnoNA2$wild)))

X <- model.matrix(~livestock + wild, data=MyData)
head(X)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(DungC)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X %*% vcov(DungC) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[3] <- "Soil.Ahor"
MyDataLiv<-cbind(aggregate(Soil.Ahor~livestock, MyData, mean),
                 aggregate(SeLo~livestock, MyData, mean)[2],
                 aggregate(SeUp~livestock, MyData, mean)[2])
colnames(MyDataLiv) <- c("livestock","Soil.Ahor","SeLo","SeUp")

MyDataWild<-cbind(aggregate(Soil.Ahor~wild, MyData, mean),
                 aggregate(SeLo~wild, MyData, mean)[2],
                 aggregate(SeUp~wild, MyData, mean)[2])
colnames(MyDataWild) <- c("wild","Soil.Ahor","SeLo","SeUp")

# ggplot 
# Livestock 
# Livestock dung: lightgoldenrod3
# Wild dung: tan2
Livestock <- ggplot(data = Total.Eco.C.CnoNA2)

Livestock + xlab(expression(paste("Livestock dung (counts 200 ", m^-2,")"))) +  ylab(expression(paste("A-horizon carbon (kg ", m^-2,")")))  +
  geom_ribbon(data=MyDataLiv,aes(x=livestock,ymin=SeLo,ymax=SeUp),fill="lightgoldenrod3",alpha=.50,lwd=FALSE,show.legend=FALSE)+
  geom_line(data=MyDataLiv,aes(y=Soil.Ahor,x=livestock)) + 
  geom_errorbar(aes(x = livestock,ymin=Soil.Ahor-SE.Soil.Ahor,ymax=Soil.Ahor+SE.Soil.Ahor),stat = "identity",width=1.3,lwd=0.5,show.legend=F) +
  geom_point(aes(x =livestock,y = Soil.Ahor),size = 5, stroke=1.5, shape=21, fill="lightgoldenrod3") +
 # scale_x_continuous(breaks=c(-30,-20,-10,0),labels=c(30,20,10,0))  +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=16,color="black")
        ,axis.title.y=element_text(size=16,color="black")
        ,axis.title.x=element_text(size=16,color="black")
        ,axis.text.x=element_text(size=14,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")  
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size=14,margin = margin(.5,.5,.5,.5, "mm"),hjust = .02)
        ,strip.text.y = element_blank()
        ,panel.spacing = unit(.1, "lines"))

ggsave("Ecosystem carbon/Figures/LivestockAhorNormal.png",
       width= 15, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Wild 
Wild <- ggplot(data = Total.Eco.C.CnoNA2)

Wild + xlab(expression(paste("Wild dung (counts 200 ", m^-2,")"))) +  ylab(expression(paste("A-horizon carbon (kg ", m^-2,")")))  +
  geom_ribbon(data=MyDataWild,aes(x=wild,ymin=SeLo,ymax=SeUp),fill="tan2",alpha=.50,lwd=FALSE,show.legend=FALSE)+
  geom_line(data=MyDataWild,aes(y=Soil.Ahor,x=wild)) + 
  geom_errorbar(aes(x=wild, ymin=Soil.Ahor-SE.Soil.Ahor,ymax=Soil.Ahor+SE.Soil.Ahor),stat = "identity",width=0.5,lwd=0.5,show.legend=F) +
  geom_point(aes(x = wild,y = Soil.Ahor),size = 5, stroke=1.5, shape=21, fill="tan2")  +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=16,color="black")
        ,axis.title.y=element_text(size=16,color="black")
        ,axis.title.x=element_text(size=16,color="black")
        ,axis.text.x=element_text(size=14,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm") # - because we want them inwards. 
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size=14,margin = margin(.5,.5,.5,.5, "mm"),hjust = .02)
        ,strip.text.y = element_blank()
        ,panel.spacing = unit(.1, "lines"))

ggsave("Ecosystem carbon/Figures/WildAhor.png",
       width= 15, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# PLOT Importance #### 
#importance.Ahor<- read.table("Ecosystem carbon/importanceAhor.txt")
importance.AhorFull<- read.table("Ecosystem carbon/importanceAhorFull.txt")
importance.MinHorFull<- read.table("Ecosystem carbon/importanceMinHorFull.txt")
#importance.AhorHerbs<- read.table("Ecosystem carbon/importanceAhorFull.txt")
#importance.MinHorHerbs<- read.table("Ecosystem carbon/importanceMinHorFull.txt")
importance.H<- read.table("Ecosystem carbon/importanceaboveH.txt")
importance.DW<- read.table("Ecosystem carbon/importanceaboveDW.txt")
importance.W<- read.table("Ecosystem carbon/importanceaboveW.txt")
importance.W2<- read.table("Ecosystem carbon/importanceWoody2.txt")
importance.W.outl <- read.table("Ecosystem carbon/importanceaboveW.outl.txt")
importance.Ahor.dung<- read.table("Ecosystem carbon/importanceAhorDung.txt")
#importance.MinHor.dung<- read.table("Ecosystem carbon/importanceMinHorDung.txt")

# I now have all my importance variables 
colnames(importance.AhorFull)<-'Ahor'
colnames(importance.MinHorFull)<-'MinHor'
colnames(importance.H)<-'Herbs'
colnames(importance.DW)<-'DW'
colnames(importance.W)<-'Woody'
colnames(importance.W2) <- "Woody"
colnames(importance.W.outl) <- "Woody.outl"
colnames(importance.Ahor.dung)<-'Ahor'
#colnames(importance.MinHor.dung)<-'MinHor'

rownames(importance.AhorFull) <- (c("Sand","MAP","Land-use","Fire frequency","Tree biomass","Encroachment","MAP:Land-use","Sand:Land-use"))

#rownames(importance.AhorHerbs) <- (c("Sand","MAP","Land-use","Shrubbiness","Tree biomass","Herb biomass","MAP:Sand","MAP:Land-use","Fire frequency","Sand:Land-use"))

rownames(importance.MinHorFull) <- (c("Sand","Fire frequency","MAP","Land-use","Tree biomass","MAP:Land-use","Sand:Land-use","Encroachment"))

#rownames(importance.MinHorHerbs) <- (c("Sand","Land-use","MAP","Shrubbiness","MAP:Land-use","Sand:Land-use","MAP:Sand","Herb biomass","Tree biomass","Fire frequency"))

rownames(importance.H) <- (c("Fire frequency","Land-use","Sand","MAP", "Soil Nitrogen","Encroachment","Tree biomass","Sand:Land-use","MAP:Land-use"))

rownames(importance.DW) <- (c("Land-use","Fire frequency","Encroachment","Tree biomass"))

rownames(importance.W) <- (c("Land-use","Sand","MAP","Fire frequency","Soil Nitrogen","Sand:Land-use","MAP:Land-use"))

rownames(importance.W2) <- (c("Fire frequency","Domestic dung","Macro fauna", "Sand","MAP","Wild dung"))

rownames(importance.W.outl) <- (c("Fire frequency","landuse","MAP", "Sand","Soil Nitrogen","MAP:Land-use","Sand:Land-use"))

rownames(importance.Ahor.dung) <- (c("Domestic dung","Sand","Wild dung","MAP","Tree biomass","Fire frequency","Encroachment","Macro fauna","Herb biomass"))

#rownames(importance.MinHor.dung) <- (c("Sand","Fire frequency","Shrubbiness","Tree biomass","MAP","Macro fauna","Wild dung","Livestock dung", "Herb biomass","MAP:Sand"))

# Colours 
# Fire: darkorange1
# Landuse: goldenrod3
# Woody encroachment: khaki1
# Livestock dung: lightgoldenrod3
# Wild dung: tan2 
# Termites: goldenrod1

# Herb biomass: forestgreen
# Tree bm: darkolivegreen3

# Sand: lightskyblue3
# Soil N: lightskyblue
# MAP: deepskyblue4

# MAP:Land-use: gray47
# Sand:Land-use: gray72


# Plot A-hor Full
col.Ahor.full <- c("lightskyblue3","deepskyblue4","goldenrod3","darkorange1","darkolivegreen3","khaki1","gray47","gray72")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.AhorFull.png")
par(mar=c(5,14,1,2))
barplot(t(as.matrix(importance.AhorFull)), horiz=T,las=1,xlab='Relative variable importance',main='Soil A-horizon Carbon',axisname=T,col=col.Ahor.full,beside=T,cex.main = 1,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot A-hor Herbs 
# col.Ahor.herbs <- c("lightskyblue3","deepskyblue4","goldenrod3","darkolivegreen4","darkolivegreen3","forestgreen","deepskyblue2","steelblue3","darkorange2","khaki3")
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.AhorHerb.png")
# par(mar=c(5,14,1,2))
# barplot(t(as.matrix(importance.AhorFull)), horiz=T,las=1,xlab='Relative variable importance',main='Soil A-horizon Carbon',axisname=T,col=col.Ahor.full,beside=T,cex.main = 1,cex.axis=2,cex.lab=1,cex.names=2)
# dev.off()

# Plot A-hor with dung and termites 
col.Ahor.dung <- c("lightgoldenrod3","lightskyblue3","tan2","deepskyblue4", "darkolivegreen3", "darkorange1","khaki1","goldenrod1","forestgreen")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Ahor.dung.png")
par(mar=c(5,14,1,2))
barplot(t(as.matrix(importance.Ahor.dung)), horiz=T,las=1,xlab='Relative variable importance',main='Soil A-horizon Carbon',axisname=T,col=col.Ahor.dung,beside=T,cex.main = 1,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot Min-hor FULL
col.min.full <- c("lightskyblue3","darkorange1","deepskyblue4","goldenrod3",'darkolivegreen3','gray47',"gray72",'khaki1')
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.MinhorFull.png")
par(mar=c(5,14,1,2))
barplot(t(as.matrix(importance.MinHorFull)), horiz=T,las=1,xlab='Relative variable importance',main='Soil Mineral-horizon Carbon',cex.main = 1,axisname=T,col= col.min.full,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot Min-hor HERBS
# col.min.herbs <- c("darkgray","goldenrod3","deepskyblue4",'darkolivegreen4','steelblue3',"khaki3","deepskyblue2","forestgreen",'darkolivegreen3',"darkorange2")
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.MinhorHerb.png")
# par(mar=c(5,14,1,2))
# barplot(t(as.matrix(importance.MinHorFull)), horiz=T,las=1,xlab='Relative variable importance',main='Soil Mineral-horizon Carbon',cex.main = 1,axisname=T,col= col.min.full,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
# dev.off()

# Plot Min-hor with dung 
col.min.dung <- c("lightskyblue3","darkorange1","khaki1",'darkolivegreen3',"deepskyblue4","goldenrod1","tan2","lightgoldenrod3","forestgreen")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Minhor.dung.png")
par(mar=c(5,14,1,2))
barplot(t(as.matrix(importance.MinHor.dung)), horiz=T,las=1,xlab='Relative variable importance',main='Soil Mineral-horizon Carbon',cex.main = 1,axisname=T,col= col.min.dung,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot Herb 
col.herb <- c("darkorange1","goldenrod3","lightskyblue3","deepskyblue4","lightskyblue","khaki1","darkolivegreen3","gray72","gray47")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Herb.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.H)), horiz=T,las=1,xlab='Relative variable importance',main='Herbaceous Carbon',cex.main = 1,axisname=T,col=col.herb,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot DW 
col.DW <- c("goldenrod3","darkorange1","khaki1","darkolivegreen3")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.DW.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.DW)), horiz=T,las=1,xlab='Relative variable importance',main='Dead Wood Carbon',cex.main = 1,axisname=T,col=col.DW,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

#Plot Woody
col.W <- c("goldenrod3","lightskyblue3","deepskyblue4","darkorange1","lightskyblue","gray72","gray47")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Woody.outl.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.W)),horiz=T,las=1,xlab='Relative variable importance',main='Woody Carbon',cex.main = 1,axisname=T,col=col.W.out,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

#Plot Woody with dung
col.W2 <- c("darkorange1","lightgoldenrod3","goldenrod1","lightskyblue3","deepskyblue4","tan2")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Woody2.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.W2)),horiz=T,las=1,xlab='Relative variable importance',main='Woody Carbon',cex.main = 1,axisname=T,col=col.W2,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

#Plot Woody with outlier 
col.W.out <- c("darkorange1","goldenrod3","deepskyblue4","lightskyblue3","lightskyblue","gray47","gray72")
png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Woody.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.W.outl)),horiz=T,las=1,xlab='Relative variable importance',main='Woody Carbon',cex.main = 1,axisname=T,col=col.W.out,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# PLOT variable coefficients from model averages ####
con.avg.AhorFull<- read.table("Ecosystem carbon/ConAvgAhorFull.txt")
con.avg.MinHorFull<- read.table("Ecosystem carbon/ConAvgMinHorFull.txt")
#con.avg.AhorHerbs<- read.table("Ecosystem carbon/ConAvgAhorHerbs.txt")
#con.avg.MinHorHerbs<- read.table("Ecosystem carbon/ConAvgMinHorHerbs.txt")

con.avg.H<- read.table("Ecosystem carbon/ConAvgH.txt")
con.avg.DW<- read.table("Ecosystem carbon/ConAvgDW.txt")
con.avg.W<- read.table("Ecosystem carbon/ConAvgW.txt")
con.avg.W2<- read.table("Ecosystem carbon/ConAvgWoody2.txt")
con.avg.W.outl<- read.table("Ecosystem carbon/ConAvgWOutl.txt")

con.avg.Ahor.dung<- read.table("Ecosystem carbon/ConAvgAhordung.txt")
#con.avg.MinHor.dung<- read.table("Ecosystem carbon/ConAvgMinHordung.txt")

#Reorder rows
rownames(con.avg.Ahor.dung)
con.avg.AhorFull<-con.avg.AhorFull[c(1,3,2,4,7,6,8,5,9),]
con.avg.MinHorFull<-con.avg.MinHorFull[c(1,4,2,3,6,5,7,8,9),]
#con.avg.AhorHerbs<-con.avg.AhorHerbs[c(1,2,4,5,10,3,9,8,6,11,7),]
#con.avg.MinHorHerbs<-con.avg.MinHorHerbs[c(1,3,5,2,4,6,7,8,9,10,11),]
con.avg.H<-con.avg.H[c(1,2,4,3,5,6,8,7,9,10),]
con.avg.DW<-con.avg.DW[c(1,2,3,4,5),]
con.avg.W<-con.avg.W[c(1,2,3,5,4,6,7,8),] # Without outlier 
con.avg.W2 <- con.avg.W2[c(1,2,3,4,6,5,7),]
con.avg.W.outl <- con.avg.W.outl[c(1,2,3,6,4,5,7,8),]
con.avg.Ahor.dung <- con.avg.Ahor.dung[c(1,3,2,4,5,8,10,7,6,9),]
#con.avg.MinHor.dung <- con.avg.MinHor.dung[c(1,2,4,3,6,5,7,8,9,10,11),]

# remove first row 
con.avg.AhorFull<-con.avg.AhorFull[c(-1),]
con.avg.MinHorFull<-con.avg.MinHorFull[c(-1),]
#con.avg.AhorHerbs<-con.avg.AhorHerbs[c(-1),]
#con.avg.MinHorHerbs<-con.avg.MinHorHerbs[c(-1),]
con.avg.H<-con.avg.H[c(-1),]
con.avg.DW<-con.avg.DW[c(-1),]
con.avg.W<-con.avg.W[c(-1),]
con.avg.W2 <- con.avg.W2[c(-1),]
con.avg.W.outl <- con.avg.W.outl[c(-1),]
con.avg.Ahor.dung <- con.avg.Ahor.dung[c(-1),]
#con.avg.MinHor.dung <- con.avg.MinHor.dung[c(-1),]

# Add SD, rather use 95 % confint
# con.avg.AhorFull$SD <- con.avg.AhorFull$Std..Error * sqrt(length(con.avg.AhorFull$Std..Error))
# con.avg.MinHorFull$SD <- con.avg.MinHorFull$Std..Error * sqrt(length(con.avg.MinHorFull$Std..Error))
# con.avg.H$SD <- con.avg.H$Std..Error * sqrt(length(con.avg.H$Std..Error))
# con.avg.DW$SD <- con.avg.DW$Std..Error * sqrt(length(con.avg.DW$Std..Error))
# con.avg.W$SD <- con.avg.W$Std..Error * sqrt(length(con.avg.W$Std..Error))
# con.avg.Ahor.dung$SD <- con.avg.Ahor.dung$Std..Error * sqrt(length(con.avg.Ahor.dung$Std..Error))
# con.avg.MinHor.dung$SD <- con.avg.MinHor.dung$Std..Error * sqrt(length(con.avg.MinHor.dung$Std..Error))

# Add Significance 
con.avg.AhorFull$sign <- con.avg.AhorFull$Pr...z..
con.avg.AhorFull$sign[con.avg.AhorFull$sign>0.05] <- 1
con.avg.AhorFull$sign[con.avg.AhorFull$sign<0.05] <- 16

con.avg.Ahor.dung$sign <- con.avg.Ahor.dung$Pr...z..
con.avg.Ahor.dung$sign[con.avg.Ahor.dung$sign>0.05] <- 1
con.avg.Ahor.dung$sign[con.avg.Ahor.dung$sign<0.05] <- 16

con.avg.MinHorFull$sign <- con.avg.MinHorFull$Pr...z..
con.avg.MinHorFull$sign[con.avg.MinHorFull$sign>0.05] <- 1
con.avg.MinHorFull$sign[con.avg.MinHorFull$sign<0.05] <- 16

#con.avg.MinHor.dung$sign <- con.avg.MinHor.dung$Pr...z..
#con.avg.MinHor.dung$sign[con.avg.MinHor.dung$sign>0.05] <- 1
#con.avg.MinHor.dung$sign[con.avg.MinHor.dung$sign<0.05] <- 16

con.avg.H$sign <- con.avg.H$Pr...z..
con.avg.H$sign[con.avg.H$sign>0.05] <- 1
con.avg.H$sign[con.avg.H$sign<0.05] <- 16

con.avg.W$sign <- con.avg.W$Pr...z..
con.avg.W$sign[con.avg.W$sign>0.05] <- 1
con.avg.W$sign[con.avg.W$sign<0.05] <- 16

con.avg.W2$sign <- con.avg.W2$Pr...z..
con.avg.W2$sign[con.avg.W2$sign>0.05] <- 1
con.avg.W2$sign[con.avg.W2$sign<0.05] <- 16

con.avg.W.outl$sign <- con.avg.W.outl$Pr...z..
con.avg.W.outl$sign[con.avg.W.outl$sign>0.05] <- 1
con.avg.W.outl$sign[con.avg.W.outl$sign<0.05] <- 16

con.avg.DW$sign <- con.avg.DW$Pr...z..
con.avg.DW$sign[con.avg.DW$sign>0.05] <- 1
con.avg.DW$sign[con.avg.DW$sign<0.05] <- 16

# creating the "frame" and then plotting
# # A-hor 
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Ahor.png")
# par(mar=c(5,12,1,1))
# plot(rep(NA,10),1:10, xlim=c(-1.5,1.5), type="n", ann=F,axes=F, bty="n")
# points(con.avg.Ahor$Estimate,1:10,pch=16,col=c(col.Ahor), lwd=2, cex=2)
# arrows(y0=1:10, x0=con.avg.Ahor$Estimate-con.avg.Ahor$Std..Error, x1=con.avg.Ahor$Estimate+con.avg.Ahor$Std..Error,col=c(col.Ahor), angle=90,length=0.05,code=3,lwd=2)
# abline(v=0)
# axis(1,cex.axis=2)
# axis(2, at=1:10, labels= c("MAP","Herb biomass","PA","Sand","Shrubbiness","Fire frequency","Tree biomass","MAP:Sand","MAP:PA","Sand:PA"),par(las=1),cex.axis=2)
# 
# dev.off()

# A-hor FULL 
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.AhorFull.png")
par(mar=c(5,12,1,1))
plot(rep(NA,8),1:8, xlim=c(-0.5,0.5), type="n", ann=F,axes=F, bty="n")
points(con.avg.AhorFull$Estimate,1:8,pch=con.avg.AhorFull$sign,col=c(col.Ahor.full), lwd=2, cex=2)
arrows(y0=1:8, x0=con.avg.AhorFull$X2.5..,x1=con.avg.AhorFull$X97.5..,col=c(col.Ahor.full), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:8, labels= c("Sand","MAP","PA","Fire frequency","Tree biomass","Encroachment","MAP:PA","Sand:PA"),par(las=1),cex.axis=2)

dev.off()

# A-hor HERBS 
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.AhorHerb.png")
# par(mar=c(5,12,1,1))
# plot(rep(NA,10),1:10, xlim=c(-1,1), type="n", ann=F,axes=F, bty="n")
# points(con.avg.AhorFull$Estimate,1:10,pch=1,col=c(col.Ahor.Herbs), lwd=2, cex=2)
# arrows(y0=1:10, x0=con.avg.AhorFull$X2.5..,x1=con.avg.AhorFull$X97.5..,col=c(col.Ahor.full), angle=90,length=0.05,code=3,lwd=2)
# abline(v=0)
# axis(1,cex.axis=2)
# axis(2, at=1:10, labels= c("Sand","MAP","PA","Shrubbiness","Tree biomass","Herb biomass","MAP:Sand","MAP:PA","Fire frequency","Sand:PA"),par(las=1),cex.axis=2)
# 
# dev.off()

# A-hor with dung 
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Ahor.dung.png")
par(mar=c(5,13,1,1))
plot(rep(NA,9),1:9, xlim=c(-1,1), type="n", ann=F,axes=F, bty="n")
points(con.avg.Ahor.dung$Estimate,1:9,pch=con.avg.Ahor.dung$sign,col=c(col.Ahor.dung), lwd=2, cex=2)
arrows(y0=1:9, x0=con.avg.Ahor.dung$X2.5.., x1=con.avg.Ahor.dung$X97.5..,col=c(col.Ahor.dung), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:9, labels= c("Domestic dung","Sand","Wild dung","MAP","Tree biomass","Fire frequency","Encroachment","Macro fauna","Herb biomass"),par(las=1),cex.axis=2)

dev.off()

# Min-hor FULL
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.MinhorFull.png")
par(mar=c(5,12,1,1))
plot(rep(NA,8),1:8, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
arrows(y0=1:8, x0=con.avg.MinHorFull$X2.5.., x1=con.avg.MinHorFull$X97.5..,col=c(col.min.full), angle=90,length=0.05,code=3,lwd=2)
points(con.avg.MinHorFull$Estimate,1:8,pch=con.avg.MinHorFull$sign,col=c(col.min.full), lwd=2, cex=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:8, labels= c("Sand","Fire frequency","MAP","PA","Tree biomass","MAP:PA","Sand:PA","Encroachment"),par(las=1),cex.axis=2)
dev.off()

# Min-hor HERBS
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.MinhorHerb.png")
# par(mar=c(5,12,1,1))
# plot(rep(NA,10),1:10, xlim=c(-3,3), type="n", ann=F,axes=F, bty="n")
# arrows(y0=1:10, x0=con.avg.MinHorFull$X2.5.., x1=con.avg.MinHorFull$X97.5..,col=c(col.min.full), angle=90,length=0.05,code=3,lwd=2)
# points(con.avg.MinHorFull$Estimate,1:10,pch=con.avg.MinHorFull$sign,col=c(col.min.full), lwd=2, cex=2)
# abline(v=0)
# axis(1,cex.axis=2)
# axis(2, at=1:10, labels= c("Sand","PA","MAP","Shrubbiness","MAP:PA","Sand:PA","MAP:Sand","Herb biomass","Tree biomass","Fire frequency"),par(las=1),cex.axis=2)
# dev.off()

# Min-hor with dung 
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Minhor.dung.png")
# par(mar=c(5,15,1,1))
# plot(rep(NA,10),1:10, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
# arrows(y0=1:10, x0=con.avg.MinHor.dung$X2.5.., x1=con.avg.MinHor.dung$X97.5..,col=c(col.min.dung), angle=90,length=0.05,code=3,lwd=2)
# points(con.avg.MinHor.dung$Estimate,1:10,pch=con.avg.MinHor.dung$sign,col=c(col.min.dung), lwd=2, cex=2)
# abline(v=0)
# axis(1,cex.axis=2)
# axis(2, at=1:10, labels= c("Sand","Fire frequency","Shrubbiness","Tree biomass","MAP","Macro fauna","Wild dung","Livestock dung", "Herb biomass","MAP:Sand"),par(las=1),cex.axis=2)
# dev.off()

# Herb
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Herb.png")
par(mar=c(5,13,1,1))
plot(rep(NA,9),1:9, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.H$Estimate,1:9,pch=con.avg.H$sign,col=c(col.herb), lwd=2, cex=2)
arrows(y0=1:9, x0=con.avg.H$X2.5.., x1=con.avg.H$X97.5..,col=c(col.herb), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:9, labels= c("Fire frequency","PA","Sand","MAP", "Soil Nitrogen","Encroachment","Tree biomass","Sand:PA","MAP:PA"),par(las=1),cex.axis=2)
dev.off()

# DW
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.DW.png")
par(mar=c(5,13,1,1))
plot(rep(NA,4),1:4, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.DW$Estimate,1:4,pch=con.avg.DW$sign,col=c(col.DW), lwd=2, cex=2)
arrows(y0=1:4, x0=con.avg.DW$X2.5.., x1=con.avg.DW$X97.5..,col=c(col.DW), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:4, labels= c("PA","Fire frequency","Encroachment","Tree biomass"),par(las=1),cex.axis=2)
dev.off()

#Woody
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Woody.out.png")
par(mar=c(5,13,1,1))
plot(rep(NA,7),1:7, xlim=c(-1,1), type="n", ann=F,axes=F, bty="n")
points(con.avg.W$Estimate,1:7,pch=con.avg.W$sign,col=c(col.W.out), lwd=2, cex=2)
arrows(y0=1:7, x0=con.avg.W$X2.5.., x1=con.avg.W$X97.5..,col=c(col.W.out), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
#axis(2, at=1:8, labels= c("Fire frequency","MAP","Sand","MAP:Sand","PA", "Soil Nitrogen","MAP:PA","Sand:PA"),par(las=1), cex.axis=2) # With outlier 
axis(2, at=1:7, labels= c("PA","Sand","MAP","Fire frequency","Soil Nitrogen","Sand:PA","MAP:PA"),par(las=1), cex.axis=2) # Without outlier 

dev.off()

#Woody with dung 
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Woody2.png")
par(mar=c(5,13,1,1))
plot(rep(NA,6),1:6, xlim=c(-1.5,1.5), type="n", ann=F,axes=F, bty="n")
points(con.avg.W2$Estimate,1:6,pch=con.avg.W2$sign,col=c(col.W2), lwd=2, cex=2)
arrows(y0=1:6, x0=con.avg.W2$X2.5.., x1=con.avg.W2$X97.5..,col=c(col.W2), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:6, labels= c("Fire frequency","Domestic dung","Macro fauna", "Sand","MAP","Wild dung"),par(las=1), cex.axis=2) 

dev.off()

#Woody with outlier 
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.WoodyOutl.png")
par(mar=c(5,13,1,1))
plot(rep(NA,7),1:7, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.W.outl$Estimate,1:7,pch=con.avg.W.outl$sign,col=c(col.W.out), lwd=2, cex=2)
arrows(y0=1:7, x0=con.avg.W.outl$X2.5.., x1=con.avg.W.outl$X97.5..,col=c(col.W.out), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:7, labels= c("Fire frequency","PA","MAP", "Sand","Soil Nitrogen","MAP:PA","Sand:PA"),par(las=1), cex.axis=2) 

dev.off()


#### Check the relationship between the factors Landuse, climat(dry-int-wet), climate.kat (Dry vs Wet) and texture ####
names(Belowground.full)
plot(AhorC.kg_m2~climate,data = Belowground.full)
plot(MinC.kg_m2~climate,data = Belowground.full)
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
#Min-hor 
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

#### SEM model #### 
# %~~% between correlated error - telling R to not care about the correlation between these variables. 
# MySummary <- summary(modell)
# save(MySummary, file="")
library(MuMIn)
library(piecewiseSEM)
vignette('piecewiseSEM') # too look at the package 

# Add termites 
Termites <- read.csv("Termites/RecalTermEff_Wetseason_Block.csv", head=T)
Termites <- Termites[Termites$Landuse!="Agriculture",]
Termites <- Termites[c(4,6,7)]
Termites <- Termites[Termites$Site!="Seronera",]
Termites <- droplevels(Termites)
Termites$Site<- factor(Termites$Site, levels = c("Makao","Maswa","Mwantimba","Handajega"))
Termites <- Termites[order(Termites[,1]), ]
Termites$Block.ID <- as.numeric(1:16)
Termites <- Termites[c(3,4)]

names(Belowground.full)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Ahor.N <- aggregate(AhorN.kg_m2~Block.ID, mean, data=Belowground.full)
Minhor.N <- aggregate(MinN.kg_m2~Block.ID, mean, data=Belowground.full)
SE.Soil.Ahor <- aggregate(AhorC.kg_m2~Block.ID, SE, data=Belowground.full)
colnames(SE.Soil.Ahor)[2] <- "SE.Soil.Ahor"

Total.Eco.C <- left_join(Total.Eco.C,SE.Soil.Ahor,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Ahor.N,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Minhor.N,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Livestock.dung,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Wild.dung,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Termites,by="Block.ID",drop=F)
Total.Eco.C$Clivestock <- as.numeric(scale(Total.Eco.C$livestock))
Total.Eco.C$Cwild <- as.numeric(scale(Total.Eco.C$wild))
Total.Eco.C$CTermites <- as.numeric(scale(Total.Eco.C$Termite.effect))
Total.Eco.C$CAhorN.kg_m2 <- as.numeric(scale(Total.Eco.C$AhorN.kg_m2))
Total.Eco.C$CMinN.kg_m2 <- as.numeric(scale(Total.Eco.C$MinN.kg_m2))
Total.Eco.C.CnoNA<-Total.Eco.C[!is.na(Total.Eco.C$CFire_frequency.2000_2017),]
Total.Eco.C.CnoNA<-Total.Eco.C.CnoNA[(-16),]
Total.Eco.C.CnoNA<-droplevels(Total.Eco.C.CnoNA)
Total.Eco.C.CnoNA2<-Total.Eco.C.CnoNA[!is.na(Total.Eco.C.CnoNA$livestock),]
Total.Eco.C.CnoNA2 <- droplevels(Total.Eco.C.CnoNA2)

# Variation for each model component BLOCK LEVEL
# A total model of all direct effects based on literature 

Modlist <-   psem(
  lme(Woody~ CFire_frequency.2000_2017 + landuse + CMAP.mm_yr + CSand, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(DW~ Woody + CFire_frequency.2000_2017 + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Herbaceous ~  CSand + CMAP.mm_yr + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.Ahor~ Herbaceous, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.min~ Soil.Ahor + CSand, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(CFire_frequency.2000_2017~ landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA), 
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand # I know these are not correlated 
)

summary(Modlist,Total.Eco.C.CnoNA)
# Good fit, sign: Woody~Fire and Soil.min~Soil.A

# Adding Shrubbiness and Nitrogen
Modlist2 <-   psem(
  lme(Woody~ CFire_frequency.2000_2017 + landuse + CMAP.mm_yr + CSand + Ctot.N.kg_m2, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(DW~ Woody + CFire_frequency.2000_2017 + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Herbaceous ~  CShrubbiness + CSand + CMAP.mm_yr + landuse + Ctot.N.kg_m2,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.Ahor~ Herbaceous, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.min~ Soil.Ahor + CSand, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  #lme(CFire_frequency.2000_2017~ landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA), 
  lme(CShrubbiness~ landuse + CFire_frequency.2000_2017,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CShrubbiness%~~% Woody,
  Soil.Ahor%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  Soil.min%~~%Ctot.N.kg_m2
)

summary(Modlist2,Total.Eco.C.CnoNA)
# Good fit,Soil.min~Soil.A and N~Sand 
# Other sign: Shrub~MAP and Herb~Fire (dont know why)

# Add significant variables, remove non sign.
Modlist3 <-   psem(
  lme(Woody~ CFire_frequency.2000_2017 + landuse + CSand + Ctot.N.kg_m2, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(DW~ Woody + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Herbaceous ~  CShrubbiness + CFire_frequency.2000_2017 + CSand + CMAP.mm_yr + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.Ahor~ Herbaceous, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.min~ Soil.Ahor + CSand + Woody, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(CShrubbiness~  CFire_frequency.2000_2017 + CMAP.mm_yr,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CShrubbiness%~~% Woody,
  Soil.Ahor%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  Soil.min%~~%Ctot.N.kg_m2
)

summary(Modlist3,Total.Eco.C.CnoNA)
# good fit, sign: Soil.min~Soil.A, Shrubbiness~Fire, Shrubbiness~MAP, N~Sand 

# Remove non sign further 
Modlist4 <-   psem(
  lme(Woody~  Ctot.N.kg_m2 + landuse + CSand, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(DW~ landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Herbaceous ~  CShrubbiness + CSand + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.Ahor~ Herbaceous, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.min~ Soil.Ahor + CSand, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(CShrubbiness~  CFire_frequency.2000_2017 + CMAP.mm_yr,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CShrubbiness%~~% Woody,
  Soil.Ahor%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  Soil.min%~~%Ctot.N.kg_m2
)

summary(Modlist4,Total.Eco.C.CnoNA)
# good fit, sign: Soil.min~Soil.A, Shrubbiness~Fire, Shrubbiness~MAP, N~Sand 

# Remove non sign further, and add some almost sign, work on this till I find the best.. 
Modlist5 <-   psem(
  lme(Woody~ landuse, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(DW~ landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Herbaceous ~  CSand + landuse,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  #lme(Soil.Ahor~ Herbaceous, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Soil.min~ Soil.Ahor, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(CShrubbiness~  CFire_frequency.2000_2017 + CMAP.mm_yr,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CShrubbiness%~~% Woody,
  Soil.Ahor%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  Soil.min%~~%Ctot.N.kg_m2
)
SEMsummary <- summary(Modlist5,Total.Eco.C.CnoNA)
SEMsummary$coefficients
SEMsummary$R2
#write.table(SEMsummary$coefficients, file="Ecosystem carbon/SEM.coefficients.txt") 
#write.table(SEMsummary$R2, file="Ecosystem carbon/SEM.R2.txt") 

# Run a SEM with dung and termites included BLOCK
Modlist1 <-   psem(
  lme(Woody~ CFire_frequency.2000_2017 + Clivestock, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(DW~  Herbaceous,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(Herbaceous ~CFire_frequency.2000_2017,random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(Soil.Ahor~ Clivestock + Cwild, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(Soil.min~ Soil.Ahor , random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  #lme(CShrubbiness2~  CFire_frequency.2000_2017 , random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(AhorN.kg_m2~ CSand + CMAP.mm_yr, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(MinN.kg_m2~ CSand, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CTermites~ Cwild , random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(Cwild~ Clivestock + CMAP.mm_yr, random= ~ 1|Region.x,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  CShrubbiness2%~~% Woody, 
  Soil.Ahor%~~%MinN.kg_m2, # We know these are highly correlated, but no path.. 
  Soil.min%~~%MinN.kg_m2, 
  Soil.Ahor%~~%AhorN.kg_m2,  
  Soil.min%~~%AhorN.kg_m2
)
summary(Modlist1,Total.Eco.C.CnoNA2)

SEM.dung <- summary(Modlist1,Total.Eco.C.CnoNA2)  # Good fit
SEM.dung$dTable
write.csv(SEM.dung$coefficients, file = "Ecosystem carbon/SEMdung.csv")

# Run a SEM with dung and termites included FULL Not possible to get a good fit!! 
summary(Belowground.full.CnoNA2)
Modlist2 <-   psem(
  lme(Woody~ CFire_frequency.2000_2017 + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  #lme(DW~  Clivestock + Woody, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(Herbaceous ~  CFire_frequency.2000_2017,random= ~ 1|Region, na.action=na.pass, data=Belowground.full.CnoNA),
  lme(AhorC.kg_m2~Clivestock + Cwild, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(MinC.kg_m2~ AhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(CShrubbiness2~CFire_frequency.2000_2017 + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(AhorN.kg_m2~Cwild + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(MinN.kg_m2~ CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(CTermites~  Cwild + CFire_frequency.2000_2017, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(Cwild~ Clivestock + CShrubbiness2, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  lme(Clivestock~ CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA2),
  CShrubbiness2%~~% Woody, 
  AhorC.kg_m2%~~%AhorN.kg_m2, # We know these are highly correlated, but no path.. 
  MinC.kg_m2%~~%AhorN.kg_m2,
  AhorC.kg_m2%~~%MinN.kg_m2, 
  MinC.kg_m2%~~%MinN.kg_m2
)

summary(Modlist2,Belowground.full.CnoNA2)
par(mfrow=c(1,1))
plot(wild ~ Region, data=Belowground.full.CnoNA2 )
plot(CShrubbiness ~ CMAP.mm_yr, data=Belowground.full.CnoNA)

# Make a SEM for my belowground full data!! 
Modlist.below <-   psem(
    lme(Woody~ Ctot.N.kg_m2 + CFire_frequency.2000_2017 + landuse + CMAP.mm_yr + CSand, random= ~ 1|Region,na.action=na.fail, data=Belowground.full.CnoNA),
    lme(DW~ Woody + CFire_frequency.2000_2017 + landuse,random= ~ 1|Region,na.action=na.fail, data=Belowground.full.CnoNA),
    lme(Herbaceous ~  Ctot.N.kg_m2 + CShrubbiness + CSand + CMAP.mm_yr + landuse,random= ~ 1|Region,na.action=na.fail, data=Belowground.full.CnoNA),
  lme(AhorC.kg_m2~ Herbaceous, random= ~ 1|Region/Block.ID,na.action=na.fail, data=Belowground.full.CnoNA),
  lme(MinC.kg_m2~ AhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.fail, data=Belowground.full.CnoNA),
  #lme(CFire_frequency.2000_2017~ landuse,random= ~ 1|Region,na.action=na.fail, data=Belowground.full.CnoNA), 
  lme(CShrubbiness~ landuse + CFire_frequency.2000_2017,random= ~ 1|Region,na.action=na.fail, data=Belowground.full.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.fail, data=Belowground.full.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CShrubbiness%~~% Woody,
  AhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  MinC.kg_m2%~~%Ctot.N.kg_m2
)

summary(Modlist.below,Belowground.full.CnoNA) # Not a good fit, p=0
?exp
# Adding and removing - adding sand as an exponential factor for herb and tree.. 
names(Belowground.full)
summary(Belowground.full)
Belowground.full <- Belowground.full[-c(61,62,63,64),]
Modlist.below2 <-   psem(
  lme(Woody~ landuse + CSand, random= ~ 1|Region,na.action=na.fail, data=Belowground.full),
  lme(DW~ landuse,random= ~ 1|Region,na.action=na.fail, data=Belowground.full),
  lme(Herbaceous~ CFire_frequency.2000_2017 + MinN.kg_m2,random= ~ 1|Region,na.action=na.omit, data=Belowground.full),
  lme(MinC.kg_m2~ AhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.fail, data=Belowground.full),
  lme(CFire_frequency.2000_2017~ Woody + DW,random= ~ 1|Region,na.action=na.omit, data=Belowground.full),
  lme(CShrubbiness2~CMAP.mm_yr +CSand,random= ~ 1|Region,na.action=na.fail, data=Belowground.full),
  lme(AhorN.kg_m2~ CSand + CMAP.mm_yr,random= ~ 1|Region/Block.ID,na.action=na.fail, data=Belowground.full),
  lme(MinN.kg_m2~ CSand,random= ~ 1|Region/Block.ID,na.action=na.fail, data=Belowground.full),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CShrubbiness2%~~% Woody,
  AhorC.kg_m2%~~%MinN.kg_m2, # We know these are highly correlated, but no path.. 
  MinC.kg_m2%~~%MinN.kg_m2,
  AhorC.kg_m2%~~%AhorN.kg_m2, 
  MinC.kg_m2%~~%AhorN.kg_m2
  )

summary(Modlist.below2,Belowground.full.CnoNA) 
SEM.below <- summary(Modlist.below2,Belowground.full.CnoNA) # Good fit
write.csv(SEM.below$coefficients, file = "Ecosystem carbon/SEMBelow.csv")
