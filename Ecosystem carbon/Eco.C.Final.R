#### REORGANISED VERSION FOR ALL ANALYSIS FOR WRITE-UP #### 
rm(list=ls())
#### 1: PACKAGES #### 
library(tidyr)
library(plyr)
library(dplyr)
#library("Hmisc") # For the correlation plot 
#library(ggplot2)
#library(lattice) # xy.plot

library(nlme)
library(lme4)
library(glmmADMB) 
library(piecewiseSEM) # SEM
library(MuMIn) # to make "model.sel()" of different models 
library(DHARMa) # model validation using simulations
# library(emmeans) # estimated marginal means --> look at this for three ways interactions

#### 2: CLEANING THE DATA #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
colnames(Tree.carbon)
Tree.carbon <- Tree.carbon[,c(1:20,23)]
Tree.carbon <- droplevels(Tree.carbon)

Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/Herbaceous.data/Herbaceous.csv", head=T)
colnames(Herbaceous.carbon)

Deadwood.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/DW.Block.csv",head=T)

Soil.C <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.Carbon.Block.csv", head=T)
colnames(Soil.C)

Soil.texture <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.texture.Tot_Hor.csv",head=T)
Soil.texture <- Soil.texture[,c(1:3,5:8)]
colnames(Soil.texture)
Soil.texture <- droplevels(Soil.texture)

Belowground <- read.csv(file="Ecosystem Carbon/Soil.data/Belowground.Carbon.csv",head=T)
colnames(Belowground)
Belowground <- Belowground[,c(4:33,47)]
Belowground <- droplevels(Belowground)

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

#Relevel by region
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous.carbon$Region<- factor(Herbaceous.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Deadwood.carbon$Region<- factor(Deadwood.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil.carbon$Region<- factor(Soil.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil.texture$Region<- factor(Soil.texture$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# Merge the datasets 
names(Soil.carbon)
names(Tree.carbon)
names(Soil.texture)
names(Deadwood.carbon)
names(Herbaceous.carbon)

Ecosystem.C1 <- merge(Tree.carbon[,c(21,2,3,4,6,8,7,15,18:20)],Herbaceous.carbon[,c(7,11)],all.x = TRUE,by="Block.ID")
Ecosystem.C1b <- merge(Ecosystem.C1, Soil.texture[,c(4:7)],all.x=TRUE, by="Block.ID")
Ecosystem.C1c <- merge(Ecosystem.C1b,Soil.carbon[,c(3,12)],all.x=TRUE, by="Block.ID")

Ecosystem.C2 <- merge(Soil.carbon,Deadwood.carbon[,c(4:6)],all.x = TRUE,by="Block.ID")
Ecosystem.C2a <- merge(Ecosystem.C2,Soil.texture[,c(4:7)],all.x = TRUE,by="Block.ID")
Ecosystem.C2b <- merge(Ecosystem.C2a,Ecosystem.C1c[,c(1,8,10:12)],all.x = TRUE,by="Block.ID")

names(Ecosystem.C1c)
names(Ecosystem.C2b)
Ecosystem.CHerbTree <- Ecosystem.C1c[,c(1:7,13:15,8,10,11,16,9,12)]
Ecosystem.CSoilDW <- Ecosystem.C2b[,c(1:7,15:17,18:20,12,8,10,13)]
SE.Ecosystem.CSoilDW <- Ecosystem.C2b[,c(1:3,9,11,14)]

##      2.1: Fixing the MAP variable ####
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
Deadwood.C <- Belowground.C[,c(1:5,17)]

Belowground.C$BelowgroundC <- Belowground.C$SoilAC.kg_m2 + Belowground.C$SoilMC.kg_m2

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

##      2.2: Make the data into a long format instead of a wide ####
data_long.CHerbTree <- gather(Ecosystem.CHerbTree, Carbon.pool,C.amount, TreeC.kg_m2:HerbC.kg_m2,factor_key=TRUE)

data_long.CSoilDW <- gather(Ecosystem.CSoilDW, Carbon.pool,C.amount, SoilAC.kg_m2:DWC.kg_m2,factor_key=TRUE)

SE.data_long.CSoilDW <- gather(SE.Ecosystem.CSoilDW, Carbon.pool,C.amount, SE.SoilAC.kg_m2:SE.DWC.kg_m2,factor_key=TRUE)

EcosystemC.SoilDW<- cbind(data_long.CSoilDW,SE.data_long.CSoilDW[5])
names(EcosystemC.SoilDW)
names(data_long.CHerbTree)
colnames(EcosystemC.SoilDW) <- c("Block.ID","Region","Vilde.block","landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency.2000_2017","Clay.pip.per","Silt.pip.per","Sand.pip.per","Total.basal.area_m2","TreeBM.kg_m2", "No.trees_m2","mean.N.kg_m2.x","Carbon.pool","C.amount","SE.C.amount")

EcosystemC.SoilDW <- EcosystemC.SoilDW[
  order(EcosystemC.SoilDW[,1], EcosystemC.SoilDW[,2] ),
  ]

data_long.CHerbTree <- data_long.CHerbTree[
  order(data_long.CHerbTree[,1], data_long.CHerbTree[,2] ),
  ]

# Creating one long dataset with all data
data_long.CHerbTree$SE.C.amount <- c(NA*56)
names(data_long.CHerbTree)
str(data_long.CHerbTree)
names(EcosystemC.SoilDW)
str(EcosystemC.SoilDW)
Ecosystem.Carbon <- bind_rows(data_long.CHerbTree, EcosystemC.SoilDW, id=NULL)

Ecosystem.Carbon <- Ecosystem.Carbon[
  order(Ecosystem.Carbon[,1], Ecosystem.Carbon[,2] ),
  ]

# Make NA values for Tree variables to 0 
Ecosystem.Carbon$Total.basal.area_m2[is.na(Ecosystem.Carbon$Total.basal.area_m2)]<- 0
Ecosystem.Carbon$TreeBM.kg_m2[is.na(Ecosystem.Carbon$TreeBM.kg_m2)]<- 0
Ecosystem.Carbon$No.trees_m2[is.na(Ecosystem.Carbon$No.trees_m2)]<- 0
Ecosystem.Carbon$C.amount[is.na(Ecosystem.Carbon$C.amount)]<- 0

Ecosystem.Carbon$Fire_frequency.2000_2017

write.csv(Ecosystem.Carbon,file="Ecosystem carbon/Final.Ecosystem.Carbon.csv")

##    2.3: Making a table for the article ####
names(Belowground)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Properties.Region <- cbind(aggregate(Clay~Region, mean, data=Belowground),
                     aggregate(Clay~Region, SE, data=Belowground)[2],
                     aggregate(Silt~Region, mean, data=Belowground)[2],
                     aggregate(Silt~Region, SE, data=Belowground)[2],
                     aggregate(Sand~Region, mean, data=Belowground)[2],
                     aggregate(Sand~Region, SE, data=Belowground)[2],
                     aggregate(TreeBM.kg_m2~Region, mean, data=Belowground)[2],
                     aggregate(TreeBM.kg_m2~Region, SE, data=Belowground)[2],
                     aggregate(No.trees_m2~Region, mean, data=Belowground)[2],
                     aggregate(No.trees_m2~Region, SE, data=Belowground)[2])

####  3: DATA ANALYSIS ####
# About mixed effect models (nlme package)
# REML = restricted maximum likelihood estimation 
# Fixed effects influence the mean of y, while Random effects influence the variance of y 
# Use REML= F when looking at the fixed effects, maximum likelihood  - use this first when I want to figure out which model to use - when comparing models, then
# Use REML = T when looking at the random effects, and the parameter estimates - when I have found the model I want to use! 
# I have region and block.ID as random effects 
# Choose the model with the smallest AIC value 
#### PACKAGES #### 
library(tidyr)
library(plyr)
library(dplyr)

library(nlme)
library(lme4)
library(piecewiseSEM) 
library(MuMIn) 
##     3.1: Prepare data for modelling ####
#         3.1.1: Uploading data ####
Block.Eco.C <- read.csv("Ecosystem carbon/Final.Ecosystem.Carbon.csv", head=T)
Belowground.full <- read.csv("Ecosystem carbon/Soil.data/Belowground.Carbon.csv", head=T)

fire <- filter(Block.Eco.C, landuse=="Pasture")
fire <- droplevels(fire)
max(fire$Fire_frequency.2000_2017, na.rm=T)
min(fire$Fire_frequency.2000_2017, na.rm=T)

Block.Eco.C$Region<- factor(Block.Eco.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Belowground.full$Region<- factor(Belowground.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

names(Belowground.full)
Belowground.full <- Belowground.full[,c(4:28,30:33,47)]

names(Block.Eco.C)

Block.Eco.C$Carbon.pool <- factor(Block.Eco.C$Carbon.pool, levels=c("TreeC.kg_m2","HerbC.kg_m2","DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))

levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")

# Add Marit's data on accumulated and residual biomass, dung data and nitrogen
#Marit <- read.csv("Ecosystem carbon/Herbaceous.data/Herbaceous.csv", header=TRUE)
#names(Marit)
#Marit <- Marit[c(7,12,13)]

MaritAccum <- read.csv("Ecosystem carbon/Herbaceous.data/AccumHerbaceous.csv", header=TRUE)
names(MaritAccum)
MaritAccum <- MaritAccum[c("Block.ID","HerbC_year.kgm2")]

# Livestock dung 
Livestock.dung<- aggregate(livestock~Block.ID, mean, data=Belowground.full)
Wild.dung<- aggregate(wild~Block.ID, mean, data=Belowground.full)
Total.dung<- aggregate(total.dung~Block.ID, mean, data=Belowground.full)

# total nitrogen
tot.N.kg_m2 <- aggregate(tot.N.kg_m2 ~ Block.ID, mean, data=Belowground.full)

# Add Stu's data on roots (my block = block in the dataset)
Roots.full <- read.csv("Ecosystem carbon/Roots2018.csv", header=TRUE)

Roots <- Roots.full %>%
  filter(excl_vs_open=="Open")
colnames(Roots)
Roots <- Roots[,c(3,4,6,32)]
Roots <- droplevels(Roots)
Roots <- Roots[
  order(Roots[,1],Roots[,3]),]
colnames(Roots)[colnames(Roots) == "area"] <- "Region"

#relevel
levels(as.factor(Roots$Region))
Roots$Region<- factor(Roots$Region, levels = c("Makao","Maswa","Mwantimba","Handajega"))
Roots$Region<-as.factor(Roots$Region)

RootsBlock <- aggregate(Roots.kg.m2~Region + Block, mean, data=Roots)

RootsBlock <- RootsBlock[
  order(RootsBlock[,1],RootsBlock[,2]),]

RootsBlock$Block.ID <- 1:16
RootsBlock <- RootsBlock[c(4,3)]

# Add data to Block.Eco.C and scale variables 
Block.Eco.C <- merge(Block.Eco.C,Livestock.dung, by="Block.ID", all.x = TRUE)
Block.Eco.C <- merge(Block.Eco.C,Wild.dung, by="Block.ID", all.x = TRUE)
Block.Eco.C <- merge(Block.Eco.C,Total.dung, by="Block.ID", all.x = TRUE)
Block.Eco.C <- merge(Block.Eco.C,tot.N.kg_m2, by="Block.ID", all.x = TRUE)
Block.Eco.C <- merge(Block.Eco.C,MaritAccum, by="Block.ID", all.x = TRUE)
Block.Eco.C <- merge(Block.Eco.C,RootsBlock, by="Block.ID", all.x = TRUE)

colnames(Block.Eco.C)
Block.Eco.C$CMAP.mm_yr <- as.numeric(scale(Block.Eco.C$MAP.mm_yr)) 
Block.Eco.C$CFire_frequency <- as.numeric(scale(Block.Eco.C$Fire_frequency.2000_2017)) 
Block.Eco.C$CSand <- as.numeric(scale(Block.Eco.C$Sand.pip.per)) 
Block.Eco.C$CTreeBM.kg_m2 <- as.numeric(scale(Block.Eco.C$TreeBM.kg_m2)) 
Block.Eco.C$Ctot.N.kg_m2 <- as.numeric(scale(Block.Eco.C$tot.N.kg_m2)) 
Block.Eco.C$Clivestock <- as.numeric(scale(Block.Eco.C$livestock))
Block.Eco.C$Cwild <- as.numeric(scale(Block.Eco.C$wild))
Block.Eco.C$Ctotal.dung <- as.numeric(scale(Block.Eco.C$total.dung))
Block.Eco.C$CHerbC_year.kgm2 <- as.numeric(scale(Block.Eco.C$HerbC_year.kgm2))
#Block.Eco.C$CRes.bm.kg_m2 <- as.numeric(scale(Block.Eco.C$Res.bm.kg_m2))
Block.Eco.C$CRoots.kg.m2 <- as.numeric(scale(Block.Eco.C$Roots.kg.m2))

# Carbon pools 
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

Herbaceous$C.amount[Herbaceous$C.amount==0] <- NA
Woody$C.amount[Woody$C.amount==0] <- NA
Soil.Ahor <- cbind(Soil.Ahor,Herbaceous[17])
colnames(Soil.Ahor)[35] <- "Herbaceous"
Soil.Ahor$CHerbaceous <- as.numeric(scale(Soil.Ahor$Herbaceous))
Soil.min <- cbind(Soil.min,Herbaceous[17])
colnames(Soil.min)[35] <- "Herbaceous"
Soil.min$CHerbaceous <- as.numeric(scale(Soil.min$Herbaceous))

# Add data to belowground full 
Belowground.full <- merge(Belowground.full, MaritAccum, by="Block.ID", all.x = TRUE)
Belowground.full <- merge(Belowground.full, Herbaceous[ , c("Block.ID","C.amount")], by = "Block.ID", all.x=TRUE)
Belowground.full <- merge(Belowground.full, Woody[ , c("Block.ID","C.amount")], by = "Block.ID", all.x=TRUE)
Belowground.full <- merge(Belowground.full, DW[ , c("Block.ID","C.amount")], by = "Block.ID", all.x=TRUE)
Belowground.full <- merge(Belowground.full, RootsBlock, by = "Block.ID", all.x=TRUE)

colnames(Belowground.full)[32] <- "Herbaceous"
colnames(Belowground.full)[33] <- "Woody"
colnames(Belowground.full)[34] <- "DW"

#         3.1.2: Creating usefull datasets ####

# Both above and belowground block
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
names(Soil.min)
Total.Eco.C <- cbind(Soil.min[c(1,3:15,17,19:36)],Soil.Ahor[c(17)],Woody[c(17)],DW[c(17)])
colnames(Total.Eco.C)[15] <- "Soil.min"
colnames(Total.Eco.C)[34] <- "Soil.Ahor"
colnames(Total.Eco.C)[35] <- "Woody"
colnames(Total.Eco.C)[36] <- "DW"
Total.Eco.C$tot.C.kg_m2 <- Total.Eco.C$Soil.min + Total.Eco.C$Soil.Ahor + Total.Eco.C$HerbC_year.kgm2 + Total.Eco.C$Woody + Total.Eco.C$DW # Changed herbaceous to the accumulated, then we remove the NA for Total.Eco.C. 

Ahor.N <- aggregate(AhorN.kg_m2~Block.ID, mean, data=Belowground.full)
Minhor.N <- aggregate(MinN.kg_m2~Block.ID, mean, data=Belowground.full)
SE.Soil.Ahor <- aggregate(AhorC.kg_m2~Block.ID, SE, data=Belowground.full)
colnames(SE.Soil.Ahor)[2] <- "SE.Soil.Ahor"

Total.Eco.C <- left_join(Total.Eco.C,SE.Soil.Ahor,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Ahor.N,by="Block.ID",drop=F)
Total.Eco.C <- left_join(Total.Eco.C,Minhor.N,by="Block.ID",drop=F)

#         3.1.3: scaling variables and removing NAs ####

# Belowground.full
colnames(Belowground.full)
Belowground.full$CMAP.mm_yr <- as.numeric(scale(Belowground.full$MAP.mm_yr))
Belowground.full$CSand <- as.numeric(scale(Belowground.full$Sand))
Belowground.full$CFire_frequency.2000_2017 <- as.numeric(scale(Belowground.full$Fire_frequency.2000_2017.x))
Belowground.full$CLast_fire.yr <- as.numeric(scale(Belowground.full$Last_fire.yr))
Belowground.full$Ctot.N.kg_m2 <- as.numeric(scale(Belowground.full$tot.N.kg_m2))
Belowground.full$CTreeBM.kg_m2 <- as.numeric(scale(Belowground.full$TreeBM.kg_m2))
Belowground.full$Ctot.C.kg_m2 <- as.numeric(scale(Belowground.full$tot.C.kg_m2)) 
Belowground.full$Clivestock <- as.numeric(scale(Belowground.full$livestock)) 
Belowground.full$Cwild <- as.numeric(scale(Belowground.full$wild)) 
Belowground.full$Ctotal.dung <- as.numeric(scale(Belowground.full$total.dung)) 
Belowground.full$CHerbC_year.kgm2<- as.numeric(scale(Belowground.full$HerbC_year.kgm2)) 
#Belowground.full$CRes.bm.kg_m2 <- as.numeric(scale(Belowground.full$Res.bm.kg_m2)) 
Belowground.full$CHerbaceous <- as.numeric(scale(Belowground.full$Herbaceous)) 
Belowground.full$CAhorC.kg_m2 <- as.numeric(scale(Belowground.full$AhorC.kg_m2))
Belowground.full$CMinC.kg_m2 <- as.numeric(scale(Belowground.full$MinC.kg_m2))
Belowground.full$CWoody <- as.numeric(scale(Belowground.full$Woody))
Belowground.full$CDW <- as.numeric(scale(Belowground.full$DW))
Belowground.full$CRoots.kg.m2 <- as.numeric(scale(Belowground.full$Roots.kg.m2))

# Block level
colnames(Total.Eco.C)
Total.Eco.C$Ctot.C.kg_m2 <- as.numeric(scale(Total.Eco.C$tot.C.kg_m2)) 
Total.Eco.C$CAhorN.kg_m2 <- as.numeric(scale(Total.Eco.C$AhorN.kg_m2))
Total.Eco.C$CMinN.kg_m2 <- as.numeric(scale(Total.Eco.C$MinN.kg_m2))
Total.Eco.C$CSoil.min <- as.numeric(scale(Total.Eco.C$Soil.min))
Total.Eco.C$CSoil.Ahor <- as.numeric(scale(Total.Eco.C$Soil.Ahor))
Total.Eco.C$CHerbaceous <- as.numeric(scale(Total.Eco.C$Herbaceous))
Total.Eco.C$CWoody <- as.numeric(scale(Total.Eco.C$Woody))
Total.Eco.C$CDW <- as.numeric(scale(Total.Eco.C$DW))

# carbon pools
colnames(Soil.Ahor)
Soil.Ahor$Ctot.C.kg_m2 <- as.numeric(scale(Soil.Ahor$C.amount))
Soil.min$Ctot.C.kg_m2 <- as.numeric(scale(Soil.min$C.amount)) 
Herbaceous$Ctot.C.kg_m2 <- as.numeric(scale(Herbaceous$C.amount)) 
DW$Ctot.C.kg_m2 <- as.numeric(scale(DW$C.amount)) 
Woody$Ctot.C.kg_m2 <- as.numeric(scale(Woody$C.amount)) 

#Remove rows with NA
summary(Total.Eco.C)
# But first rename some variables.. 
colnames(Total.Eco.C) <- c("Block.ID","Region","Vilde.block","Landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency","Clay","Silt","Sand","Tree.basal.area_m2","TreeBM.kg_m2","No.trees_m2","Mean.N.kg_m2.x","Soil.min","Livestock","Wild", "Total.dung","Tot.N.kg_m2","Herb_year.kg_m2","Roots.kg_m2","CMAP.mm_yr","CFire_frequency","CSand","CTreeBM.kg_m2","CTot.N.kg_m2","CLivestock","CWild","CTotal.dung","CHerb_year.kg_m2","CRoots.kg_m2","Herbaceous","CHerbaceous","Soil.Ahor", "Woody","DW","Tot.C.kg_m2","SE.Soil.Ahor","AhorN.kg_m2","MinN.kg_m2","CTot.C.kg_m2","CAhorN.kg_m2","CMinN.kg_m2","CSoil.min","CSoil.Ahor","CWoody","CDW")

# Select only Makao, Maswa, Mwantimba, Handahega, Seronera
levels(Total.Eco.C$Region)
Total.Eco.C.CnoNA<-droplevels(Total.Eco.C[Total.Eco.C$Region=="Makao" | Total.Eco.C$Region== "Maswa" | Total.Eco.C$Region== "Mwantimba" | Total.Eco.C$Region=="Handajega" |
                    Total.Eco.C$Region== "Seronera",]) # Dropping Ikorongo and Park Nyigoti

# Seronera - block 4 lacks woody and root - average woody from other Seroneras + average roots Handajega
HandajegaRoots<-droplevels(Total.Eco.C.CnoNA[Total.Eco.C.CnoNA$Region=="Handajega",]) 
mean(HandajegaRoots$Roots.kg_m2) # 0.1801801
mean(HandajegaRoots$CRoots.kg_m2) # -0.2056412

Seronera134<-droplevels(Total.Eco.C.CnoNA[Total.Eco.C.CnoNA$Block.ID=="17"  | Total.Eco.C.CnoNA$Block.ID=="18" | Total.Eco.C.CnoNA$Block.ID=="19"  | Total.Eco.C.CnoNA$Block.ID=="20"   ,])
mean(Seronera134$Herb_year.kg_m2) #0.1144722
mean(Seronera134$CHerb_year.kg_m2) # 0.4015829

Seronera13<-droplevels(Total.Eco.C.CnoNA[Total.Eco.C.CnoNA$Block.ID=="17"  | Total.Eco.C.CnoNA$Block.ID=="18" | Total.Eco.C.CnoNA$Block.ID=="19"   ,])
mean(Seronera13$Woody) # 0.02024409
mean(Seronera13$CWoody) #-0.3848206

Seronera4<-droplevels(Total.Eco.C.CnoNA[Total.Eco.C.CnoNA$Block.ID=="20" ,]) # Seperates only block 4 Seronera
Seronera4$Roots.kg_m2<-0.1801801
Seronera4$CRoots.kg_m2<- -0.2056412
Seronera4$Woody<-0.02024409
Seronera4$CWoody<- -0.3848206 
Seronera4$Herb_year.kg_m2<-0.1144722
Seronera4$CHerb_year.kg_m2<-0.4015829
  
#Seronera1234<-rbind(Seronera13,Seronera4)
#Total.Eco.C.CnoNAS4<-droplevels(Total.Eco.C.CnoNA[!Total.Eco.C.CnoNA$Region=="Seronera" ,])
Total.Eco.C.CnoNAS4<-droplevels(Total.Eco.C.CnoNA[!Total.Eco.C.CnoNA$Block.ID=="20" ,])
Total.Eco.C.CnoNA1<-rbind(Total.Eco.C.CnoNAS4,Seronera4)

summary(is.na(Total.Eco.C.CnoNA1))
Total.Eco.C.CnoNA2<-droplevels(Total.Eco.C.CnoNA1[complete.cases(Total.Eco.C.CnoNA1[ , c("Livestock","Wild","Total.dung","Fire_frequency","Soil.Ahor","Soil.min","Herb_year.kg_m2","Woody","DW","Roots.kg_m2")]), ])
Total.Eco.C.CnoNA2$Region
summary(is.na(Total.Eco.C.CnoNA2)) 

# Remove Handajega with large woody C ?
#max(Total.Eco.C.CnoNA2$Woody)
#Total.Eco.C.CnoNA2<-droplevels(Total.Eco.C.CnoNA2[Total.Eco.C.CnoNA2$Woody<.5, ])

#Belowground full
Belowground.full <- Belowground.full[-c(61,62,63,64),] # remove outlier
Belowground.full.CnoNA<-Belowground.full[!is.na(Belowground.full$Fire_frequency.2000_2017),]
Belowground.full.CnoNA2 <- Belowground.full.CnoNA[!is.na(Belowground.full.CnoNA$Herbaceous),]
Belowground.full.CnoNA3<-Belowground.full.CnoNA2[!is.na(Belowground.full.CnoNA2$livestock),]
Belowground.full.CnoNA <- droplevels(Belowground.full.CnoNA)
Belowground.full.CnoNA2 <- droplevels(Belowground.full.CnoNA2)
Belowground.full.CnoNA3 <- droplevels(Belowground.full.CnoNA3)
names(Belowground.full)

#         3.1.4: Preparing Total.Eco.C.CnoNA2 and adding non-linear terms ####
# Going to use Total.Eco.C.CnoNA2, so I can remove some variables I dont need. 
colnames(Total.Eco.C.CnoNA2)
Total.Eco.C.CnoNA2 <- Total.Eco.C.CnoNA2[c("Block.ID","Region","Vilde.block","Landuse","MAP.mm_yr","Fire_frequency","Clay","Silt","Sand","Livestock","Wild", "Total.dung","Tot.N.kg_m2","Woody","DW","Herb_year.kg_m2","Roots.kg_m2","Soil.Ahor","Soil.min","CMAP.mm_yr","CFire_frequency","CSand","CTot.N.kg_m2","CLivestock","CWild","CTotal.dung","CHerb_year.kg_m2","CRoots.kg_m2","CSoil.min","CSoil.Ahor","CWoody","CDW")]

# Create non-linear terms so we can keep track in the modelling

# Quadratic terms
# Poly fx over I(variable^2) #useful: use raw https://stackoverflow.com/questions/19484053/what-does-the-r-function-poly-really-do
# Quadratic terms for key predictors (see pair plots)
# Quadratic terms - Fire frequency, Roots, Sand, Livestock, Woody,
Total.Eco.C.CnoNA2$Fire_frequencyPOLY <- as.numeric(poly(Total.Eco.C.CnoNA2$Fire_frequency,2,raw = TRUE)[,2])
Total.Eco.C.CnoNA2$Roots.kg_m2POLY <- as.numeric(poly(Total.Eco.C.CnoNA2$Roots.kg_m2,2,raw = TRUE)[,2])
Total.Eco.C.CnoNA2$SandPOLY <- as.numeric(poly(Total.Eco.C.CnoNA2$Sand,2,raw = TRUE)[,2])
Total.Eco.C.CnoNA2$LivestockPOLY <- as.numeric(poly(Total.Eco.C.CnoNA2$Livestock,2,raw = TRUE)[,2])
Total.Eco.C.CnoNA2$WoodyPOLY <- as.numeric(poly(Total.Eco.C.CnoNA2$Woody,2,raw = TRUE)[,2])

# + CFire_frequencyPOLY + CRoots.kg.m2POLY + CSandPOLY
# + ClivestockPOLY + CWoodyPOLY
# Poly SUBSETS? & !(ClivestockPOLY & CWoody) & !(Clivestock & CSandPOLY) & !(CSoil.Ahor & CWoodyPOLY)

# Scale poly variables 
Total.Eco.C.CnoNA2$CFire_frequencyPOLY <- as.numeric(scale(Total.Eco.C.CnoNA2$Fire_frequencyPOLY))
Total.Eco.C.CnoNA2$CRoots.kg_m2POLY <- as.numeric(scale(Total.Eco.C.CnoNA2$Roots.kg_m2POLY))
Total.Eco.C.CnoNA2$CSandPOLY <- as.numeric(scale(Total.Eco.C.CnoNA2$SandPOLY))
Total.Eco.C.CnoNA2$CLivestockPOLY <- as.numeric(scale(Total.Eco.C.CnoNA2$LivestockPOLY))
Total.Eco.C.CnoNA2$CWoodyPOLY <- as.numeric(scale(Total.Eco.C.CnoNA2$WoodyPOLY))
##      3.2: Correlation of variables (numeric) #### 

# RUN THIS CODE FIRST (FROM STU)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.smooth2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = "red", ...)
}


panel.lines2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    tmp=lm(y[ok]~x[ok])
    abline(tmp)}
}

# Then select the variables to use in the pair function with panel.cor

# Full model 
#names(Belowground.full.CnoNA)
#Model.var.full<-c("AhorC.kg_m2","MinC.kg_m2","MAP.mm_yr","Sand","Clay","Silt","Fire_frequency.2000_2017.x","Last_fire.yr","Woody","tot.N.kg_m2","Herbaceous","Biomass_year.kgm2")

# Subset model
names(Total.Eco.C.CnoNA2)
Model.var.sub<-c("MAP.mm_yr","Fire_frequency","Sand","Tot.N.kg_m2","Livestock","Wild","Total.dung","Woody", "Herb_year.kg_m2", "Soil.Ahor", "Soil.min","Roots.kg_m2","Fire_frequencyPOLY","Roots.kg_m2POLY",   "SandPOLY","LivestockPOLY","WoodyPOLY", "DW")

# Create correlation matrix
#pairs(Belowground.full.CnoNA[,Model.var.red],lower.panel = panel.cor)
pairs(Total.Eco.C.CnoNA2[,Model.var.sub],lower.panel=panel.smooth2, upper.panel= panel.cor, na.action = stats::na.omit)


# NOT UPDATED from here 
# If I want these values in a table:
#Model.var.FULL <- Belowground.full.CnoNA[,c(14,28,26,27,12,13,51,32,64,16,22,21,15)]
#Model.var.Herb <- Belowground.full.CnoNA[,c(14,28,26,12,13,51,32,64,16,22,21,15)]
#Model.var.SUB <- Total.Eco.C.CnoNA2[,c(5,10,8,7,15,35,34,41,42,33,24,6,44,46,47)]

#MycorFULL <- rcorr(as.matrix(Model.var.FULL), type="pearson") # Use the pearson correlation (r-value)
#MycorHERB <- rcorr(as.matrix(Model.var.Herb), type="pearson") # Use the pearson correlation (r-value)
#MycorSUB <- rcorr(as.matrix(Model.var.SUB), type="pearson") # Use the pearson correlation (r-value)
#MycorFULL <- as.data.frame(round(MycorFULL$r, digits=3))
#MycorHERB <- as.data.frame(round(MycorHERB$r, digits=3))
#MycorSUB <- as.data.frame(round(MycorSUB$r, digits=3))
#write.csv(MycorFULL, file= "Ecosystem carbon/VariableCorrelationFULL.csv")
#write.csv(MycorHERB, file= "Ecosystem carbon/VariableCorrelationHERB.csv")
#write.csv(MycorSUB, file= "Ecosystem carbon/VariableCorrelationSUB.csv")

#MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

##     3.3: Correlation of variables (factorial) ####
# Aboveground
names(Total.Eco.C.CnoNA2)

boxplot(Woody~Landuse, data= Total.Eco.C.CnoNA2) # COVARYING 
summary(lm(Woody~Landuse, data= Total.Eco.C.CnoNA2))
boxplot(Herb_year.kg_m2~Landuse, data= Total.Eco.C.CnoNA2) # COVARYING 
summary(lm(Herb_year.kg_m2~Landuse, data= Total.Eco.C.CnoNA2))
# Soil properties 
boxplot(Sand~Landuse, data= Total.Eco.C.CnoNA2) # not covarying
boxplot(Tot.N.kg_m2~Landuse,data=Total.Eco.C.CnoNA2) # not covarying
# Site traits 
boxplot(MAP.mm_yr~Landuse, data= Total.Eco.C.CnoNA2) # not covarying
boxplot(Fire_frequency~Landuse, data= Total.Eco.C.CnoNA2) # not covarying 

# Dung
boxplot(Livestock~Landuse,data=Total.Eco.C.CnoNA2) # COVARYING
boxplot(Wild~Landuse,data=Total.Eco.C.CnoNA2) # not covarying 

####  4: DATA MODELING: MODEL AVERAGING #### 
# Want all variables in the model. However, you don´t want covarying variables in the same model. 
# ALL covarying variables (R2>0.5) with Seronera: 
# LANDUSE:    &!(CWoody & Landuse)&!(CLivestock & Landuse)
# MAP:        &!(CMAP.mm_yr & CWild)&!(CMAP.mm_yr & CTotal.dung)&!(CMAP.mm_yr & CHerb_year.kg_m2)
#             &!(CMAP.mm_yr & CRoots.kg_m2)&!(CMAP.mm_yr & CRoots.kg_m2POLY)
#             &!(CMAP.mm_yr & ClivestockPOLY)
# FIRE:       &!(CFire_frequency & CFire_frequencyPOLY)&!(CFire_frequency & CWoodyPOLY) 
#             &!(CFire_frequency & CWoody) &!(CFire_frequency & CHerb_year.kg_m2) 
# SAND:       &!(CSand & CTot.N.kg_m2)&!(CSand & CWild)
#             &!(CSand & CSoil.Ahor)&!(CSand & CSoil.min)&!(CSand & CSandPOLY)
# NITROGEN:   &!(CTot.N.kg_m2 & CWild)&!(CTot.N.kg_m2 & CSoil.Ahor)&!(CTot.N.kg_m2 & CSoil.min)
#             &!(CTot.N.kg_m2 & CSandPOLY)
# LIVESTOCK   &!(CLivestock & CTotal.dung)&!(CLivestock & CHerb_year.kg_m2)
#             &!(CLivestock & CSoil.Ahor)&!(CLivestock & CLivestockPOLY)
# WILD        &!(CWild & CSoil.Ahor)&!(CWild & CRoots.kg_m2)&!(CWild & CRoots.kg.m2POLY)
#             &!(CWild & CSandPOLY)
# WOODY       &!(CWoody & CHerb_year.kg_m2)&!(CWoody & CFire_frequencyPOLY)
#             &!(CWoody & CWoodyPOLY)
# DW          &!(CDW & CHerb_year.kg_m2)
# HERBACEOUS  &!(CHerb_year.kg_m2 & CFire_frequencyPOLY)&!(CHerb_year.kg_m2 & CWoodyPOLY)
# SOIL A      &!(CSoil.Ahor & CSoil.min)&!(CSoil.Ahor & CSandPOLY)&!(CSoil.Ahor & ClivestockPOLY)
# SOIL MIN    &!(CSoil.min & CSandPOLY)
# ROOTS       &!(CRoots.kg_m2 & CRoots.kg.m2POLY)
# FIRE POLY   &!(CFire_frequencyPOLY & CWoodyPOLY)

##      4.1. Global model for A-hor C ####
source("Ecosystem Carbon/chkRank.drop.cols.R")
X1 <- model.matrix(~Landuse+CMAP.mm_yr+CFire_frequency+CSand+CLivestock+ #CTot.N.kg_m2
                     CWild+CTotal.dung+CHerb_year.kg_m2+CRoots.kg_m2+
                     CSoil.min+CWoody+CDW+CFire_frequencyPOLY+CRoots.kg_m2POLY+
                     CSandPOLY+CLivestockPOLY+CWoodyPOLY, data =  Total.Eco.C.CnoNA2)
X2<-chkRank.drop.cols(X1, kind= "warn+drop.cols")
TrtList<-colnames(X1)
TrtListRkreduced<-colnames(X2)
setdiff(TrtList,TrtListRkreduced) # "CTotal.dung" 


Ahor.block.full<-lmer(CSoil.Ahor~ Landuse+CMAP.mm_yr+CFire_frequency+CSand+CLivestock+
                                CWild+CTotal.dung+CHerb_year.kg_m2+CRoots.kg_m2+
                                CSoil.min+CWoody+CDW+CFire_frequencyPOLY+CRoots.kg_m2POLY+
                                CSandPOLY+CLivestockPOLY+CWoodyPOLY+
                   (1|Region),data = Total.Eco.C.CnoNA2, REML=F,na.action=na.fail)

# Model averaging: All possible models between null and global
# subset the covarying predictor variables (R2>0.50)
modsetbelowA.full<-dredge(Ahor.block.full,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                            !(CWoody & Landuse)&!(CLivestock & Landuse)
                          &!(CMAP.mm_yr & CWild)&!(CMAP.mm_yr & CTotal.dung)&!(CMAP.mm_yr & CHerb_year.kg_m2)
                          &!(CMAP.mm_yr & CRoots.kg_m2)&!(CMAP.mm_yr & CRoots.kg_m2POLY)
                          &!(CMAP.mm_yr & CLivestockPOLY)
                          &!(CFire_frequency & CFire_frequencyPOLY)&!(CFire_frequency & CWoodyPOLY)
                          &!(CFire_frequency & CWoody) &!(CFire_frequency & CHerb_year.kg_m2)
                          &!(CSand & CWild)
                          &!(CSand & CSoil.min)&!(CSand & CSandPOLY)
                          &!(CLivestock & CTotal.dung)&!(CLivestock & CHerb_year.kg_m2)
                          &!(CLivestock & CLivestockPOLY)
                          &!(CWild & CRoots.kg_m2)&!(CWild & CRoots.kg_m2POLY)
                          &!(CWild & CSandPOLY)
                          &!(CWoody & CHerb_year.kg_m2)&!(CWoody & CFire_frequencyPOLY)
                          &!(CWoody & CWoodyPOLY)
                          &!(CDW & CHerb_year.kg_m2)
                          &!(CHerb_year.kg_m2 & CFire_frequencyPOLY)&!(CHerb_year.kg_m2 & CWoodyPOLY)
                          &!(CSoil.min & CSandPOLY)
                          &!(CRoots.kg_m2 & CRoots.kg_m2POLY)
                          &!(CFire_frequencyPOLY & CWoodyPOLY))

#Averaging terms
modselbelowA.full<-model.sel(modsetbelowA.full) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA.full<-model.avg(modselbelowA.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA.full)#Importance of each variable
confint.Ahor.full <- confint(modavgbelowA.full)
coef.Ahor.full <- summary(modavgbelowA.full)$coefmat.subset
Ahor.full <- cbind(coef.Ahor.full, confint.Ahor.full)

# Reduced model based on variable importance (<0.10) and/or p-value (>0.10)

# Reduced model with Seronera...
Ahor.blockS<-lmer(CSoil.Ahor~ Landuse+CSand+CDW+
                        CSoil.min+CWild+CLivestock +
                        (1|Region),data = Total.Eco.C.CnoNA2, 
                        REML=F,na.action=na.fail)
summary(Ahor.blockS) # Singularity effect - random factor is not explaining anything
drop1(Ahor.blockS,test="Chisq") # CSand or CSandPoly
plot(Soil.Ahor~Sand,Total.Eco.C.CnoNA2) # looks relatively linear use CSand
AIC(Ahor.blockS) #14.32976

# Residual plot
res <- simulateResiduals(Ahor.blockS, plot = T) # All Good! 

# Model averaging: All possible models between null and global
modsetbelowAS<-dredge(Ahor.blockS,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                        !(CLivestock & Landuse)
                      &!(CSand & CWild)&!(CSand & CSoil.min))

#Averaging terms
modselbelowAS<-model.sel(modsetbelowAS) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowAS<-model.avg(modselbelowAS)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowAS)#Importance of each variable
write.table(importance(modavgbelowAS),file="Ecosystem carbon/Model_average/importanceAhor.txt")
confint.AhorS <- confint(modavgbelowAS)
coef.AhorS <- summary(modavgbelowAS)$coefmat.subset
AhorS <- cbind(coef.AhorS, confint.AhorS)
write.table(AhorS, file="Ecosystem carbon/Model_average/ConAvgAhor.txt") 

# No Seronera data - reduced model
Ahor.block<-lmer(CSoil.Ahor~ Landuse+CSand+CLivestock+
                   CWild+CHerb_year.kg_m2+
                   CSoil.min+ #CSandPOLY+
                   (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                 na.action=na.fail)

summary(Ahor.block) # Singularity effect - random factor is not explaining anything
drop1(Ahor.block,test="Chisq") # CSand or CSandPoly
plot(Soil.Ahor~Sand,Total.Eco.C.CnoNA2) # looks relatively linear use CSand
AIC(Ahor.block)

# Model averaging: All possible models between null and global
modsetbelowA<-dredge(Ahor.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                       !(CLivestock & Landuse)
                     &!(CSand & CLivestock)&!(CSand & CWild)
                     &!(CSand & CSoil.min))

#Averaging terms
modselbelowA<-model.sel(modsetbelowA) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA<-model.avg(modselbelowA)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA)#Importance of each variable

##      4.2. Global model for Mineral hor C  ####
Min.block.full<-lmer(CSoil.min~ Landuse+CMAP.mm_yr+CFire_frequency+CSand+CLivestock+ # CTot.N.kg_m2        
                  CWild+CTotal.dung+CHerb_year.kg_m2+CRoots.kg_m2+
                  CSoil.Ahor+CWoody+CDW+CFire_frequencyPOLY+CRoots.kg_m2POLY+
                  CSandPOLY+CLivestockPOLY+CWoodyPOLY+
                  (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelowM.full<-dredge(Min.block.full,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                            !(CWoody & Landuse)&!(CLivestock & Landuse)
                          &!(CMAP.mm_yr & CWild)&!(CMAP.mm_yr & CTotal.dung)&!(CMAP.mm_yr & CHerb_year.kg_m2)
                          &!(CMAP.mm_yr & CRoots.kg_m2)&!(CMAP.mm_yr & CRoots.kg_m2POLY)
                          &!(CMAP.mm_yr & CLivestockPOLY)
                          &!(CFire_frequency & CFire_frequencyPOLY)&!(CFire_frequency & CWoodyPOLY)
                          &!(CFire_frequency & CWoody) &!(CFire_frequency & CHerb_year.kg_m2)
                          &!(CSand & CWild)
                          &!(CSand & CSoil.Ahor)&!(CSand & CSandPOLY)
                          &!(CLivestock & CTotal.dung)&!(CLivestock & CHerb_year.kg_m2)
                          &!(CLivestock & CSoil.Ahor)&!(CLivestock & CLivestockPOLY)
                          &!(CWild & CSoil.Ahor)&!(CWild & CRoots.kg_m2)&!(CWild & CRoots.kg_m2POLY)
                          &!(CWild & CSandPOLY)
                          &!(CWoody & CHerb_year.kg_m2)&!(CWoody & CFire_frequencyPOLY)
                          &!(CWoody & CWoodyPOLY)
                          &!(CDW & CHerb_year.kg_m2)
                          &!(CHerb_year.kg_m2 & CFire_frequencyPOLY)&!(CHerb_year.kg_m2 & CWoodyPOLY)
                          &!(CSoil.Ahor & CSandPOLY)&!(CSoil.Ahor & CLivestockPOLY)
                          &!(CRoots.kg_m2 & CRoots.kg_m2POLY)
                          &!(CFire_frequencyPOLY & CWoodyPOLY))

modselbelowM.full<-model.sel(modsetbelowM.full) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowM.full<-model.avg(modselbelowM.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowM.full)#Importance of each variable
confint.Minhor.full <- confint(modavgbelowM.full)
coef.Minhor.full <- summary(modavgbelowM.full)$coefmat.subset
Minhor.full <- cbind(coef.Minhor.full, confint.Minhor.full)

# Reduced model based on variable importance (<0.10) or p-value (>0.10), excluding one if both poly and normal appear. 

# Reduced model with Seronera...
Min.blockS<-lmer(CSoil.min~ CSoil.Ahor+CRoots.kg_m2+CSand+CWoodyPOLY+
                   CHerb_year.kg_m2+Landuse+CDW+
                       (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                     na.action=na.fail)

summary(Min.blockS) # Singular fit again - 
drop1(Min.blockS,test="Chisq") 
anova(Min.blockS)
AIC(Min.blockS) #35.58771

# Residual plot
res <- simulateResiduals(Min.blockS, plot = T) # KS significant deviation, but others OK

modsetbelowMS<-dredge(Min.blockS,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                        !(CDW & CHerb_year.kg_m2) 
                      &!(CSand & CSoil.Ahor)
                      &!(CHerb_year.kg_m2 & CWoodyPOLY))

modselbelowMS<-model.sel(modsetbelowMS) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowMS<-model.avg(modselbelowMS)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowMS)#Importance of each variable
write.table(importance(modavgbelowMS),file="Ecosystem carbon//Model_average/importanceMinhor.txt")
confint.MinhorS <- confint(modavgbelowMS)
coef.MinhorS <- summary(modavgbelowMS)$coefmat.subset
MinhorS <- cbind(coef.MinhorS, confint.MinhorS)
write.table(MinhorS, file="Ecosystem carbon//Model_average/ConAvgMinHor.txt")

# Reduce model without Seronera...
Min.block<-lmer(CSoil.min~ Landuse+CSand+       
                       CHerb_year.kg_m2+CRoots.kg_m2+
                       CSoil.Ahor+
                       (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                     na.action=na.fail)

summary(Min.block) # Singular fit again - 
drop1(Min.block,test="Chisq") # MAP,Fire,N,TreeBM
anova(Min.block)
AIC(Min.block)

# Model averaging: All possible models between null and global
modsetbelowM<-dredge(Min.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                            !(CSand & CSoil.Ahor))
                    # &!(CTot.N.kg_m2 & CSoil.Ahor)
                    # &!(CTot.N.kg_m2 & CSandPOLY)&!(CSand & CTot.N.kg_m2))

modselbelowM<-model.sel(modsetbelowM) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowM<-model.avg(modselbelowM)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowM)#Importance of each variable

##      4.3. Global model for Herbs ####
# HERBACEOUS 
Herbaceous.block.full<-lmer(CHerb_year.kg_m2~ Landuse+CMAP.mm_yr+CFire_frequency+
                              CSand+CLivestock+ CTot.N.kg_m2+
                         CWild+CTotal.dung+CSoil.Ahor+CRoots.kg_m2+
                         CSoil.min+CWoody+CDW+CFire_frequencyPOLY+CRoots.kg_m2POLY+
                         CSandPOLY+CLivestockPOLY+CWoodyPOLY+
                         (1|Region),data = Total.Eco.C.CnoNA2, REML=F, 
                       na.action=na.fail)

# Model averaging: All possible models between null and global
modsetHerb.full<-dredge(Herbaceous.block.full,trace = TRUE, rank = "AICc", REML = FALSE, subset=            
                          !(CWoody & Landuse)&!(CLivestock & Landuse)
                        &!(CMAP.mm_yr & CWild)&!(CMAP.mm_yr & CTotal.dung)
                        &!(CMAP.mm_yr & CRoots.kg_m2)&!(CMAP.mm_yr & CRoots.kg_m2POLY)
                        &!(CMAP.mm_yr & CLivestockPOLY)
                        &!(CFire_frequency & CFire_frequencyPOLY)&!(CFire_frequency & CWoodyPOLY)
                        &!(CFire_frequency & CWoody)
                        &!(CSand & CTot.N.kg_m2)&!(CSand & CWild)
                        &!(CSand & CSoil.Ahor)&!(CSand & CSoil.min)&!(CSand & CSandPOLY)
                        &!(CTot.N.kg_m2 & CWild)&!(CTot.N.kg_m2 & CSoil.Ahor)&!(CTot.N.kg_m2 & CSoil.min)
                        &!(CTot.N.kg_m2 & CSandPOLY)
                        &!(CLivestock & CTotal.dung)
                        &!(CLivestock & CSoil.Ahor)&!(CLivestock & CLivestockPOLY)
                        &!(CWild & CSoil.Ahor)&!(CWild & CRoots.kg_m2)&!(CWild & CRoots.kg_m2POLY)
                        &!(CWild & CSandPOLY)
                        &!(CWoody & CFire_frequencyPOLY) &!(CWoody & CWoodyPOLY)
                        &!(CSoil.Ahor & CSoil.min)&!(CSoil.Ahor & CSandPOLY)&!(CSoil.Ahor & CLivestockPOLY)
                        &!(CSoil.min & CSandPOLY)
                        &!(CRoots.kg_m2 & CRoots.kg_m2POLY)
                        &!(CFire_frequencyPOLY & CWoodyPOLY))

modselHerb.full<-model.sel(modsetHerb.full) #Model selection table giving AIC, deltaAIC and weighting
modavgHerb.full<-model.avg(modselHerb.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgHerb.full)#Importance of each variable
#Estimated coefficients given weighting
confint.Herb.full <- confint(modavgHerb.full)
coef.Herb.full <- summary(modavgHerb.full)$coefmat.subset
Herb.full <- cbind(coef.Herb.full, confint.Herb.full)

# Reduced model based on variable importance (>0.10) and p-value (<0.10), Total.dung not important, but p-value<0.05 - I keep it in the reduced model 

# Reduced herbaceous model with Seronera
Herbaceous.blockS<-lmer(CHerb_year.kg_m2~ CDW +CFire_frequency+CMAP.mm_yr+Landuse+
                          CTot.N.kg_m2+ CLivestock+CSand+CWoody+
                              (1|Region),data = Total.Eco.C.CnoNA2, REML=F, 
                            na.action=na.fail)
plot(Herb_year.kg_m2~ Fire_frequency,Total.Eco.C.CnoNA2) # looks linear
summary(Herbaceous.blockS)
drop1(Herbaceous.blockS,test="Chisq")  
anova(Herbaceous.blockS)
AIC(Herbaceous.blockS) #29.45418

# Residual plot
res <- simulateResiduals(Herbaceous.blockS, plot = T) # OK

modsetaboveHS<-dredge(Herbaceous.blockS,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                       !(CWoody & Landuse)
                      &!(CLivestock & CWoody))

modselaboveHS<-model.sel(modsetaboveHS) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveHS<-model.avg(modselaboveHS)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveHS)#Importance of each variable
write.table(importance(modavgaboveHS),file="Ecosystem carbon/Model_average/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.HerbS <- confint(modavgaboveHS)
coef.HerbS <- summary(modavgaboveHS)$coefmat.subset
HerbS <- cbind(coef.HerbS, confint.HerbS)
write.table(HerbS, file="Ecosystem carbon/Model_average/ConAvgH.txt") 

# Reduced herbaceous model without Seronera
Herbaceous.block<-lmer(CHerb_year.kg_m2~ Landuse+CMAP.mm_yr+CFire_frequency+CTot.N.kg_m2+
                              CTotal.dung+
                              CWoody+CDW+CFire_frequencyPOLY+
                              (1|Region),data = Total.Eco.C.CnoNA2, REML=F, 
                            na.action=na.fail)

summary(Herbaceous.block)
drop1(Herbaceous.block,test="Chisq")  
anova(Herbaceous.block)
AIC(Herbaceous.block) #12.17814

# Model averaging: All possible models between null and global
modsetaboveH<-dredge(Herbaceous.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                       !(CWoody & Landuse)
                     &!(CFire_frequency & CFire_frequencyPOLY)
                     &!(CTotal.dung & CMAP.mm_yr)
                     &!(CTotal.dung & CWoody))

modselaboveH<-model.sel(modsetaboveH) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveH<-model.avg(modselaboveH)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveH)#Importance of each variable

##      4.4. Global model for DW #### 
DW.block.full<-lmer(CDW~ Landuse+CFire_frequency+CLivestock+
                 CWild+CTotal.dung+
                 CWoody+CHerb_year.kg_m2+CFire_frequencyPOLY+
                 CLivestockPOLY+CWoodyPOLY+
                 (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
               na.action=na.fail)

# Model averaging: All possible models between null and global
modsetDW.full<-dredge(DW.block.full,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                        !(CWoody & Landuse)&!(CLivestock & Landuse)
                      &!(CFire_frequency & CFire_frequencyPOLY)&!(CFire_frequency & CWoodyPOLY)
                      &!(CFire_frequency & CWoody) &!(CFire_frequency & CHerb_year.kg_m2)
                      &!(CLivestock & CTotal.dung)&!(CLivestock & CHerb_year.kg_m2)
                      &!(CLivestock & CLivestockPOLY)
                      &!(CWoody & CHerb_year.kg_m2)&!(CWoody & CFire_frequencyPOLY)
                      &!(CWoody & CWoodyPOLY)
                      &!(CHerb_year.kg_m2 & CFire_frequencyPOLY)&!(CHerb_year.kg_m2 & CWoodyPOLY)
                      &!(CFire_frequencyPOLY & CWoodyPOLY))

modselDW.full<-model.sel(modsetDW.full) #Model selection table giving AIC, deltaAIC and weighting
modavgDW.full<-model.avg(modselDW.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgDW.full)#Importance of each variable
confint.DW.full <- confint(modavgDW.full)
coef.DW.full <- summary(modavgDW.full)$coefmat.subset
DW.full <- cbind(coef.DW.full, confint.DW.full)

# Reduce model with Seronera
DW.blockS<-lmer(CDW~ CHerb_year.kg_m2 + Landuse + CWild + CLivestockPOLY + 
                 (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
               na.action=na.fail)

summary(DW.blockS)
drop1(DW.blockS,test="Chisq")  
anova(DW.blockS)
AIC(DW.blockS) #53.15495

# Residual plot
res <- simulateResiduals(DW.blockS, plot = T) # Issue with residual vs, predicted deviations detected...similar to be a pattern?
# Issues with outliers in dataset

modsetaboveDWS<-dredge(DW.blockS,trace = TRUE, rank = "AICc", REML = FALSE)

modselaboveDWS<-model.sel(modsetaboveDWS) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveDWS<-model.avg(modselaboveDWS)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveDWS)#Importance of each variable
write.table(importance(modavgaboveDWS),file="Ecosystem carbon/Model_average/importanceaboveDW.txt")
confint.DWS <- confint(modavgaboveDWS)
coef.DWS <- summary(modavgaboveDWS)$coefmat.subset
DWS <- cbind(coef.DWS, confint.DWS)
write.table(DWS, file="Ecosystem carbon/Model_average/ConAvgDW.txt") 

# Reduce model without Seronera
DW.block<-lmer(CDW~ Landuse+
                      CHerb_year.kg_m2+CFire_frequencyPOLY+
                      (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                    na.action=na.fail)

summary(DW.block)
drop1(DW.block,test="Chisq")  
anova(DW.block)
AIC(DW.block) #41.53007

# Model averaging: All possible models between null and global
modsetaboveDW<-dredge(DW.block,trace = TRUE, rank = "AICc", REML = FALSE)

modselaboveDW<-model.sel(modsetaboveDW) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveDW<-model.avg(modselaboveDW)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveDW)#Importance of each variable

##      4.5. Global model for Woody #### 
Woody.block.full<-lmer(CWoody~Landuse+CMAP.mm_yr+CFire_frequency+CSand+CLivestock+CTot.N.kg_m2+
                    CWild+CTotal.dung+CRoots.kg_m2
                  +CHerb_year.kg_m2+CFire_frequencyPOLY+CRoots.kg_m2POLY+
                    CSandPOLY+CLivestockPOLY+
                    (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetWoody.full<-dredge(Woody.block.full,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                           !(CLivestock & Landuse)
                         &!(CMAP.mm_yr & CWild)&!(CMAP.mm_yr & CTotal.dung)&!(CMAP.mm_yr & CHerb_year.kg_m2)
                         &!(CMAP.mm_yr & CRoots.kg_m2)&!(CMAP.mm_yr & CRoots.kg_m2POLY)
                         &!(CMAP.mm_yr & CLivestockPOLY)
                         &!(CFire_frequency & CFire_frequencyPOLY)
                         &!(CFire_frequency & CHerb_year.kg_m2)
                         &!(CSand & CTot.N.kg_m2)&!(CSand & CWild)
                         &!(CSand & CSandPOLY)
                         &!(CTot.N.kg_m2 & CWild)
                         &!(CTot.N.kg_m2 & CSandPOLY)
                         &!(CLivestock & CTotal.dung)&!(CLivestock & CHerb_year.kg_m2)
                         &!(CLivestock & CLivestockPOLY)
                         &!(CWild & CRoots.kg_m2)&!(CWild & CRoots.kg_m2POLY)
                         &!(CWild & CSandPOLY)
                         &!(CHerb_year.kg_m2 & CFire_frequencyPOLY)
                         &!(CRoots.kg_m2 & CRoots.kg_m2POLY))


modselWoody.full<-model.sel(modsetWoody.full) #Model selection table giving AIC, deltaAIC and weighting
modavgWoody.full<-model.avg(modselWoody.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgWoody.full)#Importance of each variable
summary(modavgWoody.full)#Estimated coefficients given weighting
confint.woody.full <- confint(modavgWoody.full)
coef.woody.full <- summary(modavgWoody.full)$coefmat.subset
woody.full <- cbind(coef.woody.full, confint.woody.full)

plot(Woody~Sand, data=Total.Eco.C.CnoNA2)

# Reduced model with Seronera 
# Total dung also significant, but correlated with livestock - so I keep livestock. 
Woody.blockS<-lmer(CWoody~ CFire_frequencyPOLY+CSand+CLivestock+Landuse+
                     CHerb_year.kg_m2+CTot.N.kg_m2+
                         (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                       na.action=na.fail)
summary(Woody.blockS)
drop1(Woody.blockS,test="Chisq")  
anova(Woody.blockS)
AIC(Woody.blockS) #33.62606

# Residual plot
res <- simulateResiduals(Woody.blockS, plot = T) # Issues residual vs predicted?

modsetaboveWS<-dredge(Woody.blockS,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                        !(CLivestock & Landuse)
                      &!(CSand & CTot.N.kg_m2)
                      &!(CLivestock & CHerb_year.kg_m2)
                      &!(CHerb_year.kg_m2 & CFire_frequencyPOLY))

modselaboveWS<-model.sel(modsetaboveWS) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveWS<-model.avg(modselaboveWS)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveWS)#Importance of each variable
write.table(importance(modavgaboveWS),file="Ecosystem carbon/Model_average/importanceaboveW.txt")
summary(modavgaboveWS)#Estimated coefficients given weighting
confint.woodyS <- confint(modavgaboveWS)
coef.woodyS <- summary(modavgaboveWS)$coefmat.subset
woodyS <- cbind(coef.woodyS, confint.woodyS)
write.table(woodyS, file="Ecosystem carbon/Model_average/ConAvgW.txt") 

# Check out fire: 
plot(Woody~ Fire_frequencyPOLY,Total.Eco.C.CnoNA2)

Total.Eco.C.CnoNA2$Region
Woody.blockSfire<-lmer(CWoody~ CFire_frequencyPOLY+(1|Region),data = Total.Eco.C.CnoNA2, REML=F)
ff <- seq(min(Total.Eco.C.CnoNA2$CFire_frequencyPOLY), max(Total.Eco.C.CnoNA2$CFire_frequencyPOLY), length = 50)
Sero <- rep("Seronera", length = 50)
newdata<-data.frame(CFire_frequencyPOLY =ff,Region=Sero)
tt<-predict(Woody.blockSfire, newdata, type="response")
plot(CWoody~ CFire_frequencyPOLY,Total.Eco.C.CnoNA2)
lines(ff,tt,col="red",lwd=1.5)

Woody.blockSfire<-lm(Woody~ Fire_frequencyPOLY,data = Total.Eco.C.CnoNA2)
par(mfrow=c(2,2))
plot(Woody.blockSfire) 
# in Residual vs Leverage, the large tree is outside Cook´s distance, so its an outlier? 

# Reduced model without Seronera
Woody.block<-lmer(CWoody~ Landuse+CFire_frequency+CSand+CLivestock+
                    CTot.N.kg_m2+CHerb_year.kg_m2+CFire_frequencyPOLY+
                    (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                  na.action=na.fail)

summary(Woody.block)
drop1(Woody.block,test="Chisq")  
anova(Woody.block)
AIC(Woody.block) 

# Model averaging: All possible models between null and global
modsetaboveW<-dredge(Woody.block,trace = TRUE, rank = "AICc", REML = FALSE, subset= 
                     !(CFire_frequency & CFire_frequencyPOLY)
                     &!(CSand & CTot.N.kg_m2)&!(CSand & CLivestock)
                     &!(CSand & CFire_frequencyPOLY)
                     &!(CFire_frequency & CFire_frequencyPOLY))

modselaboveW<-model.sel(modsetaboveW) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveW<-model.avg(modselaboveW)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveW)#Importance of each variable

##      4.6. Global model for Roots #### 
Root.block.full<-lmer(CRoots.kg_m2~ Landuse+CMAP.mm_yr+CFire_frequency+
                        CSand+CLivestock+CTot.N.kg_m2+
                   CWild+CTotal.dung+CSoil.Ahor+CDW+
                   CSoil.min+CWoody+CHerb_year.kg_m2+CFire_frequencyPOLY+
                   CSandPOLY+CLivestockPOLY+CWoodyPOLY+
                 (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
               na.action=na.fail)

# Model averaging: All possible models between null and global
modsetRoot.full<-dredge(Root.block.full,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                          !(CWoody & Landuse)&!(CLivestock & Landuse)
                        &!(CMAP.mm_yr & CWild)&!(CMAP.mm_yr & CTotal.dung)&!(CMAP.mm_yr & CHerb_year.kg_m2)
                        &!(CMAP.mm_yr & CLivestockPOLY)
                        &!(CFire_frequency & CFire_frequencyPOLY)&!(CFire_frequency & CWoodyPOLY) 
                        &!(CFire_frequency & CWoody) &!(CFire_frequency & CHerb_year.kg_m2)
                        &!(CSand & CTot.N.kg_m2)&!(CSand & CWild)
                        &!(CSand & CSoil.Ahor)&!(CSand & CSoil.min)&!(CSand & CSandPOLY)
                        &!(CTot.N.kg_m2 & CWild)&!(CTot.N.kg_m2 & CSoil.Ahor)&!(CTot.N.kg_m2 & CSoil.min)
                        &!(CTot.N.kg_m2 & CSandPOLY)
                        &!(CLivestock & CTotal.dung)&!(CLivestock & CHerb_year.kg_m2)
                        &!(CLivestock & CSoil.Ahor)&!(CLivestock & CLivestockPOLY)
                        &!(CWild & CSoil.Ahor)
                        &!(CWild & CSandPOLY)
                        &!(CWoody & CHerb_year.kg_m2)&!(CWoody & CFire_frequencyPOLY)
                        &!(CWoody & CWoodyPOLY)
                        &!(CDW & CHerb_year.kg_m2)
                        &!(CHerb_year.kg_m2 & CFire_frequencyPOLY)&!(CHerb_year.kg_m2 & CWoodyPOLY)
                        &!(CSoil.Ahor & CSoil.min)&!(CSoil.Ahor & CSandPOLY)&!(CSoil.Ahor & CLivestockPOLY)
                        &!(CSoil.min & CSandPOLY)
                        &!(CFire_frequencyPOLY & CWoodyPOLY))

modselRoot.full<-model.sel(modsetRoot.full) #Model selection table giving AIC, deltaAIC and weighting
modavgRoot.full<-model.avg(modselRoot.full)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgRoot.full)#Importance of each variable
confint.Root.full <- confint(modavgRoot.full)
coef.Root.full <- summary(modavgRoot.full)$coefmat.subset
Root.full<- cbind(coef.Root.full, confint.Root.full)

# Reduced model with Seronera
Root.blockS<-lmer(CRoots.kg_m2~ Landuse+ CMAP.mm_yr+ CTot.N.kg_m2+
                                CSoil.min+ CSandPOLY + CDW+ CTotal.dung +
                   (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                 na.action=na.fail)
plot(Roots.kg_m2~Wild,data=Total.Eco.C.CnoNA2)
summary(Root.blockS)
drop1(Root.blockS,test="Chisq")  
anova(Root.blockS)
AIC(Root.blockS) #42.17857

# Residual plot
res <- simulateResiduals(Root.blockS, plot = T) # OK

modsetaboveRootS<-dredge(Root.blockS,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                         !(CMAP.mm_yr & CTotal.dung)
                         &!(CTot.N.kg_m2 & CSandPOLY))

modselaboveRootS<-model.sel(modsetaboveRootS) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveRootS<-model.avg(modselaboveRootS)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveRootS)#Importance of each variable
write.table(importance(modavgaboveRootS),file="Ecosystem carbon/Model_average/importanceRoots.txt")
confint.RootS <- confint(modavgaboveRootS)
coef.RootS <- summary(modavgaboveRootS)$coefmat.subset
RootCarbonS<- cbind(coef.RootS, confint.RootS)
write.table(RootCarbonS, file="Ecosystem carbon/Model_average/ConAvgRoots.txt") 

# Reduced model without Seronera
Root.block<-lmer(CRoots.kg_m2~ Landuse+CMAP.mm_yr+CSand+
                   CSandPOLY+
                   (1|Region),data = Total.Eco.C.CnoNA2, REML=F,
                 na.action=na.fail)

summary(Root.block)
drop1(Root.block,test="Chisq")  
anova(Root.block)
AIC(Root.block) #34.09222

# Model averaging: All possible models between null and global
modsetaboveRoot<-dredge(Root.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=
                          !(CSand & CSandPOLY))

modselaboveRoot<-model.sel(modsetaboveRoot) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveRoot<-model.avg(modselaboveRoot)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveRoot)#Importance of each variable

####      4.6x. From belowground.full fine scale, DO NOT USE THIS NOW ####
# Model averaging 
summary(Belowground.full.CnoNA2)

# A horizon 
Belowground.Ahor <-lmer(CAhorC.kg_m2 ~ CMAP.mm_yr + Landuse + CFire_frequency.2000_2017  
                        + CTreeBM.kg_m2 + CSand + CBiomass_year 
                        + Landuse:CMAP.mm_yr + Landuse:CSand + 
                          (1|Region/Block.ID), data = Belowground.full.CnoNA2, REML=F, na.action=na.fail)

modsetbelow.Ahor<-dredge(Belowground.Ahor,trace = TRUE, rank = "AICc", REML = FALSE,subset=!(CTreeBM.kg_m2 & Landuse))

modselbelow.Ahor<-model.sel(modsetbelow.Ahor) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Ahor<-model.avg(modselbelow.Ahor)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Ahor)
write.table(importance(modavgbelow.Ahor), file="Ecosystem carbon/importanceAhorFull.txt")#Importance of each variable
summary(modavgbelow.Ahor)#Estimated coefficients given weighting
confint.Ahor <- confint(modavgbelow.Ahor)
coef.Ahor <- summary(modavgbelow.Ahor)$coefmat.subset
Ahor <- cbind(coef.Ahor, confint.Ahor)
write.table(Ahor, file="Ecosystem carbon/ConAvgAhorFull.txt")

# A hor with dung and roots NOT this I use 
colnames(Belowground.full.CnoNA3)
Belowground.Ahor.sub <-lmer(AhorC.kg_m2 ~ CMAP.mm_yr + CFire_frequency.2000_2017  + CTreeBM.kg_m2 + CSand + CHerbaceous + Ctotal.dung + CRoots.kg.m2 + CAccum.bm.kg_m2 +
                              (1|Region/Block.ID), data = Belowground.full.CnoNA3, REML=F, na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelow.Ahor.sub<-dredge(Belowground.Ahor.sub,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & CHerbaceous)) #&!(CWild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand)) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Ahor.sub<-model.avg(modsetbelow.Ahor.sub)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Ahor.sub)
write.table(importance(modavgbelow.Ahor), file="Ecosystem carbon/importanceAhorDung.txt")#Importance of each variable
summary(modavgbelow.Ahor.sub)#Estimated coefficients given weighting
confint.Ahor <- confint(modavgbelow.Ahor)
coef.Ahor <- summary(modavgbelow.Ahor)$coefmat.subset
Ahor <- cbind(coef.Ahor, confint.Ahor)
write.table(Ahor, file="Ecosystem carbon/ConAvgAhorDung.txt")

# Mineral horizon
Belowground.Minhor <-lmer(CMinC.kg_m2 ~ CAhorC.kg_m2 + CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CBiomass_year
                          + landuse:CMAP.mm_yr + landuse:CSand +  (1|Region/Block.ID), data = Belowground.full.CnoNA2, REML=F, na.action=na.fail)

modsetbelow.Minhor<-dredge(Belowground.Minhor,trace = TRUE, rank = "AICc", REML = FALSE,subset=!(CTreeBM.kg_m2 & landuse))

modselbelow.Minhor<-model.sel(modsetbelow.Minhor) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Minhor<-model.avg(modselbelow.Minhor)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Minhor)#Importance of each variable
write.table(importance(modavgbelow.Minhor), file="Ecosystem carbon/importanceMinHorFull.txt")#
summary(modavgbelow.Minhor)#Estimated coefficients given weighting
confint.Minhor <- confint(modavgbelow.Minhor)
coef.Minhor <- summary(modavgbelow.Minhor)$coefmat.subset
Minhor <- cbind(coef.Minhor, confint.Minhor)
write.table(Minhor, file="Ecosystem carbon/ConAvgMinhorFull.txt")


####  5: DATA MODELING: SEM ####
# %~~% between correlated error - telling R to not care about the correlation between these variables. 
# MySummary <- summary(modell)
# save(MySummary, file="")
library(MuMIn)
library(piecewiseSEM)
vignette('piecewiseSEM') # too look at the package 

# Going to make two SEM, one larger model (all sites) and a reduce model (ones with herbaceous and dung data) with more detailed measurements. Larger model is more a relationship between main pools, while smaller model is more mechanistic looking into herbaceous and root production and local herbivore assemblage etc.

#SEM.below <- summary(Modlist.below2,Belowground.full.CnoNA) # Good fit
#write.csv(SEM.below$coefficients, file = "Ecosystem carbon/SEMBelow.csv")

##      5.1: Large model with as high resolution as possible ####
names(Belowground.full.CnoNA)
summary(Belowground.full.CnoNA)
Belowground.full.CnoNA <- droplevels(Belowground.full.CnoNA)

Modlist.large <-   psem(
  lme(CWoody~ CSand + landuse, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ CFire_frequency.2000_2017 + landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CAhorC.kg_m2~ CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2 # We know these are highly correlated, but no path..
)

summary(Modlist.large,Belowground.full.CnoNA) # Not a good fit, p=0

##        5.1.1: Large model best fit WITH herbaceous accumulated (Biomass_year)####
# First model is based on conseptual model. 
Modlist.large <-   psem(
   lme(CWoody~ landuse + CSand + CFire_frequency.2000_2017 + CMAP.mm_yr, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CBiomass_year ~ CWoody + Ctot.N.kg_m2 + CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CAhorC.kg_m2~ CBiomass_year + CFire_frequency.2000_2017, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
 
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2 # We know these are highly correlated, but no path..
)

summary(Modlist.large,Belowground.full.CnoNA) 
# Fisher's C = 42.656 with P-value = 0.443 and on 42 degrees of freedom

# Removing and adding variables: 
Modlist.large1 <-   psem(
  lme(CWoody~ landuse + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CBiomass_year ~ CWoody + CMAP.mm_yr + CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  #lme(CAhorC.kg_m2~ CBiomass_year, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2 # We know these are highly correlated, but no path..
)

summary(Modlist.large1,Belowground.full.CnoNA) 
# Fisher's C = 33.131 with P-value = 0.694 and on 38 degrees of freedom

# easy plots 
par(mar=c(5.1, 4.1, 4.1, 2.1)) # default margins 
par(mfrow=c(1,2))
plot(CBiomass_year~Region, data=Belowground.full)
plot(CAhorC.kg_m2~Region, data=Belowground.full)

##        5.1.2: Large model best fit WITH herbaceous mean ####

Modlist.large3 <-   psem(
  lme(CWoody~ landuse + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
   lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CRes.bm.kg_m2 ~ CSand + CWoody,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand + CRes.bm.kg_m2, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CFire_frequency.2000_2017~ CRes.bm.kg_m2,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2 # We know these are highly correlated, but no path..
)

summary(Modlist.large3,Belowground.full.CnoNA) # good fit: P-value = 0.844 

##        5.1.3: Large model best fit WITHOUT herbaceous ####
Modlist.large4 <-   psem(
  lme(CWoody~ landuse + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand + CFire_frequency.2000_2017, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CFire_frequency.2000_2017~ CDW + CWoody,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2 # We know these are highly correlated, but no path..
)

summary(Modlist.large4,Belowground.full.CnoNA) # not a good fit: P-value = 0.497

##      5.2: Lower resolution data, subset model ####
##        5.2.1: With Livestock/wild and roots AND ACCUMULATED ####

Modlist.mecanistic1 <-   psem(
  lme(CTot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CWild ~ CLivestock + CMAP.mm_yr,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CWoody~ CFire_frequency + CLivestock, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  #lme(CDW~ Landuse,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CHerb_year.kg_m2 ~ CMAP.mm_yr + CWoody + CDW + CFire_frequency,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CRoots.kg_m2~ CMAP.mm_yr + CSand + Landuse,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CSoil.Ahor~CLivestock + CWild, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  #lme(CSoil.min~ CSoil.Ahor + CSand, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  CSoil.Ahor%~~%CTot.N.kg_m2,
  CSoil.min%~~%CTot.N.kg_m2,
  CMAP.mm_yr%~~%CSand, 
  CMAP.mm_yr%~~%Landuse,
  Landuse%~~%CSand
)

summary(Modlist.mecanistic1,Total.Eco.C.CnoNA2) 

# Add POLYterms to the model AND Seronera block 4:  
Modlist.mecanistic2 <-   psem(
  lme(CTot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),# Should we use Livestock or LivestockPOLY here? From below, the best model seems to be livestock and sand, but non of them turns out significant in the SEM.. I only keep sand for now.
  lme(CWild ~ CLivestock + CMAP.mm_yr,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CWoody~ CFire_frequency + CSandPOLY + CMAP.mm_yr, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CHerb_year.kg_m2 ~ CMAP.mm_yr + CDW + CFire_frequency + CLivestockPOLY,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  #lme(CDW~ Landuse,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  #lme(CRoots.kg_m2~ CMAP.mm_yr ,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2), 
  lme(CSoil.Ahor~ CSand, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CSoil.min~ CSoil.Ahor, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  CSoil.Ahor%~~%CTot.N.kg_m2,
  CSoil.min%~~%CTot.N.kg_m2,
  CMAP.mm_yr%~~%CSand, 
  CMAP.mm_yr%~~%Landuse,
  Landuse%~~%CSand,
  CSand%~~%CSandPOLY,
  CLivestock%~~%CLivestockPOLY
) 

summary(Modlist.mecanistic2,Total.Eco.C.CnoNA2) 

# I think it is strange that LivestockPOLY is the only significant explaining Nitrogen
# Check this out a bit further: 

N1 <- (lmer(CTot.N.kg_m2~ CSand+CLivestockPOLY + (1|Region),na.action=na.fail, data=Total.Eco.C.CnoNA2, REML = FALSE)) 
N2 <- (lmer(CTot.N.kg_m2~ CLivestockPOLY + (1|Region),na.action=na.fail, data=Total.Eco.C.CnoNA2, REML = FALSE)) 
N3 <- (lmer(CTot.N.kg_m2~ CSand + (1|Region),na.action=na.fail, data=Total.Eco.C.CnoNA2, REML = FALSE)) 
N4 <- (lmer(CTot.N.kg_m2~ CSand+CLivestock + (1|Region),na.action=na.fail, data=Total.Eco.C.CnoNA2, REML = FALSE)) 
N5 <- (lmer(CTot.N.kg_m2~ CLivestock + (1|Region),na.action=na.fail, data=Total.Eco.C.CnoNA2, REML = FALSE)) 

AIC(N1,N2,N3,N4,N5) # The best model is the model with both Sand and livestock. 
anova(N1,N3) # Adding LivestockPOLY is not significant.
anova(N1,N2) # Adding Sand is significant. 

# Want to see if I understand a bit more of the relationship between roots, sand and MAP as well
R1 <- lmer(CRoots.kg_m2~ CSandPOLY + CMAP.mm_yr + Landuse + (1|Region)
     ,na.action=na.fail, data=Total.Eco.C.CnoNA2, REML=FALSE)
R2 <- update(R1, .~.-CSandPOLY) # MAP, landuse
R3 <- update(R1, .~.-CMAP.mm_yr) # Sand, landuse 
R4 <- update(R1, .~.-Landuse) # Sand, MAP 
R5 <- update(R2, .~.-CMAP.mm_yr) # landuse
R6 <- update(R2, .~.-Landuse) # MAP 
R7 <- update(R3, .~.-Landuse) # Sand 

AIC(R1,R2,R3,R4,R5,R6,R7) # The best model is R1 Including all the variables 
R11 <- lmer(CRoots.kg_m2~ CSand + CMAP.mm_yr + Landuse + (1|Region)
         ,na.action=na.fail, data=Total.Eco.C.CnoNA2, REML=FALSE)
AIC(R1,R11) # slightly better with sandPOLY. 

# Try a simple linear model with all variables 
Root1 <- lm(CRoots.kg_m2~CMAP.mm_yr + CTot.N.kg_m2 + Landuse, 
               data=Total.Eco.C.CnoNA2)
Root2 <- update(Root1, .~.-CTot.N.kg_m2)
Root3 <- update(Root1, .~.-CMAP.mm_yr)
Root4 <- update(Root1, .~.-Landuse)
Root5 <- update(Root1, .~.+CSandPOLY)
Root6 <- update(Root5, .~.-CTot.N.kg_m2)

AIC(Root1,Root2,Root3,Root4,Root5,Root6)
##        5.2.2: With Total dung and roots AND ACCUMULATED #### 

Modlist.mecanistic2 <-   psem(
  lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region/Block.ID,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CRoots.kg.m2~ CMAP.mm_yr + CSand + Landuse,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(Ctotal.dung~ CMAP.mm_yr + CSand + Landuse, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2), 
  lme(CWoody~ Ctotal.dung + CSand + CFire_frequency, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CDW~ Landuse,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CAccum.bm.kg_m2 ~ CMAP.mm_yr,random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  lme(CSoil.min~ CSand + CAccum.bm.kg_m2, random= ~ 1|Region,na.action=na.fail, data=Total.Eco.C.CnoNA2),
  CSoil.Ahor%~~%Ctot.N.kg_m2,
  CSoil.min%~~%Ctot.N.kg_m2,
  CMAP.mm_yr%~~%CSand
)

summary(Modlist.mecanistic2,Total.Eco.C.CnoNA2) 

####  6: PLOTING  ####
##      6.1: Dung variables ####
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


##      6.2: Importance #### 
par(mfrow=c(1,1))
importance.Ahor<- read.table("Ecosystem carbon/Model_average/importanceAhor.txt")
importance.Minhor<- read.table("Ecosystem carbon/Model_average/importanceMinhor.txt")
importance.Roots<- read.table("Ecosystem carbon/Model_average/importanceRoots.txt")
#importance.AhorFull<- read.table("Ecosystem carbon/importanceAhorFull.txt")
#importance.MinHorFull<- read.table("Ecosystem carbon/importanceMinHorFull.txt")
#importance.AhorHerbs<- read.table("Ecosystem carbon/importanceAhorFull.txt")
#importance.MinHorHerbs<- read.table("Ecosystem carbon/importanceMinHorFull.txt")
importance.H<- read.table("Ecosystem carbon/Model_average/importanceaboveH.txt")
importance.DW<- read.table("Ecosystem carbon/Model_average/importanceaboveDW.txt")
importance.W<- read.table("Ecosystem carbon/Model_average/importanceaboveW.txt")
#importance.W2<- read.table("Ecosystem carbon/importanceWoody2.txt")
#importance.W.outl <- read.table("Ecosystem carbon/importanceaboveW.outl.txt")
#importance.Ahor.dung<- read.table("Ecosystem carbon/importanceAhorDung.txt")
#importance.MinHor.dung<- read.table("Ecosystem carbon/importanceMinHorDung.txt")

# I now have all my importance variables 
colnames(importance.Ahor)<-'Ahor'
colnames(importance.Minhor)<-'MinHor'
colnames(importance.Roots)<-'Roots'
colnames(importance.H)<-'Herbs'
colnames(importance.DW)<-'DW'
colnames(importance.W)<-'Woody'

rownames(importance.Ahor) <- (c("Sand","Land-use","Dead wood","Soil min","Livestock","Wild"))

rownames(importance.Minhor) <- (c("Surface soil","Roots","Sand","Dead wood","Herbaceous","Land-use","WoodyPOLY"))

rownames(importance.Roots) <- (c("Land-use","MAP","Total dung","Nitrogen","Soil min","SandPOLY","Dead wood"))

rownames(importance.H) <- (c("Fire frequency","Dead wood","MAP","Land-use","Livestock","Nitrogen","Woody", "Sand"))

rownames(importance.DW) <- (c("Herbaceous","Land-use","Wild","LivestockPOLY"))

rownames(importance.W) <- (c("FirePOLY","Livestock","Sand","Nitrogen","Land-use","Herbaceous"))

# Colours 
# Fire: darkorange3
# Landuse: burlywood, 
# Livestock: burlywood4, 
# Wild: gray19
# Herb biomass: darkolivegreen4, 
# Woody: darkolivegreen, 
# DW: darkkhaki 
# Sand: darkgray
# Soil N: floralwhite, 
# MAP: deepskyblue4
# Min-hor: bisque4, 
# A-hor: saddlebrown, 
# Roots: peru

# Plot A-hor Full
col.Ahor <- c("darkgray","burlywood","darkkhaki","bisque4","burlywood4","gray19")
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.AhorFull.png")
par(mar=c(5,14,1,2))
barplot(t(as.matrix(importance.Ahor)), horiz=T,las=1,xlab='Relative variable importance',main='Soil A-horizon Carbon',axisname=T,col=col.Ahor,beside=T,cex.main = 1,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot Min-hor FULL
col.min <- c("saddlebrown","peru","darkgray","darkkhaki","darkolivegreen4","burlywood","darkolivegreen")
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.MinhorFull.png")
par(mar=c(5,14,1,2))
barplot(t(as.matrix(importance.Minhor)), horiz=T,las=1,xlab='Relative variable importance',main='Soil Mineral-horizon Carbon',cex.main = 1,axisname=T,col= col.min,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot Herb 
col.herb <- c("darkorange3","darkkhaki","deepskyblue4","burlywood","gray19","floralwhite","darkolivegreen","darkgray")
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Herb.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.H)), horiz=T,las=1,xlab='Relative variable importance',main='Herbaceous Carbon',cex.main = 1,axisname=T,col=col.herb,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

# Plot DW 
col.DW <- c("darkolivegreen4","burlywood","gray19","burlywood4")
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.DW.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.DW)), horiz=T,las=1,xlab='Relative variable importance',main='Dead Wood Carbon',cex.main = 1,axisname=T,col=col.DW,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

#Plot Woody
col.W <- c("darkorange3","burlywood4","darkgray","floralwhite","burlywood","darkolivegreen4")
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Woody.outl.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.W)),horiz=T,las=1,xlab='Relative variable importance',main='Woody Carbon',cex.main = 1,axisname=T,col=col.W,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

#Plot Roots
col.roots <- c("burlywood","deepskyblue4","gray25","floralwhite","bisque4","darkgray","darkkhaki")
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/imp.Roots.png")
par(mar=c(5,13,1,1))
barplot(t(as.matrix(importance.Roots)),horiz=T,las=1,xlab='Relative variable importance',main='Roots Carbon',cex.main = 1,axisname=T,col=col.roots,beside=T,cex.axis=2,cex.lab=1,cex.names=2)
dev.off()

##      6.3: Variable coefficients from model averages ####
con.avg.Ahor<- read.table("Ecosystem carbon/Model_average/ConAvgAhor.txt")
con.avg.Minhor<- read.table("Ecosystem carbon/Model_average/ConAvgMinHor.txt")
con.avg.H<- read.table("Ecosystem carbon/Model_average/ConAvgH.txt")
con.avg.DW<- read.table("Ecosystem carbon/Model_average/ConAvgDW.txt")
con.avg.W<- read.table("Ecosystem carbon/Model_average/ConAvgW.txt")
con.avg.Roots<- read.table("Ecosystem carbon/Model_average/ConAvgRoots.txt")

#Reorder rows
rownames(con.avg.Ahor)
con.avg.Ahor<-con.avg.Ahor[c(1,2,3,4,6,5,7),]
con.avg.Minhor<-con.avg.Minhor[c(1,2,3,4,5,6,8,7),]
con.avg.H<-con.avg.H[c(1,3,2,4,5,6,8,7,9),]
con.avg.DW<-con.avg.DW[c(1,2,3,4,5),]
con.avg.W<-con.avg.W[c(1,2,3,4,5,6,7),] 
con.avg.Roots <- con.avg.Roots[c(1,4,2,5,3,6,7,8),]

# remove first row 
con.avg.Ahor<-con.avg.Ahor[c(-1),]
con.avg.Minhor<-con.avg.Minhor[c(-1),]
con.avg.H<-con.avg.H[c(-1),]
con.avg.DW<-con.avg.DW[c(-1),]
con.avg.W<-con.avg.W[c(-1),]
con.avg.Roots <- con.avg.Roots[c(-1),]

# Add SD, rather use 95 % confint
# con.avg.AhorFull$SD <- con.avg.AhorFull$Std..Error * sqrt(length(con.avg.AhorFull$Std..Error))
# con.avg.MinHorFull$SD <- con.avg.MinHorFull$Std..Error * sqrt(length(con.avg.MinHorFull$Std..Error))
# con.avg.H$SD <- con.avg.H$Std..Error * sqrt(length(con.avg.H$Std..Error))
# con.avg.DW$SD <- con.avg.DW$Std..Error * sqrt(length(con.avg.DW$Std..Error))
# con.avg.W$SD <- con.avg.W$Std..Error * sqrt(length(con.avg.W$Std..Error))
# con.avg.Ahor.dung$SD <- con.avg.Ahor.dung$Std..Error * sqrt(length(con.avg.Ahor.dung$Std..Error))
# con.avg.MinHor.dung$SD <- con.avg.MinHor.dung$Std..Error * sqrt(length(con.avg.MinHor.dung$Std..Error))

# Add Significance 
con.avg.Ahor$sign <- con.avg.Ahor$Pr...z..
con.avg.Ahor$sign[con.avg.Ahor$sign>0.05] <- 1
con.avg.Ahor$sign[con.avg.Ahor$sign<0.05] <- 16

con.avg.Minhor$sign <- con.avg.Minhor$Pr...z..
con.avg.Minhor$sign[con.avg.Minhor$sign>0.05] <- 1
con.avg.Minhor$sign[con.avg.Minhor$sign<0.05] <- 16

con.avg.H$sign <- con.avg.H$Pr...z..
con.avg.H$sign[con.avg.H$sign>0.05] <- 1
con.avg.H$sign[con.avg.H$sign<0.05] <- 16

con.avg.W$sign <- con.avg.W$Pr...z..
con.avg.W$sign[con.avg.W$sign>0.05] <- 1
con.avg.W$sign[con.avg.W$sign<0.05] <- 16

con.avg.Roots$sign <- con.avg.Roots$Pr...z..
con.avg.Roots$sign[con.avg.Roots$sign>0.05] <- 1
con.avg.Roots$sign[con.avg.Roots$sign<0.05] <- 16

con.avg.DW$sign <- con.avg.DW$Pr...z..
con.avg.DW$sign[con.avg.DW$sign>0.05] <- 1
con.avg.DW$sign[con.avg.DW$sign<0.05] <- 16

# creating the "frame" and then plotting

# A-hor
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Ahor.png")
par(mar=c(5,12,1,1))
plot(rep(NA,6),1:6, xlim=c(-0.5,1.5), type="n", ann=F,axes=F, bty="n")
points(con.avg.Ahor$Estimate,1:6,pch=con.avg.Ahor$sign,col=c(col.Ahor), lwd=2, cex=2)
arrows(y0=1:6, x0=con.avg.Ahor$X2.5..,x1=con.avg.Ahor$X97.5..,col=c(col.Ahor), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:6, labels= c("Sand","Land-use","Dead wood","Soil min","Livestock","Wild"),par(las=1),cex.axis=2)

dev.off()

# Min-hor
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Minhor.png")
par(mar=c(5,12,1,1))
plot(rep(NA,7),1:7, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
arrows(y0=1:7, x0=con.avg.Minhor$X2.5.., x1=con.avg.Minhor$X97.5..,col=c(col.min), angle=90,length=0.05,code=3,lwd=2)
points(con.avg.Minhor$Estimate,1:7,pch=con.avg.Minhor$sign,col=c(col.min), lwd=2, cex=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:7, labels= c("Surface soil","Roots","Sand","Dead wood","Herbaceous","Land-use","WoodyPOLY"),par(las=1),cex.axis=2)
dev.off()

# Herb
#png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Herb.png")
par(mar=c(5,13,1,1))
plot(rep(NA,8),1:8, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.H$Estimate,1:8,pch=con.avg.H$sign,col=c(col.herb), lwd=2, cex=2)
arrows(y0=1:8, x0=con.avg.H$X2.5.., x1=con.avg.H$X97.5..,col=c(col.herb), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:8, labels= c("Fire frequency","Dead wood","MAP","Land-use","Livestock","Nitrogen","Woody", "Sand"),par(las=1),cex.axis=2)
dev.off()

# DW
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.DW.png")
par(mar=c(5,13,1,1))
plot(rep(NA,4),1:4, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.DW$Estimate,1:4,pch=con.avg.DW$sign,col=c(col.DW), lwd=2, cex=2)
arrows(y0=1:4, x0=con.avg.DW$X2.5.., x1=con.avg.DW$X97.5..,col=c(col.DW), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:4, labels= c("Herbaceous","Land-use","Wild","LivestockPOLY"),par(las=1),cex.axis=2)
dev.off()

#Woody
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Woody.png")
par(mar=c(5,13,1,1))
plot(rep(NA,6),1:6, xlim=c(-1,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.W$Estimate,1:6,pch=con.avg.W$sign,col=c(col.W), lwd=2, cex=2)
arrows(y0=1:6, x0=con.avg.W$X2.5.., x1=con.avg.W$X97.5..,col=c(col.W), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:6, labels= c("FirePOLY","Livestock","Sand","Nitrogen","Land-use","Herbaceous"),par(las=1), cex.axis=2) # Without outlier 

dev.off()

#Roots 
png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Roots.png")
par(mar=c(5,13,1,1))
plot(rep(NA,7),1:7, xlim=c(-3,2), type="n", ann=F,axes=F, bty="n")
points(con.avg.Roots$Estimate,1:7,pch=con.avg.Roots$sign,col=c(col.roots), lwd=2, cex=2)
arrows(y0=1:7, x0=con.avg.Roots$X2.5.., x1=con.avg.Roots$X97.5..,col=c(col.roots), angle=90,length=0.05,code=3,lwd=2)
abline(v=0)
axis(1,cex.axis=2)
axis(2, at=1:7, labels= c("Land-use","MAP","Total dung","Nitrogen","Soil min","SandPOLY","Dead wood"),par(las=1), cex.axis=2) 

dev.off()


##      6.4: Check the relationship between the factors Landuse, climat(dry-int-wet), climate.kat (Dry vs Wet) and texture ####
names(Belowground.full)
plot(AhorC.kg_m2~climate,data = Belowground.full)
plot(MinC.kg_m2~climate,data = Belowground.full)
# 1. landuse climate belowground, A-hor, Min-hor
# Belowground
Climate.C <- lmer(tot.C.kg_m2~ landuse + climate + landuse:climate + (1|Region/Block), data = Belowground.full, REML=T)
drop1(Climate.C,test="Chisq")
AIC(Climate.C) # 294.0345

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

# With fire 
Fire.climate <- lmer(tot.C.kg_m2~CFire_frequency.2000_2017:climate.kat + (1|Region), data = Belowground.full, REML=T)

xyplot(tot.C.kg_m2~CFire_frequency.2000_2017|climate.kat,data=Belowground.full)
anova(Fire.climate)


##      6.5: Fire and aboveground C ####

plot(Herbaceous~Fire_frequency.2000_2017, data=Total.Eco.C.CnoNA2)
