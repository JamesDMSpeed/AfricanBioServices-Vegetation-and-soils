#### REORGANISED VERSION FOR ALL ANALYSIS FOR WRITE-UP #### 
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
##     3.1: Prepare data for modelling ####
#         3.1.1: Uploading data ####
Block.Eco.C <- read.csv("Ecosystem carbon/Final.Ecosystem.Carbon.csv", head=T)
Belowground.full <- read.csv("Ecosystem carbon/Soil.data/Belowground.Carbon.csv", head=T)

Block.Eco.C$Region<- factor(Block.Eco.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Belowground.full$Region<- factor(Belowground.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

names(Belowground.full)
Belowground.full <- Belowground.full[,c(4:28,30:33,47)]

names(Block.Eco.C)

Block.Eco.C$Carbon.pool <- factor(Block.Eco.C$Carbon.pool, levels=c("TreeC.kg_m2","HerbC.kg_m2","DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))

levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")

# Add Marit's data on accumulated and residual biomass, dung data and nitrogen
Marit <- read.csv("Ecosystem carbon/Herbaceous.data/Herbaceous.csv", header=TRUE)
names(Marit)
Marit <- Marit[c(7,12,13)]

# Livestock dung 
Livestock.dung<- aggregate(livestock~Block.ID, mean, data=Belowground.full)
Wild.dung<- aggregate(wild~Block.ID, mean, data=Belowground.full)
Total.dung<- aggregate(total.dung~Block.ID, mean, data=Belowground.full)

# total nitrogen
tot.N.kg_m2 <- aggregate(tot.N.kg_m2 ~ Block.ID, mean, data=Belowground.full)

# Add Stu's data on roots 
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
levels(Roots$Region)
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
Block.Eco.C <- merge(Block.Eco.C,Marit, by="Block.ID", all.x = TRUE)
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
Block.Eco.C$CAccum.bm.kg_m2 <- as.numeric(scale(Block.Eco.C$Accum.bm.kg_m2))
Block.Eco.C$CRes.bm.kg_m2 <- as.numeric(scale(Block.Eco.C$Res.bm.kg_m2))
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
Soil.Ahor <- cbind(Soil.Ahor,Herbaceous[17])
colnames(Soil.Ahor)[37] <- "Herbaceous"
Soil.Ahor$CHerbaceous <- as.numeric(scale(Soil.Ahor$Herbaceous))
Soil.min <- cbind(Soil.min,Herbaceous[17])
colnames(Soil.min)[37] <- "Herbaceous"
Soil.min$CHerbaceous <- as.numeric(scale(Soil.min$Herbaceous))

# Add data to belowground full 
Belowground.full <- merge(Belowground.full,Marit, by="Block.ID", all.x = TRUE)
Belowground.full <- merge(Belowground.full, Herbaceous[ , c("Block.ID","C.amount")], by = "Block.ID", all.x=TRUE)
Belowground.full <- merge(Belowground.full, Woody[ , c("Block.ID","C.amount")], by = "Block.ID", all.x=TRUE)
Belowground.full <- merge(Belowground.full, DW[ , c("Block.ID","C.amount")], by = "Block.ID", all.x=TRUE)
Belowground.full <- merge(Belowground.full, RootsBlock, by = "Block.ID", all.x=TRUE)

colnames(Belowground.full)[33] <- "Herbaceous"
colnames(Belowground.full)[34] <- "Woody"
colnames(Belowground.full)[35] <- "DW"

#         3.1.2: Creating usefull datasets ####

# Both above and belowground block
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
names(Soil.min)
Total.Eco.C <- cbind(Soil.min[c(1,3:15,17,19:38)],Soil.Ahor[c(17)],Woody[c(17)],DW[c(17)])
colnames(Total.Eco.C)[15] <- "Soil.min"
colnames(Total.Eco.C)[36] <- "Soil.Ahor"
colnames(Total.Eco.C)[37] <- "Woody"
colnames(Total.Eco.C)[38] <- "DW"
Total.Eco.C$tot.C.kg_m2 <- Total.Eco.C$Soil.min + Total.Eco.C$Soil.Ahor + Total.Eco.C$Herbaceous + Total.Eco.C$Woody + Total.Eco.C$DW

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
Belowground.full$CAccum.bm.kg_m2<- as.numeric(scale(Belowground.full$Accum.bm.kg_m2)) 
Belowground.full$CRes.bm.kg_m2 <- as.numeric(scale(Belowground.full$Res.bm.kg_m2)) 
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
Total.Eco.C.CnoNA<-Total.Eco.C[!is.na(Total.Eco.C$CFire_frequency),]
Total.Eco.C.CnoNA<-Total.Eco.C.CnoNA[(-16),] # Remove outlier
Total.Eco.C.CnoNA<-droplevels(Total.Eco.C.CnoNA)
Total.Eco.C.CnoNA2<-Total.Eco.C.CnoNA[!is.na(Total.Eco.C.CnoNA$livestock),]
Total.Eco.C.CnoNA2 <- droplevels(Total.Eco.C.CnoNA2)

Belowground.full <- Belowground.full[-c(61,62,63,64),] # remove outlier
Belowground.full.CnoNA<-Belowground.full[!is.na(Belowground.full$Fire_frequency.2000_2017),]
Belowground.full.CnoNA2 <- Belowground.full.CnoNA[!is.na(Belowground.full.CnoNA$Herbaceous),]
Belowground.full.CnoNA3<-Belowground.full.CnoNA2[!is.na(Belowground.full.CnoNA2$livestock),]
Belowground.full.CnoNA <- droplevels(Belowground.full.CnoNA)
Belowground.full.CnoNA2 <- droplevels(Belowground.full.CnoNA2)
Belowground.full.CnoNA3 <- droplevels(Belowground.full.CnoNA3)
names(Belowground.full)
##      3.2: Correlation of variables (numeric) #### 

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

names(Belowground.full.CnoNA)
Model.var.full<-c("MAP.mm_yr","Sand","Clay","Silt","Fire_frequency.2000_2017.x","TreeBM.kg_m2","AhorN.kg_m2","MinN.kg_m2","AhorC.kg_m2","MinC.kg_m2","Last_fire.yr","Herbaceous","Accum.bm.kg_m2","Res.bm.kg_m2","Roots.kg.m2", "total.dung")

Model.var.red<-c("TreeBM.kg_m2","DW","AhorC.kg_m2","MinC.kg_m2","MAP.mm_yr","Sand","Fire_frequency.2000_2017.x","Ctot.N.kg_m2","Herbaceous","Accum.bm.kg_m2","Res.bm.kg_m2","Roots.kg.m2", "total.dung", "livestock", "wild")

summary(Total.Eco.C.CnoNA)
Model.var.block<-c("MAP.mm_yr","Sand.pip.per","Fire_frequency.2000_2017","TreeBM.kg_m2","Herbaceous","Accum.bm.kg_m2","Res.bm.kg_m2","AhorN.kg_m2","MinN.kg_m2","Soil.Ahor","Soil.min","Roots.kg.m2")

summary(Total.Eco.C.CnoNA2)
Model.var.sub<-c("MAP.mm_yr","Sand.pip.per","Fire_frequency.2000_2017","Woody","Herbaceous","AhorN.kg_m2","MinN.kg_m2","Soil.Ahor","Soil.min", #"livestock", "wild", 
                 "total.dung")

# Want to get these two in one matrix. 
pairs(Belowground.full.CnoNA[,Model.var.red],lower.panel = panel.cor)
pairs(Total.Eco.C.CnoNA[,Model.var.block],lower.panel = panel.cor)
pairs(Total.Eco.C.CnoNA2[,Model.var.sub],lower.panel = panel.cor)
# If I want these values in a table:
Model.var.FULL <- Belowground.full.CnoNA[,c(14,28,26,27,12,13,51,32,64,16,22,21,15)]
Model.var.Herb <- Belowground.full.CnoNA[,c(14,28,26,12,13,51,32,64,16,22,21,15)]
Model.var.SUB <- Total.Eco.C.CnoNA2[,c(5,10,8,7,15,35,34,41,42,33,24,6,44,46,47)]

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

##     3.3: Correlation of variables (factorial) ####
# Trees 
plot(TreeBM.kg_m2~landuse, data= Ecosystem.Carbon) # COVARYING 
plot(No.trees_m2~landuse, data= Block.Eco.C) # not covarying
# Soil properties 
plot(Sand.pip.per~landuse, data= Ecosystem.Carbon) # not covarying
plot(mean.N.kg_m2~landuse,data=Ecosystem.Carbon) # not covarying
# Site traits 
plot(MAP.mm_yr~landuse, data= Ecosystem.Carbon) # not covarying
plot(Last.fire_yr~landuse, data= Ecosystem.Carbon) # not covarying
plot(Fire_frequency.2000_2017~landuse, data= Ecosystem.Carbon) # not covarying 

# Need to upload Belowground.full first.. 
plot(livestock~landuse,data=Belowground.full) # COVARYING
plot(livestock~Region,data=Belowground.full) # COVARYING
plot(wild~landuse,data=Belowground.full) # not covarying 

####  4: DATA MODELING: MODEL AVERAGING #### 
##      4.1. Global model for A-hor C univariate variables ####
names(Soil.Ahor)
summary(Soil.Ahor)
Soil.Ahor.CnoNA<-Soil.Ahor[!is.na(Soil.Ahor$Herbaceous),]
Soil.Ahor.CnoNA <-  Soil.Ahor.CnoNA[(-20),]
Soil.Ahor.CnoNA <-  Soil.Ahor.CnoNA[(-16),]
Ahor.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + 
                   CFire_frequency + CTreeBM.kg_m2 + 
                   CSand + CAccum.bm.kg_m2 + CRes.bm.kg_m2 + CHerbaceous +
                   + landuse:CMAP.mm_yr + landuse:CSand +
                   (1|Region),data = Soil.Ahor.CnoNA, REML=F,
                 na.action=na.fail)

summary(Ahor.block)
drop1(Ahor.block,test="Chisq")  
anova(Ahor.block)
AIC(Ahor.block) #62.22761

# Model averaging: All possible models between null and global
modsetbelowA<-dredge(Ahor.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse)&!(CAccum.bm.kg_m2&CMAP.mm_yr)&!(CRes.bm.kg_m2&CSand)&!(CMAP.mm_yr & CHerbaceous))

modselbelowA<-model.sel(modsetbelowA) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA<-model.avg(modselbelowA)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA)#Importance of each variable
write.table(importance(modavgbelowA),file="Ecosystem carbon/importanceAhor.txt")
summary(modavgbelowA)#Estimated coefficients given weighting
summary(modavgbelowA)$coefmat.full # Full average 
write.table(summary(modavgbelowA)$coefmat.subset, file="Ecosystem carbon/ConAvgAhor.txt") # conditional average - I will try first with this.. 

# With livestock and wild dung 
Soil.Ahor.CnoNA2<-Soil.Ahor.CnoNA[!is.na(Soil.Ahor.CnoNA$livestock),]
Soil.Ahor.CnoNA2 <- droplevels(Soil.Ahor.CnoNA2)
names(Soil.Ahor.CnoNA2)
Ahor2.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency + 
                    CTreeBM.kg_m2 + CSand + CHerbaceous + 
                    Clivestock + Cwild + 
                    Ctotal.dung + 
                    (1|Region),data = Soil.Ahor.CnoNA2, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelowA2<-dredge(Ahor2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CMAP.mm_yr & CHerbaceous)&!
                        (Ctotal.dung & CHerbaceous)&!
                        (Ctotal.dung & CMAP.mm_yr)&!
                        (Ctotal.dung & CTreeBM.kg_m2)&!
                        (Ctotal.dung & CFire_frequency)&!
                        (CFire_frequency & CSand)&!
                        (Cwild & CSand)&!
                        (Cwild & CMAP.mm_yr)&!
                        (Cwild & CTreeBM.kg_m2)&!
                        (Ctotal.dung & Clivestock)&!
                        (Clivestock & CSand)&!
                        (Clivestock & CTreeBM.kg_m2))

modselbelowA2<-model.sel(modsetbelowA2) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowA2<-model.avg(modselbelowA2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowA2)#Importance of each variable
write.table(importance(modavgbelowA2),file="Ecosystem carbon/importanceAhorDung.txt")
summary(modavgbelowA2)#Estimated coefficients given weighting
confint.Ahor.dung <- confint(modavgbelowA2)
coef.Ahor.dung <- summary(modavgbelowA2)$coefmat.subset
Ahor.dung <- cbind(coef.Ahor.dung, confint.Ahor.dung)
write.table(Ahor.dung, file="Ecosystem carbon/ConAvgAhordung.txt") # conditional average - I will try first with this.. 

##      4.2. Global model for Mineral hor C univariate variables ####
Soil.min.CnoNA<-Soil.min[!is.na(Soil.min$CFire_frequency.2000_2017),]
Soil.min.CnoNA <- Soil.min.CnoNA[(-16),]
Min.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + CHerb.C + #CTreeBM.N + 
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

# With livestock, wild dung
Soil.min2.CnoNA<-Soil.min2[!is.na(Soil.min2$livestock),]
Soil.min2.CnoNA$Clivestock <- as.numeric(scale(Soil.min2.CnoNA$livestock))
Soil.min2.CnoNA$Cwild <- as.numeric(scale(Soil.min2.CnoNA$wild))
#Soil.min2.CnoNA$CTermites <- as.numeric(scale(Soil.min2.CnoNA$Termite.effect))
Soil.min2.CnoNA <- Soil.min2.CnoNA[(-16),]
Soil.min2.CnoNA <- droplevels(Soil.min2.CnoNA)

Min.block2<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 + 
                   CTreeBM.kg_m2 + CSand + CHerb.C + Clivestock + Cwild 
                 + CMAP.mm_yr:CSand + 
                   (1|Region.x),data = Soil.min2.CnoNA, REML=F,
                 na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelowM2<-dredge(Min.block2,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & CHerb.C)&!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites))
modselbelowM2<-model.sel(modsetbelowM2) #Model selection table giving AIC, deltaAIC and weighting
modavgbelowM2<-model.avg(modselbelowM2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelowM2)#Importance of each variable
write.table(importance(modavgbelowM2),file="Ecosystem carbon/importanceMinhor.dung.txt")
summary(modavgbelowM2)#Estimated coefficients given weighting
confint.Minhor.dung <- confint(modavgbelowM2)
coef.Minhor.dung <- summary(modavgbelowM2)$coefmat.subset
Minhor.dung <- cbind(coef.Minhor.dung, confint.Minhor.dung)
write.table(Minhor.dung, file="Ecosystem carbon/ConAvgMinHordung.txt")

##      4.3. Global model for Herbs ####
# HERBACEOUS 
Herbaceous.CnoNA<-Herbaceous[!is.na(Herbaceous$Ctot.C.kg_m2),]
Herbaceous.CnoNA <- Herbaceous.CnoNA[(-20),]
Herbaceous.CnoNA <- Herbaceous.CnoNA[(-16),]
Herbaceous.CnoNA <- droplevels(Herbaceous.CnoNA)
summary(Herbaceous.CnoNA)

summary(lmer(C.amount~Fire_frequency.2000_2017 + (1|Region), data=Herbaceous.CnoNA))
plot(C.amount~Fire_frequency.2000_2017, data=Herbaceous.CnoNA)
abline(a=0.035862, b=-0.004594)


Herbaceous.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 + CSand + Ctot.N.kg_m2 + 
                         #CMAP.mm_yr:CSand + 
                         landuse:CMAP.mm_yr + landuse:CSand +
                         (1|Region),data = Herbaceous.CnoNA, REML=F, 
                       na.action=na.fail)

summary(Herbaceous.block)
drop1(Herbaceous.block,test="Chisq")  
anova(Herbaceous.block)
AIC(Herbaceous.block) #52.15059

# Model averaging: All possible models between null and global
modsetaboveH<-dredge(Herbaceous.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse) & !(Ctot.N.kg_m2 & CSand))
modselaboveH<-model.sel(modsetaboveH) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveH<-model.avg(modselaboveH)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveH)#Importance of each variable
write.table(importance(modavgaboveH),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgaboveH)
coef.Herb <- summary(modavgaboveH)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 

# With livestock, wild dung, not anything special.
Livestock.dung <- Livestock.dung[c(2,3)]
Wild.dung <- Wild.dung[c(2,3)]
Herbaceous2 <- left_join(Herbaceous,Livestock.dung,by="Block.ID",drop=F)
Herbaceous2 <- left_join(Herbaceous2,Wild.dung,by="Block.ID",drop=F)
Herbaceous2.CnoNA<-Herbaceous2[!is.na(Herbaceous2$livestock),]
Herbaceous2.CnoNA$Clivestock <- as.numeric(scale(Herbaceous2.CnoNA$livestock))
Herbaceous2.CnoNA$Cwild <- as.numeric(scale(Herbaceous2.CnoNA$wild))
Herbaceous2.CnoNA <- Herbaceous2.CnoNA[(-16),]
Herbaceous2.CnoNA <- droplevels(Herbaceous2.CnoNA)

Herb2.block<-lmer(Ctot.C.kg_m2~ CMAP.mm_yr + CFire_frequency.2000_2017 + 
                    CTreeBM.kg_m2 + CSand + Clivestock + Cwild
                  + CMAP.mm_yr:CSand + 
                    (1|Region.x),data = Herbaceous2.CnoNA, REML=F,
                  na.action=na.fail)

# Model averaging: All possible models between null and global
modsetHerb2<-dredge(Herb2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand) & !(Cwild & CTermites))
modselHerb2<-model.sel(modsetHerb2) #Model selection table giving AIC, deltaAIC and weighting
modavgHerb2<-model.avg(modselHerb2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgHerb2)#Importance of each variable
#write.table(importance(modavgHerb2),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgHerb2)
coef.Herb <- summary(modavgHerb2)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
#write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 

# ACCUMULATES HERBACEOUS BIOMASS 
names(Total.Eco.C.CnoNA2)

Accum.Herb<-lmer(CAccum.bm.kg_m2~ CMAP.mm_yr + landuse + CFire_frequency + CTreeBM.kg_m2 + CSand + Ctot.N.kg_m2 + CRoots.kg.m2 + Cwild + Clivestock + 
                         landuse:CMAP.mm_yr + landuse:CSand +
                         (1|Region),data = Total.Eco.C.CnoNA2, REML=F, 
                       na.action=na.fail)

summary(Accum.Herb)
drop1(Accum.Herb,test="Chisq")  
anova(Accum.Herb)
AIC(Accum.Herb) #52.15059

# Model averaging: All possible models between null and global
modsetaboveH<-dredge(Accum.Herb,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CTreeBM.kg_m2 & landuse) & !(Ctot.N.kg_m2 & CSand) & !(Clivestock & CSand) & !(Cwild & CSand) & !(CRoots.kg.m2 & CMAP.mm_yr)& !(Cwild & CMAP.mm_yr) & !(Clivestock & CTreeBM.kg_m2)& !(Cwild & CTreeBM.kg_m2))

modselaboveH<-model.sel(modsetaboveH) #Model selection table giving AIC, deltaAIC and weighting
modavgaboveH<-model.avg(modselaboveH)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgaboveH)#Importance of each variable
#write.table(importance(modavgaboveH),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgaboveH)
coef.Herb <- summary(modavgaboveH)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
#write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 

##      4.4. Global model for DW #### 
DW.CnoNA<-DW[!is.na(DW$CFire_frequency.2000_2017),]
DW.CnoNA <- DW.CnoNA[(-16),]
DW.block<-lmer(Ctot.C.kg_m2~ #CMAP.mm_yr + CSand + Ctot.N.kg_m2
                 landuse + CFire_frequency.2000_2017 + CTreeBM.kg_m2 +  #CTreeBM.N + 
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
#importance$Variable <- c("MAP","Tree biomass (N.fix)","Sand","Fire frequency","Land-use","Tree biomass","MAP:Sand")
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
                  CTreeBM.kg_m2 + Clivestock + Cwild + CTermites +
                  (1|Region.x),data = DW2.CnoNA, REML=F,
                na.action=na.fail)

# Model averaging: All possible models between null and global
modsetDW2<-dredge(DW2.block,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(Cwild & CTermites))
modselDW2<-model.sel(modsetDW2) #Model selection table giving AIC, deltaAIC and weighting
modavgDW2<-model.avg(modselDW2)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgDW2)#Importance of each variable
write.table(importance(modavgDW2),file="Ecosystem carbon/importanceaboveH.txt")
#Estimated coefficients given weighting
confint.Herb <- confint(modavgDW2)
coef.Herb <- summary(modavgDW2)$coefmat.subset
Herb <- cbind(coef.Herb, confint.Herb)
write.table(Herb, file="Ecosystem carbon/ConAvgH.txt") 
##      4.5. Global model for Woody #### 
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
#importance$Variable <- c("MAP","Tree biomass (N.fix)","Sand","Fire frequency","Land-use","Tree biomass","MAP:Sand")
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

##      4.6. From belowground.full fine scale ####

# Model averaging 
summary(Belowground.full.CnoNA2)
# A horizon added Accumulated biomass and residual biomass 
Belowground.Ahor <-lmer(CAhorC.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017  
                        + CTreeBM.kg_m2 + CSand 
                        + CHerbaceous + CAccum.bm.kg_m2 + CRes.bm.kg_m2 +
                          landuse:CMAP.mm_yr + landuse:CSand + 
                          (1|Region/Block.ID), data = Belowground.full.CnoNA2, REML=F, na.action=na.fail)

modsetbelow.Ahor<-dredge(Belowground.Ahor,trace = TRUE, rank = "AICc", REML = FALSE,subset=!(CTreeBM.kg_m2 & landuse)&!(CAccum.bm.kg_m2&CMAP.mm_yr)&!(CRes.bm.kg_m2&CSand)&!(CMAP.mm_yr & CHerbaceous))
modselbelow.Ahor<-model.sel(modsetbelow.Ahor) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Ahor<-model.avg(modselbelow.Ahor)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Ahor)
write.table(importance(modavgbelow.Ahor), file="Ecosystem carbon/importanceAhorFull.txt")#Importance of each variable
summary(modavgbelow.Ahor)#Estimated coefficients given weighting
confint.Ahor <- confint(modavgbelow.Ahor)
coef.Ahor <- summary(modavgbelow.Ahor)$coefmat.subset
Ahor <- cbind(coef.Ahor, confint.Ahor)
write.table(Ahor, file="Ecosystem carbon/ConAvgAhorFull.txt")

# A hor with dung NOT this I use 
colnames(Belowground.full.CnoNA3)
Belowground.Ahor.sub <-lmer(AhorC.kg_m2 ~ CMAP.mm_yr + CFire_frequency.2000_2017  + CTreeBM.kg_m2 + CSand + CHerbaceous + Ctotal.dung + CRoots.kg.m2 + CAccum.bm.kg_m2 +
                              (1|Region/Block.ID), data = Belowground.full.CnoNA3, REML=F, na.action=na.fail)

# Model averaging: All possible models between null and global
modsetbelow.Ahor.sub<-dredge(Belowground.Ahor.sub,trace = TRUE, rank = "AICc", REML = FALSE, subset=!(CSand & CHerbaceous)) #&!(Cwild & CSand) & !(Cwild & CMAP.mm_yr) & !(Clivestock & CSand)) #Model selection table giving AIC, deltaAIC and weighting
modavgbelow.Ahor.sub<-model.avg(modsetbelow.Ahor.sub)#Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavgbelow.Ahor.sub)
write.table(importance(modavgbelow.Ahor), file="Ecosystem carbon/importanceAhorDung.txt")#Importance of each variable
summary(modavgbelow.Ahor.sub)#Estimated coefficients given weighting
confint.Ahor <- confint(modavgbelow.Ahor)
coef.Ahor <- summary(modavgbelow.Ahor)$coefmat.subset
Ahor <- cbind(coef.Ahor, confint.Ahor)
write.table(Ahor, file="Ecosystem carbon/ConAvgAhorDung.txt")

# Mineral horizon
Belowground.Minhor <-lmer(CMinC.kg_m2 ~ CMAP.mm_yr + landuse + CFire_frequency.2000_2017  
                          + CTreeBM.kg_m2 + CSand 
                          + CHerbaceous + CAccum.bm.kg_m2 + CRes.bm.kg_m2 +
                            landuse:CMAP.mm_yr + landuse:CSand +  (1|Region/Block.ID), data = Belowground.full.CnoNA2, REML=F, na.action=na.fail)

modsetbelow.Minhor<-dredge(Belowground.Minhor,trace = TRUE, rank = "AICc", REML = FALSE,subset=!(CTreeBM.kg_m2 & landuse)&!(CAccum.bm.kg_m2&CMAP.mm_yr)&!(CRes.bm.kg_m2&CSand)&!(CMAP.mm_yr & CHerbaceous))

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

# Large model with as high resolution as possible 
names(Belowground.full.CnoNA)
summary(Belowground.full.CnoNA)
Belowground.full.CnoNA <- droplevels(Belowground.full.CnoNA)

Modlist.large <-   psem(
  lme(CWoody~ Ctot.N.kg_m2 + CFire_frequency.2000_2017 + landuse + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ CWoody + CFire_frequency.2000_2017 + landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CHerbaceous ~  Ctot.N.kg_m2 + CSand + CMAP.mm_yr + landuse + CFire_frequency.2000_2017,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CAhorC.kg_m2~ CHerbaceous + CRes.bm.kg_m2 + CAccum.bm.kg_m2, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path..
  CRes.bm.kg_m2%~~%CAccum.bm.kg_m2, # Testing variables 
  CRes.bm.kg_m2%~~%CHerbaceous, # Testing variables
  CHerbaceous%~~%CAccum.bm.kg_m2 # Testing variables
)

summary(Modlist.large,Belowground.full.CnoNA) # Not a good fit, p=0

# Large model best fit 
Modlist.large2 <-   psem(
  lme(CWoody~ landuse + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CAccum.bm.kg_m2 ~ Ctot.N.kg_m2 + CMAP.mm_yr + CDW + landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand + CAccum.bm.kg_m2, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  #lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CFire_frequency.2000_2017~ CWoody + CDW,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  landuse%~~%CMAP.mm_yr, # I know these are not correlated
  landuse%~~%CSand, # I know these are not correlated 
  CAhorC.kg_m2%~~%Ctot.N.kg_m2, # We know these are highly correlated, but no path.. 
  CMinC.kg_m2%~~%Ctot.N.kg_m2 # We know these are highly correlated, but no path..
)

summary(Modlist.large2,Belowground.full.CnoNA) # A good fit, p=0.712

#SEM.below <- summary(Modlist.below2,Belowground.full.CnoNA) # Good fit
#write.csv(SEM.below$coefficients, file = "Ecosystem carbon/SEMBelow.csv")

# Lower resolution data 

Modlist.mecanistic <-   psem(
  lme(CWoody~ Clivestock + CSand, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CDW~ landuse,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CAccum.bm.kg_m2 ~ CMAP.mm_yr + CRoots.kg.m2,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CAhorC.kg_m2~Clivestock, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CMinC.kg_m2~ CAhorC.kg_m2 + CSand, random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
  lme(CFire_frequency.2000_2017~ CWoody + CRoots.kg.m2, random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
 lme(Ctot.N.kg_m2~ CSand,random= ~ 1|Region/Block.ID,na.action=na.omit, data=Belowground.full.CnoNA),
 lme(CRoots.kg.m2~ CMAP.mm_yr,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
 lme(Cwild ~ CMAP.mm_yr,random= ~ 1|Region,na.action=na.omit, data=Belowground.full.CnoNA),
  CAhorC.kg_m2%~~%Ctot.N.kg_m2,
  CMinC.kg_m2%~~%Ctot.N.kg_m2
)

summary(Modlist.mecanistic,Belowground.full.CnoNA) 

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

#rownames(importance.AhorHerbs) <- (c("Sand","MAP","Land-use","Tree biomass","Herb biomass","MAP:Sand","MAP:Land-use","Fire frequency","Sand:Land-use"))

rownames(importance.MinHorFull) <- (c("Sand","Fire frequency","MAP","Land-use","Tree biomass","MAP:Land-use","Sand:Land-use"))

#rownames(importance.MinHorHerbs) <- (c("Sand","Land-use","MAP","MAP:Land-use","Sand:Land-use","MAP:Sand","Herb biomass","Tree biomass","Fire frequency"))

rownames(importance.H) <- (c("Fire frequency","Land-use","Sand","MAP", "Soil Nitrogen","Tree biomass","Sand:Land-use","MAP:Land-use"))

rownames(importance.DW) <- (c("Land-use","Fire frequency","Encroachment","Tree biomass"))

rownames(importance.W) <- (c("Land-use","Sand","MAP","Fire frequency","Soil Nitrogen","Sand:Land-use","MAP:Land-use"))

rownames(importance.W2) <- (c("Fire frequency","Domestic dung","Macro fauna", "Sand","MAP","Wild dung"))

rownames(importance.W.outl) <- (c("Fire frequency","landuse","MAP", "Sand","Soil Nitrogen","MAP:Land-use","Sand:Land-use"))

rownames(importance.Ahor.dung) <- (c("Domestic dung","Sand","Wild dung","MAP","Tree biomass","Fire frequency","Encroachment","Macro fauna","Herb biomass"))

#rownames(importance.MinHor.dung) <- (c("Sand","Fire frequency","Tree biomass","MAP","Macro fauna","Wild dung","Livestock dung", "Herb biomass","MAP:Sand"))

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

##      6.3: Variable coefficients from model averages ####
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
# axis(2, at=1:10, labels= c("MAP","Herb biomass","PA","Sand","Fire frequency","Tree biomass","MAP:Sand","MAP:PA","Sand:PA"),par(las=1),cex.axis=2)
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
# axis(2, at=1:10, labels= c("Sand","MAP","PA","Tree biomass","Herb biomass","MAP:Sand","MAP:PA","Fire frequency","Sand:PA"),par(las=1),cex.axis=2)
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
# axis(2, at=1:10, labels= c("Sand","PA","MAP","MAP:PA","Sand:PA","MAP:Sand","Herb biomass","Tree biomass","Fire frequency"),par(las=1),cex.axis=2)
# dev.off()

# Min-hor with dung 
# png(filename = "Ecosystem carbon/Figures/Fig.thesis/coef.Minhor.dung.png")
# par(mar=c(5,15,1,1))
# plot(rep(NA,10),1:10, xlim=c(-2,2), type="n", ann=F,axes=F, bty="n")
# arrows(y0=1:10, x0=con.avg.MinHor.dung$X2.5.., x1=con.avg.MinHor.dung$X97.5..,col=c(col.min.dung), angle=90,length=0.05,code=3,lwd=2)
# points(con.avg.MinHor.dung$Estimate,1:10,pch=con.avg.MinHor.dung$sign,col=c(col.min.dung), lwd=2, cex=2)
# abline(v=0)
# axis(1,cex.axis=2)
# axis(2, at=1:10, labels= c("Sand","Fire frequency","Tree biomass","MAP","Macro fauna","Wild dung","Livestock dung", "Herb biomass","MAP:Sand"),par(las=1),cex.axis=2)
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

