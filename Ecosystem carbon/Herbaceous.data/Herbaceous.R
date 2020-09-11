# Packages 
library(tidyr)
library(plyr)
library(dplyr)

#### Sorting the herbaceous data ####
AccumH6 <- read.csv(file="Ecosystem carbon/Herbaceous.data/AccumH6.csv", header=T)
Herbaceous <- read.csv(file="Ecosystem carbon/Herbaceous.data/12Herbaceous.csv", header=T)
HerbResBiomass <- read.csv(file="Ecosystem carbon/Herbaceous.data/HerbResBiomass.csv", header=T)

#### 1. Look at data biom ####
colnames(AccumH6)
AccumulatedHerb <- AccumH6[,c(1:6,8,10,11,81,82)]

Production <- filter(AccumulatedHerb,prodcons=="Cum_prod")
Consumption <- filter(AccumulatedHerb,prodcons=="Cum_cons")

colnames(Production)[11] <- "Cum_prod"
colnames(Consumption)[11] <- "Cum_cons"

colnames(Prod_cons)
Prod_cons <- cbind(Production,Consumption[,c(10,11)])
Prod_cons <- Prod_cons[,c(1:9,11,13)]

# new column
colnames(ExclosedData)
ExclosedData <- mutate(ExclosedData,ResidBiomass=prodtot-constot)

# want to create accumulated values of residual biomass per block (over all 6 harvests)

# Need block_ID 
ExclosedData$Block.id<-as.factor(with(ExclosedData, paste(site.id, block, sep="_")))

colnames(ExclosedData)
ExclosedDataRed <- ExclosedData[,c(1,27,5,6,7,10,20,21,26)]


#### 2. Herbaceous dataset ####
# Remove NAs 
Herbaceous <- na.omit(Herbaceous)
Herbaceous <- droplevels(Herbaceous)
names(Herbaceous)
names(HerbResBiomass)
colnames(HerbResBiomass)[2] <- "Region"
colnames(HerbResBiomass)[3] <- "Block"

# Level region 
levels(Herbaceous$Region)
Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))
HerbResBiomass$Region<- factor(HerbResBiomass$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Create one dataset 
library(dplyr)
library(plyr)
Herbaceous2 <- left_join(HerbResBiomass,Herbaceous,by=c("Region","Block"), drop=F)
names(Herbaceous2)
Herbaceous2 <- Herbaceous2[c(2:7,12:15)]

Herbaceous2$Block.ID[is.na(Herbaceous2$Block.ID)] <- 1

# Accumulated and recidual biomass is given in g/m2, want to change it to kg/m2 
Herbaceous2$Accum.bm.kg_m2 <- Herbaceous2$Accum.biomass / 1000
Herbaceous2$Res.bm.kg_m2 <- Herbaceous2$Res.biom / 1000

write.csv(Herbaceous2, "Ecosystem carbon/Herbaceous.data/Herbaceous.csv")
