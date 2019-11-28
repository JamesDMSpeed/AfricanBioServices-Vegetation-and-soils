#### Sorting the herbaceous data ####
Herbaceous <- read.csv(file="Ecosystem carbon/Herbaceous.data/12Herbaceous.csv", header=T)
HerbResBiomass <- read.csv(file="Ecosystem carbon/Herbaceous.data/HerbResBiomass.csv", header=T)

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
