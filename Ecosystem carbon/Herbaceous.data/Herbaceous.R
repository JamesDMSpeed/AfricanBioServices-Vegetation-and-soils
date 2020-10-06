# Packages 
library(tidyr)
library(plyr)
library(dplyr)

#### 1. Sorting the herbaceous data ####
AccumH6 <- read.csv(file="Ecosystem carbon/Herbaceous.data/AccumH6.csv", header=T)
Herbaceous <- read.csv(file="Ecosystem carbon/Herbaceous.data/12Herbaceous.csv", header=T)
#HerbResBiomass <- read.csv(file="Ecosystem carbon/Herbaceous.data/HerbResBiomass.csv", header=T)

#### 1. Look at data biom ####
colnames(AccumH6)
AccumulatedHerb <- AccumH6[,c(3:6,8,10,11,81,82)]

# Level site.name
AccumulatedHerb$site.name<- factor(AccumulatedHerb$site.name, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

Production <- filter(AccumulatedHerb,prodcons=="Cum_prod")
Consumption <- filter(AccumulatedHerb,prodcons=="Cum_cons")

colnames(Production)[9] <- "Cum_prod"
colnames(Consumption)[9] <- "Cum_cons"

Prod_cons <- cbind(Production,Consumption[,c(8,9)])
colnames(Prod_cons)
Prod_cons <- Prod_cons[,c(4:9,11)]

# new column
colnames(Prod_cons)
Prod_cons <- mutate(Prod_cons,ResidAccumBiomass=Cum_prod-Cum_cons)

colnames(Prod_cons)
colnames(Prod_cons)[2] <- "Region"
colnames(Prod_cons)[4] <- "Block"
Prod_cons <- droplevels(Prod_cons)

# Create one dataset 
Herbaceous2 <- left_join(Herbaceous,Prod_cons,by=c("Region","Block"), drop=F)
names(Herbaceous2)
Herbaceous2 <- Herbaceous2[c(1:3,15,5,6,8:11,17:19)]

# Accumulated recidual biomass is given in g/m2, want to change it to kg/m2 
Herbaceous2$ResidAccumBiomass <- Herbaceous2$ResidAccumBiomass / 1000

write.csv(Herbaceous2, "Ecosystem carbon/Herbaceous.data/AccumHerbaceous.csv")
