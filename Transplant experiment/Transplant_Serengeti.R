########################################################################
#### Serengeti turf trasnplant experiment ####
########################################################################
#Libraries
library(dplyr)
library(xlsx)

########################################################################
##### Joining aboveground and belowground files ####
# Import data transplant experiment
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")
AboveTrans<-read.csv(file="Aboveground.survey.May2018depth.csv", sep=",",header=TRUE)
BelowTrans<-read.csv(file="Belowground.soil.transplant.May2018.csv", sep=",",header=TRUE)
names(AboveTrans)
AboveTrans$Current.placement.of.the.turf
BelowTrans$Current.placement.of.the.turf

# Join above and belowground - test which samples are missing...
TransExp<- left_join(AboveTrans,BelowTrans, by=c("Current.placement.of.the.turf","depth"),drop=F)
dim(TransExp) # 310 23

#write.xlsx(TransExp, "/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/TransExpAbove_below.xlsx")

########################################################################
##### Joining aboveground and belowground files ####

# Import data transplant experiment
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")
AboveCover<-read.csv(file="Aboveground.survey.Nov2018target.csv", sep=",",header=TRUE)
AboveWeight<-read.csv(file="Aboveground.biomass.wts.Nov2018.csv", sep=",",header=TRUE)

names(AboveCover)
names(AboveWeight)

levels(AboveCover$CurrentPlacementID) # 152 levels
levels(AboveWeight$CurrentPlacementID) # 133

levels(AboveCover$Target) # 152 levels
levels(AboveWeight$Target) # 133

levels(AboveCover$Area) # 5 levels
levels(AboveWeight$Area) # 5 levels


# Join above and belowground - test which samples are missing...
CoverWeight<- left_join(AboveCover,AboveWeight, by=c("CurrentPlacementID","Target","Area"),drop=F)
dim(AboveCover) # 304 21
dim(CoverWeight) # 315 29 # 

# Identify duplicates PlacementID
CoverWeight$PlaceTarget<-as.factor(with(CoverWeight, paste(CurrentPlacementID,Target,Area, sep="_")))

CoverWeightDups<-CoverWeight[duplicated(CoverWeight$PlaceTarget), ]
dim(CoverWeightDups) # 11 duplicates

# Export text file
write.table(CoverWeight, "Cover.WeightsNov2018target.txt", row.name=F, sep="\t")
write.table(CoverWeightDups, "Cover.WeightsNov2018targetDuplicates.txt", row.name=F, sep="\t")

