####Making tables####

# Uploading the soil file
total.soil.data<- read.csv("01bTotal_SoilCData.csv", head = TRUE)
names(total.soil.data)

# Want to have a table with soil texture (clay, silt and sand) and chemical traits
# First, reorganizing and removing collumns 
soil.chemistry <- total.soil.data[,c(1:5,28:30,34:39)]

#Making sure R knows that Region, Site, Block and Circle are factors 

soil.chemistry$fRegion <- as.factor(soil.chemistry$Region)
soil.chemistry$fSite <- as.factor(soil.chemistry$Site)
soil.chemistry$fBlock <- as.factor(soil.chemistry$Block)
soil.chemistry$fCircle <- as.factor(soil.chemistry$Circle)

names(soil.chemistry)

#Making a table, by using the aggregata function - taking the mean values of texture by region 

Clay<-aggregate(Clay.per~fRegion,soil.chemistry,mean)
Clay.sd<-aggregate(Clay.per~fRegion,soil.chemistry,sd)

Sand<-aggregate(Sand.per~fRegion,soil.chemistry,mean)
Sand.sd<-aggregate(Sand.per~fRegion,soil.chemistry,sd)

Silt<-aggregate(Silt.per~fRegion,soil.chemistry,mean)
Silt.sd<-aggregate(Silt.per~fRegion,soil.chemistry,sd)

pH<-aggregate(pH~fRegion,soil.chemistry,mean)
pH.sd<-aggregate(pH~fRegion,soil.chemistry,sd)

CEC<-aggregate(CEC.cmol_kg~fRegion,soil.chemistry,mean)
CEC.sd<-aggregate(CEC.cmol_kg~fRegion,soil.chemistry,sd)

Al<-aggregate(Al.mg_kg~fRegion,soil.chemistry,mean)
Al.sd<-aggregate(Al.mg_kg~fRegion,soil.chemistry,sd)

Fe<-aggregate(Fe.mg_kg~fRegion,soil.chemistry,mean)
Fe.sd<-aggregate(Fe.mg_kg~fRegion,soil.chemistry,sd)

P<-aggregate(P.mg_kg~fRegion,soil.chemistry,mean)
P.sd<-aggregate(P.mg_kg~fRegion,soil.chemistry,sd)

# using cbind to make one table, then colnames to change the table names 
SoilcheSummary<-cbind(Clay,Clay.sd[2],Silt[2],Silt.sd[2],Sand[2],Sand.sd[2],pH[2],pH.sd[2],CEC[2],CEC.sd[2],Al[2],Al.sd[2],Fe[2],Fe.sd[2],P[2],P.sd[2])
colnames(SoilcheSummary)<-c("Region","Clay","Clay.sd","Silt","Silt.sd","Sand","Sand.sd","pH","pH.sd","CEC","CEC.sd","Al","Al.sd","Fe","Fe.sd","P","P.sd")

SoilcheSummary

# trying to save as an excel file... 
install.packages("openxlsx")
library(openxlsx)




####Packages####
#library(lattice)
#library(MASS)
#library(dplyr)
#library(plyr)
#library(lubridate)
#library(data.table)
#library(xlsx)


