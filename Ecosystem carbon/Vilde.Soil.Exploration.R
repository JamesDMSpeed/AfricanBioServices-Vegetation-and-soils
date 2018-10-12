####Making tables####

# Uploading the soil file
total.soil.data<- read.csv("Ecosystem Carbon/Total.soil.data.csv", head = TRUE)
names(total.soil.data)

# Want to have a table with soil texture (clay, silt and sand) and chemical traits
# First, reorganizing and removing collumns 
soil.chemistry <- total.soil.data[,c(1:5,30:32,36:41)]
names(soil.chemistry)

# Can do this to make sure R knows that Region, Site, Block and Circle are factors 
#soil.chemistry$fRegion <- as.factor(soil.chemistry$Region)
#soil.chemistry$fSite <- as.factor(soil.chemistry$Site)
#soil.chemistry$fBlock <- as.factor(soil.chemistry$Block)
#soil.chemistry$fCircle <- as.factor(soil.chemistry$Circle)

#Making a table, by using the aggregata function - taking the mean values of texture by region 

Clay<-aggregate(Clay.per~Region,soil.chemistry,mean)
Clay.sd<-aggregate(Clay.per~Region,soil.chemistry,sd)

Sand<-aggregate(Sand.per~Region,soil.chemistry,mean)
Sand.sd<-aggregate(Sand.per~Region,soil.chemistry,sd)

Silt<-aggregate(Silt.per~Region,soil.chemistry,mean)
Silt.sd<-aggregate(Silt.per~Region,soil.chemistry,sd)

pH<-aggregate(pH~Region,soil.chemistry,mean)
pH.sd<-aggregate(pH~Region,soil.chemistry,sd)

CEC<-aggregate(CEC.cmol_kg~Region,soil.chemistry,mean)
CEC.sd<-aggregate(CEC.cmol_kg~Region,soil.chemistry,sd)

Al<-aggregate(Al.mg_kg~Region,soil.chemistry,mean)
Al.sd<-aggregate(Al.mg_kg~Region,soil.chemistry,sd)

Fe<-aggregate(Fe.mg_kg~Region,soil.chemistry,mean)
Fe.sd<-aggregate(Fe.mg_kg~Region,soil.chemistry,sd)

P<-aggregate(P.mg_kg~Region,soil.chemistry,mean)
P.sd<-aggregate(P.mg_kg~Region,soil.chemistry,sd)

# using cbind to make one table, then colnames to change the table names 
SoilcheSummary<-cbind(Clay,Clay.sd[2],Silt[2],Silt.sd[2],Sand[2],Sand.sd[2],pH[2],pH.sd[2],CEC[2],CEC.sd[2],Al[2],Al.sd[2],Fe[2],Fe.sd[2],P[2],P.sd[2])
colnames(SoilcheSummary)<-c("Region","Clay","Clay.sd","Silt","Silt.sd","Sand","Sand.sd","pH","pH.sd","CEC","CEC.sd","Al","Al.sd","Fe","Fe.sd","P","P.sd")

SoilcheSummary

# want to have a table with region, land use, year of last fire, fire frequency, MAP, altitude 

# First, reorganizing and removing collumns 
site.traits <- total.soil.data[,c(1:5,13:17)]
names(site.traits)

site.traits$fRegion <- as.factor(site.traits$Region)
site.traits$fLand_Use <- as.factor(site.traits$Land_Use)
#soil.chemistry$fSite <- as.factor(soil.chemistry$Site)
#soil.chemistry$fBlock <- as.factor(soil.chemistry$Block)
#soil.chemistry$fCircle <- as.factor(soil.chemistry$Circle)

# aggrigate 
MAP <- aggregate(MAP.mm_yr~fRegion,site.traits,mean)
MAP.sd <- aggregate(MAP.mm_yr~fRegion,site.traits,sd)

Altitude <- aggregate(Altitude~fRegion,site.traits,mean)
Altitude.sd <- aggregate(Altitude~fRegion,site.traits,sd)

Last.Fire <- aggregate(Last_fire.yr~fRegion,site.traits,mean)
Last.Fire.sd <- aggregate(Last_fire.yr~fRegion,site.traits,sd)
  
Fire.freq <- aggregate(Fire_frequency.2000_2017~fRegion,site.traits,mean)
Fire.freq.sd <- aggregate(Fire_frequency.2000_2017~fRegion,site.traits,sd)

# using cbind to make one table, then colnames to change the table names 
SoiltraitsSummary<-cbind(MAP,Altitude[2],Last.Fire[2],Fire.freq[2])
colnames(SoiltraitsSummary)<-c("Region","MAP (mm/yr)","Altitude","Year of last fire","Fire frequescy") 

SoiltraitsSummary


# trying to save as an excel file... 
# install.packages("openxlsx")
# library(openxlsx)

#### Bulk Density, correlation ####

Bulk.density <- read.csv("Ecosystem Carbon/02BulkSoil.csv", head=T)
names(Bulk.density)

tail(Bulk.density) # Removed NA rows

# making vectors
BD.average <- Bulk.density$BD.average_g.cm3
BD.controll <- Bulk.density$BD.controll_g.cm3

# doing a pearson correlation test, with the result of a p< 0.05, and a 0.85 correlation coefficient  
cor.test(BD.average,BD.controll,method = "pearson",na.rm=T)

# making a lm 
BD.model <- lm(BD.average~BD.controll,Bulk.density)
summary(BD.model)
par(mfrow=(c(1,1)))
plot(BD.model) # outlayers: 6 (Mwantimba B2),9 (Handajega B2),13 (Park Nyigoti B2)

# Prediction line
av<-seq(0,1.5, length.out=15)
Bulkpred.lm<-predict(BD.model, list(BD.controll=av),
                     se.fit=T) # Se fit gives 95% confience interval estimates
length(Bulkpred.lm)

# Plot + prediction line # This is nicer/easier in ggplot with geom_ribbon...
plot(BD.average~BD.controll, data =Bulk.density)
lines(av,Bulkpred.lm$fit+Bulkpred.lm$se.fit,lty = 2, lwd =1.75, col = "red")
lines(av,Bulkpred.lm$fit-Bulkpred.lm$se.fit,lty = 2, lwd =1.75, col = "red")
lines(av,Bulkpred.lm$fit,lty = 1, lwd =1.75, col = "red")

# This is interactive to identify missing outliers - outside error
#identify(Bulk.density$BD.average~Bulk.density$BD.controll)
# 9 and 13 are large outliers
Bulk.density[9,] # Handajega   S4 
Bulk.density[13,] # Park Nyigoti   S6

####Packages####
#library(lattice)
#library(MASS)
#library(dplyr)
#library(plyr)
#library(lubridate)
#library(data.table)
#library(xlsx)


