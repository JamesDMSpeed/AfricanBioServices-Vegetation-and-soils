#### Biomass exploration ####
#clear system & add package libraries
rm(list=ls())
library(lattice) #model validation
library(MASS)
library(ggplot2)
library(dplyr)
library(gcookbook)
library(readr)
library(tidyr) #wide to long format
library(lemon)
library(Hmisc)

library(gridBase)
library(gridExtra)
library(ggpubr)
library(nlme)
library(lme4)
library(MuMIn) #for mod.sel()

Biomass <- read.csv("Moveable exclosures/Biomass.csv", header=T,sep=",")
tail(Biomass)

#Stacking target and other underneath each other
#Getting long format 
#Biomass.stack <- gather(Biomass,pool,biomass.sp, 37:38, factor_key=TRUE)
# levels(Biomass.stack$pool)<-c("target","other")
# stackB <- gather(Biomass,pool,prodsp,40,42,factor_key=TRUE)
# levels(stackB$pool)<-c("target","other")
# stackC <- gather(Biomass,pool,conssp,41,43,factor_key=TRUE)
# levels(stackC$pool)<-c("target","other")
# stackD <- gather(Biomass,pool,prodsp.per,47,49,factor_key=TRUE)
# levels(stackD$pool)<-c("target","other")
# stackE <- gather(Biomass,pool,conssp.per,48,50,factor_key=TRUE)
# levels(stackE$pool)<-c("target","other")
# 
# 
# #Adding new variables to Biomass.stack
# Biomass.stack <- cbind(Biomass.stack,stackB[,"prodsp",drop=FALSE])
# Biomass.stack <- cbind(Biomass.stack,stackC[,"conssp",drop=FALSE])
# Biomass.stack <- cbind(Biomass.stack,stackD[,"prodsp.per",drop=FALSE])
# Biomass.stack <- cbind(Biomass.stack,stackE[,"conssp.per",drop=FALSE])
# 
# #Removing the old variables
# Biomass.stack <- Biomass.stack[,-c(38:41,44:48)]

#write.csv(Biomass.stack,'Moveable exclosures/BiomassStacked2.csv')
#Databiom <- read.csv("Moveable exclosures/BiomassStacked2.csv",header=T) #712 obs

Nuts <- read.csv("Moveable exclosures/Nutrients.csv", header=T,sep=";")
Datanuts <- Nuts[Nuts$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Datanuts <- Datanuts[Datanuts$harvest!="H0",] #removing H0                #280 obs
Datanuts <- droplevels(Datanuts)

#### RUN FIRST ####
#Housekeeping
#Date variable
#Dataframes for modelling
#Functions
# For graphs
#Average NAP
#Average CONS 
#Aggregating rainfall per region

#### Housekeeping ####
# Removing Ex2 - separate analysis
Databiom <- Biomass[Biomass$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Databiom <- Databiom[Databiom$harvest!="H0",] #removing H0                #280 obs
Databiom <- droplevels(Databiom)

# Creating factor variables
Databiom$landuse<-as.factor(Databiom$landuse)
Databiom$region<-as.factor(Databiom$region)
Databiom$site.name <- as.factor(Databiom$site.name)
Databiom$block<-as.factor(Databiom$block)
Databiom$treatment<-as.factor(Databiom$treatment)
Databiom$harvest<-as.factor(Databiom$harvest)
Databiom$site.id <- as.factor(Databiom$site.id)
Databiom$block.id.harvest <- as.factor(Databiom$block.id.harvest)

#Renaming total productivity and consumption columns
colnames(Databiom)[colnames(Databiom)=="productivity.target.g.m2.day"] <- "prodtarg"
colnames(Databiom)[colnames(Databiom)=="productivity.target.g.m2.dayWEIGHTED"] <- "prodtarg.per"

colnames(Databiom)[colnames(Databiom)=="productivity.total.g.m2.day"] <- "prodtot"
colnames(Databiom)[colnames(Databiom)=="productivity.total.g.m2.dayWEIGHTED"] <- "prodtot.per"

colnames(Databiom)[colnames(Databiom)=="consumption.target.g.m2.day"] <- "constarg"
colnames(Databiom)[colnames(Databiom)=="consumption.target.g.m2.day.WEIGHTED"] <- "constarg.per"

colnames(Databiom)[colnames(Databiom)=="consumption.total.g.m2.day"] <- "constot"
colnames(Databiom)[colnames(Databiom)=="consumption.total.g.m2.dayWEIGHTED"] <- "constot.per"


#Productivity and consumptions per harvest period
Databiom$prodtarg.sum <- Databiom$prodtarg*Databiom$growth.period
Databiom$constarg.sum <- Databiom$constarg*Databiom$growth.period

Databiom$prodtargper.sum <- Databiom$prodtarg.per*Databiom$growth.period
Databiom$constargper.sum <- Databiom$constarg.per*Databiom$growth.period

Databiom$prodtot.sum <- Databiom$prodtot*Databiom$growth.period
Databiom$constot.sum <- Databiom$constot*Databiom$growth.period
Databiom$prodtotper.sum <- Databiom$prodtot.per*Databiom$growth.period
Databiom$constotper.sum <- Databiom$constot.per*Databiom$growth.period


colnames(Databiom)[colnames(Databiom)=="sand.per"] <- "sand"

#Renaming levels in region, landuse and treatment columns
levels(Databiom$region)<-c("Dry Region","Intermediate Region","Wet Region")
levels(Databiom$landuse)<-c("pasture","wild")
levels(Databiom$treatment)<-c("exclosed","open")

#### Date variable ####
# Rdate create month column. default was (="%d.%m.%Y")
Rdate<-strptime(as.character(Databiom$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )# East African time #USE
class(Rdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
Databiom$Rdate<-Rdate# Add to the dataframe #
# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
Databiom$YrMonth<-format(as.Date(Rdate), "%Y-%m")

Databiom$month<-Databiom$Rdate$mon+1
Databiom$month <- month.abb[Databiom$month] #Changing to month name abbrevitations
Databiom$month<-as.factor(Databiom$month)

# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
Databiom$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(Databiom$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
Databiom$plot.code <- as.factor(with(Databiom,paste(region,landuse,block,treatment,sep="_")))
levels(Databiom$plot.code) #40 levels

#### New Y variables for hypotheses ####
# Adding new columns NAP-cons
Databiom$difftarg <- Databiom$prodtarg-Databiom$constarg
Databiom$difftotal <- Databiom$prodtot-Databiom$constot


# Adding new column with % NAP consumed 
Databiom$consper <- Databiom$constot/Databiom$prodtot*100
Databiom$constargfrac <- Databiom$constarg/Databiom$prodtarg*100

# Getting N total in Datanuts
Datanuts$N.target.biom <- Datanuts$N.target*Datanuts$biomass.target.sp./100
Datanuts$N.other.biom <- Datanuts$N.other*Datanuts$biomass.other.sp./100
Datanuts$N.total <- (Datanuts$N.target.biom+Datanuts$N.other.biom)/Datanuts$biomass.total.g*100

# Adding nitrogen data to Databiom
Databiom <- cbind(Databiom,Datanuts[,"N.target",drop=FALSE])
Databiom <- cbind(Databiom,Datanuts[,"N.other",drop=FALSE])
Databiom <- cbind(Databiom,Datanuts[,"N.total",drop=FALSE])

#Rain per day for each harvest period
Databiom$rain.day <- Databiom$rain.sum/Databiom$growth.period

#### Average df site per harvest ####
DataEx <- Databiom[Databiom$treatment!="open",] #Removing Open
DataEx <- DataEx[order(DataEx[,3],DataEx[,9],DataEx[,12]),]
DataEx$Rdate <- as.character(DataEx$Rdate)
DataOp <- Databiom[Databiom$treatment!="exclosed",] #Removing Exclosure
DataOp <- DataOp[order(DataOp[,3],DataOp[,9],DataOp[,12]),]
DataOp$Rdate <- as.character(DataOp$Rdate)

Exmean <- aggregate.data.frame(DataEx[,c(18,19,21,26:33,40:61,66:73)], list(as.factor(DataEx$YrMonth), DataEx$site.id, DataEx$landuse), mean, na.rm=TRUE,na.action="na.pass")
Exmean$treatment <- "exclosed"
Exmean$treatment <- factor(Exmean$treatment)
Exmean$Rdate <- as.character(Exmean$Rdate)

Opmean <- aggregate.data.frame(DataOp[,c(18,19,21,26:33,40:61,66:73)], list(as.factor(DataOp$YrMonth), DataOp$site.id, DataOp$landuse), mean, na.rm=TRUE, na.action="na.pass")
Opmean$treatment <- "open"
Opmean$treatment <- factor(Opmean$treatment)
Opmean$Rdate <- as.character(Opmean$Rdate)

Datamean <- bind_rows(Exmean,Opmean) #NB! Using rbind resulted in aborting the whole session!!
colnames(Datamean)[1] <- "YrMonth"
colnames(Datamean)[2] <- "site.id"
colnames(Datamean)[3] <- "landuse"

#### Cumulative production, consumption, rainfall and N contents for aggregated data #### 
Datamean$Cum_prod<-c(cumsum(Datamean[1:7,"prodtot.sum"]),cumsum(Datamean[8:14,"prodtot.sum"]),cumsum(Datamean[15:21,"prodtot.sum"]),cumsum(Datamean[22:28,"prodtot.sum"]),cumsum(Datamean[29:35,"prodtot.sum"]),cumsum(Datamean[36:42,"prodtot.sum"]),cumsum(Datamean[43:49,"prodtot.sum"]),cumsum(Datamean[50:56,"prodtot.sum"]),cumsum(Datamean[57:63,"prodtot.sum"]),cumsum(Datamean[64:70,"prodtot.sum"]))

Datamean$Cum_prod2<-c(cumsum(Datamean[1:7,"prodtotper.sum"]),cumsum(Datamean[8:14,"prodtotper.sum"]),cumsum(Datamean[15:21,"prodtotper.sum"]),cumsum(Datamean[22:28,"prodtotper.sum"]),cumsum(Datamean[29:35,"prodtotper.sum"]),cumsum(Datamean[36:42,"prodtotper.sum"]),cumsum(Datamean[43:49,"prodtotper.sum"]),cumsum(Datamean[50:56,"prodtotper.sum"]),cumsum(Datamean[57:63,"prodtotper.sum"]),cumsum(Datamean[64:70,"prodtotper.sum"]))

Datamean$Cum_cons<-c(cumsum(Datamean[1:7,"constot.sum"]),cumsum(Datamean[8:14,"constot.sum"]),cumsum(Datamean[15:21,"constot.sum"]),cumsum(Datamean[22:28,"constot.sum"]),cumsum(Datamean[29:35,"constot.sum"]),cumsum(Datamean[36:42,"constot.sum"]),cumsum(Datamean[43:49,"constot.sum"]),cumsum(Datamean[50:56,"constot.sum"]),cumsum(Datamean[57:63,"constot.sum"]),cumsum(Datamean[64:70,"constot.sum"]))

Datamean$Cum_cons2<-c(cumsum(Datamean[1:7,"constotper.sum"]),cumsum(Datamean[8:14,"constotper.sum"]),cumsum(Datamean[15:21,"constotper.sum"]),cumsum(Datamean[22:28,"constotper.sum"]),cumsum(Datamean[29:35,"constotper.sum"]),cumsum(Datamean[36:42,"constotper.sum"]),cumsum(Datamean[43:49,"constotper.sum"]),cumsum(Datamean[50:56,"constotper.sum"]),cumsum(Datamean[57:63,"constotper.sum"]),cumsum(Datamean[64:70,"constotper.sum"]))

Datamean$Cum_prodtarg<-c(cumsum(Datamean[1:7,"prodtarg.sum"]),cumsum(Datamean[8:14,"prodtarg.sum"]),cumsum(Datamean[15:21,"prodtarg.sum"]),cumsum(Datamean[22:28,"prodtarg.sum"]),cumsum(Datamean[29:35,"prodtarg.sum"]),cumsum(Datamean[36:42,"prodtarg.sum"]),cumsum(Datamean[43:49,"prodtarg.sum"]),cumsum(Datamean[50:56,"prodtarg.sum"]),cumsum(Datamean[57:63,"prodtarg.sum"]),cumsum(Datamean[64:70,"prodtarg.sum"]))

Datamean$Cum_prodtarg2<-c(cumsum(Datamean[1:7,"prodtargper.sum"]),cumsum(Datamean[8:14,"prodtargper.sum"]),cumsum(Datamean[15:21,"prodtargper.sum"]),cumsum(Datamean[22:28,"prodtargper.sum"]),cumsum(Datamean[29:35,"prodtargper.sum"]),cumsum(Datamean[36:42,"prodtargper.sum"]),cumsum(Datamean[43:49,"prodtargper.sum"]),cumsum(Datamean[50:56,"prodtargper.sum"]),cumsum(Datamean[57:63,"prodtargper.sum"]),cumsum(Datamean[64:70,"prodtargper.sum"]))

Datamean$Cum_constarg<-c(cumsum(Datamean[1:7,"constarg.sum"]),cumsum(Datamean[8:14,"constarg.sum"]),cumsum(Datamean[15:22,"constarg.sum"]),cumsum(Datamean[23:28,"constarg.sum"]),cumsum(Datamean[29:35,"constarg.sum"]),cumsum(Datamean[36:42,"constarg.sum"]),cumsum(Datamean[43:49,"constarg.sum"]),cumsum(Datamean[50:56,"constarg.sum"]),cumsum(Datamean[57:63,"constarg.sum"]),cumsum(Datamean[64:70,"constarg.sum"]))

Datamean$Cum_constarg2<-c(cumsum(Datamean[1:7,"constargper.sum"]),cumsum(Datamean[8:14,"constargper.sum"]),cumsum(Datamean[15:22,"constargper.sum"]),cumsum(Datamean[23:28,"constargper.sum"]),cumsum(Datamean[29:35,"constargper.sum"]),cumsum(Datamean[36:42,"constargper.sum"]),cumsum(Datamean[43:49,"constargper.sum"]),cumsum(Datamean[50:56,"constargper.sum"]),cumsum(Datamean[57:63,"constargper.sum"]),cumsum(Datamean[64:70,"constargper.sum"]))

Datamean$Cum_rain<-c(cumsum(Datamean[1:7,5]),cumsum(Datamean[8:14,5]),cumsum(Datamean[15:21,5]),cumsum(Datamean[22:28,5]),cumsum(Datamean[29:35,5]),cumsum(Datamean[36:42,5]),cumsum(Datamean[43:49,5]),cumsum(Datamean[50:56,5]),cumsum(Datamean[57:63,5]),cumsum(Datamean[64:70,5]))

Datamean$Cum_N.targ<-c(cumsum(Datamean[1:7,"N.target"]),cumsum(Datamean[8:14,"N.target"]),cumsum(Datamean[15:21,"N.target"]),cumsum(Datamean[22:28,"N.target"]),cumsum(Datamean[29:35,"N.target"]),cumsum(Datamean[36:42,"N.target"]),cumsum(Datamean[43:49,"N.target"]),cumsum(Datamean[50:56,"N.target"]),cumsum(Datamean[57:63,"N.target"]),cumsum(Datamean[64:70,"N.target"]))

Datamean$Cum_N.tot<-c(cumsum(Datamean[1:7,"N.total"]),cumsum(Datamean[8:14,"N.total"]),cumsum(Datamean[15:21,"N.total"]),cumsum(Datamean[22:28,"N.total"]),cumsum(Datamean[29:35,"N.total"]),cumsum(Datamean[36:42,"N.total"]),cumsum(Datamean[43:49,"N.total"]),cumsum(Datamean[50:56,"N.total"]),cumsum(Datamean[57:63,"N.total"]),cumsum(Datamean[64:70,"N.total"]))

# % production consumed 
Datamean$Cum_perc_cons<-Datamean$Cum_cons/Datamean$Cum_prod*100

#### Cumulative production, consumption, rainfall and N contents for non-aggregated data ####
##DataEx 
DataEx$Cum_prod<-c(cumsum(DataEx[1:7,"prodtot.sum"]),cumsum(DataEx[8:14,"prodtot.sum"]),cumsum(DataEx[15:21,"prodtot.sum"]),cumsum(DataEx[22:28,"prodtot.sum"]),cumsum(DataEx[29:35,"prodtot.sum"]),cumsum(DataEx[36:42,"prodtot.sum"]),cumsum(DataEx[43:49,"prodtot.sum"]),cumsum(DataEx[50:56,"prodtot.sum"]),cumsum(DataEx[57:63,"prodtot.sum"]),cumsum(DataEx[64:70,"prodtot.sum"]),cumsum(DataEx[71:77,"prodtot.sum"]),cumsum(DataEx[78:84,"prodtot.sum"]),cumsum(DataEx[85:91,"prodtot.sum"]),cumsum(DataEx[92:98,"prodtot.sum"]),cumsum(DataEx[99:105,"prodtot.sum"]),cumsum(DataEx[106:112,"prodtot.sum"]),cumsum(DataEx[113:119,"prodtot.sum"]),cumsum(DataEx[120:126,"prodtot.sum"]),cumsum(DataEx[127:133,"prodtot.sum"]),cumsum(DataEx[134:140,"prodtot.sum"]))

DataEx$Cum_prod2<-c(cumsum(DataEx[1:7,"prodtotper.sum"]),cumsum(DataEx[8:14,"prodtotper.sum"]),cumsum(DataEx[15:21,"prodtotper.sum"]),cumsum(DataEx[22:28,"prodtotper.sum"]),cumsum(DataEx[29:35,"prodtotper.sum"]),cumsum(DataEx[36:42,"prodtotper.sum"]),cumsum(DataEx[43:49,"prodtotper.sum"]),cumsum(DataEx[50:56,"prodtotper.sum"]),cumsum(DataEx[57:63,"prodtotper.sum"]),cumsum(DataEx[64:70,"prodtotper.sum"]),cumsum(DataEx[71:77,"prodtotper.sum"]),cumsum(DataEx[78:84,"prodtotper.sum"]),cumsum(DataEx[85:91,"prodtotper.sum"]),cumsum(DataEx[92:98,"prodtotper.sum"]),cumsum(DataEx[99:105,"prodtotper.sum"]),cumsum(DataEx[106:112,"prodtotper.sum"]),cumsum(DataEx[113:119,"prodtotper.sum"]),cumsum(DataEx[120:126,"prodtotper.sum"]),cumsum(DataEx[127:133,"prodtotper.sum"]),cumsum(DataEx[134:140,"prodtotper.sum"]))

DataEx$Cum_cons<-c(cumsum(DataEx[1:7,"constot.sum"]),cumsum(DataEx[8:14,"constot.sum"]),cumsum(DataEx[15:21,"constot.sum"]),cumsum(DataEx[22:28,"constot.sum"]),cumsum(DataEx[29:35,"constot.sum"]),cumsum(DataEx[36:42,"constot.sum"]),cumsum(DataEx[43:49,"constot.sum"]),cumsum(DataEx[50:56,"constot.sum"]),cumsum(DataEx[57:63,"constot.sum"]),cumsum(DataEx[64:70,"constot.sum"]),cumsum(DataEx[71:77,"constot.sum"]),cumsum(DataEx[78:84,"constot.sum"]),cumsum(DataEx[85:91,"constot.sum"]),cumsum(DataEx[92:98,"constot.sum"]),cumsum(DataEx[99:105,"constot.sum"]),cumsum(DataEx[106:112,"constot.sum"]),cumsum(DataEx[113:119,"constot.sum"]),cumsum(DataEx[120:126,"constot.sum"]),cumsum(DataEx[127:133,"constot.sum"]),cumsum(DataEx[134:140,"constot.sum"]))

DataEx$Cum_cons2<-c(cumsum(DataEx[1:7,"constotper.sum"]),cumsum(DataEx[8:14,"constotper.sum"]),cumsum(DataEx[15:21,"constotper.sum"]),cumsum(DataEx[22:28,"constotper.sum"]),cumsum(DataEx[29:35,"constotper.sum"]),cumsum(DataEx[36:42,"constotper.sum"]),cumsum(DataEx[43:49,"constotper.sum"]),cumsum(DataEx[50:56,"constotper.sum"]),cumsum(DataEx[57:63,"constotper.sum"]),cumsum(DataEx[64:70,"constotper.sum"]),cumsum(DataEx[71:77,"constotper.sum"]),cumsum(DataEx[78:84,"constotper.sum"]),cumsum(DataEx[85:91,"constotper.sum"]),cumsum(DataEx[92:98,"constotper.sum"]),cumsum(DataEx[99:105,"constotper.sum"]),cumsum(DataEx[106:112,"constotper.sum"]),cumsum(DataEx[113:119,"constotper.sum"]),cumsum(DataEx[120:126,"constotper.sum"]),cumsum(DataEx[127:133,"constotper.sum"]),cumsum(DataEx[134:140,"constotper.sum"]))

DataEx$Cum_prodtarg<-c(cumsum(DataEx[1:7,"prodtarg.sum"]),cumsum(DataEx[8:14,"prodtarg.sum"]),cumsum(DataEx[15:21,"prodtarg.sum"]),cumsum(DataEx[22:28,"prodtarg.sum"]),cumsum(DataEx[29:35,"prodtarg.sum"]),cumsum(DataEx[36:42,"prodtarg.sum"]),cumsum(DataEx[43:49,"prodtarg.sum"]),cumsum(DataEx[50:56,"prodtarg.sum"]),cumsum(DataEx[57:63,"prodtarg.sum"]),cumsum(DataEx[64:70,"prodtarg.sum"]),cumsum(DataEx[71:77,"prodtarg.sum"]),cumsum(DataEx[78:84,"prodtarg.sum"]),cumsum(DataEx[85:91,"prodtarg.sum"]),cumsum(DataEx[92:98,"prodtarg.sum"]),cumsum(DataEx[99:105,"prodtarg.sum"]),cumsum(DataEx[106:112,"prodtarg.sum"]),cumsum(DataEx[113:119,"prodtarg.sum"]),cumsum(DataEx[120:126,"prodtarg.sum"]),cumsum(DataEx[127:133,"prodtarg.sum"]),cumsum(DataEx[134:140,"prodtarg.sum"]))

DataEx$Cum_prodtarg2<-c(cumsum(DataEx[1:7,"prodtargper.sum"]),cumsum(DataEx[8:14,"prodtargper.sum"]),cumsum(DataEx[15:21,"prodtargper.sum"]),cumsum(DataEx[22:28,"prodtargper.sum"]),cumsum(DataEx[29:35,"prodtargper.sum"]),cumsum(DataEx[36:42,"prodtargper.sum"]),cumsum(DataEx[43:49,"prodtargper.sum"]),cumsum(DataEx[50:56,"prodtargper.sum"]),cumsum(DataEx[57:63,"prodtargper.sum"]),cumsum(DataEx[64:70,"prodtargper.sum"]),cumsum(DataEx[71:77,"prodtargper.sum"]),cumsum(DataEx[78:84,"prodtargper.sum"]),cumsum(DataEx[85:91,"prodtargper.sum"]),cumsum(DataEx[92:98,"prodtargper.sum"]),cumsum(DataEx[99:105,"prodtargper.sum"]),cumsum(DataEx[106:112,"prodtargper.sum"]),cumsum(DataEx[113:119,"prodtargper.sum"]),cumsum(DataEx[120:126,"prodtargper.sum"]),cumsum(DataEx[127:133,"prodtargper.sum"]),cumsum(DataEx[134:140,"prodtargper.sum"]))

DataEx$Cum_constarg<-c(cumsum(DataEx[1:7,"constarg.sum"]),cumsum(DataEx[8:14,"constarg.sum"]),cumsum(DataEx[15:21,"constarg.sum"]),cumsum(DataEx[22:28,"constarg.sum"]),cumsum(DataEx[29:35,"constarg.sum"]),cumsum(DataEx[36:42,"constarg.sum"]),cumsum(DataEx[43:49,"constarg.sum"]),cumsum(DataEx[50:56,"constarg.sum"]),cumsum(DataEx[57:63,"constarg.sum"]),cumsum(DataEx[64:70,"constarg.sum"]),cumsum(DataEx[71:77,"constarg.sum"]),cumsum(DataEx[78:84,"constarg.sum"]),cumsum(DataEx[85:91,"constarg.sum"]),cumsum(DataEx[92:98,"constarg.sum"]),cumsum(DataEx[99:105,"constarg.sum"]),cumsum(DataEx[106:112,"constarg.sum"]),cumsum(DataEx[113:119,"constarg.sum"]),cumsum(DataEx[120:126,"constarg.sum"]),cumsum(DataEx[127:133,"constarg.sum"]),cumsum(DataEx[134:140,"constarg.sum"]))

DataEx$Cum_constarg2<-c(cumsum(DataEx[1:7,"constargper.sum"]),cumsum(DataEx[8:14,"constargper.sum"]),cumsum(DataEx[15:21,"constargper.sum"]),cumsum(DataEx[22:28,"constargper.sum"]),cumsum(DataEx[29:35,"constargper.sum"]),cumsum(DataEx[36:42,"constargper.sum"]),cumsum(DataEx[43:49,"constargper.sum"]),cumsum(DataEx[50:56,"constargper.sum"]),cumsum(DataEx[57:63,"constargper.sum"]),cumsum(DataEx[64:70,"constargper.sum"]),cumsum(DataEx[71:77,"constargper.sum"]),cumsum(DataEx[78:84,"constargper.sum"]),cumsum(DataEx[85:91,"constargper.sum"]),cumsum(DataEx[92:98,"constargper.sum"]),cumsum(DataEx[99:105,"constargper.sum"]),cumsum(DataEx[106:112,"constargper.sum"]),cumsum(DataEx[113:119,"constargper.sum"]),cumsum(DataEx[120:126,"constargper.sum"]),cumsum(DataEx[127:133,"constargper.sum"]),cumsum(DataEx[134:140,"constargper.sum"]))

DataEx$Cum_N.tot<-c(cumsum(DataEx[1:7,"N.total"]),cumsum(DataEx[8:14,"N.total"]),cumsum(DataEx[15:21,"N.total"]),cumsum(DataEx[22:28,"N.total"]),cumsum(DataEx[29:35,"N.total"]),cumsum(DataEx[36:42,"N.total"]),cumsum(DataEx[43:49,"N.total"]),cumsum(DataEx[50:56,"N.total"]),cumsum(DataEx[57:63,"N.total"]),cumsum(DataEx[64:70,"N.total"]),cumsum(DataEx[71:77,"N.total"]),cumsum(DataEx[78:84,"N.total"]),cumsum(DataEx[85:91,"N.total"]),cumsum(DataEx[92:98,"N.total"]),cumsum(DataEx[99:105,"N.total"]),cumsum(DataEx[106:112,"N.total"]),cumsum(DataEx[113:119,"N.total"]),cumsum(DataEx[120:126,"N.total"]),cumsum(DataEx[127:133,"N.total"]),cumsum(DataEx[134:140,"N.total"]))

# DataEx$Cum_prod<-c(cumsum(DataEx[1:7,50]),cumsum(DataEx[8:14,50]),cumsum(DataEx[15:21,50]),cumsum(DataEx[22:28,50]),cumsum(DataEx[29:35,50]),cumsum(DataEx[36:42,50]),cumsum(DataEx[43:49,50]),cumsum(DataEx[50:56,50]),cumsum(DataEx[57:63,50]),cumsum(DataEx[64:70,50]),cumsum(DataEx[71:77,50]),cumsum(DataEx[78:84,50]),cumsum(DataEx[85:91,50]),cumsum(DataEx[92:98,50]),cumsum(DataEx[99:105,50]),cumsum(DataEx[106:112,50]),cumsum(DataEx[113:119,50]),cumsum(DataEx[120:126,50]),cumsum(DataEx[127:133,50]),cumsum(DataEx[134:140,50]))
# 
# DataEx$Cum_cons<-c(cumsum(DataEx[1:7,51]),cumsum(DataEx[8:14,51]),cumsum(DataEx[15:21,51]),cumsum(DataEx[22:28,51]),cumsum(DataEx[29:35,51]),cumsum(DataEx[36:42,51]),cumsum(DataEx[43:49,51]),cumsum(DataEx[50:56,51]),cumsum(DataEx[57:63,51]),cumsum(DataEx[64:70,51]),cumsum(DataEx[71:77,51]),cumsum(DataEx[78:84,51]),cumsum(DataEx[85:91,51]),cumsum(DataEx[92:98,51]),cumsum(DataEx[99:105,51]),cumsum(DataEx[106:112,51]),cumsum(DataEx[113:119,51]),cumsum(DataEx[120:126,51]),cumsum(DataEx[127:133,51]),cumsum(DataEx[134:140,51]))

###DataOp
DataOp$Cum_prod<-c(cumsum(DataOp[1:7,"prodtot.sum"]),cumsum(DataOp[8:14,"prodtot.sum"]),cumsum(DataOp[15:21,"prodtot.sum"]),cumsum(DataOp[22:28,"prodtot.sum"]),cumsum(DataOp[29:35,"prodtot.sum"]),cumsum(DataOp[36:42,"prodtot.sum"]),cumsum(DataOp[43:49,"prodtot.sum"]),cumsum(DataOp[50:56,"prodtot.sum"]),cumsum(DataOp[57:63,"prodtot.sum"]),cumsum(DataOp[64:70,"prodtot.sum"]),cumsum(DataOp[71:77,"prodtot.sum"]),cumsum(DataOp[78:84,"prodtot.sum"]),cumsum(DataOp[85:91,"prodtot.sum"]),cumsum(DataOp[92:98,"prodtot.sum"]),cumsum(DataOp[99:105,"prodtot.sum"]),cumsum(DataOp[106:112,"prodtot.sum"]),cumsum(DataOp[113:119,"prodtot.sum"]),cumsum(DataOp[120:126,"prodtot.sum"]),cumsum(DataOp[127:133,"prodtot.sum"]),cumsum(DataOp[134:140,"prodtot.sum"]))

DataOp$Cum_prod2<-c(cumsum(DataOp[1:7,"prodtotper.sum"]),cumsum(DataOp[8:14,"prodtotper.sum"]),cumsum(DataOp[15:21,"prodtotper.sum"]),cumsum(DataOp[22:28,"prodtotper.sum"]),cumsum(DataOp[29:35,"prodtotper.sum"]),cumsum(DataOp[36:42,"prodtotper.sum"]),cumsum(DataOp[43:49,"prodtotper.sum"]),cumsum(DataOp[50:56,"prodtotper.sum"]),cumsum(DataOp[57:63,"prodtotper.sum"]),cumsum(DataOp[64:70,"prodtotper.sum"]),cumsum(DataOp[71:77,"prodtotper.sum"]),cumsum(DataOp[78:84,"prodtotper.sum"]),cumsum(DataOp[85:91,"prodtotper.sum"]),cumsum(DataOp[92:98,"prodtotper.sum"]),cumsum(DataOp[99:105,"prodtotper.sum"]),cumsum(DataOp[106:112,"prodtotper.sum"]),cumsum(DataOp[113:119,"prodtotper.sum"]),cumsum(DataOp[120:126,"prodtotper.sum"]),cumsum(DataOp[127:133,"prodtotper.sum"]),cumsum(DataOp[134:140,"prodtotper.sum"]))

DataOp$Cum_cons<-c(cumsum(DataOp[1:7,"constot.sum"]),cumsum(DataOp[8:14,"constot.sum"]),cumsum(DataOp[15:21,"constot.sum"]),cumsum(DataOp[22:28,"constot.sum"]),cumsum(DataOp[29:35,"constot.sum"]),cumsum(DataOp[36:42,"constot.sum"]),cumsum(DataOp[43:49,"constot.sum"]),cumsum(DataOp[50:56,"constot.sum"]),cumsum(DataOp[57:63,"constot.sum"]),cumsum(DataOp[64:70,"constot.sum"]),cumsum(DataOp[71:77,"constot.sum"]),cumsum(DataOp[78:84,"constot.sum"]),cumsum(DataOp[85:91,"constot.sum"]),cumsum(DataOp[92:98,"constot.sum"]),cumsum(DataOp[99:105,"constot.sum"]),cumsum(DataOp[106:112,"constot.sum"]),cumsum(DataOp[113:119,"constot.sum"]),cumsum(DataOp[120:126,"constot.sum"]),cumsum(DataOp[127:133,"constot.sum"]),cumsum(DataOp[134:140,"constot.sum"]))

DataOp$Cum_cons2<-c(cumsum(DataOp[1:7,"constotper.sum"]),cumsum(DataOp[8:14,"constotper.sum"]),cumsum(DataOp[15:21,"constotper.sum"]),cumsum(DataOp[22:28,"constotper.sum"]),cumsum(DataOp[29:35,"constotper.sum"]),cumsum(DataOp[36:42,"constotper.sum"]),cumsum(DataOp[43:49,"constotper.sum"]),cumsum(DataOp[50:56,"constotper.sum"]),cumsum(DataOp[57:63,"constotper.sum"]),cumsum(DataOp[64:70,"constotper.sum"]),cumsum(DataOp[71:77,"constotper.sum"]),cumsum(DataOp[78:84,"constotper.sum"]),cumsum(DataOp[85:91,"constotper.sum"]),cumsum(DataOp[92:98,"constotper.sum"]),cumsum(DataOp[99:105,"constotper.sum"]),cumsum(DataOp[106:112,"constotper.sum"]),cumsum(DataOp[113:119,"constotper.sum"]),cumsum(DataOp[120:126,"constotper.sum"]),cumsum(DataOp[127:133,"constotper.sum"]),cumsum(DataOp[134:140,"constotper.sum"]))

DataOp$Cum_prodtarg<-c(cumsum(DataOp[1:7,"prodtarg.sum"]),cumsum(DataOp[8:14,"prodtarg.sum"]),cumsum(DataOp[15:21,"prodtarg.sum"]),cumsum(DataOp[22:28,"prodtarg.sum"]),cumsum(DataOp[29:35,"prodtarg.sum"]),cumsum(DataOp[36:42,"prodtarg.sum"]),cumsum(DataOp[43:49,"prodtarg.sum"]),cumsum(DataOp[50:56,"prodtarg.sum"]),cumsum(DataOp[57:63,"prodtarg.sum"]),cumsum(DataOp[64:70,"prodtarg.sum"]),cumsum(DataOp[71:77,"prodtarg.sum"]),cumsum(DataOp[78:84,"prodtarg.sum"]),cumsum(DataOp[85:91,"prodtarg.sum"]),cumsum(DataOp[92:98,"prodtarg.sum"]),cumsum(DataOp[99:105,"prodtarg.sum"]),cumsum(DataOp[106:112,"prodtarg.sum"]),cumsum(DataOp[113:119,"prodtarg.sum"]),cumsum(DataOp[120:126,"prodtarg.sum"]),cumsum(DataOp[127:133,"prodtarg.sum"]),cumsum(DataOp[134:140,"prodtarg.sum"]))

DataOp$Cum_prodtarg2<-c(cumsum(DataOp[1:7,"prodtargper.sum"]),cumsum(DataOp[8:14,"prodtargper.sum"]),cumsum(DataOp[15:21,"prodtargper.sum"]),cumsum(DataOp[22:28,"prodtargper.sum"]),cumsum(DataOp[29:35,"prodtargper.sum"]),cumsum(DataOp[36:42,"prodtargper.sum"]),cumsum(DataOp[43:49,"prodtargper.sum"]),cumsum(DataOp[50:56,"prodtargper.sum"]),cumsum(DataOp[57:63,"prodtargper.sum"]),cumsum(DataOp[64:70,"prodtargper.sum"]),cumsum(DataOp[71:77,"prodtargper.sum"]),cumsum(DataOp[78:84,"prodtargper.sum"]),cumsum(DataOp[85:91,"prodtargper.sum"]),cumsum(DataOp[92:98,"prodtargper.sum"]),cumsum(DataOp[99:105,"prodtargper.sum"]),cumsum(DataOp[106:112,"prodtargper.sum"]),cumsum(DataOp[113:119,"prodtargper.sum"]),cumsum(DataOp[120:126,"prodtargper.sum"]),cumsum(DataOp[127:133,"prodtargper.sum"]),cumsum(DataOp[134:140,"prodtargper.sum"]))

DataOp$Cum_constarg<-c(cumsum(DataOp[1:7,"constarg.sum"]),cumsum(DataOp[8:14,"constarg.sum"]),cumsum(DataOp[15:21,"constarg.sum"]),cumsum(DataOp[22:28,"constarg.sum"]),cumsum(DataOp[29:35,"constarg.sum"]),cumsum(DataOp[36:42,"constarg.sum"]),cumsum(DataOp[43:49,"constarg.sum"]),cumsum(DataOp[50:56,"constarg.sum"]),cumsum(DataOp[57:63,"constarg.sum"]),cumsum(DataOp[64:70,"constarg.sum"]),cumsum(DataOp[71:77,"constarg.sum"]),cumsum(DataOp[78:84,"constarg.sum"]),cumsum(DataOp[85:91,"constarg.sum"]),cumsum(DataOp[92:98,"constarg.sum"]),cumsum(DataOp[99:105,"constarg.sum"]),cumsum(DataOp[106:112,"constarg.sum"]),cumsum(DataOp[113:119,"constarg.sum"]),cumsum(DataOp[120:126,"constarg.sum"]),cumsum(DataOp[127:133,"constarg.sum"]),cumsum(DataOp[134:140,"constarg.sum"]))

DataOp$Cum_constarg2<-c(cumsum(DataOp[1:7,"constargper.sum"]),cumsum(DataOp[8:14,"constargper.sum"]),cumsum(DataOp[15:21,"constargper.sum"]),cumsum(DataOp[22:28,"constargper.sum"]),cumsum(DataOp[29:35,"constargper.sum"]),cumsum(DataOp[36:42,"constargper.sum"]),cumsum(DataOp[43:49,"constargper.sum"]),cumsum(DataOp[50:56,"constargper.sum"]),cumsum(DataOp[57:63,"constargper.sum"]),cumsum(DataOp[64:70,"constargper.sum"]),cumsum(DataOp[71:77,"constargper.sum"]),cumsum(DataOp[78:84,"constargper.sum"]),cumsum(DataOp[85:91,"constargper.sum"]),cumsum(DataOp[92:98,"constargper.sum"]),cumsum(DataOp[99:105,"constargper.sum"]),cumsum(DataOp[106:112,"constargper.sum"]),cumsum(DataOp[113:119,"constargper.sum"]),cumsum(DataOp[120:126,"constargper.sum"]),cumsum(DataOp[127:133,"constargper.sum"]),cumsum(DataOp[134:140,"constargper.sum"]))

DataOp$Cum_N.tot<-c(cumsum(DataOp[1:7,"N.total"]),cumsum(DataOp[8:14,"N.total"]),cumsum(DataOp[15:21,"N.total"]),cumsum(DataOp[22:28,"N.total"]),cumsum(DataOp[29:35,"N.total"]),cumsum(DataOp[36:42,"N.total"]),cumsum(DataOp[43:49,"N.total"]),cumsum(DataOp[50:56,"N.total"]),cumsum(DataOp[57:63,"N.total"]),cumsum(DataOp[64:70,"N.total"]),cumsum(DataOp[71:77,"N.total"]),cumsum(DataOp[78:84,"N.total"]),cumsum(DataOp[85:91,"N.total"]),cumsum(DataOp[92:98,"N.total"]),cumsum(DataOp[99:105,"N.total"]),cumsum(DataOp[106:112,"N.total"]),cumsum(DataOp[113:119,"N.total"]),cumsum(DataOp[120:126,"N.total"]),cumsum(DataOp[127:133,"N.total"]),cumsum(DataOp[134:140,"N.total"]))

#### Dataframes for modelling ####
# Dataframe total productivity
Dataprod <- Databiom[complete.cases(Databiom[c("prodtot")]),]   #271 obs
  #Replacing neg values with 0
  #Dataprod0 <- Dataprod
  #Dataprod0[,c(40:65)] <- replace(Dataprod[,c(40:65)], Dataprod[,c(40:65)] < 0, 0)

# Dataframe total consumption
Datacons <- Databiom[complete.cases(Databiom[c("constot")]),]   #135 obs
Datacons <- Datacons[complete.cases(Datacons[c("N.total")]),]   #105 obs
Datacons <- Datacons[complete.cases(Datacons[c("prodtot")]),]   #104 obs
  #Replacing neg values with 0
#Datacons0 <- Datacons
#Datacons0[,c(40:65)] <- replace(Datacons[,c(40:65)], Datacons[,c(40:65)] < 0, 0)

# Combine again DataEx and DataOp. Getting accumulated values on block level
Databiom2 <- bind_rows(DataEx,DataOp) 

#Dataframe NAPtarget in proportion to NAPtotal
Datatarg <- Dataprod[complete.cases(Dataprod[c("prodtargfrac")]),]   #250 obs
plot(Datatarg$prodtargfrac) 
#identify(Datatarg$prodtargfrac) # 223
Datatarg <- Datatarg[-223,]#Removing the extreme value from WET_P_3_EX_H7 ---> 249 obs
hist(Datatarg$prodtarg)
plot(Datatarg$prodtarg)
#identify(Datatarg$prodtarg) #223 Same reason as the other one. Due to really high target values in H6, and then very low for H7. 
Datatarg <- Datatarg[-223,]
plot(Datatarg$prodtarg~Datatarg$prodtot)
plot(Datatarg$prodtargfrac) #Mostly values in range <-5,5>
hist(Datatarg$prodtargfrac)

#Adding SE to Datamean
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
CumprodSE <- aggregate(Cum_prod~YrMonth+site.id+landuse+treatment,Databiom2,SE)
CumprodSE$Cum_prod<-round(CumprodSE$Cum_prod,digits=2)
colnames(CumprodSE)[5]<-"Cum_prodSE"

CumconsSE <- aggregate(Cum_cons~YrMonth+site.id+landuse+treatment,Databiom2,SE)
CumconsSE$Cum_cons<-round(CumconsSE$Cum_cons,digits=2)
colnames(CumconsSE)[5]<-"Cum_consSE"

Datamean$Cumprod_SE<-CumprodSE$Cum_prodSE
Datamean$Cumcons_SE<-CumconsSE$Cum_consSE
#### Stacked data for dominant species ####
Datastack <- read.csv("Moveable exclosures/BiomassStacked2.csv",header=T)
# Removing Ex2 - separate analysis
Datastack <- Datastack[Datastack$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Datastack <- Datastack[Datastack$harvest!="H0",] #removing H0                #280 obs
Datastack <- droplevels(Datastack)

# Creating factor variables
Datastack$landuse<-as.factor(Datastack$landuse)
Datastack$region<-as.factor(Datastack$region)
Datastack$site.name <- as.factor(Datastack$site.name)
Datastack$block<-as.factor(Datastack$block)
Datastack$treatment<-as.factor(Datastack$treatment)
Datastack$harvest<-as.factor(Datastack$harvest)
Datastack$site.id <- as.factor(Datastack$site.id)
Datastack$block.id.harvest <- as.factor(Datastack$block.id.harvest)

#Renaming total productivity and consumption columns
colnames(Datastack)[colnames(Datastack)=="productivity.total.g.m2.day"] <- "prodtot"
colnames(Datastack)[colnames(Datastack)=="productivity.total.g.m2.dayWEIGHTED"] <- "prodtot.per"

colnames(Datastack)[colnames(Datastack)=="consumption.total.g.m2.day"] <- "constot"
colnames(Datastack)[colnames(Datastack)=="consumption.total.g.m2.dayWEIGHTED"] <- "constot.per"

#Productivity and consumptions per harvest period
Datastack$prodsp.sum <- Datastack$prodsp*Datastack$growth.period
Datastack$conssp.sum <- Datastack$conssp*Datastack$growth.period

Datastack$prodspper.sum <- Datastack$prodsp.per*Datastack$growth.period
Datastack$consspper.sum <- Datastack$conssp.per*Datastack$growth.period

Datastack$prodtot.sum <- Datastack$prodtot*Datastack$growth.period
Datastack$constot.sum <- Datastack$constot*Datastack$growth.period
Datastack$prodtotper.sum <- Datastack$prodtot.per*Datastack$growth.period
Datastack$constotper.sum <- Datastack$constot.per*Datastack$growth.period

colnames(Datastack)[colnames(Datastack)=="sand.per"] <- "sand"

#Renaming levels in region, landuse and treatment columns
levels(Datastack$region)<-c("Dry Region","Intermediate Region","Wet Region")
levels(Datastack$landuse)<-c("pasture","wild")
levels(Datastack$treatment)<-c("exclosed","open")

# Adding new columns NAP-cons
# Datastack$difftarg <- Datastack$prodtarg-Datastack$constarg
# Datastack$difftotal <- Datastack$prodtot-Datastack$constot
# 
# # Adding new column with % NAP consumed 
# Datastack$consper <- Datastack$constot/Datastack$prodtot*100
# Datastack$constargfrac <- Datastack$constarg/Datastack$prodtarg*100

# Getting N total in Datanuts
Datanuts$N.target.biom <- Datanuts$N.target*Datanuts$biomass.target.sp./100
Datanuts$N.other.biom <- Datanuts$N.other*Datanuts$biomass.other.sp./100
Datanuts$N.total <- (Datanuts$N.target.biom+Datanuts$N.other.biom)/Datanuts$biomass.total.g*100

# Adding nitrogen data to Datastack
Datastack <- cbind(Datastack,Datanuts[,"N.target",drop=FALSE])
Datastack <- cbind(Datastack,Datanuts[,"N.other",drop=FALSE])
Datastack <- cbind(Datastack,Datanuts[,"N.total",drop=FALSE])

#Rain per day for each harvest period
Datastack$rain.day <- Datastack$rain.sum/Datastack$growth.period

# Rdate create month column. default was (="%d.%m.%Y")
Rdate<-strptime(as.character(Datastack$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )# East African time #USE
class(Rdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
Datastack$Rdate<-Rdate# Add to the dataframe #
# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
Datastack$YrMonth<-format(as.Date(Rdate), "%Y-%m")

Datastack$month<-Datastack$Rdate$mon+1
Datastack$month <- month.abb[Datastack$month] #Changing to month name abbrevitations
Datastack$month<-as.factor(Datastack$month)

# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
Datastack$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(Datastack$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
Datastack$plot.code <- as.factor(with(Datastack,paste(region,landuse,block,treatment,sep="_")))
levels(Datastack$plot.code) #40 levels

# Dataframe productivity
Stackprod <- Datastack[complete.cases(Datastack[c("prodsp.per")]),]   #253 obs
Stackprod <- Stackprod[Stackprod$treatment!="open",]  #248 obs
# Dataframe consumption
Stackcons <- Datastack[complete.cases(Datastack[c("conssp.per")]),]   #272 obs
Stackcons <- Stackcons[complete.cases(Stackcons[c("N.total")]),]   #210 obs
#Stackcons <- Datacons[complete.cases(Datacons[c("prodsp.per")]),]   #104 obs

####Functions ####
# Loading function for determining marginal and conditional R square of mixed 
# models following Paul C.D. Johnson 2014. Extension of Nakagawa & Schielzeth's 
# R2Glmm to random slopes models, Methods in Ecology and Evolution

#' R-squared and pseudo-rsquared for a list of (generalized) linear (mixed) models
#'
#' This function calls the generic \code{\link{r.squared}} function for each of the
#' models in the list and rbinds the outputs into one data frame
#'
#' @param a list of fitted (generalized) linear (mixed) model objects
#' @return a dataframe with one row per model, and "Class",
#'         "Family", "Marginal", "Conditional" and "AIC" columns
rsquared.glmm <- function(modlist) {
  # Iterate over each model in the list
  do.call(rbind, lapply(modlist, r.squared))
}

#' R-squared and pseudo-rsquared for (generalized) linear (mixed) models
#'
#' This generic function calculates the r squared and pseudo r-squared for
#' a variety of(generalized) linear (mixed) model fits.
#' Currently implemented for \code{\link{lm}}, \code{\link{lmerTest::merMod}},
#' and \code{\link{nlme::lme}} objects.
#' Implementing methods usually call \code{\link{.rsquared.glmm}}
#'
#' @param mdl a fitted (generalized) linear (mixed) model object
#' @return Implementing methods usually return a dataframe with "Class",
#'         "Family", "Marginal", "Conditional", and "AIC" columns
r.squared <- function(mdl){
  UseMethod("r.squared")
}

#' Marginal r-squared for lm objects
#'
#' This method uses r.squared from \code{\link{summary}} as the marginal.
#' Contrary to other \code{\link{r.squared}} methods, 
#' this one doesn't call \code{\link{.rsquared.glmm}}
#'
#' @param mdl an lm object (usually fit using \code{\link{lm}},
#' @return a dataframe with with "Class" = "lm", "Family" = "gaussian",
#'        "Marginal" = unadjusted r-squared, "Conditional" = NA, and "AIC" columns
r.squared.lm <- function(mdl){
  data.frame(Class=class(mdl), Family="gaussian", Link="identity",
             Marginal=summary(mdl)$r.squared,
             Conditional=NA, AIC=AIC(mdl))
}

#' Marginal and conditional r-squared for merMod objects
#'
#' This method extracts the variance for fixed and random effects, residuals,
#' and the fixed effects for the null model (in the case of Poisson family),
#' and calls \code{\link{.rsquared.glmm}}
#'
#' @param mdl an merMod model (usually fit using \code{\link{lme4::lmer}},
#'        \code{\link{lme4::glmer}}, \code{\link{lmerTest::lmer}},
#'        \code{\link{blme::blmer}}, \code{\link{blme::bglmer}}, etc)
r.squared.merMod <- function(mdl){
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(lme4::fixef(mdl) %*% t(mdl@pp$X)))
  # Get variance of random effects by extracting variance components
  # Omit random effects at the observation level, variance is factored in later
  VarRand <- sum(
    sapply(
      VarCorr(mdl)[!sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))],
      function(Sigma) {
        X <- model.matrix(mdl)
        Z <- X[,rownames(Sigma)]
        sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X) } ) )
  # Get the dispersion variance
  VarDisp <- unlist(VarCorr(mdl)[sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))])
  if(is.null(VarDisp)) VarDisp = 0 else VarDisp = VarDisp
  if(inherits(mdl, "lmerMod")){
    # Get residual variance
    VarResid <- attr(lme4::VarCorr(mdl), "sc")^2
    # Get ML model AIC
    mdl.aic <- AIC(update(mdl, REML=F))
    # Model family for lmer is gaussian
    family <- "gaussian"
    # Model link for lmer is identity
    link <- "identity"
  }
  else if(inherits(mdl, "glmerMod")){
    # Get the model summary
    rmdl.summ <- summary(mdl)
    # Get the model's family, link and AIC
    family <- mdl.summ$family
    link <- mdl.summ$link
    mdl.aic <- AIC(mdl)
    # Pseudo-r-squared for poisson also requires the fixed effects of the null model
    if(family=="poisson") {
      # Get random effects names to generate null model
      rand.formula <- reformulate(sapply(findbars(formula(mdl)),
                                         function(x) paste0("(", deparse(x), ")")),
                                  response=".")
      # Generate null model (intercept and random effects only, no fixed effects)
      null.mdl <- update(mdl, rand.formula)
      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.mdl))
    }
  }
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, VarDisp, family = family, link = link,
                 mdl.aic = mdl.aic,
                 mdl.class = class(mdl),
                 null.fixef = null.fixef)
}

#' Marginal and conditional r-squared for lme objects
#'
#' This method extracts the variance for fixed and random effects,
#' as well as residuals, and calls \code{\link{.rsquared.glmm}}
#'
#' @param mdl an lme model (usually fit using \code{\link{nlme::lme}})
r.squared.lme <- function(mdl){
  # Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(mdl$call$fixed)[-2], mdl$data)
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(nlme::fixef(mdl) %*% t(Fmat)))
  # Get variance of random effects by extracting variance components
  VarRand <- sum(suppressWarnings(as.numeric(nlme::VarCorr(mdl)
                                             [rownames(nlme::VarCorr(mdl)) != "Residual",
                                               1])), na.rm=T)
  # Get residual variance
  VarResid <- as.numeric(nlme::VarCorr(mdl)[rownames(nlme::VarCorr(mdl))=="Residual", 1])
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, family = "gaussian", link = "identity",
                 mdl.aic = AIC(update(mdl, method="ML")),
                 mdl.class = class(mdl))
}

#' Marginal and conditional r-squared for glmm given fixed and random variances
#'
#' This function is based on Nakagawa and Schielzeth (2013). It returns the marginal
#' and conditional r-squared, as well as the AIC for each glmm.
#' Users should call the higher-level generic "r.squared", or implement a method for the
#' corresponding class to get varF, varRand and the family from the specific object
#'
#' @param varF Variance of fixed effects
#' @param varRand Variance of random effects
#' @param varResid Residual variance. Only necessary for "gaussian" family
#' @param family family of the glmm (currently works with gaussian, binomial and poisson)
#' @param link model link function. Working links are: gaussian: "identity" (default);
#'        binomial: "logit" (default), "probit"; poisson: "log" (default), "sqrt"
#' @param mdl.aic The model's AIC
#' @param mdl.class The name of the model's class
#' @param null.fixef Numeric vector containing the fixed effects of the null model.
#'        Only necessary for "poisson" family
#' @return A data frame with "Class", "Family", "Marginal", "Conditional", and "AIC" columns
.rsquared.glmm <- function(varF, varRand, varResid = NULL, varDisp = NULL, family, link,
                           mdl.aic, mdl.class, null.fixef = NULL){
  if(family == "gaussian"){
    # Only works with identity link
    if(link != "identity")
      family_link.stop(family, link)
    # Calculate marginal R-squared (fixed effects/total variance)
    Rm <- varF/(varF+varRand+varResid)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varResid)
  }
  else if(family == "binomial"){
    # Get the distribution-specific variance
    if(link == "logit")
      varDist <- (pi^2)/3
    else if(link == "probit")
      varDist <- 1
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else if(family == "poisson"){
    # Get the distribution-specific variance
    if(link == "log")
      varDist <- log(1+1/exp(null.fixef))
    else if(link == "sqrt")
      varDist <- 0.25
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else
    family_link.stop(family, link)
  # Bind R^2s into a matrix and return with AIC values
  data.frame(Class=mdl.class, Family = family, Link = link,
             Marginal=Rm, Conditional=Rc, AIC=mdl.aic)
}

#' stop execution if unable to calculate variance for a given family and link
family_link.stop <- function(family, link){
  stop(paste("Don't know how to calculate variance for",
             family, "family and", link, "link."))
}

### Setting LMM's
ctrl <- lmeControl(opt="optim", msMaxIter=500,msVerbose=TRUE)
varveg <- varIdent(form= ~ 1 | Veg_type)

####|####
#### DATA EXPLORATION ####
# Zuur 2010: 
# 1:outliers (Y&X) 2:heterogeneity (Y) 3:normality (Y) 4:zero's (Y) 5:collinearity(X)
# 6:relations between variables(X&Y) 7:interactions 8:independence(Y)

#### Spread of data ####
hist(Databiom$prodtot) # right-skewed + some values up to 8!
hist(Databiom$constot) # as much neg.values as pos! Mean cons. ~0... 
hist(Databiom$prodtarg) # most around +/-1, one outlier -4
hist(Databiom$constarg) #one value above 3

hist(Databiom$prodtot.per) # Good spread <-5,10>
hist(Databiom$constot.per) # Outlier ~6? 
hist(Databiom$prodtarg.per) # Skewed! Four values below -10!
hist(Databiom$constarg.per) # Most around zero, +-0.5, outliers -1 and ~+2

#### Outliers ####
# Using boxplot to visualize mean and spread of data (lower and upper end of box is 25% and 75% quantile)

par(mfrow = c(1, 1), mar = c(4, 3, 3, 2)) #mfrow several graphs on the same page
dotchart(Databiom$prodtarg,groups=as.factor(Databiom$plot.id)) # Outlier -4.29
dotchart(Databiom$prodtarg,groups=Databiom$landuse,main = "landuse") # Outlier -4.29 #Separating between landuses --> Both outliers in pasture
#To identify the outliers. plot(), then identify(). Click on outliers to define, then esc. 
plot(Databiom$prodtot) 
#identify(Databiom$prodtot) # 25  37 243 247
plot(Databiom$prodtarg) 
#identify(Databiom$prodtarg) # 253 254
plot(Databiom$constot) 
#identify(Databiom$constot) # 25 273
plot(Databiom$constarg) 
#identify(Databiom$constarg) #45 273

# 25 is outlier in both prodtot and constot   #Large biomass! ~109g, due to underneath tree?

plot(Databiom$prodtot.per) 
plot(Databiom$prodtarg.per)
#identify(Databiom$prodtarg.per) #  9 111 145 275
plot(Databiom$constot.per) 
#identify(Databiom$constot) # 25 273
plot(Databiom$constarg.per) 

#Looking at specific rows with the outliers
Databiom[c(25,37,243,247),]  #49, 63, 311, 315
#All >7, exclosed, H1/H1/H7/H7, dry/int/wet/wet, Makao/Seronera/Hand/Hand
Databiom[c(253,254),]
#Both wet_P_3_H7  -4.29 -4.66 #There was a lot more target sp. in the setup than harvest plots
Databiom[c(25,273),]
#Both ex, Dry_p_1 and se_1, H1/H7 4.59 and -2.79
Databiom[c(45),"constarg"]
#Handajega Ex H2  3.35

Databiom[c(9,111,145,275),]

#Plotting difftarget and difftotal
plot(Databiom$difftarget~Databiom$rain.sum)
#identify(Databiom$difftarget~Databiom$rain.sum) #213 253

#### MAP of the study period #### 
# H3 to H7 (May 2017- May 2018) (Including H1 and H2 would give an overlap period, and want to include the large rainfall values from last harvest to show the unusual year)
AvgMAP <- aggregate(rain.sum~site.name+harvest,Databiom,mean)
AvgMAP <- AvgMAP[AvgMAP$harvest!="H1",]
AvgMAP <- AvgMAP[AvgMAP$harvest!="H2",]
AvgMAP1 <- aggregate(rain.sum~site.name,AvgMAP,sum)

####|####
#### GRAPHING ####
# #### Trying to plot rain.sum with date on the x-axis ####
# #plot(as.Date(harvest.date, format ="%m/%d/%Y"),rain.sum)
# #plot(as.factor(harvest.date), rain.sum)
# plot((rain.sum)~as.Date(Rdate), Databiom,na.rm=T)
# #abline(lm((consTotal)~rain.sum, DataEx5))
# #summary(lm((consTotal)~rain.sum, DataEx5))
# plot(Rdate,rain.sum,x_breaks, xlab =month.abb,Databiom)
# ?plot
# #ggplot
# rain <- ggplot(Databiom, aes(x=YrMonth, y=rain.sum, colour="dark blue"))
# rain <- rain + geom_line(aes(y=rain.sum, colour="dark blue",linetype=1,size=1, alpha=.1))
# 
# rain <-rain + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) #NOT working
# 
# dp<-dp+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
# rain <-rain + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(Databiom$YrMonth),max=max(Databiom$YrMonth))) 

#### Average NAP ####
# Average of each site per harvest
Totprod <- aggregate(prodtot~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
Totprod$prodtot<- round(Totprod$prodtot, digits=2)
colnames(Totprod)[6]<-"Productivity"
Totprod$pool<-"total" #Tagging these data with total productivity - combining later

Tarprod<-aggregate(prodtarg~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
Tarprod$prodtarg<-round(Tarprod$prodtarg,digits=2)  
colnames(Tarprod)[6]<-"Productivity"
Tarprod$pool<-"target"

# Average total and target productivity, in one dataframe
Avgprod<-rbind(Totprod,Tarprod)

#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgprod
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TotprodSE <- aggregate(prodtot~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TotprodSE$prodtot<-round(TotprodSE$prodtot,digits=2)
colnames(TotprodSE)[6]<-"SE"
TotprodSE$pool<-"total"

TarprodSE <- aggregate(prodtarg~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TarprodSE$prodtarg<-round(TarprodSE$prodtarg,digits=2)
colnames(TarprodSE)[6]<-"SE"
TarprodSE$pool<-"target"

Seprod<-rbind(TotprodSE,TarprodSE)
Avgprod$SE<-Seprod$SE

# Convert to date
Avgprod$YrMonth<-as.Date(paste(Avgprod$YrMonth,"-01",sep=""))#Adding day (first of month)

# Redo code to include differences in pool - linetype
Avgprod$site.id<-as.factor(with(Avgprod, paste(region,landuse,treatment,pool, sep="_")))

#### Average NAP, weighted ####
TotprodW <- aggregate(prodtot.per~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
TotprodW$prodtot.per<- round(TotprodW$prodtot.per, digits=2)
colnames(TotprodW)[6]<-"Productivity"
TotprodW$pool<-"total" #Tagging these data with total productivity - combining later

TarprodW<-aggregate(prodtarg.per~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
TarprodW$prodtarg.per<-round(TarprodW$prodtarg.per,digits=2)  
colnames(TarprodW)[6]<-"Productivity"
TarprodW$pool<-"target"

# Average total and target productivity, in one dataframe
AvgprodW<-rbind(TotprodW,TarprodW)

#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgprod
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TotprodWSE <- aggregate(prodtot.per~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TotprodWSE$prodtot.per<-round(TotprodWSE$prodtot.per,digits=2)
colnames(TotprodWSE)[6]<-"SE"
TotprodWSE$pool<-"total"

TarprodWSE <- aggregate(prodtarg.per~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TarprodWSE$prodtarg.per<-round(TarprodWSE$prodtarg.per,digits=2)
colnames(TarprodWSE)[6]<-"SE"
TarprodWSE$pool<-"target"

SeprodW<-rbind(TotprodWSE,TarprodWSE)
AvgprodW$SE<-SeprodW$SE

# Convert to date
AvgprodW$YrMonth<-as.Date(paste(AvgprodW$YrMonth,"-01",sep=""))#Adding day (first of month)

# Redo code to include differences in pool - linetype
AvgprodW$site.id<-as.factor(with(AvgprodW, paste(region,landuse,treatment,pool, sep="_")))

#### Average CONS ####
# Average of each site per harvest
Totcons <- aggregate(constot~region+landuse+site.id+YrMonth,na.rm=T,Databiom,mean)
Totcons$constot<- round(Totcons$constot, digits=2)
colnames(Totcons)[5]<-"Consumption"
Totcons$pool<-"total" #Tagging these data with total productivity - combining later

Tarcons<-aggregate(constarg~region+landuse+site.id+YrMonth,na.rm=T,Databiom,mean)
Tarcons$constarg<-round(Tarcons$constarg,digits=2)  
colnames(Tarcons)[5]<-"Consumption"
Tarcons$pool<-"target"

# Average total and target CONS, in one dataframe
Avgcons<-rbind(Totcons,Tarcons)


#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgcons
TotconsSE <- aggregate(constot~region+landuse+site.id+YrMonth,Databiom,SE)
TotconsSE$constot<-round(TotconsSE$constot,digits=2)
colnames(TotconsSE)[5]<-"SE"
TotconsSE$pool<-"total"

TarconsSE <- aggregate(constarg~region+landuse+site.id+YrMonth,Databiom,SE)
TarconsSE$constarg<-round(TarconsSE$constarg,digits=2)
colnames(TarconsSE)[5]<-"SE"
TarconsSE$pool<-"target"

#Including SE in the averagecons frame
Secons<-rbind(TotconsSE,TarconsSE)
Avgcons$SE<-Secons$SE

# Convert to date
Avgcons$YrMonth<-as.Date(paste(Avgcons$YrMonth,"-01",sep=""))

# Redo code to include differences in pool - linetype
Avgcons$site.id<-as.factor(with(Avgcons, paste(region,landuse,pool, sep="_")))

#### Average CONS WEIGHTED ####
# Average of each site per harvest
TotconsW <- aggregate(constot.per~region+landuse+site.id+YrMonth,na.rm=T,Databiom,mean)
TotconsW$constot.per<- round(TotconsW$constot.per, digits=2)
colnames(TotconsW)[5]<-"Consumption"
TotconsW$pool<-"total" #Tagging these data with total productivity - combining later

TarconsW<-aggregate(constarg.per~region+landuse+site.id+YrMonth,na.rm=T,Databiom,mean)
TarconsW$constarg.per<-round(TarconsW$constarg.per,digits=2)  
colnames(TarconsW)[5]<-"Consumption"
TarconsW$pool<-"target"

# Average total and target CONS, in one dataframe
AvgconsW<-rbind(TotconsW,TarconsW)

#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgcons
TotconsWSE <- aggregate(constot.per~region+landuse+site.id+YrMonth,Databiom,SE)
TotconsWSE$constot.per<-round(TotconsWSE$constot.per,digits=2)
colnames(TotconsWSE)[5]<-"SE"
TotconsWSE$pool<-"total"

TarconsWSE <- aggregate(constarg.per~region+landuse+site.id+YrMonth,Databiom,SE)
TarconsWSE$constarg.per<-round(TarconsWSE$constarg.per,digits=2)
colnames(TarconsWSE)[5]<-"SE"
TarconsWSE$pool<-"target"

#Including SE in the averagecons frame
SeconsW<-rbind(TotconsWSE,TarconsWSE)
AvgconsW$SE<-SeconsW$SE

# Convert to date
AvgconsW$YrMonth<-as.Date(paste(AvgconsW$YrMonth,"-01",sep=""))

# Redo code to include differences in pool - linetype
AvgconsW$site.id<-as.factor(with(AvgconsW, paste(region,landuse,pool, sep="_")))

#### Aggregating rainfall per region #### 
# Averaging rainfall data and getting SE by region # To be included in the NAP aggregated dataframes per site
#per rainfall region (WET, SE , DRY)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Rainregion<-aggregate(rain.sum~region+YrMonth,Databiom,mean)
RainregionSE<-aggregate(rain.sum~region+YrMonth,Databiom,SE)

RainregionX<-cbind(Rainregion,RainregionSE[,3])
colnames(RainregionX)[4]<-"SE"

# Defining upper and lower limits
RainregionX$SeUp<-RainregionX$rain.sum+RainregionX$SE
RainregionX$SeLo<-RainregionX$rain.sum-RainregionX$SE

# Convert to date
RainregionX$YrMonth<-as.Date(paste(RainregionX$YrMonth,"-01",sep=""))

#### Adding NAP AND rainfall to aggregated dataframes ####
# using left join.
Avgprod<-left_join(Avgprod,RainregionX, by=c("region","YrMonth"),drop=F)
names(Avgprod)
AvgprodW <- left_join(AvgprodW,RainregionX, by=c("region","YrMonth"),drop=F)
names(AvgprodW)

Avgcons<-left_join(Avgcons,RainregionX, by=c("region","YrMonth"),drop=F)
names(Avgcons)
AvgconsW <- left_join(AvgconsW,RainregionX, by=c("region","YrMonth"),drop=F)
names(AvgconsW)

## Set values <0 to zero
#AvgProd3$Productivity[AvgProd3$Productivity<0.01]<-0

# Calculate error bars
#AvgProd3$SeUp<-AvgProd3$Productivity+AvgProd3$SE.x
#AvgProd3$SeLo<-AvgProd3$Productivity-AvgProd3$SE.x
#AvgProd3$SeUp[AvgProd3$SeUp<0.01]<-0
#AvgProd3$SeLo[AvgProd3$SeLo<0.01]<-0


#Stu averaged rainfall per region (aggregate(rain.sum~region+YrMonth,DataEx,mean))
#avgRain <- aggregate(rain.sum~region+YrMonth,na.rm=T,Databiom,mean)
#rain <- ggplot( data = avgRain, aes( x=YrMonth, y=rain.sum,colour="dark blue" )) + geom_line()

####OVERVIEW dataframes ####
#Avgprod - 138 obs, Joined with RainRegionX
#Avgprod2 - 
#Avgprod3 - 
#Avgprod4 - From Avgprod3, without target
#Avgprod5 - From Avgprod4 without Seronera
#Avgprod6 - From Avgprod5, without Seronera and open
#Avgprod6b - From Avgprod4, with Seronera, without open

#Avgcons - 70 obs, 7 var
#Avgcons2 - without Seronera
#Avgcons3 - From Avgcons, joined with RainregionX
#Avgcons4 - From Avgcons3, without target
#Avgcons5 - From Avgcons4, without Seronera

#Avgprodcons  - From Avgprod6,Avgcons5    Without Seronera
#Avgprodcons2 - Avgprod6b,Avgcons4        With Seronera 

#### NAP target+total ####
legend_title<-"land-use"
legend_title2<-"treatment"
nap <- ggplot(Avgprod, aes(x=YrMonth, y=Productivity, colour=landuse,shape=treatment,
                           group=site.id))
nap<-nap+geom_line(size=1.2, alpha=.5, show.legend=F)
nap<-nap+geom_errorbar(aes(ymin=Productivity-SE.y, ymax=Productivity+SE.y),width=.2,lwd=1.1,show.legend=F)
nap<-nap+geom_point(size=4, fill="white", stroke=2)
nap<-nap+facet_wrap(~region+pool,ncol=2,scales='fixed')
nap<-nap+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(Avgprod$YrMonth),max=max(Avgprod$YrMonth))) 
#nap<-nap+scale_fill_manual(values=c("white","white"),show.legend=F)
nap<-nap+scale_colour_manual(legend_title, values=c( "tan3","turquoise3")) #ABS colors
nap<-nap+ xlab("Time (month|year)") + ylab(expression(paste("Net Aboveground Productivity(g ",m^-2," ",day^-1,")")))
nap<-nap+scale_shape_manual(legend_title2,values=c(22,21))
nap<-nap+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
nap
#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPtarg.png",
#  width= 26, height = 18,units ="cm",
# dpi = 600, limitsize = TRUE)

#### NAP with Seronera ####
# Remove target species
Avgprod4 <- Avgprod[Avgprod$pool!="target",]
Avgprod4<-droplevels(Avgprod4)
legend_title<-"land-use"
legend_title2<-"treatment"
nap2<- ggplot(Avgprod4, aes(x=YrMonth, y=Productivity, colour=landuse,shape=treatment,
                            group=site.id))
nap2<-nap2+ geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
nap2<-nap2+geom_line(aes(linetype=landuse),size=1.2, alpha=.5, show.legend=F)
nap2<-nap2+geom_errorbar(aes(ymin=Productivity-SE.x, ymax=Productivity+SE.x),linetype="solid",width=.2,lwd=1.1,show.legend=F)
nap2<-nap2+geom_point(size=4, fill="white", stroke=2)
nap2<-nap2+facet_wrap(~region,ncol=1,scales='fixed', drop=F)
#nap2<-nap2+coord_capped_cart(left='both')
nap2<-nap2+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
nap2<-nap2+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
nap2<-nap2+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)
nap2<-nap2+geom_point(aes(y = rain.sum/70),colour="dark blue",size=.9,alpha=.1)
#nap2<-nap2+scale_fill_manual(values=c("white","white"),show.legend=F)
nap2<-nap2+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
nap2<-nap2+scale_shape_manual(legend_title2,values=c(22,21))
nap2 <- nap2+scale_size_manual(legend_title2,values=1)
nap2<-nap2+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
nap2<-nap2+ xlab("Month|Year") + ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
nap2<-nap2+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#nap2<-nap2+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#nap2 <- nap2+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#nap2<-nap2+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
nap2

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPwithSeronera.png",
#   width= 26, height = 18,units ="cm",
#  dpi = 600, limitsize = TRUE)

#### NAP without Seronera ####
Avgprod5 <- Avgprod4[Avgprod4$region!="Intermediate Region",]
Avgprod5 <- droplevels(Avgprod5)
legend_title<-"land-use"
legend_title2<-"treatment"
nap3<- ggplot(Avgprod5, aes(x=YrMonth, y=Productivity, colour=landuse,shape=treatment,
                            group=site.id))
nap3<-nap3+ geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
nap3<-nap3+geom_line(aes(linetype=landuse),size=1.2, alpha=.5, show.legend=F)
nap3<-nap3+geom_errorbar(aes(ymin=Productivity-SE.x, ymax=Productivity+SE.x),linetype="solid",width=.2,lwd=1.1,show.legend=F)
nap3<-nap3+geom_point(size=4, fill="white", stroke=2)
nap3<-nap3+facet_wrap(~region,ncol=1,scales='fixed', drop=F)
#nap3<-nap3+coord_capped_cart(left='both')
nap3<-nap3+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
nap3<-nap3+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
nap3<-nap3+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)
nap3<-nap3+geom_point(aes(y = rain.sum/70),colour="dark blue",size=.9,alpha=.1)
#nap3<-nap3+scale_fill_manual(values=c("white","white"),show.legend=F)
nap3<-nap3+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
nap3<-nap3+scale_shape_manual(legend_title2,values=c(22,21))
nap3 <- nap3+scale_size_manual(legend_title2,values=1)
nap3<-nap3+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
nap3<-nap3+ xlab("Month|Year") + ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
nap3<-nap3+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#nap3<-nap3+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#nap3 <- nap3+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#nap3<-nap3+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
nap3

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAP.png",
#   width= 26, height = 18,units ="cm",
#   dpi = 600, limitsize = TRUE)
#### CONS plot without Seronera ####
Avgcons2 <- Avgcons[Avgcons$region!="Intermediate Region",]
legend_title<-"land-use"
legend_title2<-"treatment"
cons<- ggplot(Avgcons2, aes(x=YrMonth, y=Consumption, colour=landuse,shape=pool,
                            linetype=pool,group=site.id))
cons<-cons+geom_line(size=1.2, alpha=.5, show.legend=F)
cons<-cons+geom_errorbar(aes(ymin=Consumption-SE, ymax=Consumption+SE),width=.2,lwd=1.1,show.legend=F)
cons<-cons+geom_point(size=4, fill="white", stroke=2)
cons<-cons+facet_wrap(~pool+region,ncol=2,scales='fixed')
cons<-cons+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(Avgprod$YrMonth),max=max(Avgprod$YrMonth))) 
#cons<-cons+scale_fill_manual(values=c("white","white"),show.legend=F)
cons<-cons+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cons<-cons+xlab("Time (month|year)") + ylab(expression(paste("Net Aboveground Productivity(g ",m^-2," ",day^-1,")")))
cons

### CONS plot ####
# Adding avg rainfall to the avgcons dataframe
Avgcons <- left_join(Avgcons,RainregionX, by=c("region","YrMonth"),drop=F)

# Removing target species
Avgcons4 <- Avgcons[Avgcons$pool!="target",]
Avgcons4 <- droplevels(Avgcons4)

legend_title<-"land-use"
cons2<- ggplot(Avgcons4, aes(x=YrMonth, y=Consumption, colour=landuse,
                             group=site.id))
cons2<-cons2+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey") #the dashed zero-line
cons2<-cons2+geom_line(aes(linetype=landuse),size=1.2, alpha=.5, show.legend=F) #2 lines per landuse
cons2<-cons2+geom_errorbar(aes(ymin=Consumption-SE.x, ymax=Consumption+SE.x),width=.2,lwd=1.1,show.legend=F)
cons2<-cons2+geom_point(size=4, fill="white", stroke=2,show.legend=F)
cons2<-cons2+facet_wrap(~region,ncol=1,scales='fixed')
cons2<-cons2+scale_y_continuous(limits=c(-1,3.5),sec.axis = sec_axis(~ . *150, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
cons2<-cons2+geom_line(aes(y = rain.sum/150),colour="dark blue",linetype=1,size=1, alpha=.1) #Why dividing by 150? 
cons2<-cons2+geom_point(aes(y = rain.sum/150),colour="dark blue",fill="white",size=.9,alpha=.1)
cons2<-cons2+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
cons2<-cons2+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cons2<-cons2+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
cons2<-cons2+xlab("Time (month|year)") + ylab(expression(paste("Consumption (g ",m^-2," ",day^-1,")")))
cons2<-cons2+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
cons2<-cons2+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#cons2<-cons2+ annotate(geom="text",x=as.Date("2017-02-28"), y=3.5, label=c("(b)",""),color="black",fontface="bold", size=6)
cons2

#### Avg NAP+CONS in one dataframe ####
Avgcons5 <- Avgcons4[Avgcons4$region!="Intermediate Region",]
Avgprod6 <- Avgprod5[Avgprod5$treatment!="open",]
Avgprod6b <- Avgprod4[Avgprod4$treatment!="open",]
Avgprod6b <- droplevels(Avgprod6b)
#Redoing code for site.id - to remove treatment
Avgprod6b$site.id<-as.factor(with(Avgprod6b, paste(region,landuse,pool, sep="_")))

#Without Seronera
dim(Avgprod6) # 28 12
dim(Avgcons5) #28 11

Avgprodcons<-left_join(Avgprod6,Avgcons5, by=c("region","landuse","YrMonth","pool","rain.sum"),drop=F)

Avgprodcons$col<-Avgprodcons$landuse
levels(Avgprodcons$col)<-c("tan","turquoise3")
#levels(AvgProd5b$col)<-c("#D2B48C","#00C5CD")

Avgprodcons$Biomass_change<-c("Productivity","Consumption")

# Error bars
# Se.x.x = productivity
# Se.x.y = consumption
#SeLo and SeUp= rainfall

#This one is not the same as the AvgProd5b dataframe! Here values of biomass change is in same column and not in each Productivity and Consumption
#Avgprodcons <- gather(Avgprodcons, biomass_change,biomass, Productivity, Consumption, factor_key=TRUE )

#With Seronera
dim(Avgprod6b) #35 12
dim(Avgcons4) #35 11

Avgprodcons2<-left_join(Avgprod6b,Avgcons4, by=c("region","landuse","YrMonth","pool","rain.sum"),drop=F)

Avgprodcons2$col<-Avgprodcons2$landuse
levels(Avgprodcons2$col)<-c("tan","turquoise3")

Avgprodcons2$Biomass_change<-c("Productivity","Consumption","Productivity","Consumption","Productivity")

#### NAP+CONS plot without Seronera ####
legend_title<-"land-use"
napcons<- ggplot(Avgprodcons, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,shape=Biomass_change,
                                  group=site.id.y))
napcons<-napcons+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons<-napcons+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons<-napcons+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons<-napcons+geom_line(linetype=1,size=1.2, alpha=.5, show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons<-napcons+facet_wrap(~landuse+region,ncol=2,scales='fixed')
napcons<-napcons+scale_y_continuous(limits=c(-2,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons<-napcons+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.2)
napcons<-napcons+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
napcons<-napcons+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
napcons<-napcons+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#napcons<-napcons+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
napcons<-napcons+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption (g ",m^-2," ",day^-1,")")))
napcons<-napcons+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
napcons<-napcons+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
napcons<-napcons+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","",""),color="black", size=5)
#napcons<-napcons+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
#                                                                         size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons<-napcons+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                                    size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))
napconsb <-napcons+geom_point(data =Avgprodcons, aes(size=landuse, shape = NA), colour = "grey50")
napconsb<-napconsb+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                             nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napconsb


#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONS.png",
#   width= 26, height = 18,units ="cm",
#   dpi = 600, limitsize = TRUE)

legend_title<-"land-use"
napcons<- ggplot(Avgprodcons, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,shape=Biomass_change,
                                  group=site.id.y))
napcons<-napcons+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons<-napcons+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons<-napcons+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons<-napcons+geom_line(linetype=1,size=1.2, alpha=1, show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons<-napcons+facet_wrap(~landuse+region,ncol=2,scales='fixed')
napcons<-napcons+scale_y_continuous(limits=c(-1.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons<-napcons+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=0.5)
#napcons<-napcons+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
napcons<-napcons+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
napcons<-napcons+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#napcons<-napcons+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
napcons<-napcons+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption (g ",m^-2," ",day^-1,")")))
napcons<-napcons+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
napcons<-napcons+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
napcons<-napcons+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","Dry Region","Wet Region"),color="black", size=5)
napcons<-napcons+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
                                                                                  size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons<-napcons+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                                    size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))

napconsb <-napcons+geom_point(data =Avgprodcons, aes(size=landuse, shape = NA), colour = "grey50")
napconsb<-napconsb+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                             nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napconsb <- napconsb+theme(panel.spacing.x=unit(2, "lines"),panel.spacing.y=unit(1, "lines"))
napconsb

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONS2.png",
#  width= 26, height = 18,units ="cm",
# dpi = 600, limitsize = TRUE)

#### NAP+CONS plot with Seronera ####
#prod6b cons4
#Weighted by cover: 
AvgprodTot <- AvgprodW[AvgprodW$pool!="target",]
AvgconsTot <- AvgconsW[AvgconsW$pool!="target",]
AvgprodconsW <- left_join(AvgprodTot,AvgconsTot,by=c("region","landuse","YrMonth","pool","rain.sum"),drop=F)
AvgprodconsW$Biomass_change<-c("Productivity","Consumption")

# Plot without intermediate and then add intermediate later
Avgprodcons2I<-droplevels(Avgprodcons2[Avgprodcons2$region=="Intermediate Region",])
Avgprodcons2excI<-droplevels(Avgprodcons2[Avgprodcons2$region!="Intermediate Region",])

Avgprodcons2$landuse <-factor (Avgprodcons2$landuse,levels=c("pasture","wild"))
Avgprodcons2$region <-factor (Avgprodcons2$region,levels=c("Dry Region","Wet Region","Intermediate Region"))


legend_title<-"land-use"
napcons2<- ggplot(AvgprodconsW, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,
                                    #shape=Biomass_change,
                                    group=site.id.y))
napcons2<-napcons2+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons2<-napcons2+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons2<-napcons2+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons2<-napcons2+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons2<-napcons2+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons2<-napcons2+geom_line(linetype=1,size=1.2, alpha=1, show.legend=F)
napcons2<-napcons2+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons2<-napcons2+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons2<-napcons2+facet_wrap(~landuse+region,ncol=2,scales='fixed')
napcons2<-napcons2+scale_y_continuous(limits=c(-1.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons2<-napcons2+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=0.5)
#napcons2<-napcons2+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
napcons2<-napcons2+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
napcons2<-napcons2+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#napcons2<-napcons2+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
napcons2<-napcons2+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption (g ",m^-2," ",day^-1,")")))
napcons2<-napcons2+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
napcons2<-napcons2+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
napcons2<-napcons2+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","Dry Region","Wet Region", "Intermediate Region"),color="black", size=5)
napcons2<-napcons2+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
                                                                                    size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons2<-napcons2+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                                      size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))

napcons2b <-napcons2+geom_point(data =Avgprodcons2, aes(size=landuse, shape = NA), colour = "grey50")
napcons2b<-napcons2b+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                               nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napcons2b <- napcons2b+theme(panel.spacing.x=unit(2, "lines"),panel.spacing.y=unit(1, "lines"))
napcons2b
#could also use the lemon package with facet_rep_wrap(), but might need to reinstall R for this to work 

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONSSeronera2BEST.png",
#  width= 26, height = 18,units ="cm",
# dpi = 600, limitsize = TRUE)

#### Target NAP + CONS #### 
AvgprodTarg <- AvgprodW[AvgprodW$pool!="total",]
AvgconsTarg <- AvgconsW[AvgconsW$pool!="total",]
AvgprodconsTarg <- left_join(AvgprodTarg,AvgconsTarg,by=c("region","landuse","YrMonth","pool","rain.sum"),drop=F)
AvgprodconsTarg$Biomass_change<-c("Productivity","Consumption")

legend_title<-"land-use"
napcons3<- ggplot(AvgprodconsTarg, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,shape=Biomass_change, group=site.id.y))
napcons3<-napcons3+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons3<-napcons3+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons3<-napcons3+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons3<-napcons3+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons3<-napcons3+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons3<-napcons3+geom_line(linetype=1,size=1.2, alpha=1, show.legend=F)
napcons3<-napcons3+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons3<-napcons3+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons3<-napcons3+facet_wrap(~landuse+region,ncol=2,scales='fixed')
napcons3<-napcons3+scale_y_continuous(limits=c(-1.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons3<-napcons3+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=0.5)
#napcons3<-napcons3+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
napcons3<-napcons3+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
napcons3<-napcons3+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#napcons3<-napcons3+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
napcons3<-napcons3+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption dominant sp. (g ",m^-2," ",day^-1,")")))
napcons3<-napcons3+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
napcons3<-napcons3+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
napcons3<-napcons3+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","Dry Region","Wet Region", "Intermediate Region"),color="black", size=5)
napcons3<-napcons3+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
                                                                                    size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons3<-napcons3+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                                      size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))

napcons3b <-napcons3+geom_point(data =Avgprodcons2, aes(size=landuse, shape = NA), colour = "grey50")
napcons3b<-napcons3b+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                               nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napcons3b <- napcons3b+theme(panel.spacing.x=unit(2, "lines"),panel.spacing.y=unit(1, "lines"))
napcons3b

####|####
#### ANALYSIS ####
#### Precipitation line plot ####
#Producivity
par(mfrow=c(1,1))
plot(Databiom$rain.sum,Databiom$prodtot, ylab="productivity", xlab = "Rainfall", main="Rainfall and productivity",col=Databiom$landuse)
abline(lm((prodtot)~rain.sum, Databiom))
summary(lm(prodtot~rain.sum,data=Databiom)) #lm: y= 0.19+0.0032x, r^2=0.068

#Consumption
plot(Databiom$rain.sum,Databiom$constot, ylab="Consumption", xlab = "Rainfall", main="Rainfall and consumption",col=Databiom$landuse)
abline(lm((constot)~rain.sum, Databiom))
summary(lm((constot)~rain.sum, Databiom)) #lm: y=0.46+0.00081, r^2=0.004 

#### Pearsons correlation ####
#If 100% correlation - points on a scatter plot lie on a straight line
#positive: slope=+1, negative: slope=-1
# From Stu: You can run the function below and use code – this only works with numerical or integers – you need to use boxplots for factors

# RUN THIS CODE FIRST
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

# Then select the variables to use in the pairs function with panel.cor
names(Databiom) #Showing column names

NumericVar <- c("rain.sum","pasture.disc.setup","height. setup", "total.veg.cover.setup", "pasture.disc.harvest","height.harvest") 
pairs(Databiom[,NumericVar],lower.panel = panel.cor)
#Mostly not any strong correlations
#pasture.disc.harvest and height.harvest 0.8 
#pasture.disc.setup and height.setup 0.5
#rain.sum and height.harvest 0.6

#Correlations between categorical variables
# aov (continuous vs categorical)
#otherwise a chisq.test? (categorical vs categorical)
pairs(~region+landuse+treatment+harvest+harvest.date+rain.sum+pasture.disc.harvest+height.harvest+total.veg.cover.harvest,data=Databiom) 
MyVar <- c("landuse", "treatment", "harvest", "rain.sum","region") 

#Test correlations between explanatory variables with boxplots

# Correlation between rainfall and productivity
rainprod <- Databiom %>%
  select("rain.sum", "prodtot")

mycor <- rcorr(rainprod$rain.sum, rainprod$prodtot,type="pearson")
mycor$r
mycor$P

# Different landuse  
rainprod_pasture <- Databiom %>%
  select("rain.sum", "prodtot")%>%
  subset(Databiom$landuse =="pasture")
mycor_pasture <- rcorr(rainprod_pasture$rain.sum, rainprod_pasture$prodtot,type="pearson")
mycor_pasture$r
mycor_pasture$P

rainprod_wild <- Databiom %>%
  select("rain.sum", "prodtot")%>%
  subset(Databiom$landuse =="wild")
mycor_wild <- rcorr(rainprod_wild$rain.sum, rainprod_wild$prodtot,type="pearson")
mycor_wild$r
mycor_wild$P

#Correlation between rainfall and consumption
raincons <- Databiom %>%
  select("rain.sum", "constot")
mycor2 <- rcorr(raincons$rain.sum, raincons$constot,type="pearson")
mycor2$r
mycor2$P

# Different landuse  
raincons_pasture <- Databiom %>%
  select("rain.sum", "constot")%>%
  subset(Databiom$landuse =="pasture")
mycor2_pasture <- rcorr(raincons_pasture$rain.sum, raincons_pasture$constot,type="pearson")
mycor2_pasture$r
mycor2_pasture$P

raincons_wild <- Databiom %>%
  select("rain.sum", "constot")%>%
  subset(Databiom$landuse =="wild")
mycor2_wild <- rcorr(raincons_wild$rain.sum, raincons_wild$constot,type="pearson")
mycor2_wild$r
mycor2_wild$P


#### Exploring variables with lm ####
# NAP and Landuse
napmodlanduse <- lm(prodtot~landuse,data=Databiom)
anova(napmodlanduse) #F is 1.04, non-significant
summary(napmodlanduse) #AdjRsquared: 0.00016
plot(napmodlanduse) #Outliers 49,311,315

par(mfrow=c(1,1))
boxplot(prodtot~landuse,data=Databiom) #no difference
boxplot(prodtot~landuse+region,data=Databiom) #no difference


#Rainfall 
napmodrainfall <- lm(prodtot~rain.sum,data=Databiom)
summary(napmodrainfall) #F:21.39, p<0.05, R-squared: 0.069
plot(prodtot~rain.sum,pch=landuse) #Trying but failing 
abline(napmodrainfall)

#Treatment
napmodtreatment <- lm(prodtot~treatment,data=Databiom)
summary(napmodtreatment) #F:7.985, p:0.005, R-squared: 0.025

#Interactions
napmod <- lm(prodtot~landuse*region*treatment*rain.sum,data=Databiom)
summary(napmod) #rain.sum regionSE and landuseW:regionWET #R-squared: 0.1528
napmod1 <- lm(prodtot~landuse+region+rain.sum+landuse:region, Databiom)
summary(napmod1) #rainsum and regionIntermediate is significant
anova(napmod1) 
par(mfrow=c(2,2))
plot(napmod) #rows 25 37 outliers

#Removing these rows
Databiomtest <- Databiom[-c(25,37),]

#### Example from QA ####
plot(lengt, SM)
abline(model.cond1) # line for arithmetic model
range(lengt)
xx<-seq(35,52,0.1) #defining range of x-axis
yy<-exp(-11.87896)*xx^2.96885 #Equation, to make a curve. intercept * slope
mean(yy)
lines(xx,yy, lty=2) #Plotting a line based on points of two vectors. Here yy vector is based on the exponential function. lty= stipling av linje

#ANOVA
plot(factor(fertil[subset=-c(6,19,29)]), yield[subset=-c(6,19,29)])
hist(residuals(model.2))

plot(model.2)
library(sciplot)
par(mfrow=c(1,2))
lineplot.CI(fertil, yield)
lineplot.CI(factor(fertil[subset=-c(6,19,29)]), yield[subset=-c(6,19,29)], xlab = "Fertilization level", ylab = "Yield (tons)")

####MIXED models with auto-correlation ####
#AUTOCORRELATION - adding as a random component
#a.Extracting residuals from a linear model
#b.Look at residuals in acf - is there a pattern? 
#c.Making autocorrelation matrix
#d.Including autocorr in the mixed model

#Dataframe without the Handajega H7 values
Dataprod1 <- Dataprod[!(Dataprod$site.name=="Handajega" & Dataprod$harvest=="H7"),]
DataprodEx <- Dataprod[Dataprod$treatment!="open",] #135 obs
DataprodEx <- DataprodEx[complete.cases(DataprodEx[c("prodtot.per")]),] # without WET_P_3, DRY_W_3 --> 133 obs
DataprodEx1 <- DataprodEx[!(DataprodEx$site.name=="Handajega" & DataprodEx$harvest=="H7"),]

#### Total NAP periodic lme #### 
# Linear model
napmod <- lm(prodtot~landuse+treatment+poly(rain.sum,2)+sand+
               landuse:poly(rain.sum,2),data=Dataprod)
summary(napmod)
plot(resid(napmod)~Dataprod$landuse,xlab="landuse",ylab="residuals")
plot(resid(napmod)~Dataprod$rain.sum,xlab="rainfall",ylab="residuals")
par(mfrow=c(1,1))

#Plotting residuals against time (YrMonthNumber)
plot(resid(napmod)~Dataprod$YrMonthNumber,xlab="YrMonth",ylab="residuals") #not evenly distributed, so there is a pattern

#a.Extracting residuals from lm
E <- residuals(napmod,type="pearson")
I1 <- !is.na(Dataprod$prodtot)
Efull <- vector(length=length(Dataprod$prodtot))
Efull <- NA
Efull[I1]<- E
Efull

#b.time auto-correlated
acf(Efull, na.action=na.pass,main="Auto-correlation plot for residuals") #again, there is a pattern
xyplot(Efull~YrMonthNumber|site.name, col=1,ylab="Residuals",data=Dataprod)

#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|site.name/block.id/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = Dataprod)
corMatrix(cs1AR1.) #What does this give? 

#LME with temporal auto-correlation (using nlme package)
NAP.lme <- lme(prodtot~landuse+treatment+sand+rain.sum+
                 landuse:treatment+
                 landuse:rain.sum+
                 treatment:rain.sum+
                 landuse:sand+
                 rain.sum:sand+
                 landuse:treatment:rain.sum, 
               random=~1|site.name/block.id, method="REML",correlation=cs1AR1,data=Dataprod)
summary(NAP.lme)#for parameter estimates, don't use the p-values
anova(NAP.lme) #get F statistics and P-values
AIC(NAP.lme) #1185.861

# Checking the temporal autocorrelation
# Extracting residuals from mixed model
E2 <- resid(NAP.lme, type ="n")  # nlme: type = "n" , lme4: type= "pearson"
F2 <- fitted(NAP.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals NAP.lme")
abline(v = 0, lwd = 2, col = 2) 
abline(h = 0, lty = 2, col = 1)

# Time auto-correlated
acf(E2, na.action=na.pass,main="Auto-correlation plot for residuals") # Temproal correlation

#Selecting fixed structure using ML. Simplifying with drop1
#Rain.sum non-transformed
NAPfull1 <- lme(prodtot~treatment+rain.sum,
                #landuse:treatment+
                #landuse:rain.sum+
                #treatment:rain.sum,
                #landuse:sand+
                #rain.sum:sand+
                #landuse:treatment:rain.sum, 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull1,test="Chisq") #dropping if not significant term
AIC(NAPfull1) #1094.439
P1 <- NAPfull1
anova(P1)

#Poly(rain.sum,2) 
# As we expect effect size to level off at certain threshold
NAPfull2 <- lme(prodtot~landuse+treatment+sand+poly(rain.sum,2)+
                  #landuse:treatment+
                  landuse:poly(rain.sum,2)+
                  #treatment:poly(rain.sum,2)+
                  #landuse:sand+
                  sand:poly(rain.sum,2),
                #landuse:treatment:poly(rain.sum,2), 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull2,test="Chisq") #dropping if not significant term
AIC(NAPfull2) #1032.561
P2 <- NAPfull2
anova(P2)

#Poly(rain.day,2)
NAPfull2.2 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
                    #landuse:treatment+
                    #landuse:sand+
                    landuse:poly(rain.day,2),
                  #treatment:poly(rain.day,2),
                  #sand:poly(rain.day,2)+
                  #landuse:treatment:poly(rain.day,2), 
                  random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull2.2,test="Chisq") #dropping if not significant term
AIC(NAPfull2.2) #1026.974
P2.2 <- NAPfull2.2
anova(P2.2)
summary(P2.2) #Parameter estimates

#Model averaging: All possible models from the global P2
  #P2dredge <- dredge(P2,trace=T,rank="AICc",REML=F) #No doubt in keeping all variables from this one. All other have deltaAIC >2

# Updating the model - generating p-values for each term (with ML)
# landuse + treatment + poly(rain.day,2) + land:rain
P2b <- update(P2.2,  .~. -landuse:poly(rain.day,2))
P2c <- update(P2b, .~. -landuse)
P2d <- update(P2b, .~. -treatment)
P2e <- update(P2b, .~. -poly(rain.day,2))

anova(P2,P2b) #landuse:poly(rain.day,2)   p-value: <.0001
anova(P2b,P2c) #landuse             p-value: NS (.32)
anova(P2b,P2d) #treatment           p-value: <.01
anova(P2b,P2e) #rain                p-value <.0001

#Log(rain.sum)
NAPfull3 <- lme(prodtot~landuse+treatment+rain.sum+log(rain.sum)+
                  #landuse:treatment+
                  landuse:rain.sum+
                  landuse:log(rain.sum),
                  #treatment:rain.sum+
                  #treatment:log(rain.sum),
                  #landuse:sand+
                  #sand:rain.sum+
                  #sand:log(rain.sum)+
                #landuse:treatment:rain.sum+
                #landuse:treatment:log(rain.sum), 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull3,test="Chisq") #dropping if not significant term
AIC(NAPfull3) #1075.48
P3 <- NAPfull3
anova(P3)
model.sel(P1,P2,P2.2,P3) #P2.2 seems best fit based on AIC (poly(rain.day,2))

#### Model without Handajega H7
#Poly(rain.day,2)
NAPfull2.3 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
                    #landuse:treatment+
                    #landuse:sand+
                    landuse:poly(rain.day,2),
                  #treatment:poly(rain.day,2),
                  #sand:poly(rain.day,2)+
                  #landuse:treatment:poly(rain.day,2), 
                  random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod1)
drop1(NAPfull2.3,test="Chisq") #dropping if not significant term
AIC(NAPfull2.3) #
P2.3 <- NAPfull2.3
anova(P2.3)
summary(P2.3)
  #P2.3dredge <- dredge(P2.3,trace=T,rank="AICc",REML=F)

# Updating the model - generating p-values for each term (with ML)
# landuse + treatment + poly(rain.day,2) + land:rain
P2b <- update(P2.3,  .~. -landuse:poly(rain.day,2))
P2c <- update(P2b, .~. -landuse)
P2d <- update(P2b, .~. -treatment)
P2e <- update(P2b, .~. -poly(rain.day,2))

anova(P2.3,P2b) #landuse:poly(rain.day,2)   p-value: 0.04
anova(P2b,P2c) #landuse             p-value: NS (.41)
anova(P2b,P2d) #treatment           p-value: <.001
anova(P2b,P2e) #rain                p-value <.0001

#### Total NAP EX ONLy #### 
hist(DataprodEx$prodtot)
hist(DataprodEx$constot)
hist(DataprodEx$prodtot.per)
hist(DataprodEx$constot.per)
# Linear model
plot(DataprodEx$prodtot.per)
napmod <- lm(prodtot~landuse+poly(rain.sum,2)+sand+
               landuse:poly(rain.sum,2),data=DataprodEx)
summary(napmod)
plot(resid(napmod)~DataprodEx$landuse,xlab="landuse",ylab="residuals")
#identify(resid(napmod)~DataprodEx$landuse,xlab="landuse",ylab="residuals") #12 13 17
plot(resid(napmod)~DataprodEx$rain.sum,xlab="rainfall",ylab="residuals")
par(mfrow=c(1,1))

#Plotting residuals against time (YrMonthNumber)
plot(resid(napmod)~DataprodEx$YrMonthNumber,xlab="YrMonth",ylab="residuals") #not evenly distributed, so there is a pattern

#a.Extracting residuals from lm
E <- residuals(napmod,type="pearson")
I1 <- !is.na(DataprodEx$prodtot.per)
Efull <- vector(length=length(DataprodEx$prodtot.per))
Efull <- NA
Efull[I1]<- E
Efull

#b.time auto-correlated
acf(Efull, na.action=na.pass,main="Auto-correlation plot for residuals") #again, there is a pattern
xyplot(Efull~YrMonthNumber|site.name, col=1,ylab="Residuals",data=DataprodEx)

#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|site.name/block.id/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = DataprodEx)
corMatrix(cs1AR1.) #What does this give? 

#LME with temporal auto-correlation (using nlme package)
NAP.lme <- lme(prodtot.per~landuse+sand+rain.sum+
                 landuse:rain.sum+
                 landuse:sand+
                 rain.sum:sand, 
               random=~1|site.name/block.id, method="REML",correlation=cs1AR1,data=DataprodEx)
summary(NAP.lme)#for parameter estimates, don't use the p-values
anova(NAP.lme) #get F statistics and P-values
AIC(NAP.lme) #1185.861

# Checking the temporal autocorrelation
# Extracting residuals from mixed model
E2 <- resid(NAP.lme, type ="n")  # nlme: type = "n" , lme4: type= "pearson"
F2 <- fitted(NAP.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals NAP.lme")
abline(v = 0, lwd = 2, col = 2) 
abline(h = 0, lty = 2, col = 1)

# Time auto-correlated
acf(E2, na.action=na.pass,main="Auto-correlation plot for residuals") # Temproal correlation

#Selecting fixed structure using ML. Simplifying with drop1
#Rain.sum non-transformed ----> Poor fit
NAPfull1 <- lme(prodtot.per~landuse+sand+rain.sum,
                  #landuse:rain.sum+
                  #landuse:sand+
                  #rain.sum:sand,
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=DataprodEx)
drop1(NAPfull1,test="Chisq") #dropping if not significant term
AIC(NAPfull1) #1094.439
P1 <- NAPfull1
anova(P1)

#Poly(rain.sum,2) 
# As we expect effect size to level off at certain threshold
NAPfull2 <- lme(prodtot.per~landuse+sand+poly(rain.sum,2)+
                  landuse:poly(rain.sum,2)+
                  #landuse:sand+
                  poly(rain.sum,2):sand,
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=DataprodEx)
drop1(NAPfull2,test="Chisq") #dropping if not significant term
AIC(NAPfull2) #1032.561
P2 <- NAPfull2
anova(P2)

#Poly(rain.day,2)
NAPfull2.2 <- lme(prodtot.per~landuse+poly(rain.day,2)+
                    landuse:poly(rain.day,2),
                    #landuse:sand+
                    #poly(rain.day,2):sand, 
                  random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=DataprodEx)
drop1(NAPfull2.2,test="Chisq") #dropping if not significant term
AIC(NAPfull2.2) #1026.974
P2.2 <- NAPfull2.2
anova(P2.2)
summary(P2.2) #Parameter estimates

model.sel(P2,P2.2)

#Model averaging: All possible models from the global P2
#P2dredge <- dredge(P2,trace=T,rank="AICc",REML=F) #No doubt in keeping all variables from this one. All other have deltaAIC >2

# Updating the model - generating p-values for each term (with ML)
# landuse + poly(rain.day,2) + land:rain
P2a <- update(P2,  .~. -sand:poly(rain.sum,2))
P2b <- update(P2,  .~. -landuse:poly(rain.sum,2))
P2c <- update(P2b, .~. -landuse)
P2d <- update(P2b, .~. -poly(rain.sum,2))
P2e <- update(P2b, .~. -sand)

anova(P2,P2a) #sand:poly(rain.sum,2)
anova(P2,P2b) #landuse:poly(rain.sum,2)   p-value: 17.44524  0.0037
anova(P2b,P2c) #landuse             p-value: 0.1616744  0.6876
anova(P2b,P2d) #rain                 p-value: 32.5699  <.0001
anova(P2b,P2e) #sand

#Log(rain.sum) --> poor
NAPfull3 <- lme(prodtot.per~landuse+sand+log(rain.sum,2),
                  #landuse:log(rain.sum,2)+
                  #landuse:sand+
                  #log(rain.sum,2):sand, 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=DataprodEx)
drop1(NAPfull3,test="Chisq") #dropping if not significant term
AIC(NAPfull3) #1075.48
P3 <- NAPfull3
anova(P3)
model.sel(P2,P2.2,P3) #P2.2 seems best fit based on AIC (poly(rain.day,2))

#### Model without Handajega H7
plot(DataprodEx$prodtot.per~DataprodEx$harvest) #The H7 Handajega not so problematic when weighted by cover
#Poly(rain.day,2)
NAPfull2.3 <- lme(prodtot.per~landuse+sand+poly(rain.sum,2)+
                    landuse:poly(rain.sum,2)+
                    landuse:sand+
                    poly(rain.sum,2):sand, 
                  random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=DataprodEx1)
drop1(NAPfull2.3,test="Chisq") #dropping if not significant term
AIC(NAPfull2.3) #
P2.3 <- NAPfull2.3
anova(P2.3)
summary(P2.3)
#P2.3dredge <- dredge(P2.3,trace=T,rank="AICc",REML=F)

# Updating the model - generating p-values for each term (with ML)
# landuse + treatment + poly(rain.day,2) + land:rain
P2b <- update(P2.3,  .~. -landuse:poly(rain.day,2))
P2c <- update(P2b, .~. -landuse)
P2d <- update(P2b, .~. -treatment)
P2e <- update(P2b, .~. -poly(rain.day,2))

anova(P2.3,P2b) #landuse:poly(rain.day,2)   p-value: 0.04
anova(P2b,P2c) #landuse             p-value: NS (.41)
anova(P2b,P2d) #treatment           p-value: <.001
anova(P2b,P2e) #rain                p-value <.0001

####Validating models ####
#Step 9 and 10 - Zuur. The aftermath
# Refitting with REML and validating the model
P2.2 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
              landuse:poly(rain.day,2),
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Dataprod)
summary(P2.2)

#Without WET_H7
P2.3 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
              landuse:poly(rain.day,2),
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Dataprod1)
summary(P2.3)

#Graphical model validation checking for homogeneity by plotting standardized residuals vs fitted values
par(mfrow=c(1,1))
E <- resid(P2.2,type="normalized")
Fit <- fitted(P2.2)
#plot(x=Fit,y=E,xlab="Fitted values",ylab="Residuals") 
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = Fit, 
     y = E,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals P2.2")
abline(v = 0, lwd = 2, col = 2) #Fitted values: many below zero, and some large... bad spread? 
abline(h = 0, lty = 2, col = 1) 
#Alternatively: plot(P2cfinal)   Get the same, residuals Vs fitted
plot(E~landuse,data=Dataprod,main="Landuse",ylab="Residuals") #a bit less var. for pasture
plot(E~treatment,data=Dataprod,main="Treatment",ylab="Residuals") #quite equal
plot(x=Dataprod$rain.sum,y=E,ylab="Residuals",xlab="Rainfall",main="Rainfall")
hist(E) #Residuals of the model
hist(Dataprod$prodtot) #hist of Y-variable
plot(P2.2,prodtot~fitted(.)) #Y variable vs fitted values 
plot(P2.2,prodtot~resid(.))  # Y variable vs residuals

par(mfrow=c(2,2))
plot(predict(P2.2)~landuse+treatment+rain.day+
       landuse:rain.day,data=Dataprod)

#Another way of validating the model - line should be straight!
xyplot(E~rain.sum|treatment*landuse,
       data=Dataprod, ylab="Residuals", xlab="Rainfall (periodic)",
       panel=function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

####Validating models EX only####
#Step 9 and 10 - Zuur. The aftermath
# Refitting with REML and validating the model
P2.2 <- lme(prodtot~landuse+poly(rain.day,2)+
              landuse:poly(rain.day,2),
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=DataprodEx)
summary(P2.2)

#Without WET_H7
P2.3 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
              landuse:poly(rain.day,2),
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Dataprod1)
summary(P2.3)

#Graphical model validation checking for homogeneity by plotting standardized residuals vs fitted values
par(mfrow=c(1,1))
E <- resid(P2.2,type="normalized")
Fit <- fitted(P2.2)
#plot(x=Fit,y=E,xlab="Fitted values",ylab="Residuals") 
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = Fit, 
     y = E,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals P2.2")
abline(v = 0, lwd = 2, col = 2) #Fitted values: many below zero, and some large... bad spread? 
abline(h = 0, lty = 2, col = 1) 
#Alternatively: plot(P2cfinal)   Get the same, residuals Vs fitted
plot(E~landuse,data=DataprodEx,main="Landuse",ylab="Residuals") #a bit less var. for pasture
plot(x=DataprodEx$rain.day,y=E,ylab="Residuals",xlab="Rainfall",main="Rainfall")
hist(E) #Residuals of the model
hist(DataprodEx$prodtot) #hist of Y-variable
plot(P2.2,prodtot~fitted(.)) #Y variable vs fitted values 
plot(P2.2,prodtot~resid(.))  # Y variable vs residuals

par(mfrow=c(2,2))
plot(predict(P2.2)~landuse+rain.day+
       landuse:rain.day,data=DataprodEx)

####Validating models WEIGHTED ####
#Step 9 and 10 - Zuur. The aftermath
# Refitting with REML and validating the model
P2.2 <- lme(prodtot.per~landuse+sand+poly(rain.sum,2)+
              landuse:poly(rain.sum,2)+
              sand:poly(rain.sum,2),
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=DataprodEx)
summary(P2.2)

#Without WET_H7
P2.3 <- lme(prodtot.per~landuse+sand+poly(rain.sum,2)+
              sand:poly(rain.sum,2)+
              landuse:poly(rain.sum,2)+
              landuse:sand,
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=DataprodEx1)
summary(P2.3)

#Graphical model validation checking for homogeneity by plotting standardized residuals vs fitted values
par(mfrow=c(1,1))
E <- resid(P2.2,type="normalized")
Fit <- fitted(P2.2)
#plot(x=Fit,y=E,xlab="Fitted values",ylab="Residuals") 
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = Fit, 
     y = E,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals P2.2")
abline(v = 0, lwd = 2, col = 2) #Fitted values: many below zero, and some large... bad spread? 
abline(h = 0, lty = 2, col = 1) 
#Alternatively: plot(P2cfinal)   Get the same, residuals Vs fitted
plot(E~landuse,data=DataprodEx,main="Landuse",ylab="Residuals") #a bit less var. for pasture
plot(E~sand,data=DataprodEx,main="Sand",ylab="Residuals") #quite equal
plot(x=DataprodEx$rain.sum,y=E,ylab="Residuals",xlab="Rainfall",main="Rainfall")
hist(E) #Residuals of the model
hist(DataprodEx$prodtot.per) #hist of Y-variable
plot(P2.2,prodtot.per~fitted(.)) #Y variable vs fitted values 
plot(P2.2,prodtot.per~resid(.))  # Y variable vs residuals

par(mfrow=c(2,2))
plot(predict(P2.2)~landuse+sand+rain.sum+
       landuse:rain.sum,data=DataprodEx)

#Another way of validating the model - line should be straight!
xyplot(E~rain.sum|treatment*landuse,
       data=DataprodEx, ylab="Residuals", xlab="Rainfall (periodic)",
       panel=function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

#### Extracting estimates and F statistics ####
summary(P2.2) #Parameter estimates
anova(P2.2) #F statistics with p-values and df 
r.squared.lme(P2.2) #To get conditional and marginal R^2 for the model

#Exploring some raw data again. 
par(mfrow=c(1,2))
Prod1 <- boxplot(prodtot~landuse,data=DataprodEx, main="Land-use",ylab="Productivity")
Prod2 <- boxplot(prodtot~landuse,data=DataprodEx1, main="Land-use",ylab="Productivity1")

boxplot(prodtot~YrMonth,data=DataprodEx,main="Time|harvest",ylab="Productivity")
plot(prodtot~rain.sum,data=DataprodEx,main="Rainfall",ylab="Productivity")
plot(difftotal~landuse,data=DataprodEx)

#### Sketch fitted values, following Stu's script ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE
str(Dataprod)

#A:Specify covariate values for predictions
MyData <- expand.grid(landuse=levels(Dataprod$landuse),
                      #treatment=levels(Dataprod$treatment),
                      rain.day=seq(min(Dataprod$rain.day), max(Dataprod$rain.day), length = 25)) #Length of rain.sum estimates 25 random numbers between the min and max for every other category (if just landuse in the model, then it would estimate 50 random points  - 25 for pasture/ 25 for wild)
#B. Create X matrix with expand.grid
X <- model.matrix(~landuse+
                    #treatment+
                    poly(rain.day,2)+
                    landuse:poly(rain.day,2),data=MyData)
head(X)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(P2.2)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X %*% vcov(P2.2) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[3]<-"prodtot"

library(tidybayes)
#### Sketch fitted values WEIGHTED ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE
str(DataprodEx)

#A:Specify covariate values for predictions
MyData <- expand.grid(landuse=levels(DataprodEx$landuse), sand=seq(min(DataprodEx$sand),max(DataprodEx$sand),length.out = 25),
                      rain.sum=seq(min(DataprodEx$rain.sum), max(DataprodEx$rain.sum), length = 25)) #Length of rain.sum estimates 25 random numbers between the min and max for every other category (if just landuse in the model, then it would estimate 50 random points  - 25 for pasture/ 25 for wild)
#B. Create X matrix with expand.grid
X <- model.matrix(~landuse+sand+poly(rain.sum,2)+
                    landuse:poly(rain.sum,2)+
                    sand:poly(rain.sum,2),data=MyData)
head(X)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(P2.2)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X %*% vcov(P2.2) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[4]<-"prodtot.per"

library(tidybayes)

#### Plotting observed data versus prediction ####
# Scatter plot with community NAP and rainfall
NAPpred<-ggplot(data=DataprodEx,aes(x=rain.sum, y=prodtot.per)) #observed
NAPpred<-NAPpred+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="springgreen4",colour="springgreen4",alpha=.65,lwd=NA,show.legend=F)
NAPpred<-NAPpred+geom_line(data=MyData,aes(ymin=SeUp, ymax=SeLo),colour="springgreen4",alpha=.9,lwd=2,show.legend=F)
NAPpred<-NAPpred+geom_point(stats="identity",size=2.5) #observed values
  #,aes(colour=region,fill=region)
#NAPpred <- NAPpred+scale_colour_manual(values=c("goldenrod1","dodgerblue1","deepskyblue4"))
NAPpred<-NAPpred+facet_wrap(~landuse, scale="fixed")
NAPpred<-NAPpred+scale_x_continuous(limits=c(0,530), breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), expand=c(0,0))
#NAPpred<-NAPpred+scale_x_continuous(limits=c(0,8), breaks = c(0,2,4,6), labels = c(0,2,4,6), expand=c(0,0))
NAPpred<-NAPpred+scale_y_continuous(limits=c(-4,10), breaks = c(-2,0,2,4,6,8), labels = c(-2,0,2,4,6,8), expand=c(0,0))
NAPpred<-NAPpred+ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))+xlab("Periodic rainfall (mm)") # Adding x and ylabs to plot
NAPpred<-NAPpred+theme_bw()+
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
NAPpred<-NAPpred+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
NAPpred<-NAPpred+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 0.5) 
NAPpred

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPpredicted.png",
 #width= 26, height = 18,units ="cm",
 #dpi = 600, limitsize = TRUE)

#### Total CONS, periodic lme ####
#Dataframe without the Handajega H7 values
Datacons1 <- Datacons[!(Datacons$site.name=="Handajega" & Datacons$harvest=="H7"),]

#Linear model
consmod <- lm(constot~landuse+poly(rain.sum,2)+sand+prodtot+N.total+
                landuse:poly(rain.sum,2),data=Datacons)
summary(consmod)
plot(resid(consmod)~Datacons$landuse,xlab="landuse",ylab="residuals")
plot(resid(consmod)~Datacons$YrMonthNumber,xlab="YrMonth",ylab="residuals") #pattern through time

#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|site.name/block.id/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = Datacons)
corMatrix(cs1AR1.)

#LME with temporal auto-correlation (using nlme package)
CONS.lme <- lme(constot~landuse+prodtot+sand+poly(rain.sum,2)+N.total+
                  landuse:prodtot+
                  landuse:sand+
                  landuse:poly(rain.sum,2)+
                  landuse:N.total+
                  poly(rain.sum,2):sand+
                  poly(rain.sum,2):N.total+
                  prodtot:N.total+
                  sand:N.total+
                  prodtot:sand+
                  landuse:sand:poly(rain.sum,2), 
                random=~1|site.name/block.id, method="REML",correlation=cs1AR1,data=Datacons)
summary(CONS.lme)#don't use the p-values from here
anova(CONS.lme) 
AIC(CONS.lme) #

# Checking the temporal autocorrelation on this model. Extracting residuals.
EC <- resid(CONS.lme, type ="n")  # nlme: type = "n" , lme4: type= "pearson"
FC <- fitted(CONS.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = FC, 
     y = EC,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals CONS.lme")
abline(v = 0, lwd = 2, col = 2) 
abline(h = 0, lty = 2, col = 1)  # Quite equally spread above/below zero

# Time auto-correlated
acf(EC, na.action=na.pass,main="Auto-correlation plot for residuals") # Temproal correlation 

#Selecting fixed structure using ML. Simplifying with drop1
#With poly(rain.sum,2)
CONS.lme <- lme(constot~prodtot+landuse+prodtot+sand+poly(rain.sum,2)+N.total+
                  #landuse:prodtot+
                  landuse:sand+
                  landuse:poly(rain.sum,2)+
                  landuse:N.total+
                  poly(rain.sum,2):sand+
                  poly(rain.sum,2):N.total+
                  prodtot:N.total+
                  sand:N.total,
                  #prodtot:sand+
                  #landuse:sand:poly(rain.sum,2),
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Datacons)
drop1(CONS.lme,test="Chisq") #dropping if not significant term
C1 <- CONS.lme
AIC(C1)

#With poly(rain.day,2)
CONS2 <- lme(constot~prodtot+sand+N.total+poly(rain.day,2)+
                  landuse:prodtot+
                  landuse:sand+
                  landuse:poly(rain.day,2)+
                  landuse:N.total+
                  poly(rain.day,2):sand+
                  poly(rain.day,2):N.total+
                  prodtot:N.total+
                  sand:N.total+
                prodtot:sand+
                landuse:sand:poly(rain.day,2),
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Datacons)
drop1(CONS2,test="Chisq") #dropping if not significant term
C2 <- CONS2
AIC(C2)
# constot~prodtot+sand+N.total+poly(rain.day,2)+
#   landuse:prodtot+
#   landuse:sand+
#   landuse:poly(rain.day,2)+
#   landuse:N.total+
#   poly(rain.day,2):sand+
#   poly(rain.day,2):N.total+
#   prodtot:N.total+
#   sand:N.total+
#   prodtot:sand+
#   landuse:sand:poly(rain.day,2)
# C2dredge <- dredge(C2,trace=T,rank="AICc",REML=F) --> This gave three best models with prod+N.tot+ prod:N.tot. The best model with prod only. Land-use and rain was not significant
C2.1 <- lme(constot~prodtot,
            random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Datacons)
drop1(C2.1,test="Chisq") #dropping if not significant term
AIC(C2.1)

C2.2 <- lme(constot~prodtot+N.total+
               prodtot:N.total,
             random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Datacons)
drop1(C2.2,test="Chisq") #dropping if not significant term
AIC(C2.2)

C2.3 <- lme(constot~prodtot+N.total,
               random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Datacons)
drop1(C2.3,test="Chisq") #dropping if not significant term
AIC(C2.3)

model.sel(C2.1,C2.2,C2.3)

#### DREDGING - THREE VERY SIMILAR MODELS ####
#Anders model
LabileMainMod1 <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
                         Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment-
                         C.N:Temp-C.N:Sand-Temp:Sand+
                         (1|Site/Blockcode/Plot), na.action=na.fail,REML = F,data=LabileMain_NA_filtered)

CONS.lme <- lme(constot~landuse+prodtot+sand+poly(rain.sum,2)+N.total+
                  landuse:prodtot+
                  landuse:sand+
                  landuse:poly(rain.sum,2)+
                  landuse:N.total+
                  poly(rain.sum,2):sand+
                  poly(rain.sum,2):N.total+
                  prodtot:N.total+
                  sand:N.total+
                  prodtot:sand+
                  landuse:sand:poly(rain.sum,2), 
                random=~1|site.name/block.id, method="ML",correlation=cs1AR1,data=Datacons)

modsetlmer_CONSlme <- dredge(CONS.lme,trace=2) 
model.sel(CONS.lme) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_CONSlme<-model.avg(modsetlmer_CONSlme) #Averages coefficient estimates across multiple models according to the weigthing from above
importance(modsetlmer_CONSlme)#Importance of each variable
write.table(importance(modavglmer_CONSlme),file="Moveable exclosures/Importance_CONSlme.txt")
summarymodmodavglmer_CONSlme <- summary(modavglmer_CONSlme)#Estimated coefficients given weighting
write.table(summary(modavglmer_CONSlme)$coefmat.subset,file="Moveable exclosure/SumCoef_CONSlme.txt")

# Refitting with REML and validating the model
C1 <- lme(constot~prodtot,
               random=~1|site.name/block.id,method="REML",na.action=na.pass, correlation=cs1AR1, data=Datacons)
summary(C1)

C2 <- lme(constot~prodtot+N.total+
              prodtot:N.total,
            random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Datacons)
summary(C2) #Nothing is significant

C3 <- lme(constot~prodtot+N.total,
          random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Datacons)
summary(C3) #Only productivity is significant


#Checking for homogeneity by plotting standardized residuals vs fitted values, and resid. against each fixed component
par(mfrow=c(1,1))
EC <- resid(C1,type="normalized")
FC <- fitted(C1)
plot(x=FC,y=EC,xlab="Fitted values",ylab="Residuals") #looks like good spread
plot(C1) #Gives same as above 

plot(EC~prodtot,data=Datacons,main="Productivity",ylab="Residuals")
hist(EC) #Residuals of the model
plot(C1,constot~fitted(.)) #Y variable vs fitted values. Should be straight line? 
plot(C1,constot~resid(.),ylab="Residuals",xlab="Consumption",main="Residuals model C1")  # Observed observations vs model residuals. Should be straight line? 
plot(predict(C1)~prodtot,data=Datacons) #Predicted(fitted) vs fixed effect

#### Sketch fitted values CONS ####
#A:Specify covariate values for predictions
MyData <- expand.grid(prodtot = seq(min(Datacons$prodtot), max(Datacons$prodtot), length = 25),rain.day=seq(min(Datacons$rain.day), max(Datacons$rain.day), length = 25))

#B. Create X2 matrix with expand.grid
X2 <- model.matrix(~ prodtot,rain.day, data = MyData)
head(X2)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X2 %*% fixef(C1)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X2 %*% vcov(C1) %*% t(X2))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[2]<-"constot"

#### CONS obs VS pred #####
# Scatter plot with community CONS and rainfall
CONSpred<-ggplot(data=Datacons,aes(x=rain.day, y=constot)) #observed
#CONSpred<-CONSpred+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)
#CONSpred<-CONSpred+geom_line(data=MyData,aes(ymin=SeUp, ymax=SeLo),colour="red",alpha=.9,lwd=2,show.legend=F)
CONSpred<-CONSpred+geom_point(stats="identity",colour="grey50",fill="grey50",size=2.5) #observed values
CONSpred<-CONSpred+facet_wrap(~landuse, scale="fixed")
CONSpred<-CONSpred+scale_x_continuous(limits=c(0,8), breaks = c(0,2,4,6,8), labels = c(0,2,4,6,8), expand=c(0,0))
CONSpred<-CONSpred+scale_y_continuous(limits=c(-3,5),breaks=c(-3,-2,-1,0,1,2,3,4,5,6), labels =c(-3,-2,-1,0,1,2,3,4,5,6),expand=c(0,0))
CONSpred<-CONSpred+ylab(expression(paste("Consumption (g ",m^-2," ",day^-1,")")))+xlab("Daily rainfall (mm)") # Adding x and ylabs to plot
CONSpred<-CONSpred+theme_bw()+
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
    ,panel.spacing = unit(2,"lines")
    ,strip.background = element_rect(fill="transparent",colour=NA))
CONSpred<-CONSpred+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 0.5) 
CONSpred<-CONSpred+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 0.5) 
CONSpred

#### DOMINANT sp ####
#DFs Stackprod and Stackcons
###NAP periodic ###
#Excluding outliers due to unreliable productivity estimates
Stackprod1 <- Stackprod[-c(4,58,114,201),]

# Linear model
napmod <- lm(prodsp.per~landuse+poly(rain.sum,2)+sand+pool+
               landuse:poly(rain.sum,2)+
                landuse:sand+
               poly(rain.sum,2):pool+
               sand:poly(rain.sum,2),
                data=Stackprod1)
summary(napmod)
par(mfrow=c(1,1))
plot(resid(napmod)~Stackprod1$landuse,xlab="landuse",ylab="residuals")
plot(resid(napmod)~Stackprod1$rain.sum,xlab="rainfall",ylab="residuals")
plot(resid(napmod)~Stackprod1$sand,xlab="sand",ylab="residuals")
plot(resid(napmod)~Stackprod1$pool,xlab="target.other",ylab="residuals")
#identify(resid(napmod)~Stackprod1$pool,xlab="target.other",ylab="residuals") # 4  58 114 201
hist(Stackprod1$prodsp.per)

#Plotting residuals against time (YrMonthNumber)
plot(resid(napmod)~Dataprod$YrMonthNumber,xlab="YrMonth",ylab="residuals") #not evenly distributed, so there is a pattern

#a.Extracting residuals from lm
E <- residuals(napmod,type="pearson")
I1 <- !is.na(Dataprod$prodtot)
Efull <- vector(length=length(Dataprod$prodtot))
Efull <- NA
Efull[I1]<- E
Efull

#b.time auto-correlated
acf(Efull, na.action=na.pass,main="Auto-correlation plot for residuals") #again, there is a pattern
xyplot(Efull~YrMonthNumber|site.name, col=1,ylab="Residuals",data=Dataprod)

#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|site.name/block.id/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = Dataprod)
corMatrix(cs1AR1.) #What does this give? 

#LME with temporal auto-correlation (using nlme package)
NAP.lme <- lme(prodtot~landuse+treatment+sand+rain.sum+
                 landuse:treatment+
                 landuse:rain.sum+
                 treatment:rain.sum+
                 landuse:sand+
                 rain.sum:sand+
                 landuse:treatment:rain.sum, 
               random=~1|site.name/block.id, method="REML",correlation=cs1AR1,data=Dataprod)
summary(NAP.lme)#for parameter estimates, don't use the p-values
anova(NAP.lme) #get F statistics and P-values
AIC(NAP.lme) #1185.861

# Checking the temporal autocorrelation
# Extracting residuals from mixed model
E2 <- resid(NAP.lme, type ="n")  # nlme: type = "n" , lme4: type= "pearson"
F2 <- fitted(NAP.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals NAP.lme")
abline(v = 0, lwd = 2, col = 2) 
abline(h = 0, lty = 2, col = 1)

# Time auto-correlated
acf(E2, na.action=na.pass,main="Auto-correlation plot for residuals") # Temproal correlation

#Selecting fixed structure using ML. Simplifying with drop1
#Rain.sum non-transformed
NAPfull1 <- lme(prodtot~treatment+rain.sum,
                #landuse:treatment+
                #landuse:rain.sum+
                #treatment:rain.sum,
                #landuse:sand+
                #rain.sum:sand+
                #landuse:treatment:rain.sum, 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull1,test="Chisq") #dropping if not significant term
AIC(NAPfull1) #1094.439
P1 <- NAPfull1
anova(P1)



####ANNUAL #### 
# ANCOVA: Total annual production 
#Subset with last period only

Meanannual <- subset(Datamean, YrMonth=="2018-05")
Dataannual <- subset(Databiom2, YrMonth=="2018-05")
Dataannual <- Dataannual[complete.cases(Dataannual[c("Cum_prod")]),] #Removing DRY_P 3 and 4 and DRY_W 1 and 3
m1<-lm(Cum_prod~treatment+landuse+poly(Cum_rain,2)+landuse:poly(Cum_rain,2),
       Meanannual, na.action=na.omit)
summary(m1)
anova(m1)
shapiro.test(resid(m1)) #Normal distribution of residuals. Normally distributed if p>0.05
library(car)
leveneTest(resid(m1)~landuse, data=Meanannual) #Homogenous residuals. p-value >0.05 means equal  variances


## Figure Annual NAP
levels(Meanannual$site)<-c("Makao","Maswa","Seronera","Mwantimba","Handajega")
levels(Meanannual$site.id)<-c("Dry \n Pasture","Dry \n Wild","Intermediate \n Wild","Wet \n Pasture","Wet \n Wild")
levels(Meanannual$landuse)<-c("Pasture","Wild")

### Total primary production
#getting mean obs from grassmean, and ci from standing
cum_prod<-tapply(Meanannual$Cum_prod,list(Meanannual$treatment, Meanannual$site.id),mean) 
cum_prod.ci<-tapply(Dataannual$Cum_prod,list(Dataannual$treatment, Dataannual$site.id),sd) 

#Plot annual NAP
legend_title<-"landuse"
annual <- ggplot(Meanannual, aes(x=site.id, y=Cum_prod, colour=landuse,fill=landuse, group=treatment))
annual <- annual+geom_errorbar(aes(ymin=Cum_prod, ymax=Cum_prod+Cumprod_SE),position=position_dodge(width=.95),width=.2,lwd=1.1,show.legend=F) # ymin=Cum_prod-Cumprod_SE
annual <- annual+geom_col(position=position_dodge(width=.95),size=1.2, alpha=.5, show.legend=F)
annual <- annual +scale_fill_manual(legend_title, values=c( "tan3","turquoise3"))
annual <- annual +scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
annual<-annual+ xlab("Site") + ylab(expression(paste("Cumulative productivity (g ",m^-2,")")))
annual <- annual + theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=12)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
annual

####|####
####VEGETATION COVER ####

##Harvest covers##
Speciesh <- read.csv("Moveable exclosures/Species.cover.harvest.csv", header=T)
# Removing Ex2 and H0
Speciesh <- Speciesh[Speciesh$treatment!="EX2",] #Removing Mesh exclosures
Speciesh <- Speciesh[Speciesh$harvest!="H0",] #removing H0    
Speciesh <- droplevels(Speciesh)

# Creating factor variables
Speciesh$landuse<-as.factor(Speciesh$landuse)
Speciesh$region<-as.factor(Speciesh$region)
Speciesh$site.name <- as.factor(Speciesh$site.name)
Speciesh$block<-as.factor(Speciesh$block)
Speciesh$treatment<-as.factor(Speciesh$treatment)
Speciesh$harvest<-as.factor(Speciesh$harvest)
Speciesh$site.id <- as.factor(Speciesh$site.id)
Speciesh$block.id.harvest <- as.factor(Speciesh$block.id.harvest)

#Renaming several columns
colnames(Speciesh)[colnames(Speciesh)=="total.veg.cover.harvest"] <- "totalcover"
colnames(Speciesh)[colnames(Speciesh)=="pasture.disc.harvest"] <- "pasture.disc"
colnames(Speciesh)[colnames(Speciesh)=="height.harvest"] <- "height"
colnames(Speciesh)[colnames(Speciesh)=="sum.sp.harvest"] <- "sum.sp"

#Getting long format 
Speciesh_long <- gather(Speciesh, sp.abb, cover, 21:108, factor_key=TRUE)
Speciesh_long$setup.harvest <- "harvest"
Speciesh_long$sp.abb <- as.factor(Speciesh_long$sp.abb)
levels(Speciesh_long$sp.abb)
Speciesh_long <- droplevels(Speciesh_long)



#Removing NAs in cover column
Speciesh_long <- na.omit(Speciesh_long, cols="cover")

##SETUP covers ##
SpeciesS <- read.csv("Moveable exclosures/Species.cover.setup.csv", header=T,sep=";")

# Removing Ex2 and H0
SpeciesS <- SpeciesS[SpeciesS$treatment!="EX2",] #Removing Mesh exclosures
SpeciesS <- SpeciesS[SpeciesS$harvest!="H0",] #removing H0    
SpeciesS <- droplevels(SpeciesS)

# Creating factor variables
SpeciesS$landuse<-as.factor(SpeciesS$landuse)
SpeciesS$region<-as.factor(SpeciesS$region)
SpeciesS$site.name <- as.factor(SpeciesS$site.name)
SpeciesS$block<-as.factor(SpeciesS$block)
SpeciesS$treatment<-as.factor(SpeciesS$treatment)
SpeciesS$harvest<-as.factor(SpeciesS$harvest)
SpeciesS$site.id <- as.factor(SpeciesS$site.id)
SpeciesS$block.id.harvest <- as.factor(SpeciesS$block.id.harvest)

#Renaming several columns
colnames(SpeciesS)[colnames(SpeciesS)=="total.veg.cover.setup"] <- "totalcover"
colnames(SpeciesS)[colnames(SpeciesS)=="target.sp.cover.setup"] <- "targetcover"
colnames(SpeciesS)[colnames(SpeciesS)=="pasture.disc.setup"] <- "pasture.disc"
colnames(SpeciesS)[colnames(SpeciesS)=="height.setup"] <- "height"
colnames(SpeciesS)[colnames(SpeciesS)=="sum.sp.setup"] <- "sum.sp"

#Getting long format 
SpeciesS_long <- gather(SpeciesS, sp.abb, sp.cover, 21:137, factor_key=TRUE)

#Need to make all NAs as zeros - to calculate means
SpeciesS_long$observations <- SpeciesS_long$sp.cover #keeping column with NA for #observations
SpeciesS_long$sp.cover[is.na(SpeciesS_long$sp.cover)]<-0
SpeciesS_long$sp.cover[SpeciesS_long$sp.cover<5]<-0

SpeciesS_long$setup.harvest <- "setup"
levels(SpeciesS_long$sp.abb)

#### Aggregated sp.covers per site ####
AvgSpecies <- aggregate(sp.cover~sp.abb+site.name,SpeciesS_long, mean) #580
AvgSpecies$sp.cover <- round(AvgSpecies$sp.cover,digits=0)

#Adding SE to AvgSpecies
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
AvgSpeciesSE <- aggregate(sp.cover~sp.abb+site.name,SpeciesS_long, SE) #580
AvgSpeciesSE$sp.cover <- round(AvgSpeciesSE$sp.cover,digits=2)
colnames(AvgSpeciesSE)[3]<-"SE"

AvgSpecies$SE<-AvgSpeciesSE$SE

#Number of observations per species
AvgSpeciesObs <- aggregate(observations~sp.abb+site.name,SpeciesS_long, length) #151
AvgSpecies <- left_join(AvgSpeciesObs,AvgSpecies)

#Defining upper and lower limits
AvgSpecies$SeUp<-round(AvgSpecies$sp.cover+AvgSpecies$SE,digits=2)
AvgSpecies$SeLo<-round(AvgSpecies$sp.cover-AvgSpecies$SE,digits=2)

#### Average total vegetation cover per site ####
Avgtotal <- aggregate(totalcover~site.name,SpeciesS_long,mean)
AvgtotalSE <- aggregate(totalcover~site.name,SpeciesS_long,SE) #This SE must be bigger? 
AvgtotalSE$totalcover <- round(AvgtotalSE$totalcover,digits=2)
colnames(AvgtotalSE)[2] <- "SE"
Avgtotal$SE <- AvgtotalSE$SE

# Average veg. height per site 
Avgheight <- aggregate(height..setup~site.name,SpeciesS_long,mean)

#######################

#Getting average veg.covers per site per treatment for each harvest
AvgVegcover <- aggregate(total.veg.cover.harvest~region+landuse+site.name+harvest+treatment+setup.harvest,na.rm=T,Speciesh_long,mean)

#### HOW TO - aggregate correctly and adding SE ####
#Demo from previously
Totprod <- aggregate(prodtot~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
Totprod$prodtot<- round(Totprod$prodtot, digits=2)
colnames(Totprod)[6]<-"Productivity"
Totprod$pool<-"total" #Tagging these data with total productivity - combining later

Tarprod<-aggregate(prodtarg~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
Tarprod$prodtarg<-round(Tarprod$prodtarg,digits=2)  
colnames(Tarprod)[6]<-"Productivity"
Tarprod$pool<-"target"

# Average total and target productivity, in one dataframe
Avgprod<-rbind(Totprod,Tarprod)

#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgprod
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TotprodSE <- aggregate(prodtot~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TotprodSE$prodtot<-round(TotprodSE$prodtot,digits=2)
colnames(TotprodSE)[6]<-"SE"
TotprodSE$pool<-"total"

TarprodSE <- aggregate(prodtarg~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TarprodSE$prodtarg<-round(TarprodSE$prodtarg,digits=2)
colnames(TarprodSE)[6]<-"SE"
TarprodSE$pool<-"total"

Seprod<-rbind(TotprodSE,TarprodSE)
Avgprod$SE<-Seprod$SE


####|####
#### USEFUL STUFF ####
#### Vildes soil data ####
#Soil <- Total_soil_data #Total.soil.data.csv

#Getting N values per block
Nblock <- aggregate(Tot.N.per~Region+Block,Soil,mean)
Nsite <- aggregate(Tot.N.per~Region,Soil,mean)

#Getting SOC per block
SOCblock <- aggregate(Org.C.per~Region+Block,Soil,mean)
SOCsite <- aggregate(Org.C.per~Region,Soil,mean)

#### Combined graphs ####
library(grid)
require(gridExtra)
grid.arrange(Nt, Nt2, ncol=2,nrow=1)
####Rounding decimals ####
round(column, digits=2)
####Replacing one value in a variable ####
my.df$V2[my.df$V2 == "-sh2"] <- -100
####From wide to long format ####
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(olddata_wide, condition, measurement, control:cond2, factor_key=TRUE)
#### Citation from R #### 
#citate() 
#### Extracting random effects from a model ####
ranef(model) #using REML
#### Aggregate by several variables at once #### 
aggregate(cbind(var1,var2)~region+block,df,mean) #Anders brukte denne

#A:Specify covariate values for predictions
MyData <- expand.grid(landuse=levels(Dataprod$landuse),treatment=levels(Dataprod$treatment),
                      rain.sum=seq(min(Dataprod$rain.sum), max(Dataprod$rain.sum), length = 25),sand=seq(min(Dataprod$sand),max(Dataprod$sand),length.out = 25)) 

####Biomass stacked - target and other species connected in one column #### 
Stacked <- read.csv("Moveable exclosures/Biomass.Stacked.csv", header=T,sep=",")

Stacked <- Stacked[Stacked$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Stacked <- Stacked[Stacked$harvest!="H0",] #removing H0                #280 obs
Stacked <- droplevels(Stacked)

Stacked$landuse<-as.factor(Stacked$landuse)
Stacked$region<-as.factor(Stacked$region)
Stacked$site.name <- as.factor(Stacked$site.name)
Stacked$block<-as.factor(Stacked$block)
Stacked$treatment<-as.factor(Stacked$treatment)
Stacked$harvest<-as.factor(Stacked$harvest)
Stacked$site.id <- as.factor(Stacked$site.id)
Stacked$block.id.harvest <- as.factor(Stacked$block.id.harvest)

colnames(Stacked)[colnames(Stacked)=="productivity.g.m2.day"] <- "prodsp"
colnames(Stacked)[colnames(Stacked)=="consumption.g.m2.day"] <- "conssp"
colnames(Stacked)[colnames(Stacked)=="productivity.total.g.m2.day"] <- "prodtot"
colnames(Stacked)[colnames(Stacked)=="consumption.total.g.m2.day"] <- "constot"

Datatarget <- subset(Stacked,target.sp.!="other")
Dataother <- subset(Stacked,target.sp.=="other")