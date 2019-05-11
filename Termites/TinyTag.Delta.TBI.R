#########################################################################################################################
#Temperature + Moisture logger + NASA data - TeaBag Index incubation #
#Stuart Smith
#11/5/2019 
#########################################################################################################################
#clear system & package libraries
rm(list=ls())
library(devtools)
library(sp)
library(raster)
library(rgdal)
library(RNetCDF)
library(ncdf4) 
library(lubridate)
#########################################################################################################################

########################################################################
# Temperature and moisture logger for TBI incubation
########################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Moisture.temp.loggers/")

TinyTagTemp<-read.csv("Soil.temp.logger.long.csv")
DeltaMoist<-read.csv("Soil.moist.logger.long.csv")

head(TinyTagTemp)
head(DeltaMoist)
dim(TinyTagTemp) # 117842     15
dim(DeltaMoist) #30560    15
names(TinyTagTemp)
names(DeltaMoist)

#### ANDERS TEABAG INCUBATION TIMES ######
# Import data TBI incubation
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/")
SerEbio<-read.csv(file="ANDERS_MSc_Incubationtime_For_Workshop.csv", sep=",",header=TRUE)
names(SerEbio)

# Creating shared block id
TinyTagTemp$Area_landuse<-as.factor(with(TinyTagTemp, paste(area,landuse, sep="_")))
DeltaMoist$Area_landuse<-as.factor(with(DeltaMoist, paste(area,landuse, sep="_")))
SerEbio$Area_landuse<-as.factor(with(SerEbio, paste(area,landuse, sep="_")))

levels(TinyTagTemp$Area_landuse)<-c("Makao_agriculture","Makao_pasture","MakaoWMA_wild","MaswaGR_wild",      
                                    "Makao_pasture","Mwantimba_agriculture", "Mwantimba_pasture","Ololosokwan_pasture",
                                    "Seronera_wild","SNP_wild" )
levels(DeltaMoist$Area_landuse)<-c("Makao_agriculture","Makao_pasture","MakaoWMA_wild","MaswaGR_wild",      
                                   "Makao_pasture","Mwantimba_agriculture", "Mwantimba_pasture",
                                   "Seronera_wild","SNP_wild" )
levels(SerEbio$Area_landuse)<-c( "SNP_wild","Makao_agriculture","Makao_pasture","MaswaGR_wild","Mwantimba_agriculture",
                                 "Mwantimba_pasture", "Seronera_wild" )

# Remove non shared area and land-uses
TinyTagTemp<-droplevels(TinyTagTemp[!TinyTagTemp$Area_landuse=="MakaoWMA_wild" & #!TinyTagTemp$Area_landuse=="MaswaGR_wild_illegal" &
                           !TinyTagTemp$Area_landuse=="Ololosokwan_pasture",])

DeltaMoist<-droplevels(DeltaMoist[!DeltaMoist$Area_landuse=="MakaoWMA_wild" ,])# & !DeltaMoist$Area_landuse=="MaswaGR_wild_illegal",])

# Loop does not work with Rdate - just works with as.date!
names(SerEbio)
names(TinyTagTemp)
names(DeltaMoist)
SerEbio$date2 = as.Date(SerEbio$date.recovery,"%d/%m/%Y") #as.Date(SerEbio3$date,"%d/%m/%Y")
SerEbio$date3 = as.Date(SerEbio$date.burial,"%d/%m/%Y") #as.Date(SerEbio3$date0,"%d/%m/%Y")
TinyTagTemp$date4 = as.Date(TinyTagTemp$date,"%d.%m.%Y") #as.Date(ex_precip$date,"%d.%m.%Y")
DeltaMoist$date4 = as.Date(DeltaMoist$date,"%d.%m.%Y") #as.Date(ex_precip$date,"%d.%m.%Y")

# Temperature loggers - mean/sd/min/max
# Mean
for(i in 1:nrow(SerEbio)){
  SerEbio$Temp.C_mean[i] <-mean(TinyTagTemp$temperature.C[which(TinyTagTemp$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                                   TinyTagTemp$date4>= SerEbio$date3[i] &
                                                                   TinyTagTemp$date4<= SerEbio$date2[i])]) 
}
# sd
for(i in 1:nrow(SerEbio)){
  SerEbio$Temp.C_sd[i] <-sd(TinyTagTemp$temperature.C[which(TinyTagTemp$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                                  TinyTagTemp$date4>= SerEbio$date3[i] &
                                                                  TinyTagTemp$date4<= SerEbio$date2[i])]) 
}
# min
for(i in 1:nrow(SerEbio)){
  SerEbio$Temp.C_min[i] <-min(TinyTagTemp$temperature.C[which(TinyTagTemp$Area_landuse==SerEbio$Area_landuse[i] & #block_code
                                                              TinyTagTemp$date4>= SerEbio$date3[i] &
                                                              TinyTagTemp$date4<= SerEbio$date2[i])]) 
}

# max
for(i in 1:nrow(SerEbio)){
  SerEbio$Temp.C_max[i] <-max(TinyTagTemp$temperature.C[which(TinyTagTemp$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                                TinyTagTemp$date4>= SerEbio$date3[i] &
                                                                TinyTagTemp$date4<= SerEbio$date2[i])]) 
}

# Moisture loggers - mean/sd/min/max

levels(DeltaMoist$Area_landuse)
levels(SerEbio$Area_landuse)
class(DeltaMoist$Moisture.m3.m3)

summary(is.na(DeltaMoist$Moisture.m3.m3)) # 171 NAS
DeltaMoist<-DeltaMoist[!is.na(DeltaMoist$Moisture.m3.m3),]

# Correct mmoisture for lowest values

# Mean
for(i in 1:nrow(SerEbio)){
  SerEbio$Moist_mean[i] <-mean(DeltaMoist$Moisture.m3.m3[which(DeltaMoist$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                                 DeltaMoist$date4>= SerEbio$date3[i] &
                                                                 DeltaMoist$date4<= SerEbio$date2[i])]) 
}
# sd
for(i in 1:nrow(SerEbio)){
  SerEbio$Moist_sd[i] <-sd(DeltaMoist$Moisture.m3.m3[which(DeltaMoist$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                             DeltaMoist$date4>= SerEbio$date3[i] &
                                                             DeltaMoist$date4<= SerEbio$date2[i])]) 
}


# min
for(i in 1:nrow(SerEbio)){
  SerEbio$Moist_min[i] <-min(DeltaMoist$Moisture.m3.m3[which( DeltaMoist$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                                 DeltaMoist$date4>= SerEbio$date3[i] &
                                                                 DeltaMoist$date4<= SerEbio$date2[i])]) 
}

# max
for(i in 1:nrow(SerEbio)){
  SerEbio$Moist_max[i] <-max(DeltaMoist$Moisture.m3.m3[which( DeltaMoist$Area_landuse== SerEbio$Area_landuse[i] & #block_code
                                                                DeltaMoist$date4>= SerEbio$date3[i] &
                                                                DeltaMoist$date4<= SerEbio$date2[i])]) 
}

# Summarize by block
names(  SerEbio)
SerEbio$sitecode.1
SerEbio2<-SerEbio%>% 
  group_by(Area_landuse,season) %>% 
 filter(any(sitecode.1==1))
dim(SerEbio)

write.csv(SerEbio, "/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/TBI.loggers.csv",row.names=F) #,sep = ",",dec = ".",col.names = TRUE, row.names=F)

# Import summary
TinyD<-read.csv(file="TBI.loggerSummary.csv", sep=",",header=TRUE)
names(TinyD)
ggplot(TinyD, aes(y=Moist.3.m3_meanCorrected,x=landuse, ymax=Moist.3.m3_meanCorrected+Moist.3.m3_sd,
                  ymin=Moist.3.m3_meanCorrected-Moist.3.m3_sd))+
  geom_errorbar(width=.1)+geom_point(size=4)+facet_wrap(~season+rain.region)+theme_classic()

ggplot(TinyD, aes(y=Temp.C_mean,x=landuse,ymax=Temp.C_mean+Temp.C_sd,ymin=Temp.C_mean-Temp.C_sd))+
  geom_errorbar(width=.1)+geom_point(size=4)+facet_wrap(~season+rain.region)+theme_classic()

