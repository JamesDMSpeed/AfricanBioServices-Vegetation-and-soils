##################
#example mapping of Serengeti rainfall gradient, admin boundaries and wildebeest trajectories

rm(list=ls(all=TRUE))

#packages - not sure these are all necessary
libs<-c("sp","rgdal",'raster','maptools','geosphere','rgeos','lubridate')
lapply(libs, require, character.only = TRUE)

# Parameters
utmproj<-("+proj=utm +south +zone=36 +init=EPSG:21036") #spatial coordinate reference system in Serengeti
latlongproj<-("+proj=longlat +datum=WGS84")

# Read funciton for making map/plot objects easily transparent 
setwd("/Users/vildehaukenes/Google Drive/Skole/Master biologi /Statistikk/02MasterSessions /serengeti_mapping_stu")
source('MakeTransparent.R')

# Read interpolated rainfall map (Raster)
setwd("/Users/vildehaukenes/Google Drive/Skole/Master biologi /Statistikk/02MasterSessions /serengeti_mapping_stu/RAINFALL_MAP")
rain<-raster("RAIN_INTERPOLATED_UTM36S_Arc1960.tif")

# Read boundary data
setwd("/Users/vildehaukenes/Google Drive/Skole/Master biologi /Statistikk/02MasterSessions /serengeti_mapping_stu/Admin_Boundaries_ABS_UTM36S")
sme<-readOGR(dsn=".", "Admin_Boundaries_ABS_UTM36S")
sme<-spTransform(sme,utmproj) #transform this to the correct projection
sme<-sme[!(sme$name %in% c("Lake Manyara NP","Tarangire NP","Lake Naivasha","Hell's gate","Ruma")),] #remove unneccesary protected areas

# Read wildebeest data and format dates
setwd("/Users/vildehaukenes/Google Drive/Skole/Master biologi /Statistikk/02MasterSessions /serengeti_mapping_stu")
wb<-read.csv("WB_ZB_SNP_COMPILED_2015-11-03_2016-09-22.csv")
wb$Date<-as.POSIXct(wb$Date,format="%Y-%m-%d %H:%M:%S",tz="GMT") # format dates correctly
wb<-wb[order(wb$AID,wb$Date),] #order by individual, then date

# convert wb data to spatial object
wb_proj <-wb
coordinates(wb_proj)<- ~Longitude+Latitude #define which columns correspond to x's and y's
proj4string(wb_proj)<-utmproj #define projection (originally defined above)

# generate trajectories (i.e. polylines) from sequential wb GPS points
wbsub<-wb[!is.na(wb$AID),]
wbsub$AID<-as.character(wbsub$AID)
wbtraj<-as.ltraj(cbind(wbsub$Longitude,wbsub$Latitude),date = wbsub$Date,id = (wbsub$AID))
wbtraj<-ltraj2sldf(wbtraj)
proj4string(wbtraj) <- CRS(utmproj)

### Plot everything
plot(rain)
plot(sme, lwd=2,add=T)
plot(wb_proj,add=T,col=makeTransparent("blue"),pch=16,cex=0.6)
plot(wbtraj,add=T,col=makeTransparent("lightblue"))

