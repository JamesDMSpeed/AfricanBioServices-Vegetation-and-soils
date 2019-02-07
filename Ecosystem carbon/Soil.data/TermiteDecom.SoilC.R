#########################################################################################################################
#Soil C and Tea decomposition rates
#Stuart Smith
#4/2/2019 
#########################################################################################################################
#clear system & add package libraries
rm(list=ls())
#########################################################################################################################


# Uploading the soil file
wsdata<- read.csv('Termites/Main & CG experiment/Wetseason.csv', sep=';',dec='.')#Wetseason data
dsdata <- read.csv("Termites/Main & CG experiment/Dryseason.csv", sep=";",dec=".")#Dryseason data
head(wsdata)
head(dsdata)
fulldata<-rbind(wsdata,dsdata)
fulldata$Massloss.per <- (1-fulldata$Ashed.final.corrected.weight..tea.only..g./fulldata$Ashed.initial.corrected.weight..tea.only..g.)*100

total.soil.data<- read.csv("Ecosystem Carbon/Soil.data/Belowground.Carbon.csv", head = TRUE)

# Datasets
names(total.soil.data) # Vilde's soil
names(fulldata) # Anders teabags

# Remove outliers from Tea bag dataset
#R414 and R275
fulldata<-fulldata[fulldata$Teabag.code!="R414" & fulldata$Teabag.code!="R275",]

# Combing shared block names
# Remove regions not shared in dataset
TotSoil<-droplevels(total.soil.data[total.soil.data$Region!="Park Nyigoti" & total.soil.data$Region!="Ikorongo",])

# Rename levels in Vilde's data to match Anders
levels(TotSoil$Region)<-c("Dry","Dry","Wet","Wet","Intermediate")

# Remove treatment plots from common garden
levels(fulldata$Blockcode)

TeaBags<-droplevels(fulldata[fulldata$Experiment!="CG",])
CGTeaBags<-droplevels(fulldata[fulldata$Experiment=="CG",])

LocalCG<-CGTeaBags[CGTeaBags$Blockcode=="Local1" | CGTeaBags$Blockcode=="Local2" |
                     CGTeaBags$Blockcode=="Local3" | CGTeaBags$Blockcode=="Local4",]

# Put Seronera back into dataset - LOCAL ONLY
TeaBags2<-rbind(TeaBags,LocalCG)

# Subset Teabags dataset by roobios
# Subset teabags dataset so Roobios ONly
TeaBags2OP<-droplevels(TeaBags2[TeaBags2$Littertype=="Rooibos" & TeaBags2$Treatment=="Open",])
TeaBags2EX<-droplevels(TeaBags2[TeaBags2$Littertype=="Rooibos" & TeaBags2$Treatment=="Exclosed",])

# Teabag
TeaBags2R<-merge(TeaBags2OP,TeaBags2EX,by=c("Lat","Long","blockcode", "Region","Block","Season"))
TeaBags2R$Massloss.per<-TeaBags2R$Massloss.per.x-TeaBags2R$Massloss.per.y
dim(TeaBags2R)
plot(TeaBags2R$Massloss.per)

# Block code for each dataset
TotSoil$blockcode<- as.factor(with(TotSoil, paste(Region,Block, sep="_")))
TeaBags2$blockcode<- as.factor(with(TeaBags2, paste(Region,Block, sep="_")))
levels(TeaBags2$blockcode)[levels(TeaBags2$blockcode)=="Intermediate_2"]<-"Intermediate_1"
levels(TeaBags2$blockcode)[levels(TeaBags2$blockcode)=="Intermediate_3"]<-"Intermediate_1"
levels(TeaBags2$blockcode)[levels(TeaBags2$blockcode)=="Intermediate_4"]<-"Intermediate_1"
TeaBags2$blockcode[TeaBags2$blockcode=="Intermediate_1"]

# Drop Intermediate 2 - 4 does not make sense - would need to Average Anders values here
TotSoil<- droplevels(TotSoil[TotSoil$blockcode!="NA_NA",])
TotSoil<- droplevels(TotSoil[TotSoil$blockcode!="Intermediate_2" & TotSoil$blockcode!="Intermediate_3" & TotSoil$blockcode!="Intermediate_4",])
#TeaBags2<- droplevels(TeaBags2[TeaBags2$blockcode!="Intermediate_2" & TeaBags2$blockcode!="Intermediate_3" & TeaBags2$blockcode!="Intermediate_4",])

# Merge datasets
TeaC<-merge(TotSoil,TeaBags2, by =c("blockcode","Region","Block"))

# Subset teabags dataset so Roobios ONly
TeaCRooibosOP<-droplevels(TeaC[TeaC$Littertype=="Rooibos" & TeaC$Treatment=="Open",])
TeaCRooibosEX<-droplevels(TeaC[TeaC$Littertype=="Rooibos" & TeaC$Treatment=="Exclosed",])

# Teabag
TeaCRooibos<-merge(TeaCRooibosOP,TeaCRooibosEX,by=c("Lat","Long","blockcode", "Region","Block","AhorC.kg_m2","MinC.kg_m2","Season","rain.sum..mm."))
TeaCRooibos$Massloss.per<-TeaCRooibos$Massloss.per.x-TeaCRooibos$Massloss.per.y

# Turn negative to zero...

DRY2Roo<-TeaCRooibos[TeaCRooibos$blockcode=="Dry_2",]
plot(TeaCRooibos$Massloss.per)
TeaCRooibos$Massloss.per[TeaCRooibos$Massloss.per<0]<-0 # Replace negative values with ZERO
plot(TeaCRooibos$Massloss.per)
dim(TeaCRooibos)
# A horizon: Aggregate soil carbon by blockcode and roobios tea
names(TeaCRooibos) #MinC.kg_m2 or AhorC.kg_m2
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BlockC<-aggregate(AhorC.kg_m2~Region+Block+blockcode+Season,na.action=na.omit,TeaCRooibos,mean)
BlockCSE<-aggregate(AhorC.kg_m2~Region+Block+blockcode+Season,TeaCRooibos,se)

RoobiosMass<-aggregate(Massloss.per~Region+Block+blockcode+Season,na.action=na.omit,TeaCRooibos,mean)
RoobiosMassSE<-aggregate(Massloss.per~Region+Block+blockcode+Season,TeaCRooibos,se)
RainSUM<-aggregate(rain.sum..mm.~Region+Block+blockcode+Season,TeaCRooibos,mean)

BlockC$C.se<-BlockCSE$AhorC.kg_m2
BlockC$Massloss.per<-RoobiosMass$Massloss.per
BlockC$Massloss.se<-RoobiosMassSE$Massloss.per
BlockC$rain.sum..mm.<-RainSUM$rain.sum..mm.

library(ggplot2)
RooC<-ggplot(data=BlockC, aes(x=Massloss.per, y=AhorC.kg_m2,size=rain.sum..mm.))
RooC<-RooC+geom_errorbarh(data=BlockC,aes(xmin = Massloss.per-Massloss.se,xmax = Massloss.per+Massloss.se),colour="grey20",width=.1,lwd=1,show.legend=F) 
RooC<-RooC+geom_errorbar(aes(ymin = AhorC.kg_m2-C.se,ymax = AhorC.kg_m2+C.se),colour="grey20",width=.1,lwd=1,show.legend=F)
RooC<-RooC+geom_point(colour="grey20",fill="grey90")
RooC<-RooC+facet_wrap(~Season, ncol=1)
RooC<-RooC+xlab("Termite only: Rooibos mass loss (%)")+ylab("A-horizon C kg m-2")
RooC<-RooC+theme_classic()
RooC       

# Dry Only
TeaCRooibos2D<-TeaCRooibos[TeaCRooibos$Season=="Dry",]
BlockC<-aggregate(AhorC.kg_m2~Region+Block+blockcode,na.action=na.omit,TeaCRooibos2D,mean)
BlockCSE<-aggregate(AhorC.kg_m2~Region+Block+blockcode,TeaCRooibos2D,se)

RoobiosMass<-aggregate(Massloss.per~Region+Block+blockcode,na.action=na.omit,TeaCRooibos2D,mean)
RoobiosMassSE<-aggregate(Massloss.per~Region+Block+blockcode,TeaCRooibos2D,se)
RainSUM<-aggregate(rain.sum..mm.~Region+Block+blockcode, TeaCRooibos2D,mean)

BlockC$C.se<-BlockCSE$AhorC.kg_m2
BlockC$Massloss.per<-RoobiosMass$Massloss.per
BlockC$Massloss.se<-RoobiosMassSE$Massloss.per
BlockC$rain.sum..mm.<-RainSUM$rain.sum..mm.

plot(BlockC$AhorC.kg_m2~RoobiosMass$Massloss.per,col=c(RoobiosMass$Region),cex=c(RainSUM$rain.sum..mm./100))
abline(lm(BlockC$AhorC.kg_m2~RoobiosMass$Massloss.per))
summary(lm(AhorC.kg_m2~Massloss.per+Massloss.per:rain.sum..mm.,data=BlockC))  
anova(lm(AhorC.kg_m2~Massloss.per,data=BlockC)) #NS

# Subset teabags dataset so Roobios ONly
RooibosT<-droplevels(TeaBags2[TeaBags2$Littertype=="Rooibos" & TeaBags2$Treatment=="Open",])
RooibosO<-droplevels(TeaBags2[TeaBags2$Littertype=="Rooibos" & TeaBags2$Treatment=="Exclosed",])

# Ratio of decomposition
RooibosT$TermMass.per<-abs(RooibosT$Massloss.per-RooibosO$Massloss.per)
RoobiosMass<-aggregate(Massloss.per~Region+Block+blockcode,RooibosT,mean)
plot(BlockC$AhorC.kg_m2~RoobiosMass$Massloss.per,col=c(RoobiosMass$Region),cex=c(RainSUM$rain.sum..mm./100))
abline(lm(BlockC$AhorC.kg_m2~RoobiosMass$Massloss.per))
summary(lm(BlockC$AhorC.kg_m2~RoobiosMass$Massloss.per))  

# A horizon: Aggregate soil carbon by blockcode and roobios tea
names(TeaCRooibos) #MinC.kg_m2 or AhorC.kg_m2
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BlockM<-aggregate(MinC.kg_m2~Region+Block+blockcode+Season,na.action=na.omit,TeaCRooibos,mean)
BlockMSE<-aggregate(MinC.kg_m2~Region+Block+blockcode+Season,TeaCRooibos,se)

RoobiosMass<-aggregate(Massloss.per~Region+Block+blockcode+Season,na.action=na.omit,TeaCRooibos,mean)
RoobiosMassSE<-aggregate(Massloss.per~Region+Block+blockcode+Season,TeaCRooibos,se)
RainSUM<-aggregate(rain.sum..mm.~Region+Block+blockcode+Season,TeaCRooibos,mean)

BlockM$C.se<-BlockMSE$MinC.kg_m2
BlockM$Massloss.per<-RoobiosMass$Massloss.per
BlockM$Massloss.se<-RoobiosMassSE$Massloss.per
BlockM$rain.sum..mm.<-RainSUM$rain.sum..mm.

RooM<-ggplot(data=BlockM, aes(x=Massloss.per, y=MinC.kg_m2,size=rain.sum..mm.))
RooM<-RooM+geom_errorbarh(data=BlockM,aes(xmin = Massloss.per-Massloss.se,xmax = Massloss.per+Massloss.se),colour="grey20",width=.1,lwd=1,show.legend=F) 
RooM<-RooM+geom_errorbar(aes(ymin = MinC.kg_m2-C.se,ymax = MinC.kg_m2+C.se),colour="grey20",width=.1,lwd=1,show.legend=F)
RooM<-RooM+geom_point(colour="grey20",fill="grey90")
RooM<-RooM+facet_wrap(~Season, ncol=1)
RooM<-RooM+xlab("Termite only: Rooibos mass loss (%)")+ylab("Mineral horizon C kg m-2")
RooM<-RooM+theme_classic()
RooM

plot(BlockM$MinC.kg_m2~RoobiosMass$Massloss.per,col=c(RoobiosMass$Region),cex=c(RainSUM$rain.sum..mm./100))
abline(lm(BlockM$MinC.kg_m2~RoobiosMass$Massloss.per))
summary(lm(MinC.kg_m2~Massloss.per+Massloss.per:Season:rain.sum..mm.,data=BlockM))  
anova(lm(MinC.kg_m2~Massloss.per,data=BlockM)) #NS

# Dry season
TeaCRooibos2D<-TeaCRooibos[TeaCRooibos$Season=="Dry",]
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BlockM<-aggregate(MinC.kg_m2~Region+Block+blockcode,na.action=na.omit,TeaCRooibos2D,mean)
BlockMSE<-aggregate(MinC.kg_m2~Region+Block+blockcode,TeaCRooibos2D,se)

RoobiosMass<-aggregate(Massloss.per~Region+Block+blockcode,na.action=na.omit,TeaCRooibos2D,mean)
RoobiosMassSE<-aggregate(Massloss.per~Region+Block+blockcode,TeaCRooibos2D,se)
RainSUM<-aggregate(rain.sum..mm.~Region+Block+blockcode,TeaCRooibos2D,mean)

BlockM$C.se<-BlockMSE$MinC.kg_m2
BlockM$Massloss.per<-RoobiosMass$Massloss.per
BlockM$Massloss.se<-RoobiosMassSE$Massloss.per
BlockM$rain.sum..mm.<-RainSUM$rain.sum..mm.

summary(lm(MinC.kg_m2~Massloss.per+Massloss.per:rain.sum..mm.,data=BlockM))  
anova(lm(MinC.kg_m2~Massloss.per,data=BlockM)) #NS

########################################################################################
# Spatial join - soil C with nearest point of Roobios tea decomposition
library(mapview)
library(sp)
library(rgdal)
library(rgeos)
library(rgdal)

# Parameters
utmproj<-"+proj=utm +south +zone=36 +init=EPSG:21036" #spatial coordinate reference system in Serengeti
latlongproj<-("+proj=longlat +datum=WGS84")
#TotSoil$Latitude <- gsub(" ", "", TotSoil$Latitude)
#TotSoil$Longitude  <- gsub(" ", "", TotSoil$Longitude )

TotSoil$Latitude <- as.numeric(as.character(TotSoil$UTM.North))
TotSoil$Longitude <- as.numeric(as.character(TotSoil$UTM.East))

TotSoil<-TotSoil[!is.na(TotSoil$Longitude),]

VilSpat<- cbind(TotSoil$Longitude,TotSoil$Latitude)# get geographical location
latlongproj<-("+proj=longlat +datum=WGS84")
Vilsp<-SpatialPointsDataFrame(VilSpat,TotSoil, proj4string=CRS(utmproj),match.ID = TRUE)
dim(Vilsp) # 66
VilspWGS84<-spTransform(Vilsp,latlongproj)
extent(VilspWGS84)
TotSoil$Longitude<-VilspWGS84$coords.x1
TotSoil$Latitude<-VilspWGS84$coords.x2

write.csv(TotSoil, "/Users/anotherswsmith/Documents/Teaching/R_workshop/TotSoil.csv",row.names=F) #,sep = ",",dec = ".",col.names = TRUE, row.names=F)


# Projected layer with WGS84
VildeSP_proj <-TotSoil
coordinates(VildeSP_proj)<- ~Longitude + Latitude
proj4string(VildeSP_proj)<-latlongproj

# Anders teabags
names(TeaBags2R) #Roobios only
colnames(TeaBags2R)[1:2]<-c("Latitude","Longitude")
TeaLoc<-cbind(TeaBags2R$Longitude,TeaBags2R$Latitude)# get geographical location
TeaSp<-SpatialPointsDataFrame(TeaLoc,TeaBags2R, proj4string=CRS(latlongproj),match.ID = TRUE)
TeaSpUTM<-spTransform(TeaSp,utmproj)
extent(TeaSpUTM)

# Projected layer with WGS84
TeaBags2R_proj <-TeaBags2R
coordinates(TeaBags2R_proj)<- ~Longitude + Latitude
proj4string(TeaBags2R_proj)<-latlongproj

# Stu data
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Exclosures Serengeti/Biomass excl")

StuExcl<- read.csv("Sero.seasonal.bio_enviroFULL.csv", head = TRUE)
names(StuExcl)
colnames(StuExcl)[11:12]<-c("Latitude","Longitude")

library(ggplot2)
RooC<-ggplot(data=TotSoil, aes(x=Longitude, y=Latitude))
RooC<-RooC+geom_point(size =2)
RooC<-RooC+geom_point(data=TeaBags2R,colour = "red")
RooC<-RooC+geom_point(data=StuExcl,colour = "blue")
RooC<-RooC+theme_bw()
RooC # Distances are off - hence issues binding data

#### Nearest distance ####
d <- gDistance(TeaSpUTM,Vilsp, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
min.d<-as.data.frame(min.d)
dim(min.d) # 4 bags!!! 60 somthing points...
min.d$min.d
newdata <- cbind(Vilsp, TeaSp[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))

Vilsp$nearest_in_set2 <- apply(gDistance(TeaSpUTM,Vilsp, byid=T), 1, which.min)
dim(TeaSp)
Vilsp$min.d<-min.d$min.d
TeaSp$nearest_in_set2<-seq(1:390)
TeaSp$min.d<-seq(1:390)
plot(Vilsp$nearest_in_set2~TeaSp$nearest_in_set2)

Vilsp<-as.data.frame(Vilsp)
TeaSp<-as.data.frame(TeaSp)

# Use nearest points to combine Vilde and Anders
Vilsp2<-left_join(Vilsp,TeaSp, by=c("nearest_in_set2"),drop=F)
Vilsp2<-as.data.frame(Vilsp2)
plot(Vilsp2$AhorC.kg_m2~Vilsp2$Massloss.per)
abline(lm(Vilsp2$AhorC.kg_m2~Vilsp2$Massloss.per))
summary(lm(Vilsp2$AhorC.kg_m2~Vilsp2$Massloss.per))

#### Rasterize TeaBag and then intercept Vilde's points ####
r<-raster(ncols=100, nrows=90,xmn=32.49097, xmx=37.08969, ymn=-4.997215, ymx=7.63e-06, resolution=c(0.025, 0.025))
r1<-rasterize(TeaBags2R_proj, r, field=TeaSp$Massloss.per, fun=mean)#Or fun = mean?
plot(r1)

TeaRast<- extract(r1,VildeSP_proj,sp=T, method='simple')
#TeaRast<- as.data.frame(TeaRast)
names(TeaRast)
TeaRast$layer

#### OLD SCRIPT IGNORE ####
### compute the complete distance matrix between the two sets of points
dist_mat <- pointDistance(Vilsp,TeaSp, lonlat = T, allpairs = F)
dmat <- spDists(Vilsp,TeaSp,longlat = T)
min.dist <- 1
dmat[dmat <= min.dist] <- NA
nearest <- apply(dmat, 1, which.min)

### bind together the data from the dataset B (in your case the "red points")
### at the closest point to dataset A ("black points")
Vilsp@data<- cbind(Vilsp@data, TeaSp@data[nearest,])

Vilsp2<-as.data.frame(Vilsp)
plot(AhorC.kg_m2~Massloss.per,Vilsp2)


Vilsp$nearest_in_set2 <- apply(gDistance(TeaSp,Vilsp, byid=T), 1, which.min)
dim(TeaSp)
TeaSp$nearest_in_set2<-seq(1:390)

Vilsp2<-merge(Vilsp,TeaSp, by=c("nearest_in_set2"),drop=F)


# Using a buffer...
buff <- gBuffer(Vilsp,width=1,byid=TRUE) # Buffer cannot be small - this does not work!
plot(buff)
Vilsp_buff <- TeaSpUTM[buff,]
plot(Vilsp_buff)

mapview(buff) + mapview(TeaSpUTM,color="red") + mapview(Vilsp_buff,color="green")

o<-over(Vilsp_buff,buff)
Vilsp_buff@data<-cbind(Vilsp_buff@data,o)
Vilsp_buff@data

Vilsp2<-as.data.frame(Vilsp_buff)
names(Vilsp2)
Vilsp2$Massloss.per.2 # ALL THE SAME in WGS84!!!

plot(TeaLoc)
plot(VilSpat,add=T)


library(dplyr)
library(sf)
library(RANN)

# fast nearest neighbour search
closest <- nn2(TeaSpUTM,Vilsp, k = 1, searchtype = "radius", radius = 0.0001)
closest <- sapply(closest, cbind) %>% as_tibble
head(closest)
names(closest)
closest$nn.dists #ALL THE SAME!
