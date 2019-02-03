
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
TeaCRooibos<-merge(TeaCRooibosOP,TeaCRooibosEX,by=c("Lat","Long","blockcode", "Region","Block","AhorC.kg_m2","Season","rain.sum..mm."))
TeaCRooibos$Massloss.per<-TeaCRooibos$Massloss.per.x-TeaCRooibos$Massloss.per.y

# Turn negative to zero...

DRY2Roo<-TeaCRooibos[TeaCRooibos$blockcode=="Dry_2",]
plot(TeaCRooibos$Massloss.per)
TeaCRooibos$Massloss.per[TeaCRooibos$Massloss.per<0]<-0 # Replace negative values with ZERO
plot(TeaCRooibos$Massloss.per)

# Aggregate soil carbon by blockcode and roobios tea
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

########################################################################################
# Spatial join - soil C with nearest point of Roobios tea decomposition
# Parameters
utmproj<-"+proj=utm +south +zone=36 +init=EPSG:21036" #spatial coordinate reference system in Serengeti
latlongproj<-("+proj=longlat +datum=WGS84")

wp222loc<- cbind(TotSoil$X,nsreharvest3loc$Y)# get geographical location
Biosp<-SpatialPointsDataFrame(wp222loc,nsreharvest3loc, proj4string=CRS(utmproj),match.ID = TRUE)

Herbloc<-cbind(nsherb3$Xcent,nsherb3$Ycent)# get geographical location
Herbsp<-SpatialPointsDataFrame(Herbloc,nsherb3, proj4string=CRS(utmproj),match.ID = TRUE)

# Nearest number code
nsreharvest3loc$nearest_in_set2 <- apply(gDistance(Herbsp,Biosp, byid=TRUE), 1, which.min)
nsherb3$nearest_in_set2<-seq(1:17)