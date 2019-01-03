rm(list=ls())
library(ggplot2)
#Soil texture exploration-Anders MSc

#Comparing vilde and Anders soil texture data####
##Anders soil####
SoiltextA<-read.csv("Termites/Soil data/Soil_texture.csv", sep=';',dec='.')
SoiltextA$Landuse<-replace(SoiltextA$Landuse, SoiltextA$Landuse=="Common Garden", "Wild")
head(SoiltextA)
SoiltextA<- SoiltextA[c(1,3,5,7:9)]
colnames(SoiltextA)<-c("Region","Landuse","Block","Clay.per","Silt.per","Sand.per")
head(SoiltextA)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

Clay<-aggregate(Clay.per~Region+Landuse,SoiltextA,mean)
Clay.se<-aggregate(Clay.per~Region+Landuse,SoiltextA,se)

Silt<-aggregate(Silt.per~Region+Landuse,SoiltextA,mean)
Silt.se<-aggregate(Silt.per~Region+Landuse,SoiltextA,se)

Sand<-aggregate(Sand.per~Region+Landuse,SoiltextA,mean)
Sand.se<-aggregate(Sand.per~Region+Landuse,SoiltextA,se)


SoiltextA.Summary<-cbind(Clay[c(1:3)],Clay.se[3],Silt[3],Silt.se[3],Sand[3],Sand.se[3])

colnames(SoiltextA.Summary)<-c("Region","Landuse","AClay","AClay.se","ASilt","ASilt.se","ASand","ASand.se")
head(SoiltextA.Summary)
SoiltextA.Summary$Who <- SoiltextA.Summary$AWho
SoiltextA.Summary$AWho<-"SUA"
head(SoiltextA.Summary)
SoiltextA.Summary<- SoiltextA.Summary[SoiltextA.Summary$Landuse!="Agriculture",]
SoiltextA.Summary[2,"Landuse"]
SoiltextA.Summary$Landuse <- gsub("Common Garden", "Wild",SoiltextA.Summary$Landuse)

#Vilde soil####
SoiltextV<-read.csv("Ecosystem carbon/Soil.data/Soil.texture.csv", head = TRUE)
head(SoiltextV)
SoiltextV<-SoiltextV[SoiltextV$Horizon=="A-hor",]
SoiltextV<-SoiltextV[c(3,5,8:11)]
colnames(SoiltextV)<-c("Region","Block","Landuse","Clay.per","Silt.per","Sand.per")
head(SoiltextV)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

Clay<-aggregate(Clay.per~Region+Landuse,SoiltextV,mean)
Clay.se<-aggregate(Clay.per~Region+Landuse,SoiltextV,se)

Silt<-aggregate(Silt.per~Region+Landuse,SoiltextV,mean)
Silt.se<-aggregate(Silt.per~Region+Landuse,SoiltextV,se)

Sand<-aggregate(Sand.per~Region+Landuse,SoiltextV,mean)
Sand.se<-aggregate(Sand.per~Region+Landuse,SoiltextV,se)


SoiltextV.Summary<-cbind(Clay[c(1:3)],Clay.se[3],Silt[3],Silt.se[3],Sand[3],Sand.se[3])

colnames(SoiltextV.Summary)<-c("Region","Landuse","VClay","VClay.se","VSilt","VSilt.se","VSand","VSand.se")
head(SoiltextV.Summary)
SoiltextV.Summary$Who<-SoiltextV.Summary$VWho
SoiltextV.Summary$VWho<-"NMBU"
SoiltextV.Summary<-SoiltextV.Summary[SoiltextV.Summary$Region!="Ikorongo",]
SoiltextV.Summary<-SoiltextV.Summary[SoiltextV.Summary$Region!="Park Nyigoti",]

#Plotting the soil texture for comparison####
SoiltextAV<-merge(SoiltextA.Summary,SoiltextV.Summary)

names(SoiltextAV)

Clayplot <- ggplot(SoiltextAV, aes(x=VClay, y=AClay, shape=Landuse,fill=Region,color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_errorbar(aes(ymin = AClay-AClay.se,ymax =  AClay+AClay.se),show.legend=F) + 
  geom_errorbarh(aes(xmin = VClay-VClay.se, xmax =  VClay+VClay.se),show.legend=F) +
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  #scale_fill_manual(values=c("green4","orangered3","white","white"))+
  scale_shape_manual(values=c(21,23,24,22)) + 
  #guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
  scale_x_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Clay (%) NMBU") +  ylab("Clay (%) SUA")
    
Clayplot

ggsave("Termites/Soil data/ClayP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

Siltplot <-ggplot(SoiltextAV, aes(x=VSilt, y=ASilt, shape=Landuse,fill=Region,color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_errorbar(aes(ymin = ASilt-ASilt.se,ymax =  ASilt+ASilt.se),show.legend=F) + 
  geom_errorbarh(aes(xmin = VSilt-VSilt.se, xmax =  VSilt+VSilt.se),show.legend=F) +
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  #scale_fill_manual(values=c("green4","orangered3","white","white"))+
  scale_shape_manual(values=c(21,23,24,22)) + 
  #guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
  scale_x_continuous(limits = c(0,40), expand = c(0,0),breaks = c(0,10,20,30,40), labels = c(0,10,20,30,40))+
  scale_y_continuous(limits = c(0,40), expand = c(0,0),breaks = c(0,10,20,30,40), labels = c(0,10,20,30,40))+
  xlab("Silt (%) NMBU") +  ylab("Silt (%) SUA")

Siltplot

ggsave("Termites/Soil data/SiltP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

Sandplot <-ggplot(SoiltextAV, aes(x=VSand, y=ASand, shape=Landuse,fill=Region,color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_errorbar(aes(ymin = ASand-ASand.se,ymax =  ASand+ASand.se),show.legend=F) + 
  geom_errorbarh(aes(xmin = VSand-VSand.se, xmax =  VSand+VSand.se),show.legend=F) +
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  #scale_fill_manual(values=c("green4","orangered3","white","white"))+
  scale_shape_manual(values=c(21,23,24,22)) + 
  #guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
  scale_x_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Sand (%) NMBU") +  ylab("Sand (%) SUA")

Sandplot

ggsave("Termites/Soil data/SandP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)
