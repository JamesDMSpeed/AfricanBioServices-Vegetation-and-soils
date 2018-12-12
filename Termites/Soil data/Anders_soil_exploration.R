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

Clay<-aggregate(Clay.per~Region+Landuse,SoiltextA,mean)
Clay.sd<-aggregate(Clay.per~Region+Landuse,SoiltextA,sd)

Silt<-aggregate(Silt.per~Region+Landuse,SoiltextA,mean)
Silt.sd<-aggregate(Silt.per~Region+Landuse,SoiltextA,sd)

Sand<-aggregate(Sand.per~Region+Landuse,SoiltextA,mean)
Sand.sd<-aggregate(Sand.per~Region+Landuse,SoiltextA,sd)


SoiltextA.Summary<-cbind(Clay[c(1:3)],Clay.sd[3],Silt[3],Silt.sd[3],Sand[3],Sand.sd[3])

colnames(SoiltextA.Summary)<-c("Region","Landuse","Clay","Clay.sd","Silt","Silt.sd","Sand","Sand.sd")
head(SoiltextA.Summary)

SoiltextA.Summary$Who<-"SUA"
#Vilde soil####
SoiltextV<-read.csv("Ecosystem carbon/Soil.data/Soil.texture.csv", head = TRUE)
head(SoiltextV)
SoiltextV<-SoiltextV[SoiltextV$Horizon=="A-hor",]
SoiltextV<-SoiltextV[c(3,5,8:11)]
colnames(SoiltextV)<-c("Region","Block","Landuse","Clay.per","Silt.per","Sand.per")
head(SoiltextV)


Clay<-aggregate(Clay.per~Region+Landuse,SoiltextV,mean)
Clay.sd<-aggregate(Clay.per~Region+Landuse,SoiltextV,sd)

Silt<-aggregate(Silt.per~Region+Landuse,SoiltextV,mean)
Silt.sd<-aggregate(Silt.per~Region+Landuse,SoiltextV,sd)

Sand<-aggregate(Sand.per~Region+Landuse,SoiltextV,mean)
Sand.sd<-aggregate(Sand.per~Region+Landuse,SoiltextV,sd)


SoiltextV.Summary<-cbind(Clay[c(1:3)],Clay.sd[3],Silt[3],Silt.sd[3],Sand[3],Sand.sd[3])

colnames(SoiltextV.Summary)<-c("Region","Landuse","Clay","Clay.sd","Silt","Silt.sd","Sand","Sand.sd")
head(SoiltextV.Summary)
SoiltextV.Summary$Who<-"NMBU"
SoiltextV.Summary<-SoiltextV.Summary[SoiltextV.Summary$Region!="Ikorongo",]
SoiltextV.Summary<-SoiltextV.Summary[SoiltextV.Summary$Region!="Park Nyigoti",]

#Plotting the soil texture for comparison####
SoiltextAV<-rbind(SoiltextA.Summary,SoiltextV.Summary)

head(SoiltextAV)
ClayP <- ggplot(data=SoiltextAV, aes(x=Region,y=Clay, ymin=Clay-Clay.sd , ymax=Clay+Clay.sd, shape=Landuse))
ClayP<- ClayP+geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
ClayP<- ClayP+geom_point(size=5,stroke=1, color="Darkgreen",position=position_dodge(width=.35),show.legend=T)
ClayP <- ClayP + facet_wrap( ~ Who, scale ="fixed")
ClayP <- ClayP + xlab("Region") +  ylab("Clay %") 
ClayP
ggsave("Termites/Soil data/ClayP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)


SiltP <- ggplot(data=SoiltextAV, aes(x=Region,y=Silt, ymin=Silt-Silt.sd , ymax=Silt+Silt.sd, shape=Landuse))
SiltP<- SiltP+geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
SiltP<- SiltP+geom_point(size=5,stroke=1, color="Darkgreen",position=position_dodge(width=.35),show.legend=T)
SiltP <- SiltP + facet_wrap( ~ Who, scale ="fixed")
SiltP <- SiltP + xlab("Region") +  ylab("Silt %") 
SiltP
ggsave("Termites/Soil data/SiltP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

SandP <- ggplot(data=SoiltextAV, aes(x=Region,y=Sand, ymin=Sand-Sand.sd , ymax=Sand+Sand.sd, shape=Landuse))
SandP<- SandP+geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
SandP<- SandP+geom_point(size=5,stroke=1, color="Darkgreen",position=position_dodge(width=.35),show.legend=T)
SandP <- SandP + facet_wrap( ~ Who, scale ="fixed")
SandP <- SandP + xlab("Region") +  ylab("Sand %") 
SandP
ggsave("Termites/Soil data/SandP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

write.csv(SoiltextAV,file="Termites/Soil data/Soiltexture_comparison.csv")
