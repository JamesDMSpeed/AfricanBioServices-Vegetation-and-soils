rm(list=ls())
library(ggplot2)
#Soil texture exploration-Anders MSc

#Comparing vilde and Anders soil texture data####
##Anders soil C,N####
SoiltextA<-read.csv("Termites/Soil data/Soil_texture.csv", sep=';',dec='.')
SoiltextA$Landuse<-replace(SoiltextA$Landuse, SoiltextA$Landuse=="Common Garden", "Wild")

#Removing site 2 og 4 from Anders soil, since Vilde has only 1 og 3:
SoiltextA<- SoiltextA[SoiltextA$Block!="2",]
SoiltextA<- SoiltextA[SoiltextA$Block!="4",]

head(SoiltextA)
SoiltextA<- SoiltextA[c(1,3,5,7:9)]
colnames(SoiltextA)<-c("Region","Landuse","Block","Clay.per","Silt.per","Sand.per")
head(SoiltextA)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

Clay<-aggregate(Clay.per~Region+Block+Landuse,SoiltextA,mean)
Clay.se<-aggregate(Clay.per~Region+Block+Landuse,SoiltextA,se) #No se calculated as there is no replicated data for each block.

Silt<-aggregate(Silt.per~Region+Block+Landuse,SoiltextA,mean)
Silt.se<-aggregate(Silt.per~Region+Block+Landuse,SoiltextA,se) #No se calculated as there is no replicated data for each block.

Sand<-aggregate(Sand.per~Region+Block+Landuse,SoiltextA,mean)
Sand.se<-aggregate(Sand.per~Region+Block+Landuse,SoiltextA,se) #No se calculated as there is no replicated data for each block.


SoiltextA.Summary<-cbind(Clay[c(1:4)],Clay.se[4],Silt[4],Silt.se[4],Sand[4],Sand.se[4])
names(SoiltextA.Summary)
colnames(SoiltextA.Summary)<-c("Region","Block","Landuse","AClay","AClay.se","ASilt","ASilt.se","ASand","ASand.se")
head(SoiltextA.Summary)
SoiltextA.Summary$Who <- SoiltextA.Summary$AWho
SoiltextA.Summary$AWho<-"SUA"
head(SoiltextA.Summary)
SoiltextA.Summary<- SoiltextA.Summary[SoiltextA.Summary$Landuse!="Agriculture",]
SoiltextA.Summary$Landuse <- gsub("Common Garden", "Wild",SoiltextA.Summary$Landuse)
SoiltextA.Summary <-  droplevels(SoiltextA.Summary)

SoiltextA.Summary$Landuse<-as.factor(SoiltextA.Summary$Landuse)
levels(SoiltextA.Summary$Landuse)

SoiltextA.Summary$Block<-as.factor(SoiltextA.Summary$Block)
levels(SoiltextA.Summary$Block)


#Vilde soil####
SoiltextV<-read.csv("Ecosystem carbon/Soil.data/Soil.texture.all.csv", head = TRUE)
head(SoiltextV)
#SoiltextV<-SoiltextV[SoiltextV$Horizon=="A-hor",]
SoiltextV<-SoiltextV[c(1:3,6:8)]
colnames(SoiltextV)<-c("Region","Landuse","Block","Clay.per","Silt.per","Sand.per")
head(SoiltextV)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

Clay<-aggregate(Clay.per~Region+Block+Landuse,SoiltextV,mean)
Clay.se<-aggregate(Clay.per~Region+Block+Landuse,SoiltextV,se)

Silt<-aggregate(Silt.per~Region+Block+Landuse,SoiltextV,mean)
Silt.se<-aggregate(Silt.per~Region+Block+Landuse,SoiltextV,se)

Sand<-aggregate(Sand.per~Region+Block+Landuse,SoiltextV,mean)
Sand.se<-aggregate(Sand.per~Region+Block+Landuse,SoiltextV,se)


SoiltextV.Summary<-cbind(Clay[c(1:4)],Clay.se[4],Silt[4],Silt.se[4],Sand[4],Sand.se[4])
names(SoiltextV.Summary)
colnames(SoiltextV.Summary)<-c("Region","Block","Landuse","VClay","VClay.se","VSilt","VSilt.se","VSand","VSand.se")
head(SoiltextV.Summary)
SoiltextV.Summary$Who<-SoiltextV.Summary$VWho
SoiltextV.Summary$VWho<-"NMBU"
SoiltextV.Summary<-SoiltextV.Summary[SoiltextV.Summary$Region!="Ikorongo",]
SoiltextV.Summary<-SoiltextV.Summary[SoiltextV.Summary$Region!="Park Nyigoti",]

SoiltextV.Summary$Landuse<-as.factor(SoiltextV.Summary$Landuse)
levels(SoiltextV.Summary$Landuse)

SoiltextV.Summary$Block<-as.factor(SoiltextV.Summary$Block)
levels(SoiltextV.Summary$Block)

#Plotting the soil texture for comparison####
SoiltextAV<-merge(SoiltextA.Summary,SoiltextV.Summary)

####Making Graf####
names(SoiltextAV)
Clayplot <- ggplot(SoiltextAV, aes(y=VClay, x=AClay, shape=Landuse,fill=Block,color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_abline(slope=1.4111, intercept=-13.5147, size =.95, color="red") +
  geom_abline(slope = 1.4111+0.2944,intercept = -13.5147+9.6423, size =0.95, linetype="dotted",color="red")+
  geom_abline(slope = 1.4111-0.2944,intercept = -13.5147-9.6423, size =0.95, linetype="dotted",color="red")+
  geom_errorbar(aes(ymin = AClay-AClay.se,ymax =  AClay+AClay.se),show.legend=F) + 
  geom_errorbarh(aes(xmin = VClay-VClay.se, xmax =  VClay+VClay.se),show.legend=F) +
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("Grey","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  scale_shape_manual(values=c(21,23)) + 
  guides(fill = guide_legend(override.aes=list(shape=21, color=c("Grey","Black")))) +
  scale_x_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Clay (%) SUA (Hydrometer)") +  ylab("Clay (%) NMBU (Pipette)")
    
Clayplot

ggsave("Termites/Soil data/ClayP.png",
     width= 25, height = 15,units ="cm",bg ="transparent",
      dpi = 600, limitsize = TRUE)

Siltplot <- ggplot(SoiltextAV, aes(y=VSilt,x=ASilt, shape=Landuse,fill=Block,color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_abline(slope=0.8145, intercept=12.8013, size =.95,color="red") +
  geom_abline(slope =0.8145 +0.1494,intercept = 12.8013+2.1944, size =0.95, linetype="dotted",color="red")+
  geom_abline(slope =0.8145-0.1494,intercept = 12.8013-2.1944, size =0.95, linetype="dotted",color="red")+
  #geom_errorbar(aes(ymin = ASilt-ASilt.se,ymax =  ASilt+ASilt.se),show.legend=F) + 
  #geom_errorbarh(aes(xmin = VSilt-VSilt.se, xmax =  VSilt+VSilt.se),show.legend=F) +
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("Grey","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  scale_shape_manual(values=c(21,23)) + 
  guides(fill = guide_legend(override.aes=list(shape=21, color=c("Grey","Black")))) +
  scale_x_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Silt (%) SUA (Hydrometer)") +  ylab("Silt (%) NMBU (Pipette)")

Siltplot
ggsave("Termites/Soil data/SiltP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

Sandplot <- ggplot(SoiltextAV, aes(y=VSand, x=ASand, shape=Landuse,fill=Block,color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_abline(slope=1.1681, intercept=-19.4235, size =.95,color="red") +
  geom_abline(slope = 1.1681+0.1071,intercept = -19.4235+6.2851, size =0.95, linetype="dotted",color="red")+
  geom_abline(slope = 1.1681-0.1071,intercept = -19.4235-6.2851, size =0.95, linetype="dotted",color="red")+
  geom_errorbar(aes(ymin = ASand-ASand.se,ymax =  ASand+ASand.se),show.legend=F) + 
  geom_errorbarh(aes(xmin = VSand-VSand.se, xmax =  VSand+VSand.se),show.legend=F) +
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("Grey","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple","Black")) +
  scale_shape_manual(values=c(21,23)) + 
  guides(fill = guide_legend(override.aes=list(shape=21, color=c("Grey","Black")))) +
  scale_x_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(0,80), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Sand (%) SUA (Hydrometer)") +  ylab("Sand (%) NMBU (Pipette)")

Sandplot
ggsave("Termites/Soil data/SandP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

####Anders soil P exploration####
SoilNutWet <- read.csv("Termites/Soil data/Soil_Nutrient_Wet.csv",sep=';',dec='.')
SoilNutDry <- read.csv("Termites/Soil data/Soil_Nutrient_Dry.csv",sep=';',dec='.')

SoilNutWet$ID[duplicated(SoilNutWet$ID) | duplicated(SoilNutWet$ID, fromLast=TRUE)]
#From above, I see that Mwan P3 and Makao P2 have duplicates.
SoilNutDry$Corrected.ID[duplicated(SoilNutDry$Corrected.ID) | duplicated(SoilNutDry$Corrected.ID, fromLast=TRUE)]
#From above, I see that Mwan P2 and SNP W2 have duplicates.
par(mfrow=c(1,2))
plot(SoilNutWet$X..N.kapsel~SoilNutWet$Landuse)
plot(SoilNutDry$X..N.kapsel~SoilNutDry$Landuse)

SoilNutWet <- SoilNutWet[SoilNutWet$Landuse!="Agriculture",]
SoilNutDry <- SoilNutDry[SoilNutDry$Landuse!="Agriculture",]
SoilNutWet <- SoilNutWet[SoilNutWet$Landuse!="Seronera",]
SoilNutDry <- SoilNutDry[SoilNutDry$Landuse!="Seronera",]

SoilNutDry<- SoilNutDry[c(6,8,10)]
colnames(SoilNutDry)<-c("P%_SUA","Block","Region")
SoilNutWet<- SoilNutWet[c(5,7,9)]
colnames(SoilNutWet)<-c("P%_SUA","Block","Region")

#Removing site 2 og 4 from Anders soil, since Vilde has only 1 og 3:
SoilNutWet<- SoilNutWet[SoilNutWet$Block!="2",]
SoilNutWet<- SoilNutWet[SoilNutWet$Block!="4",]
SoilNutDry<- SoilNutDry[SoilNutDry$Block!="2",]
SoilNutDry<- SoilNutDry[SoilNutDry$Block!="4",]
#Loosing Mwantimba plot in Dry samples when excuding block 2 and 4.

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

DryPhos <- aggregate(`P%_SUA`~Region+Block, SoilNutDry,mean)
WetPhos <- aggregate(`P%_SUA`~Region+Block,SoilNutWet,mean)

WetPhos$Block <- as.factor(WetPhos$Block)
levels(WetPhos$Block)
WetPhos$Region <- droplevels(WetPhos$Region)
WetPhos$Region <- as.factor(WetPhos$Region)
levels(WetPhos$Region)
WetPhos$`P%_SUA` <- as.numeric(WetPhos$`P%_SUA`)

####Vilde Soil P data####
Vilde.soil <- read.csv("Ecosystem carbon/Soil.data/Soil.Properties.csv", head = TRUE)
Vilde.soil<-Vilde.soil[Vilde.soil$Region!="Ikorongo",]
Vilde.soil<-Vilde.soil[Vilde.soil$Region!="Park Nyigoti",]
Vilde.soil<-Vilde.soil[Vilde.soil$Region!="Seronera",]

Vilde.Psoil<- Vilde.soil[c(2,3,16)]
colnames(Vilde.Psoil)<-c("Block","Region","P%_NMBU")

Vilde.Psoil$Block <- as.factor(Vilde.Psoil$Block)
levels(Vilde.Psoil$Block)
Vilde.Psoil$Region <- droplevels(Vilde.Psoil$Region)
Vilde.Psoil$Region <- as.factor(Vilde.Psoil$Region)
levels(Vilde.Psoil$Region)
Vilde.Psoil$`P%_NMBU` <- as.numeric(Vilde.Psoil$`P%_NMBU`)

names(Vilde.Psoil)
names(SoilNutWet)

####WetSoilSUA with NMBU P###
WetPVildeP <- merge(Vilde.Psoil,WetPhos)
names(WetPVildeP)

levels(WetPVildeP$Block)
levels(WetPVildeP$Region)

WetPVildeP$`P%_NMBU` <- as.numeric(WetPVildeP$`P%_NMBU`)
WetPVildeP$`P%_SUA` <- as.numeric(WetPVildeP$`P%_SUA`)

####DrySoilSUA and NMBU Soil
DryPVildeP <- merge(Vilde.Psoil,DryPhos)
names(DryPVildeP)

levels(DryPVildeP$Block)
levels(DryPVildeP$Region)

DryPVildeP$`P%_NMBU` <- as.numeric(DryPVildeP$`P%_NMBU`)
DryPVildeP$`P%_SUA` <- as.numeric(DryPVildeP$`P%_SUA`)

####Plotting NMBU P against WET SUA P
library(ggplot2)
WetPhosplot <- ggplot(WetPVildeP, aes(y=as.numeric(`P%_NMBU`),x=as.numeric(`P%_SUA`),fill=Block, color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("Grey","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple")) +
  #scale_shape_manual(values=c(21,23)) + 
  #guides(fill = guide_legend(override.aes=list(shape=21, color=c("Grey","Black")))) +
  scale_x_continuous(limits = c(0,70))+
  scale_y_continuous(limits = c(0,2))+
  xlab("Wet P (%) SUA") +  ylab("P (%) NMBU")

WetPhosplot
####Plotting NMBU P against Dry SUA P
DryPhosplot <- ggplot(DryPVildeP, aes(y=as.numeric(`P%_NMBU`),x=as.numeric(`P%_SUA`),fill=Block, color=Region))+
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_point(size=4.5,stroke=1.5,position=position_dodge(width=.35), show.legend=T) +
  scale_fill_manual(values=c("Grey","Black")) +
  scale_color_manual(values=c("green4","orangered3","Blue","Purple")) +
  #scale_shape_manual(values=c(21,23)) + 
  #guides(fill = guide_legend(override.aes=list(shape=21, color=c("Grey","Black")))) +
  scale_x_continuous(limits = c(0,70))+
  scale_y_continuous(limits = c(0,2))+
  xlab("DRy P (%) SUA") +  ylab("P (%) NMBU")

DryPhosplot















###Corrigating original soil data####
SoiltextA.orig<-read.csv("Termites/Soil data/Soil_texture.csv", sep=';',dec='.')

####Estimating prediction lines and adding the the corrected value into a dataset####
Claymod <- lm(SoiltextAV$VClay~SoiltextAV$AClay)
summary(Claymod)
#CLAY: Y= 1.3392X -10.4221, R^2 = 0.7338
SoiltextA.orig$ClaySlope <- 1.3392
SoiltextA.orig$ClayIntercept <- -10.4221
SoiltextA.orig$Claycorr <- SoiltextA.orig$ClaySlope*SoiltextA.orig$CLAY..+SoiltextA.orig$ClayIntercept
SoiltextA.orig$ClaySlope <- NULL
SoiltextA.orig$ClayIntercept <- NULL

Siltmod <- lm(SoiltextAV$VSilt~SoiltextAV$ASilt)
summary(Siltmod)
#SILT: Y=  0.8139X + 12.6331, R^2 = 0.8072
SoiltextA.orig$SiltSlope <- 0.8139
SoiltextA.orig$SiltIntercept <- 12.6331
SoiltextA.orig$Siltcorr <- SoiltextA.orig$SiltSlope*SoiltextA.orig$SILT..+SoiltextA.orig$SiltIntercept

SoiltextA.orig$SiltSlope <- NULL
SoiltextA.orig$SiltIntercept <- NULL

Sandmod <- lm(SoiltextAV$VSand~SoiltextAV$ASand)
summary(Sandmod)
#SAND: Y= 1.1281X-17.9957, R^2 = 0.9093
SoiltextA.orig$SandSlope <- 1.1281
SoiltextA.orig$SandIntercept <- -17.9957
SoiltextA.orig$ASandcorr <- SoiltextA.orig$SandSlope*SoiltextA.orig$SAND..+SoiltextA.orig$SandIntercept
SoiltextA.orig$SandSlope <- NULL
SoiltextA.orig$SandIntercept <- NULL

write.csv(SoiltextA.orig,file="Termites/Soil data/Soil_texture_corrected.csv")
#### Soil texture triangle####
library(lattice)
library(MASS)
library(soiltexture)

#Insert data & observe data structure

Soiltexture<-read.csv("Termites/Soil data/Soil_texture.csv", sep=';',dec='.')
Soiltexture<- Soiltexture[c(1:3,5,7:9)]
colnames(Soiltexture)<-c("Site","Region","Landuse","Block","CLAY","SILT","SAND")
dim(Soiltexture) #28 11
str(Soiltexture)
head(Soiltexture)	
names(Soiltexture)
Soiltexture.sum <- aggregate(cbind(CLAY,SILT,SAND)~Site+Landuse+Region,Soiltexture,mean)


#Need to add rainfall data:
Rainwet<-read.csv("Termites/Precipitation data/Rainfall_wet.csv", sep=';',dec='.')
names(Rainwet)
Rainwet<-aggregate(rain.sum~Site+rain.region+Landuse,Rainwet,mean)
colnames(Rainwet)<-c("Site","Region","Landuse","Rain")
#Merge the data
Soiltext <- merge(Soiltexture.sum,Rainwet)
names(Soiltext)


# Colour for rainfall
color_pallete_function2 <- colorRampPalette(
  colors = c("lightcyan", "deepskyblue","steelblue2", "blue4"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)

num_colors2 <- nlevels(as.factor(Soiltext$Rain))
num_colors2
diamond_color_colors2 <- color_pallete_function2(num_colors2)
Soiltext$pt.col<-diamond_color_colors2[as.factor(Soiltext$Rain)]
factCOLS<-unlist(levels(as.factor(Soiltext$pt.col)))


# Points for landuse
pt.landuse<- nlevels(Soiltext$Landuse) # 4 landuses
pt.to.landuse <- c(21,23,24,22)
Soiltext$pt.pch<-pt.to.landuse [Soiltext$Landuse]
Soiltext$pt.pch


# Load texture triangle space
geo     <- TT.plot(class.sys = "none", add=T, grid.show =F,frame.bg.col ="white", cex.axis = 1.2 , cex.lab=1.2, cex.main=1.2, lwd=1.2)

TT.plot( class.sys = "UK.SSEW.TT", main="",
         tri.data=Soiltext,frame.bg.col ="white", col=Soiltext$pt.col, bg="white",pch =Soiltext$pt.pch,# bg=Soiltext$pt.col,
         grid.show =F,cex.axis = 1.2 , cex.lab=1.2, cex.main=1.5, cex=1.75, lwd=9.9)
#z.name="Rainfall (mm)", z.cex.range =c(0.5,2.5),
#z.pch=my.text$pt.pch,
#z.col.hue =0.99)

# Legend
max(Soiltext$Rain)
legend (x= 90,  y= 100, legend=levels(as.factor(Soiltext$Rain)),  pch=21, 
        pt.bg =rev(c(factCOLS)), cex =1.2, pt.cex=1.2,
        y.intersp =.8,x.intersp =.8, text.col=rev(c(factCOLS)), col=rev(c(factCOLS)), bty= "n")


# Seperate legend as gradient colour
#layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1)) # Can add to plot using layout
legend_image <- as.raster(matrix(rev(color_pallete_function2 (20)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Rainfall (mm)')
text(x=1.5, y = seq(0,1,l=5), labels = seq(170,230,l=5))
rasterImage(legend_image, 0, 0, 1,1)


#names(Rootex)
#Rootex[,"WETrain.sum.mm"] 
#z.cex.range <- TT.get("z.cex.range")
#def.pch <- par("pch")
#def.cex <- TT.get("cex")
#def.col <- par( "col")
#oc.str <- TT.str( Rootex[,"WETrain.sum.mm"],
#      z.cex.range[1],
#      z.cex.range[2])

#legend (x = 50,  y = 50,  title = "Rainfall (mm)",
#legend = formatC(c(max(Rootex[,"WETrain.sum.mm"] ), quantile(Rootex[,"WETrain.sum.mm"] ,probs=c(75,50,25)/100), min(Rootex[,"WETrain.sum.mm"] )), 
#                 format  = "f", digits  = 1, width   = 4, flag    = "0"), #
# pt.lwd = NA, col = c(factCOLS), pt.cex  = c(
#min( oc.str ),
# quantile(oc.str ,probs=c(25,50,75)/100), max( oc.str ) 
#pch = 19,
## ),  #,
#bty="o",
#y.intersp=1.4,x.intersp=1.2,
#bg=NA, 
#box.col = NA, # Uncomment this to remove the legend box
#text.col="black",
#cex = 1)







