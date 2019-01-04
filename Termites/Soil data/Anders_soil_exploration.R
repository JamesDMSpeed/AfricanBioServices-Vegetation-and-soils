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

#ggsave("Termites/Soil data/ClayP.png",
 #      width= 25, height = 15,units ="cm",bg ="transparent",
  #     dpi = 600, limitsize = TRUE)

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

#ggsave("Termites/Soil data/SiltP.png",
 #      width= 25, height = 15,units ="cm",bg ="transparent",
  #     dpi = 600, limitsize = TRUE)

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

#ggsave("Termites/Soil data/SandP.png",
 #      width= 25, height = 15,units ="cm",bg ="transparent",
  #     dpi = 600, limitsize = TRUE)


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
layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1)) # Can add to plot using layout
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







