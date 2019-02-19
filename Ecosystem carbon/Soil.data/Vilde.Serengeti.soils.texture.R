#################################################################
# Serengeti  - soil texture

rm(list=ls())
library(lattice)
library(MASS)

#### corrected the sand,silt,clay to SUM100 ####

Soil.texture <- read.csv("Ecosystem carbon/Soil.data/Soil.texture.Tot_Hor.csv", head=T)
Soil.texture <- Soil.texture[,c(1:3,5:8)]
Soil.texture <- na.omit(Soil.texture)

Texture.sum<-aggregate(cbind(Soil.texture$Clay.pip.per,Soil.texture$Silt.pip.per,Soil.texture$Sand.pip.per), by=list(Soil.texture$Region), mean)

colnames(Texture.sum)<-c("Region","Clay","Silt","Sand")
Texture.sum

# Region    Clay    Silt    Sand
# 1    Handajega 21.4425 18.0725 60.4900
# 2     Ikorongo 33.1725 34.3325 32.4925
# 3        Makao 22.2000 16.5150 61.2800
# 4        Maswa 46.4750 32.9525 20.5725
# 5    Mwantimba 31.9150 20.4025 47.6850
# 6 Park Nyigoti 31.5350 36.5550 31.9075
# 7     Seronera 20.1675 18.0725 61.7600

# # Loading soil texture graphics
library(soiltexture)
#UK.SSEW.TT - the "value" we use for class. system later 
names(Soil.texture)
levels(Soil.texture$Region)

# relevel 
Soil.texture$Region <- factor(Soil.texture$Region,levels = c("Makao","Maswa","Mwantimba","Handajega", "Seronera","Park Nyigoti","Ikorongo"))
levels(Soil.texture$Region)

levels(Soil.texture$Landuse)
Soil.texture$Landuse <- factor(Soil.texture$Landuse,levels = c("Pasture","Wild"))
#pt.col<-as.numeric(Rootex$Area)
#pt.col

color_pallete_function <- colorRampPalette(
  colors = c("goldenrod1","darkorange3", "dodgerblue", "deepskyblue4","darkolivegreen3", "lightskyblue", "darkslategray4"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)

# Dataframe for soil texture 
my.text<-data.frame(Soil.texture$Region,Soil.texture$Clay.pip.per,Soil.texture$Silt.pip.per,Soil.texture$Sand.pip.per)

# Points for landuse
pt.landuse<- nlevels(Soil.texture$Landuse) # 2 landuses
pt.to.landuse <- c(0,19)
my.text$pt.pch<-pt.to.landuse [Soil.texture$Landuse]
my.text$pt.pch

colnames(my.text)<-c("REGION","CLAY","SILT","SAND","LANDUSE")
my.text

#Colours from my text
num_colors <- nlevels(Soil.texture$Region)
num_colors
diamond_color_colors <- color_pallete_function(num_colors)
my.text$pt.col<-diamond_color_colors[Soil.texture$Region]

#Park Nyigoti NOT SHOWING?
plot(SAND~jitter(CLAY),pch=21, cex=1.2, bg=pt.col, data=my.text)
my.textMAK<-my.text[my.text$REGION=="Makao",]
my.textMAS<-my.text[my.text$REGION=="Maswa",]
my.textMWA<-my.text[my.text$REGION=="Mwantimba",]
my.textHAN<-my.text[my.text$REGION=="Handajega",]
my.textSE<-my.text[my.text$REGION=="Seronera",]
my.textPN<-my.text[my.text$REGION=="Park Nyigoti",]
my.textIK<-my.text[my.text$REGION=="Ikorongo",]
points(SAND~jitter(CLAY),pch=21, cex=1.2, bg="black",data=my.textMWA)

# Load texture triangle space
geo     <- TT.plot(class.sys = "none", add=T, grid.show =F,frame.bg.col ="white", cex.axis = 1.2 , cex.lab=1.2, cex.main=1.2, lwd=1.2)

# Texture Triangle
names(Soil.texture)
?TT.plot
TT.plot(class.sys = "UK.SSEW.TT", main="", cex=1.5,
	tri.data=my.text,frame.bg.col ="white", pch=my.text$LANDUSE, col=my.text$pt.col,
	grid.show =F,cex.axis = 1.2 , cex.lab=1.2, cex.main=1.2, lwd=3)

#Legend
levels(my.text$REGION)
(my.text$pt.col)

# Legend for points and regions
legend (x= 98,  y= 100, legend=levels(my.text$REGION),  pch=19, 
        pt.bg =c("#FFC025", "#CC6600","#1E90FF", "#00678B","#A1CC59","#86CDF9","#528B8B"), 
        cex =1.2, pt.cex=1.2,y.intersp =.5,x.intersp =.8, 
        text.col=c("#FFC025", "#CC6600","#1E90FF", "#00678B","#A1CC59","#86CDF9","#528B8B"), 
        col=c("#FFC025", "#CC6600","#1E90FF", "#00678B","#A1CC59","#86CDF9","#528B8B"), bty= "n")

# Legend only for names 
#legend (x= 90,  y= 100, legend=levels(my.text$REGION), 
#       cex =1.2, pt.cex=1.2,y.intersp =.8,x.intersp =.8, 
#       text.col=c("#FFC025", "#CC9A1D","#104D8B", "#1873CC","#A1CC59","#86CDF9","#B0E1FF"),
#       bty= "n")

# Legend for landuse 
legend (x= 98,  y= 40, legend=levels(Soil.texture$Landuse),pch=c(19,0), 
        cex =1.2, pt.cex=2, y.intersp =.5,x.intersp =.8, text.col="black", col="gray21",bty= "n")

# Quick linear model to see if differences in Area by Clay %
names(Soil.texture)
Sertext<-lm(Clay.pip.per~Region,data=Soil.texture)
summary(Sertext)
anova(Sertext) # Significant 

