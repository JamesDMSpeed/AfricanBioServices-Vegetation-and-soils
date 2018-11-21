#################################################################
# Serengeti  - soil texture

################################################################
#clear system & package libraries
rm(list=ls())
library(lattice)
library(MASS)
################################################################
# Serengeti - Soil texture triangle
################################################################
#Insert data & observe data structure
Total.soil.data<-read.csv("Ecosystem carbon/Total.soil.data.csv", sep=",",header=TRUE)
Soil.texture <- Total.soil.data[,c(1:6,14,30:32)]
tail(Soil.texture)
Soil.texture <- na.omit(Soil.texture)
dim(Soil.texture) #28 10
str(Soil.texture)
head(Soil.texture)	
names(Soil.texture)

write.csv(Soil.texture,file="Ecosystem carbon/Soil.texture.csv")

#### corrected the sand,silt,clay to SUM100 ####

Soil.texture <- read.csv("Ecosystem carbon/Soil.texture.csv", head=T)
Texture.sum<-aggregate(cbind(Soil.texture$Clay.per,Soil.texture$Silt.per,Soil.texture$Sand.per), by=list(Soil.texture$Region), mean)
colnames(Texture.sum)<-c("Region","Clay","Silt","Sand")
Texture.sum

#Region  Clay  Silt  Sand
#1    Handajega 19.75 17.75 62.00
#2     Ikorongo 31.75 40.00 28.00
#3        Makao 24.25 15.75 60.50
#4        Maswa 53.25 32.50 14.25
#5    Mwantimba 33.25 23.00 43.75
#6 Park Nyigoti 23.00 39.50 38.00
#7     Seronera 16.75 18.75 64.50

# Loading soil texture graphics
library(soiltexture)
#UK.SSEW.TT - the "value" we use for class. system later 
names(Soil.texture)
levels(Soil.texture$Region)

# relevel 
Soil.texture$Region <- factor(Soil.texture$Region,levels = c("Makao","Maswa","Mwantimba","Handajega", "Seronera","Park Nyigoti","Ikorongo"))
levels(Soil.texture$Region)

#pt.col<-as.numeric(Rootex$Area)
#pt.col

color_pallete_function <- colorRampPalette(
  colors = c("goldenrod1","goldenrod3", "dodgerblue4", "dodgerblue3","darkolivegreen3", "lightskyblue", "lightskyblue1"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)

# Dataframe for soil texture 
my.text<-data.frame(Soil.texture$Region,Soil.texture$Clay.per,Soil.texture$Silt.per,Soil.texture$Sand.per)

# Points for landuse
pt.landuse<- nlevels(Soil.texture$Land_Use) # 2 landuses
pt.to.landuse <- c(15:16)
my.text$pt.pch<-pt.to.landuse [Soil.texture$Land_Use]
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

TT.plot(class.sys = "UK.SSEW.TT", main="", cex=1.5,
	tri.data=my.text,frame.bg.col ="white",pch=my.text$LANDUSE, col=my.text$pt.col,
	grid.show =F,cex.axis = 1.2 , cex.lab=1.2, cex.main=1.2, lwd=1.2)
		
#Legend
levels(my.text$REGION)
(my.text$pt.col)

# Legend for points and regions
#legend (x= 110,  y= 100, legend=levels(my.text$REGION),  pch=21, 
#        pt.bg =c("#FFC025", "#CC9A1D","#104D8B", "#1873CC","#A1CC59","#86CDF9","#B0E1FF"), 
#        cex =1.2, pt.cex=1.2,y.intersp =.8,x.intersp =.8, 
#        text.col=c("#FFC025", "#CC9A1D","#104D8B", "#1873CC","#A1CC59","#86CDF9","#B0E1FF"), 
#        col=c("#FFC025", "#CC9A1D","#104D8B", "#1873CC","#A1CC59","#86CDF9","#B0E1FF"), bty= "n")

# Legend only for names 
legend (x= 90,  y= 100, legend=levels(my.text$REGION), 
       cex =1.2, pt.cex=1.2,y.intersp =.8,x.intersp =.8, 
       text.col=c("#FFC025", "#CC9A1D","#104D8B", "#1873CC","#A1CC59","#86CDF9","#B0E1FF"),
       bty= "n")

# Legend for landuse 
legend (x= 98,  y= 40, legend=levels(Soil.texture$Land_Use),  pch=c(15:16), 
        cex =1.2, pt.cex=2, y.intersp =.8,x.intersp =.8, text.col="black", col="light grey", bty= "n")


# Quick linear model to see if tdifferences in Area by Clay %
names(Soil.texture)
Sertext<-lm(Clay.per~Region,data=Soil.texture)
summary(Sertext)
anova(Sertext) # NS
