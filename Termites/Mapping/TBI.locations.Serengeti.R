######################################################
#TBI locations - Serengeti Map
######################################################
# Stuart Smith 5/9/2019
######################################################
# Packages
rm(list=ls(all=TRUE))
library(devtools)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(rgeos)
library(maptools)
library(lubridate)
######################################################
### Make colors transparent
######################################################
makeTransparent<-function(someColor, alpha=75)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
} #End function
#########################################################################################################################
# Stack daily precipitation tiff files from specific dates
# Stack 13th May 2016 to 24th July 2017

# Parameters
utmproj<-"+proj=utm +south +zone=36 +init=EPSG:21036" #spatial coordinate reference system in Serengeti
latlongproj<-("+proj=longlat +datum=WGS84")
#crs="+proj=utm +zone=36 ellps=WGS84"

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Moisture.temp.loggers/Precipitation/NASA.gtif/")

Serlist<-list.files(getwd(), pattern="tif$", full.names=FALSE)
Serlist # 1247 days

tiflist<-list.files(path="/Users/anotherswsmith/Documents/AfricanBioServices/Data/Moisture.temp.loggers/Precipitation/NASA.gtif/", 
                    pattern="tif", full.names=T)

# Bricking is taking a long time
brk2 <- do.call(brick, lapply(list.files(path = "./", pattern = "*.tif",full.names=T), raster))

sumIgnoringZeroes <- function(x) {
  sum(x[x!=0],na.rm=T)
}

# Sum rain faster Jan 2015 - May 2018
RAINstack1sum<-sum(brk2)/4 # Divide by 3.5 years 
brick(RAINstack1sum,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(RAINstack1sum)

# Import sampling locations
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Termites/Mapping/")
wp222SP<-read.csv("TBI.site.GeoRef.csv")
levels(wp222SP$Landuse)
latlongproj<-("+proj=longlat +datum=WGS84")
wp222SP_proj <-wp222SP
coordinates(wp222SP_proj)<- ~Longitude + Latitude
proj4string(wp222SP_proj)<-latlongproj

wp222loc<- cbind(wp222SP$Longitude,wp222SP$Latitude)# get geographical location
wp222SP<-read.csv("TBI.site.GeoRef.csv")
latlongproj<-("+proj=longlat +datum=WGS84")
# Sampling points
mysp<- SpatialPointsDataFrame(wp222loc,wp222SP, proj4string=CRS(latlongproj),match.ID = TRUE)

# Read Serengeti boundary
sme<-readOGR(dsn=".", "v4_serengeti_ecosystem")
names(sme)
#sme<-spTransform(sme,utmproj) #transform this to the correct projection
sme<-spTransform(sme,latlongproj)
levels(sme$NAME)
sme<-sme[!(sme$NAME %in% c("Kijereshi","Koyaki","Lemek" ,"Ruma","Masaai Mara Nati","Mwiba",#"Ikona" ,#"Makao","Mwiba",
                           "Ol Chorro Oirowu","Olkinyie","Siana")),] 
#sme<-sme[!(sme$NAME %in% c("Lake Manyara NP","Tarangire NP","Lake Naivasha","Hell's gate","Ruma","Koyiaki RA",
#      "Lemek RA", "Mara reserve", "Ol Kinyei RA", "Siana           RA")),] 
smeInner<-sme[!(sme$NAME %in% c("Loliondo","Ngorongoro","Kijereshi","Koyaki","Lemek" ,"Ruma","Masaai Mara Nati","Ikona",#"Makao",
                                "Mwiba","Ol Chorro Oirowu","Olkinyie","Siana")),] 
#remove unneccesary protected areas + Kenyan areas
plot(sme, lwd=1.5)
plot(smeInner, add=T, col="grey",lwd=1.5)
class(sme)
#Bounding box
#34.02313, 34.85505, -3.425976, -2.272658
coords = matrix(c(33.90013, -3.525976,
               33.90013, -2.172658,
               34.95505, -2.172658,
               34.95505, -3.525976,
               33.90013, -3.525976), 
             ncol = 2, byrow = TRUE)
P1 <- Polygon(coords)
bb <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

##################################################
# Map Rain Serengeti - May 2016 - July 2017
##################################################
# Central box 
coords3 = matrix(c(34.9, -2.8,
               34.9, -2.69215,
               34.5, -2.69215,
               34.5, -2.8,
               34.9, -2.8), 
             ncol = 2, byrow = TRUE)
P3 <- Polygon(coords3)
bb3 <- SpatialPolygons(list(Polygons(list(P3), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Tanzania map
tz0<-raster::getData('GADM',country='TZ',level=0)#Level = 0 for country, # TZA
tzbb<-extent(tz0)
SeroRainclip<- mask(RAINstack1sum, bb3)
tzSeroRain<-extend(SeroRainclip,tz0)

# Kenya,Uganda,Mozambique
ken0<-raster::getData('GADM',country='KEN',level=0)#Level = 0 for country,
#ken1<-raster::getData('GADM',country='KEN',level=1)#Level = 0 for country,
ug0<-raster::getData('GADM',country='UGA',level=0)#Level = 0 for country,
mz0<-raster::getData('GADM',country='MOZ',level=0)#Level = 0 for country,

# Crop bordering countries to the extent of Tz0 map

coordsK = matrix(c(40.44514, -11.7457,
                   40.44514, 0.1,
                   29.32717, 0.1,
                   29.32717, -11.7457,
                   40.44514, -11.7457),ncol = 2, byrow = TRUE)
Pk <- Polygon(coordsK)
bbK <- SpatialPolygons(list(Polygons(list(Pk), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
extent(bbK)
tzExt<-extent(tz0)
ken0c<-crop(ken0,bbK)
ug0c<-crop(ug0,bbK) # Larger object first and bounding box next
mz0c<-crop(mz0,tzExt) 

# Water bodies i.e. Lake Victoria
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Termites/Mapping/GLWD-level1")
LakVic<-readOGR(dsn=".", "glwd_1")
LakVicCrop<-crop(LakVic,bbK)
plot(LakVicCrop, col="dodgerblue")

# Margins of plot
my.padding <- list(layout.heights = list( 
 top.padding = 0, 
 main.key.padding = 0, 
key.axis.padding = 0, 
axis.xlab.padding = 0, 
xlab.key.padding = 0, 
 key.sub.padding = 0), 
layout.widths = list( 
 left.padding = 0, 
key.ylab.padding = 0, 
 ylab.axis.padding = 0, 
axis.key.padding = 0, 
 right.padding = 0) 
) 

p1<-levelplot(tzSeroRain,margin=F,scales = list(draw = FALSE), colorkey=NULL,par.settings=list(axis.line = list(col = "black")), xlab =  NULL, ylab = NULL,main=list("Tanzania", font=1))
p1<-p1+layer(sp.polygons(tz0))
p1<-p1+layer(sp.polygons(ken0c))
p1<-p1+layer(sp.polygons(ug0c))
p1<-p1+layer(sp.polygons(mz0c))
p1<-p1+layer(sp.polygons(LakVicCrop, fill="dodgerblue"))
p1<-p1+layer(sp.polygons(sme, fill="white", labels=c(sme$name)))
p1<-p1+layer(sp.polygons(smeInner, fill="light grey", labels=c(sme$name)))
p1<-p1+layer(sp.polygons(bb, col=makeTransparent("tan4"), lwd=4))
p1<-p1+layer(SpatialPolygonsRescale(layout.north.arrow(),offset = c(39,2),scale = 400))
p1

# RAIN MAP
# Colourkey
my.at <- seq(300, 1500, by = 25)
my.at2 <- seq(300, 1500, by = 500)

# Set colour range for rain
color_pallete_function <- colorRampPalette(
  colors = c('white',"gold","darkgoldenrod","darkgoldenrod1", "deepskyblue","dodgerblue1", "dodgerblue3","dodgerblue4","midnightblue"),
  space = "Lab" 
  #alpha=c(0.8)
)
#'white',"darkgoldenrod1", "deepskyblue","dodgerblue1", "dodgerblue3","dodgerblue4","midnightblue"
rain.seq<-seq(minValue(RAINstack1sum),maxValue(RAINstack1sum), length=50)
num_colors <- nlevels(as.factor(rain.seq))
num_colors
diamond_color_colors <- color_pallete_function(num_colors)

# Colourkey bar
myColorkey <- list(space="right",at=my.at, ## where the colors change
                   labels=list(axis.line = list(col = NA),my.at2), ## where to print labels
       tck = c(0,0), height=1,width=1.2, col=diamond_color_colors)

# Map settings
#33.86399, 35.96337, -3.616774, -1.418749  (xmin, xmax, ymin, ymax)
coords2 = matrix(c(33.69, -3.87,
               33.6, -1.29215,
               36.09, -1.29215,
               36.09, -3.87,
               33.69, -3.87), 
             ncol = 2, byrow = TRUE)
P2 <- Polygon(coords2)
bb2 <- SpatialPolygons(list(Polygons(list(P2), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

mapTheme <- rasterTheme(region=c(diamond_color_colors))  
RainSME<- crop(RAINstack1sum, bb2)
par.settings = list(layout.heights=list(ylab.key.padding=.05))

# Rain map
lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)

# Park boundary plot
names(mysp)
mysp@data$Landuse
myWild<-(mysp[mysp@data$Landuse =="Wildlife",])
myPast<-(mysp[mysp@data$Landuse =="Pasture",])
myAg<-(mysp[mysp@data$Landuse =="Agriculture",])
myCG<-(mysp[mysp@data$Landuse =="Common Garden",])


p3<-levelplot(RainSME,margin=F,at=my.at, colorkey=myColorkey,par.settings=mapTheme, 
xlab= NULL,ylab= NULL, main=list("Annual rainfall (mm)",font=1)) # scales = list(draw = FALSE)
p3<-p3+layer(sp.polygons(sme, labels=c(sme$name),lwd=3))
p3<-p3+layer(sp.polygons(bb, col=makeTransparent("tan4"), lwd=4))
p3<-p3+layer(sp.points(myWild, pch =24,lwd=1, cex =1.1, fill="white",col="black"))
p3<-p3+layer(sp.points(myPast, pch =22,lwd=1, cex =1.1, fill="white",col="black"))
p3<-p3+layer(sp.points(myAg, pch =23,lwd=1, cex =1.1, fill="white",col="black"))
p3<-p3+layer(sp.points(myCG, pch =21,lwd=1, cex =1.1, fill="white",col="black"))
p3


# Sampling location + protection status
# Park boundary plot
lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)

SSeroRainclip<- crop(SeroRainclip,bb3)
SSeroRainclip2<-extend(SSeroRainclip,bb)
p2<-levelplot(SSeroRainclip2, colorkey=NULL,par.settings=mapTheme)
p2<-p2+layer(sp.polygons(sme, fill="white", labels=c(sme$name)))
p2<-p2+layer(sp.polygons(smeInner, fill="light grey", labels=c(sme$name)))
p2<-p2+layer(sp.points(myWild, pch =24,lwd=1, cex =1.1, fill="white",col="black"))
p2<-p2+layer(sp.points(myPast, pch =22,lwd=1, cex =1.1, fill="white",col="black"))
p2<-p2+layer(sp.points(myAg, pch =23,lwd=1, cex =1.1, fill="white",col="black"))
p2<-p2+layer(sp.points(myCG, pch =21,lwd=1, cex =1.1, fill="white",col="black"))
p2$legend$top <- NULL
p2$legend$right<- NULL
p2

###########################################################################
#### Publication plot ####
###########################################################################

#### Tanzanian map ####
lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)

p1<-levelplot(tzSeroRain,margin=F,scales = list(draw = FALSE), colorkey=NULL,par.settings= list(axis.line = list(col = "black")), xlab =  NULL, ylab = NULL,main=list("Tanzania", font=1))
p1<-p1+layer(sp.polygons(tz0))
p1<-p1+layer(sp.polygons(ken0c))
p1<-p1+layer(sp.polygons(ug0c))
p1<-p1+layer(sp.polygons(mz0c))
p1<-p1+layer(sp.polygons(LakVicCrop,fill="dodgerblue"))
p1<-p1+layer(sp.polygons(sme, fill="white", labels=c(sme$name)))
p1<-p1+layer(sp.polygons(smeInner, fill="light grey", labels=c(sme$name)))
p1<-p1+layer(sp.polygons(bb, col=makeTransparent("black"), lwd=2))
p1<-p1+layer(SpatialPolygonsRescale(layout.north.arrow(),offset = c(39,2),scale = 400))
p1

#### RAIN MAP####
# Colourkey
my.at <- seq(300, 1500, by = 25)
my.at2 <- seq(300, 1500, by = 500)

# Set colour range for rain
color_pallete_function <- colorRampPalette(
  colors = c('white',"gold","goldenrod","darkgoldenrod1", "deepskyblue","dodgerblue1", "dodgerblue3","dodgerblue4","midnightblue"),
  space = "Lab" 
  #alpha=c(0.8)
)
#'white',"darkgoldenrod1", "deepskyblue","dodgerblue1", "dodgerblue3","dodgerblue4","midnightblue"
rain.seq<-seq(minValue(RAINstack1sum),maxValue(RAINstack1sum), length=50)
num_colors <- nlevels(as.factor(rain.seq))
num_colors
diamond_color_colors <- color_pallete_function(num_colors)

# Colourkey bar
myColorkey <- list(space="right",at=my.at, ## where the colors change
                   labels=list(axis.line = list(col = NA),my.at2), ## where to print labels
                   tck = c(0,0), height=1,width=1.2, col=diamond_color_colors)

# Map settings
#33.86399, 35.96337, -3.616774, -1.418749  (xmin, xmax, ymin, ymax)
coords2 = matrix(c(33.69, -3.87,
                   33.6, -1.29215,
                   36.09, -1.29215,
                   36.09, -3.87,
                   33.69, -3.87), 
                 ncol = 2, byrow = TRUE)
P2 <- Polygon(coords2)
bb2 <- SpatialPolygons(list(Polygons(list(P2), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

mapTheme <- rasterTheme(region=c(diamond_color_colors))  
RainSME<- crop(RAINstack1sum, bb2)
par.settings = list(layout.heights=list(ylab.key.padding=.05))

# Rain map
lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)

# Sites
myWild<-(mysp[mysp@data$Landuse =="Wildlife",])
myPast<-(mysp[mysp@data$Landuse =="Pasture",])
myAg<-(mysp[mysp@data$Landuse =="Agriculture",])
myCG<-(mysp[mysp@data$Landuse =="Common Garden",])

p3<-levelplot(RainSME,margin=F,at=my.at,scales = list(draw = FALSE), colorkey=myColorkey,par.settings=mapTheme, 
              xlab= NULL,ylab= NULL, main=list("Annual rain (mm)",font=1)) # scales = list(draw = FALSE)
p3<-p3+layer(sp.polygons(sme, labels=c(sme$name),lwd=1.5))
p3<-p3+layer(sp.polygons(bb, col=makeTransparent("black"), lwd=4))
p3<-p3+layer(sp.points(myWild, pch =24,lwd=1, cex =1.1, fill="white",col="black"))
p3<-p3+layer(sp.points(myPast, pch =22,lwd=1, cex =1.1, fill="white",col="black"))
p3<-p3+layer(sp.points(myAg, pch =23,lwd=1, cex =1.1, fill="white",col="black"))
p3<-p3+layer(sp.points(myCG, pch =21,lwd=1, cex =1.1, fill="white",col="black"))
p3

#### Sites and protection status ####
# Sampling location + protection status
# Park boundary plot
lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)
smeInnerC<-crop(smeInner,bb3)
tan.adm.centroids.df <- data.frame(longitude = coordinates(smeInnerC)[, 1],Latitude = coordinates(smeInnerC)[, 2]) 
smeInnerC@data$NAME<-droplevels(smeInnerC@data$NAME)
coordinates(tan.adm.centroids.df)
#tan.adm.centroids.df$NAME <- smeInnerC@data$NAME

longtitude <- c(34.70738, 34.55,34.72)
latitude <- c(-2.5, -3.25, -3.459)
sme.Cent<-cbind(longtitude,latitude)

# Sites
myWild<-(mysp[mysp@data$Landuse =="Wildlife",])
myPast<-(mysp[mysp@data$Landuse =="Pasture",])
myAg<-(mysp[mysp@data$Landuse =="Agriculture",])
myCG<-(mysp[mysp@data$Landuse =="Common Garden",])

SSeroRainclip<- crop(RainSME,bb)
SSeroRainclip2<-extend(SSeroRainclip,bb)

p2<-levelplot(SSeroRainclip, margin=F,at=my.at,scales = list(draw = FALSE), colorkey=myColorkey,par.settings=mapTheme, 
              xlab= NULL,ylab= NULL, main=list("Serengeti                      Annual rain (mm)",font=1)) 
p2<-p2+layer(sp.polygons(sme,lwd=1.5)) 
#p2<-p2+layer(sp.polygons(sme, fill="white", labels=c(sme@data$NAME),lwd=1.5))
#p2<-p2+layer(sp.polygons(smeInner, fill="light grey", labels=c(smeInnerC@data$NAME)))
p2<-p2+layer(sp.text(coordinates(sme.Cent), txt = c("SNP"," "," "), scale=.85))
p2<-p2+layer(sp.points(myWild, pch =24,lwd=1.5, cex =1.5, fill="white",col="black"))
p2<-p2+layer(sp.points(myPast, pch =22,lwd=1.5, cex =1.5, fill="white",col="black"))
p2<-p2+layer(sp.points(myAg, pch =23,lwd=1.5, cex =1.5, fill="white",col="black"))
p2<-p2+layer(sp.points(myCG, pch =21,lwd=1.5, cex =1.5, fill="white",col="black"))
#p2$legend$top <- NULL
#p2$legend$right<- NULL
p2<-p2+ layer({SpatialPolygonsRescale(layout.north.arrow(),offset = c(34.88,-2.34), scale=.1) }) 
p2<-p2+layer({ xs <- seq(33.96, 34.16, by=.02) # 20 km
grid.rect(x=xs, y=-3.39,
          width=.02, height=.01,
          gp=gpar(fill=rep(c('transparent', 'black'), 2)),
          default.units='native')
grid.text(x= xs, y=-3.425, c("0","","","","","10","","","","","       20 km"),
          gp=gpar(cex=.8), rot=0,
          default.units='native')
})
p2

#########################
# Grid arrange 
#########################

# Combined graphs
library(grid)
require(gridExtra)

lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=1), right.padding=list(x=.1)))

# Export - remember to set working directory
#filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Publications/TBI_landuse/", "TBI.site.map", "_",Sys.Date(), ".jpeg" )
#jpeg (filename, width=18, height=16, res=400, unit="cm")
#grid.arrange(p1,p2,p3, ncol=4, nrow=4, widths=c(1,1,1,1), heights=c(1,1,1,1),vp = grid::viewport(width=1,height=1),layout_matrix = cbind(c(1,1,2,2), c(1,1,2,2),c(3,3,2,2),c(3,3,2,2)))
#grid.arrange(p1,p2,p3, ncol=3, nrow=2, widths=c(1,1,1), heights=c(.9,1),vp = grid::viewport(width=1.01,height=1),layout_matrix = cbind(c(1,3), c(2,2),c(2,2)))
#dev.off()

# Export - remember to set working directory 2
filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Publications/TBI_landuse/", "TBI.site.map2", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=18, height=16, res=400, unit="cm")
#grid.arrange(p1,p2,p3, ncol=4, nrow=4, widths=c(1,1,1,1), heights=c(1,1,1,1),vp = grid::viewport(width=1,height=1),layout_matrix = cbind(c(1,1,2,2), c(1,1,2,2),c(3,3,2,2),c(3,3,2,2)))
grid.arrange(p1,p2, ncol=1, nrow=2, widths=c(1.5), heights=c(.9,2),vp = grid::viewport(width=1,height=1),layout_matrix = cbind(c(1,2)))
dev.off()

#########################################################################################################
#### END ####
#########################################################################################################
