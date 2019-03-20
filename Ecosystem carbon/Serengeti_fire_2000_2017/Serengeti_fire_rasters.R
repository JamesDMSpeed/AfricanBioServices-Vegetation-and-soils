#########################################################################################
# MODIS burn area product
# Fire present or absent - each year 2000 - 2017 
# Added annually
#January to March 2000 were not included as MODIS was not in operation
# June 2001 was excluded due to a satellite technical failure
# Library
library(rgdal)
library(raster)
#########################################################################################
#Cumulative fire occurrence was estimate from maps of separate years

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017")
Serlist<-list.files(getwd(), pattern="tif$", full.names=FALSE)
Serlist # 18 years

# Try a single raster from the list
a<-raster("/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017/Fire_2000_MODIS_MCD45A.tif")
b<-raster("/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017/Fire_2008_MODIS_MCD45A.tif")
c<-raster("/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017/Fire_2016_MODIS_MCD45A.tif")
d<-raster("/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017/Fire_2017_MODIS_MCD45A.tif")
extent(a) # UTM format..
extent(b) # All same format up to 2015
extent(c) # 2016 WGS format!
extent(d) # 2017 different extent

# Resolution issues as well
res(a) # 489 486
res(b) # 489 486
res(c) # 489 486
res(d) # 489 486 # Originally higher resolution...250 250 
#resolution for d 2017 is lower...

#c2016<-projectRaster(c,crs="+proj=utm +south +zone=36 +init=EPSG:21036", method = "ngb" )
#extent(c2016)

# Smaller bounding box
#Bounding box
bb <-c(xmn = 448440, xmx = 958472.8 , ymn = 9442919, ymx = 9997931)

# Crop extent for new files
#2000 - 2015 data
c2016<- crop(c,bb2)
d2017<- crop(d,bb)
extent(c2016)
extent(d2017)
#d2017<- crop(d,bb)
#extent(d2017)

# Address the resolution method of 2017 data 
c2016<- resample(c,a,method = 'bilinear')
d2017<- resample(d,a,method = 'bilinear')
res(c2016)
res(d2017) # 489 486 # no longer 250 x 250 

extent(c2016)
extent(d2017) # Same extents now

# Export formatted 2016 and 2017 
#writeRaster(c2016,"/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017/Fire_2016_MODIS_MCD45A","GTiff", overwrite=TRUE)
#writeRaster(d2017,"/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Serengeti_fire_2000_2017/Fire_2017_MODIS_MCD45A","GTiff", overwrite=TRUE)

# Extent
bb <-c(xmn = 448440, xmx = 958472.8 , ymn = 9442919, ymx = 9997931)
results <- list()
for(i in 1:length(Serlist)) {
  r <- raster(Serlist[i]) #
  e <- extent(bb)
  rc <- crop(r, e)
 
  #if(sum(as.matrix(extent(rc))!=as.matrix(e)) == 0){ # edited
  #  rc <- mask(rc, a) # You can't mask with extent, only with a Raster layer, RStack or RBrick
  #}else{
  #  rc <- extend(rc,a)
  #  rc<- mask(rc, a)
  #}
  # commented for reproducible example      
  results[[i]] <- rc # rw <- writeRaster(rc, outfiles[i], overwrite=TRUE)
  # print(outfiles[i])
}
# Loop is not working but individual crop works?!

fire_data<- stack(results)
firesum<-sum(fire_data)


# Stack geotiff # Need to ensure tif is raster format # Need to ensure ful names are preserved 
PhilipoPoints<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philipo_corners_coordinatesWGS84.csv")
PhilipoPoints<-subset(PhilipoPoints, !is.na(PhilipoPoints$Longitude))
Philp <-PhilipoPoints
coordinates(Philp)<- ~Longitude + Latitude #define which columns correspond to x's and y's

# Parameters
utmproj<-"+proj=utm +south +zone=36 +init=EPSG:21036" #spatial coordinate reference system in Serengeti
latlongproj<-("+proj=longlat +datum=WGS84")
proj4string(Philp)<-latlongproj #define projection (originally defined above)
Philp2<-spTransform(Philp,utmproj)
extent(Philp2) 

wp222SP<-read.csv("WP222.sample.points.csv")
PhilDATA<- read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philipo_corners_coordinatesWGS84.csv")
PhilDATA<-subset(PhilDATA, !is.na(PhilDATA$Longitude))
PhilDATA$Longitude<-Philp2$Longitude
PhilDATA$Latitude<-Philp2$Latitude

mysp<- SpatialPointsDataFrame(Philoc,PhilDATA, proj4string=CRS(utmproj),match.ID = TRUE)

# Extract rainfall data from brick object
CumFire<- extract(firesum,mysp,sp=T, method='simple')
head(CumFire)

# Year of last fire
brk<-brick(fire_data)
YearofLast<- extract(brk,mysp,sp=T, method='simple')
head(YearofLast)

# Export Fire
#library(xlsx)
#write.xlsx(CumFire, "/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/Cumulative_fire.xlsx")
#write.xlsx(YearofLast, "/Users/anotherswsmith/Documents/AfricanBioServices/Mapping/Basline ABS_maps/Fire/YearofLast_fire.xlsx")
