#### Nutrient exploration ####
########################
#clear system & add package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(ggplot2)
library(dplyr)
library(gcookbook)
library(readr)
########################

#Data.Ex<-read.csv("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/GitHub/AfricanBioServices-Vegetation-and-soils/Moveable exclosures/Exclosure.data.biomass.csv", sep=",",header=TRUE)
Datanutr <- read.csv("Movable.exclosure.data.Nov.csv",sep=";")
Datanutr <- Datanutr[-c(19:32,34,35,37,38,41,44:50,52:55)] #Excluding these columns

#Importing updated file
Ex.data<- read_csv("Movable.exclosure.data.dec.csv")
View(Ex.data)
names(Ex.data)
Datanutr <- Ex.data[-c(11:13,19:32,34,35,37,38,56:59)] #Excluding these columns
#Removing Ex2#
Datanutr <- Datanutr[treatment!="EX2",]
#Removing H0
Datanutr <- Datanutr[treatment!="H0",]
Datanutr <- Datanutr[-c(281:336),] #removing the NA rows in the end
levels(Datanutr$treatment)

####Making new columns for total N and P #### 
Datanutr$N.target.biom <- N.target.SUA*biomass.target.sp./100
Datanutr$N.other.biom <- N.other.SUA*biomass.other.sp./100
Datanutr$P.target.biom <- P.target.SUA*biomass.target.sp./100
Datanutr$P.other.biom <- P.otherSUA*biomass.other.sp./100
Datanutr$N.total <- (Datanutr$N.target.biom+Datanutr$N.other.biom)/Datanutr$biomass.total.g*100
Datanutr$P.total <- (Datanutr$P.target.biom+Datanutr$P.other.biom)/Datanutr$biomass.total.g*100
View(Datanutr)

####Checking the data in total nutrient columns #### 
colSums(is.na(Datanutr)) #246 missing for N and P total
attach(Datanutr)

dotchart(N.total)
plot(N.total)        
#identify(N.total) 
dotchart(P.total)
plot(P.total)
identify(P.total) #To identify the outliers. Click on outliers to define, then esc. 
Datanutr[c(117,170,243,253),] #Looking at these rows, from Handajega H3, Handajega H4, Makao H5, Seronera H5




#### Mean and SE N.total per region ####
SE <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
names(Datanutr)
Data.N.mean <- aggregate(N.total~region+landuse+treatment+harvest,Datanutr,mean) #Mean
Data.N.SE <- aggregate(N.total~region+landuse+treatment+harvest,Datanutr,SE) #SE
aggregate(N.total~landuse+treatment+harvest,Datanutr,length) #Number of obs
Data.N.mean$SE <- Data.N.SE$N.total

#Plotting N.total#
names(Data.N.mean)

Nitrogen <- ggplot(Data.N.mean,aes(x=harvest,y=N.total,ymin=N.total-SE,ymax=N.total+SE,
                                   col=landuse,
                                   shape=treatment))

Nitrogen <- Nitrogen+geom_point(size=5,stroke=1.2,position=position_dodge(width=.35),show.legend=T) #Adding point
Nitrogen <- Nitrogen+geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F) #Adding errorbar
Nitrogen <- Nitrogen+facet_grid(harvest~region,scale="fixed",labeller=label_both)

Nitrogen

#### Mean and SE Nitrogen per landuse ####
names(Datanutr)
Data.N.mean2 <- aggregate(N.total~landuse+treatment+harvest,Datanutr,mean)
Data.N.SE2 <- aggregate(N.total~landuse+treatment+harvest,Datanutr,SE)
Nitrogen.obs <- aggregate(N.total~landuse+treatment+harvest,Datanutr,length)
write.csv(Nitrogen.obs,file="Nitrogen.obs.Nov.csv")
aggregate(N.total~landuse+treatment+harvest,Datanutr,range)
Data.N.mean2$SE <- Data.N.SE2$N.total

#Plotting N.total#
Nitrogen2 <- ggplot(Data.N.mean2,aes(x=harvest,y=N.total,ymin=N.total-SE,ymax=N.total+SE,
                                   col=landuse,
                                   shape=treatment))

Nitrogen2 <- Nitrogen2+geom_point(size=5,fill="white",stroke=1.2,position=position_dodge(width=.5),show.legend=T) #Adding point
Nitrogen2 <- Nitrogen2+geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.5),show.legend=F) #Adding errorbar
#Nitrogen2 <- Nitrogen2+facet_grid(harvest~landuse,scale="fixed",labeller=label_both)
Nitrogen2<-Nitrogen2+scale_shape_manual(values=c(15,1)) #adding squares and circles
Nitrogen2<- Nitrogen2+scale_colour_manual(values=c( "tan3","turquoise3"))#adding right colors for landuse
Nitrogen2 <- Nitrogen2+ylab("Nitrogen (%)")
Nitrogen2


#### P Mean and SE#### 
Data.P.mean <- aggregate(P.total~landuse+treatment+harvest,Datanutr,mean)
Data.P.SE <- aggregate(P.total~landuse+treatment+harvest,Datanutr,SE)

Phosphorus.obs <- aggregate(P.total~landuse+treatment+harvest,Datanutr,length) #Number of obs for each site/harvest
Data.P.mean$SE <- Data.P.SE$P.total


#Plotting P.total#
Phosphorus <- ggplot(Data.P.mean,aes(x=harvest,y=P.total,ymin=P.total-SE,ymax=P.total+SE,
                                     col=landuse,
                                     shape=treatment))

Phosphorus <- Phosphorus+geom_point(size=5,fill="white",stroke=1.2,position=position_dodge(width=.5),show.legend=T) #Adding point
Phosphorus <- Phosphorus+geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.5),show.legend=F) #Adding errorbar
#Phosphorus <- Phosphorus+facet_grid(harvest~landuse,scale="fixed",labeller=label_both)
Phosphorus<-Phosphorus+scale_shape_manual(values=c(15,1)) #adding squares and circles
Phosphorus<- Phosphorus+scale_colour_manual(values=c( "tan3","turquoise3"))#adding right colors for landuse
Phosphorus <- Phosphorus+ylab("Phosphorus (%)")
Phosphorus

----------------------------
#### Testing nutrient file ####


Nutrients_test <- read_csv("Moveable exclosures/Nutrients_test.csv", 
                             col_types = cols(P.other.NMBU = col_number(), 
                             P.other.USDM = col_number()))
dotchart(Nutrients_test$P.target.NMBU)
class(Nutrients_test)
