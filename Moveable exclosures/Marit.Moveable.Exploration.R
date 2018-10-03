#################################################################
#Serengeti moveable exclosure
# Marit
#02/10/2018 
################################################################
#clear system & package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(dplyr)
#library(plyr)
library(lubridate)
library(data.table)  
library(ggplot2)
library(grid)
library(egg)
library(ggpubr)
################################################################

# Import data seasonal biomass, dung, PRC and environmental data
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_GitHub/AfricanBioServices-Vegetation-and-soils/Moveable exclosures")
OnMove<-read.csv(file="Movable.exclosure.data.csv", sep=",",header=TRUE)

# Explore dataset
names(OnMove)
str(OnMove)
dim(OnMove)

# H0 = 20 samples
# H1-H7 harvest 48 samples 
names(OnMove)
OnMove$site.name
# Count function
OnMove$harvest
Total.NP<-count(OnMove, c("site.name","harvest")) # Total - 20, 48 etc.

# Count whether missing or not
TargetNP<-table(OnMove$harvest,OnMove$site.name, !is.na(OnMove$N.target))
OtherNP<-table(OnMove$harvest,OnMove$site.name, !is.na(OnMove$N.other))
# dataframe
TargetNP<-as.data.frame(TargetNP)
OtherNP<-as.data.frame(OtherNP)
# Make column for function - target vs other
TargetNP$plant.fx<-"target"
OtherNP$plant.fx<-"other"
#Combin
TotalNP<-rbind(TargetNP,OtherNP)
colnames(TotalNP)<-c("harvest","site.name","absence","freq","plant.fx")
levels(TotalNP$absence)<-c("Absence","Prescence")
TotalNP$freq.total<-Total.NP$freq

TotalNP$prop<-TotalNP$freq/TotalNP$freq.total*100

TargetAb<-TotalNP[TotalNP$absence=="Absence" & TotalNP$plant.fx=="target",]
TargetAbPlot<-ggplot(TargetAb,aes(x=harvest,y=prop, colour=site.name))+
  ylab("Missing target prop")+ggtitle("Missing target N and P")+
geom_jitter(stat="identity", size=5, fill="white", shape=21, stroke=1, show.legend=F) +theme_bw()


OtherAb<-TotalNP[TotalNP$absence=="Absence" & TotalNP$plant.fx=="other",]
OtherAbPlot<-ggplot(OtherAb,aes(x=harvest,y=prop, colour=site.name))+ggtitle("Missing other N and P")+
  ylab("Missing otherprop")+
  geom_jitter(stat="identity", size=5, fill="white", shape=21, stroke=1) +theme_bw()

# Combine plots
egg::ggarrange(TargetAbPlot,OtherAbPlot, ncol=2)

# Export jpeg
#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_GitHub/AfricanBioServices-Vegetation-and-soils/Moveable exclosures/MissingNP.jpeg",
#       width= 18, height = 12,units ="cm",
#       dpi = 600, limitsize = TRUE)

# Nicer way to do this with dplyer
NtargetSummary<-OnMove  %>% group_by (harvest) %>%
  summarise(harvest, count(N.target))
  
  summarise(total.count=n(),count=sum(is.na(N.target)))

