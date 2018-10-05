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
library(lubridate)
library(data.table)  
library(ggplot2)
library(grid)
library(egg)
library(ggpubr)
################################################################

# Import data seasonal biomass, dung, PRC and environmental data
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/GitHubVegSoils/AfricanBioServices-Vegetation-and-soils-master/Moveable exclosures")
OnMove<-read.csv(file="Movable.exclosure.data.FULL.csv", sep=";",header=TRUE)

# Explore dataset
names(OnMove)
str(OnMove)
dim(OnMove)

# H0 = 20 samples
# H1-H7 harvest 48 samples 
names(OnMove)
OnMove$site.name
# Count function
length(OnMove$harvest)
length(OnMove$site.name)
#Total.NP<-count(OnMove, c("site.name","harvest")) # Total - 20, 48 etc.

Total.NP<-table(OnMove$harvest,OnMove$site.name)
Total.NP<-as.data.frame(Total.NP)

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
TotalNP$freq.total<-Total.NP$Freq

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

# How many subsamples are at SUA/NTNU
count(OnMove$Sub.SUA)
class(OnMove$Sub.NTNU)
SubsamplesSUA<-OnMove[!is.na(OnMove$Sub.SUA), ] # Remove NAs
SubsamplesSUA<-droplevels(Subsamples)
dim(SubsamplesSUA$Sub.SUA)# 226

SubsamplesNTNU<-OnMove[!is.na(OnMove$Sub.NTNU), ] # Remove NAs
SubsamplesNTNU<-droplevels(SubsamplesNTNU)
dim(SubsamplesNTNU$Sub.NTNU)# 138

# Target NTNU and SUA
Sub2Institues<-OnMove[OnMove$Sub.SUA>.1 & OnMove$Sub.NTNU>.1,]
Sub2Institues<-subset(OnMove, Sub.SUA >.1 | Sub.NTNU>.1) 
Sub2Institues<-Sub2Institues[!is.na(Sub2Institues$Sub.NTNU), ]
Sub2Institues<-Sub2Institues[!is.na(Sub2Institues$Sub.SUA), ]
dim(Sub2Institues)  # 70 target shared
SharedMissingN<-Sub2Institues[is.na(Sub2Institues$N.target), ]
dim(SharedMissingN) # 9 target

# Non-target (other) NTNU and SUA
Sub2Institues1<-OnMove[OnMove$Sub.SUA.1>.1 & OnMove$Sub.NTNU.1>.1,]
Sub2Institues1<-subset(OnMove, Sub.SUA.1 >.1 | Sub.NTNU.1>.1) 
Sub2Institues1<-Sub2Institues1[!is.na(Sub2Institues1$Sub.NTNU.1), ]
Sub2Institues1<-Sub2Institues1[!is.na(Sub2Institues1$Sub.SUA.1), ]
dim(Sub2Institues1)  # 119 non-target shared 

SharedMissingNother<-Sub2Institues1[is.na(Sub2Institues1$N.other), ]
dim(SharedMissingNother) # 18 non-target


MissingSamplesSHARED<-rbind(SharedMissingN,SharedMissingNother)
# Export missing samples
write.table(MissingSamplesSHARED, file = "MissingSamplesSHARED.csv",row.names=T, na="",col.names=T, sep=";")
