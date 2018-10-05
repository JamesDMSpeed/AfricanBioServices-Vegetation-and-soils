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
OnMove<-read.csv(file="Moveable exclosures/Movable.exclosure.data.FULL.csv", sep=",",header=TRUE)

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

##############################################
####  Sub-sampling for NTNU CNP analysis ####
##############################################

##############################################
####  Samples with issues at SUA ####
##############################################
# How many subsamples are at SUA/NTNU
# BASED ON TARGET SUB SUA AND SUB NTNU
SubsamplesSUA<-OnMove[!is.na(OnMove$Sub.SUA), ] # Remove NAs
SubsamplesSUA$harvest
dim(SubsamplesSUA)# 226 
SubsamplesSUA<-SubsamplesSUA[!SubsamplesSUA$harvest=="H5" & !SubsamplesSUA$harvest=="H6" & !SubsamplesSUA$harvest=="H7",]
SubsamplesSUA<-droplevels(SubsamplesSUA)
dim(SubsamplesSUA)# 118

SubsamplesNTNU<-OnMove[!is.na(OnMove$Sub.NTNU), ] # Remove NAs
SubsamplesNTNU<-droplevels(SubsamplesNTNU)
dim(SubsamplesNTNU)# 138

# Remove H5, 6 and 7
OnMove2<-OnMove[!OnMove$harvest=="H5" & !OnMove$harvest=="H6" & !OnMove$harvest=="H7",]
OnMove2<-droplevels(OnMove2)

# Target NTNU and SUA
Sub2Institues<-OnMove2[OnMove2$Sub.SUA>.1 & OnMove2$Sub.NTNU>.1,]
Sub2Institues<-subset(OnMove2, Sub.SUA >.1 | Sub.NTNU>.1) 
Sub2Institues<-Sub2Institues[!is.na(Sub2Institues$Sub.NTNU), ]
Sub2Institues<-Sub2Institues[!is.na(Sub2Institues$Sub.SUA), ]
dim(Sub2Institues)  # ****70 target shared****
SharedMissingN<-Sub2Institues[is.na(Sub2Institues$N.target), ]
dim(SharedMissingN) # ****9 target OVERLAP AND ISSUE ****

# Non-target (other) NTNU and SUA
Sub2Institues1<-OnMove2[OnMove2$Sub.SUA.1>.1 & OnMove2$Sub.NTNU.1>.1,]
Sub2Institues1<-subset(OnMove2, Sub.SUA.1 >.1 | Sub.NTNU.1>.1) 
Sub2Institues1<-Sub2Institues1[!is.na(Sub2Institues1$Sub.NTNU.1), ]
Sub2Institues1<-Sub2Institues1[!is.na(Sub2Institues1$Sub.SUA.1), ]
dim(Sub2Institues1)  # 119 non-target shared 

SharedMissingNother<-Sub2Institues1[is.na(Sub2Institues1$N.other), ]
dim(SharedMissingNother) # ******18 non-target*******


MissingSamplesSHARED<-rbind(SharedMissingN,SharedMissingNother)
dim(MissingSamplesSHARED)# 27 samples with issues

# Export missing sample from SUA that overlap with NTNU 
write.table(MissingSamplesSHARED, file = "Moveable exclosures/MissingSamplesSHARED.csv",row.names=T, na="",col.names=T, sep=";")

##############################################################
####  Samples no issues at SUA - retest for equipment ####
##############################################################

OnMoveOK<-OnMove[!MissingSamplesSHARED$plot.id, ]
subset(!MissingSamplesSHARED$plot.id)

library(dplyr)
OnMove2<-OnMove[!OnMove$harvest=="H5" & !OnMove$harvest=="H6" & !OnMove$harvest=="H7",] # Remove H5, H6, H7
OnMove2Pt<-OnMove2[!is.na(OnMove2$P.target), ] #Remove any target P without value
OnMove2Pt$P.target
OnMove2PtOK<-OnMove2Pt %>% filter(!plot.id %in% MissingSamplesSHARED$plot.id) # Remove samples codes with issues
OnMove2PtOK$P.target
dim(OnMove2PtOK) # 82
OnMove2PtOK$P.target 
OnMove2PtOKb<-OnMove2PtOK[rev(order(OnMove2PtOK$P.target)),]
OnMove2PtOKb$P.target
# 70 - 9 = 61

OnMove2Po<-OnMove2[!is.na(OnMove2$P.other), ] #Remove any target P without value
OnMove2Po$P.other
OnMove2PoOK<-OnMove2Po %>% filter(!plot.id %in% MissingSamplesSHARED$plot.id) # Remove samples codes with issues
OnMove2PoOK$P.other
dim(OnMove2PoOK) # 131
OnMove2PoOK$P.other #
OnMove2PoOKb<-OnMove2PoOK[rev(order(OnMove2PoOK$P.other)),]
OnMove2PoOKb$P.other # 131

# Select only relevant columns # Create a target and other column
names(OnMove2PtOKb)
OnMove2PtOKb$target
MyVar<-c("plot.id","block.id", "harvest","P.target","P.other")
OverlapPtarget<-OnMove2PtOKb[ ,MyVar]
OverlapPother<-OnMove2PoOKb[ ,MyVar]
OverlapPtarget$fx<-"target"
OverlapPother$fx<-"other"

# Combine target and non-target P samples
SUA_P_overlap<-rbind(OverlapPtarget,OverlapPother)

# Export overlap samples between SUA and NTNU - select 16 for reanalysis
write.table(SUA_P_overlap, file = "Moveable exclosures/SUA_P_overlap.csv",row.names=F, na="",col.names=T, sep=";")
# Please select from target and other category 

##############################################################
####  End  ####
##############################################################