###############################################################
# Generalized additive mixed model (GAMM)
# Need to include a spatial component - residuals correlate with Lat/long 
#library(devtools)
#devtools::install_url("https://www.math.ntnu.no/inla/R/stable/bin/macosx/contrib/3.3/INLA_0.0-1483775362.tgz")
#INLA_0.0-1485844051.tgz	# More recent
# Clean workspace
rm(list=ls())
library(lattice)
library(MASS)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(plyr)
library(dplyr)
library(Hmisc)
library(data.table)
library(emmeans)
library(nlme)
library(MuMIn)
library(INLA)
library(mgcv)
library(gstat)
library(sp)
library(sjstats)
###############################################################
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/")
#Loading masslossdata
Massloss <-read.csv('Termites/Main & CG experiment/Massloss_data_CGMain.csv', sep=';',dec='.')
#Loading rainfalldata
precip <- read.csv('Termites/Precipitation data/Rainfalldata.csv', sep=';',dec='.')
#Loading soil texture data
soiltext <- read.csv('Termites/Soil data/Soil_texture_corrected.csv', sep=',',dec='.')
#Loading Soil Nutrient data
soilnut <- read.csv('Termites/Soil data/Soil_Nutrient.csv', sep=';',dec='.')

Massloss$Massloss.per <- (-1)*((((Massloss$Ashed.final.corrected.weight..tea.only..g.-Massloss$Ashed.initial.corrected.weight..tea.only..g.)/(Massloss$Ashed.initial.corrected.weight..tea.only..g.)))*100)
Massloss$rain.sum..mm. <- NULL
Massloss$OC.. <- NULL
Massloss$Soil.Class <- NULL
Massloss$SAND.. <- NULL
Massloss$SILT.. <- NULL
Massloss$CLAY.. <- NULL

dim(Massloss) # 1760 
summary(is.na(Massloss$Massloss.per)) # 1604 litterbags

####Merge data####
Data <- Massloss
DataCG<-droplevels(Data[Data$Experiment=="CG",]) # Only commongarden data
levels(DataCG$Landuse)[levels(DataCG$Landuse)=="Seronera"] <- "Wild"
#Creating a block nr column in CG data to correspond to Main data, before merging later.
#But first, renaming the column already called block to CGblock (Corresponds to the 4 blocks within the CG)
colnames(DataCG)[(names(DataCG) == "Block")] <- "CGBlock"

#Extract last number from Blockcode to create blocknr column:
DataCG$Block <- sub(".*(?=.$)","",perl = T, DataCG$Blockcode)

#Renaming Block nr for all Local soil to 1 as I want to have avarage among the four blocks.

#Subset of local soil from CG 
LocalCGsoil <- DataCG[DataCG$Site=="Seronera",]
#Removing the subsetted data from the whole CG data, then putting in the adjusted subset:

DataMain<-droplevels(Data[Data$Experiment=="Main",]) #Only landuse experiement data

levels(Data$Landuse) # All levels incl. common garden
levels(DataMain$Landuse)#Common Garden removed
levels(DataCG$Landuse)#All landuses included, but only for CG experiment
#Adding adjsted block numbering in CG back into Main experiemnt for a full dataset:
DataMain$CGBlock <- NA

Fulldata <- rbind(DataMain,DataCG) #Same as Data df, but now with adjusted Block numbers in CG local soil

#Adding Soildata (texture and nutrient) and rainfalldata to masslossdata:
levels(soilnut$Landuse)[levels(soilnut$Landuse)=="Seronera"] <- "Wild"
levels(soiltext$Landuse)[levels(soiltext$Landuse)=="Common Garden"] <- "Wild"
levels(soilnut$Landuse)
levels(soiltext$Landuse)

landuselvl <- levels(soilnut$Landuse) 
soiltext$Landuse <- factor(soiltext$Landuse,levels= landuselvl)

levels(soiltext$Region)[levels(soiltext$Region)=="Dry-Wet"] <- "Intermediate"
levels(soilnut$Region)
levels(soiltext$Region)

soilnut$Block <- as.factor(soilnut$Block)
soiltext$Block <- as.factor(soiltext$Block)
levels(soilnut$Block)
levels(soiltext$Block)
names(soiltext)
names(soilnut)
Soildata <- left_join(soilnut,soiltext, by=c("Site","Region","Landuse","Block"))
Soildata$X <- NULL
Soildata$Stuart_correction <- NULL
Soildata$ID_SUA <- NULL

Fulldata$Site <- as.factor(Fulldata$Site)
Soildata$Site <- as.factor(Soildata$Site)
levels(Fulldata$Site)
levels(Soildata$Site)

levels(Fulldata$Landuse)
levels(Soildata$Landuse)

Fulldata$Block <- as.factor(Fulldata$Block)
Fulldata$Region <- as.factor(Fulldata$Region)
Soildata$Region <- as.factor(Soildata$Region)
Fulldata$Region <- factor(Fulldata$Region,levels=c("Dry","Intermediate","Wet"))
levels(Fulldata$Region)
levels(Soildata$Region)
Fulldata$Blockcode <- as.factor(Fulldata$Blockcode)
Soildata$Blockcode <- as.factor(Soildata$Blockcode)
Blockcodelvl <- levels(Fulldata$Blockcode) #Assign the levelorder in one dataset
Soildata$Blockcode <- factor(Soildata$Blockcode,levels= Blockcodelvl) #...And assign that level to the other dataset

Fulldata <- left_join(Fulldata,Soildata,by=c("Season","Site","Region","Landuse","Block"))

which(is.na(Fulldata$Claycorr))#Checking that the join operation went OK. SHould be zero NA's

Fulldata <- left_join(Fulldata,precip, by=c("Season","Site"))
which(is.na(Fulldata$precip))

#Add new Block.ID to account for that Makao and Mwantiba has both Ag and Pasture.
#So need to create a new column where Makao + Pasture =Makao.p, etc...
Fulldata$Site.ID <- as.factor(with(Fulldata, ifelse(Site %in% c("Makao") &Landuse == "Pasture", "Makao.p",
                    ifelse(Site %in% c("Makao") & Landuse == "Agriculture", "Makao.ag",
                    ifelse(Site %in% c("Mwantimba") & Landuse == "Pasture", "Mwantimba.p",
                     ifelse(Site %in% c("Mwantimba") & Landuse == "Agriculture","Mwantimba.ag",
                     ifelse(Site == "Handajega","Handajega",ifelse(Site == "Maswa", "Maswa",
                     ifelse(Site == "Seronera","Seronera","WRONG")))))))))
levels(Fulldata$Site.ID)                          
table(Fulldata$Site.ID,Fulldata$Landuse) #OK

Fulldata$X <- NULL
Fulldata$Blockcode.y <-  NULL
colnames(Fulldata)[(names(Fulldata) == "Blockcode.x")] <- "Blockcode"
Fulldata$CGBlock.y <- NULL
Fulldata$CGBlock <- NULL
Fulldata$CGBlock.x <- NULL

#SEP EXP'S####
DataCG<-droplevels(Fulldata[Fulldata$Experiment=="CG",]) # Only commongarden data
DataMain<-droplevels(Fulldata[Fulldata$Experiment=="Main",]) #Only landuse experiement data

#Housekeeping # Ensuring factors are factors####
names(Data)
DataMain$fsite<-as.factor(DataMain$Site)
DataMain$fregion<-as.factor(DataMain$Region)
DataMain$fseason<-as.factor(DataMain$Season)
DataMain$flanduse<-as.factor(DataMain$Landuse)
DataMain$ftreatment<-as.factor(DataMain$Treatment)
DataMain$flittertype<-as.factor(DataMain$Littertype) 
DataMain$fplot.id<-as.factor(DataMain$Plot) # 220 plots # correct
DataMain$fteabag_code<-as.factor(DataMain$Teabag.code) # 879 teabags - missing 1 teabag (Maybe one is a duplicate?)
DataMain$fsoil.class<-as.factor(DataMain$Soil.Class)
DataMain$fmound_type<-as.factor(DataMain$Mound.type)
DataMain$ftree_ants<-as.factor(DataMain$Tree.with.ants)
DataMain$fblock<-as.factor(DataMain$Block)
DataMain$fblockcode<-as.factor(DataMain$Blockcode)
DataMain$fholes<-as.factor(DataMain$Sign.of.hole.s.)
DataMain$fcheeting<-as.factor(DataMain$Sign.of.termite.cheeting)
DataMain$froots<-as.factor(DataMain$Sign.of.roots)

#########################################################################################################
#### Graphs  ####
#########################################################################################################

#Raw mass loss graph #### 

DataMain2<-droplevels(Fulldata[Fulldata$Experiment=="Main",]) #Only landuse experiement data

# Main experiment means and standard error (exclude blocks) # From Stu: Need to seperate out Agricutlure in Makao and Mwantimba(WHY?)
names(DataMain2)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

#Creating means by landuse (excluding blocks)
DataMainmean<-aggregate(Massloss.per~Season+Region+Treatment+Littertype+Landuse, DataMain, mean)
DataMainse <-aggregate(Massloss.per~Season+Region+Treatment+Littertype+Landuse, DataMain, se)
#Creating new column with the SE in the Mainmean dataset.
DataMainmean$SE <- DataMainse$Massloss.per 
# Fill by termite * landuse = empty = absence, filled = prescence 
DataMainmean$tea.hole<-as.factor(with(DataMainmean, paste(Littertype, Treatment, sep=" ")))
levels(DataMainmean$tea.hole)

DataMainmean$Region <- as.factor(DataMainmean$Region)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(DataMainmean$Region)
levels(DataMainmean$Season)
Mainexp <- DataMainmean
#Now, ready for graphing: Main experiment massloss against landuse
#Want to reorder tea.hole to have a nicer legend:
Mainexp$tea.hole <- ordered(Mainexp$tea.hole, levels=c("Green Exclosed", "Rooibos Exclosed","Green Open","Rooibos Open"))
levels(Mainexp$tea.hole)

colnames(Mainexp)[1]<-"Season"
colnames(Mainexp)[2]<-"Region"
names(Mainexp)

Mainp <- ggplot(data=Mainexp, aes(x=Landuse,y=Massloss.per,ymin=(Massloss.per-SE),ymax=(Massloss.per+SE),
                                  fill = tea.hole,color = Littertype,shape=Landuse))
Mainp <- Mainp+geom_errorbar(width=.9,lwd=1,position=position_dodge(width=.35),show.legend=F)
Mainp <- Mainp+geom_point(size=4,stroke=1.2,position=position_dodge(width=.35),show.legend=T)
Mainp <- Mainp+facet_wrap(Region ~ Season, scale ="fixed", ncol=1, labeller=labeller(Region = c(`Dry`= "Dry Region", `Wet`="Wet Region"),
                                                                             Season = c(`Wet`= "Wet Season", `Dry`="Dry Season")))
Mainp <- Mainp+scale_color_manual(values=c("green4", "orangered3"))
Mainp <- Mainp+scale_fill_manual(values=c("green4","orangered3","white","white"))
Mainp <- Mainp+scale_shape_manual(values=c(21,22,24))
Mainp <- Mainp+guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)
Mainp <- Mainp+scale_y_continuous(limits = c(0,100), expand = c(0,0),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80))
Mainp <- Mainp+xlab("Land-use")+ylab("Mass loss (%)")
Mainp <- Mainp+theme(rect = element_rect(fill ="transparent")
                     ,panel.background=element_rect(fill="transparent")
                     ,plot.background=element_rect(fill="transparent",colour=NA)
                     ,panel.grid.major = element_blank()
                     ,panel.grid.minor = element_blank()
                     ,panel.border = element_blank()
                     ,panel.grid.major.x = element_blank()
                     ,panel.grid.major.y = element_blank()
                     ,axis.text=element_text(size=12,color="black")
                     ,axis.title.y=element_text(size=14,color="black")
                     ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
                     ,axis.text.x = element_text(size=10,color="black",angle =90,
                                                 margin=margin(2.5,2.5,2.5,2.5,"mm"))
                     ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
                     ,axis.ticks.length=unit(-1.5, "mm")
                     #,axis.line.y = element_blank()
                    # ,axis.line.x = element_blank()
                     ,plot.margin = unit(c(8,50,5,5), "mm")
                     ,strip.background = element_blank()#element_rect(fill="transparent",colour=NA)
                     ,strip.text.x = element_blank()#element_text(size = 14,colour = "black")
                     ,strip.text.y = element_blank()#element_text(size = 14,colour = "black")
                     ,panel.spacing = unit(1, "lines")
                     ,legend.background = element_rect(fill = "transparent")
                     ,legend.title=element_blank()
                     ,legend.position = c(1.06,0.5)
                     ,legend.spacing.y = unit(-0.8, "mm")
                     ,legend.key.height=unit(7.5,"mm")
                     ,legend.key.width=unit(7.5,"mm")
                     ,legend.key = element_rect(colour = NA, fill = NA)
                     ,legend.key.size = unit(7,"mm")
                     ,legend.text=element_text(size=12,color="black")
                     
)#+
  
 # annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
#  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
 # annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  #annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15)

Mainp

#5     Dry    Dry      Open      Green Agriculture    24.347765 3.9036820       Green Open
#13    Dry    Dry      Open    Rooibos Agriculture    32.199632 7.7388356     Rooibos Open
#21    Dry    Dry      Open      Green     Pasture    13.251911 2.3074455       Green Open
#29    Dry    Dry      Open    Rooibos     Pasture    28.581530 6.7125496     Rooibos Open
#37    Dry    Dry      Open      Green        Wild     8.981508 1.8180347       Green Open
#45    Dry    Dry      Open    Rooibos        Wild    11.830754 3.0975844     Rooibos Open

32.199632-24.347765 # 8 % more in ag
28.581530-13.251911 # 15% more in pasture
11.830754- 8.981508 # 3% more wildlife protected

### Barplots: termite effect ####

#Seperate out datasets####
Greenop<-DataMain[DataMain$Littertype=="Green" & DataMain$Treatment=="Open",] # Only Green Open data
Greenex<-DataMain[DataMain$Littertype=="Green" & DataMain$Treatment=="Exclosed",] # Only Green Open data
Redop<-DataMain[DataMain$Littertype=="Rooibos" & DataMain$Treatment=="Open",] # Only Green Open data
Redex<-DataMain[DataMain$Littertype=="Rooibos" & DataMain$Treatment=="Exclosed",] # Only Green Open data
names(Greenop)

#Creating dataframe with termite and microbe effect variable=Exlosed and Termite effect=Open minus exclosed):
GreendataT <- Greenop
GreendataM <- Greenop

GreendataM$Massloss.per <- Greenex$Massloss.per
GreendataT$Massloss.per <- abs(Greenop$Massloss.per-Greenex$Massloss.per)
GreendataT$Massloss.per <- (Greenop$Massloss.per-Greenex$Massloss.per)
GreendataT$Massloss.per[GreendataT$Massloss.per<0] <- 0

ReddataT <- Redop
ReddataM <- Redop
ReddataM$Massloss.per<-Redex$Massloss.per
ReddataT$Massloss.per<- (Redop$Massloss.per-Redex$Massloss.per)
ReddataT$Massloss.per[ReddataT$Massloss.per<0] <- 0

#Means and error
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
#Redtea
MeanReddataM<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataM,mean)
MeanReddataMSE<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataM,se)
MeanReddataM$SE<-MeanReddataMSE$Massloss.per
MeanReddataM$Decomposer <- "Microbe"

MeanReddataT<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataT,mean)
MeanReddataTSE<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataT,se)
MeanReddataT$SE<-MeanReddataTSE$Massloss.per
MeanReddataT$Decomposer <- "Termite"


#combine decomposition of red tea by termite and microbe:
ReddataTM <- rbind(MeanReddataM,MeanReddataT)
ReddataTM$Littertype <- "Recalcitrant"
#Green tea
MeanGreendataM<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataM,mean)
MeanGreendataMSE<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataM,se)
MeanGreendataM$SE<-MeanGreendataMSE$Massloss.per
MeanGreendataM$Decomposer <- "Microbe"

MeanGreendataT<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataT,mean)
MeanGreendataTSE<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataT,se)
MeanGreendataT$SE<-MeanGreendataTSE$Massloss.per
MeanGreendataT$Decomposer <- "Termite"

#Combine decomposition of red tea by termite and microbe:
GreendataTM <- rbind(MeanGreendataM,MeanGreendataT)
GreendataTM$Littertype <- "Labile"

#Combine both littertypes:
TMMasslossMain <- rbind(GreendataTM,ReddataTM) 
TMMasslossMain$Decomposer<-as.factor(TMMasslossMain$Decomposer)
levels(TMMasslossMain$Decomposer)

#Termite effect Dataset (CG+Main)####
Greenop<-Fulldata[Fulldata$Littertype=="Green" & Fulldata$Treatment=="Open",] # Only Green Open data
Greenex<-Fulldata[Fulldata$Littertype=="Green" & Fulldata$Treatment=="Exclosed",] # Only Green Open data
Redop<-Fulldata[Fulldata$Littertype=="Rooibos" & Fulldata$Treatment=="Open",] # Only Green Open data
Redex<-Fulldata[Fulldata$Littertype=="Rooibos" & Fulldata$Treatment=="Exclosed",] # Only Green Open data

#For green littertype dataset
length(Greenop$Massloss.per)#440
length(Greenex$Massloss.per)#440
#Joining the data:
GreenOpEx <- cbind(Greenop,Greenex$Massloss.per)
#Renaming columns to avoid confusion:
colnames(GreenOpEx)[(names(GreenOpEx) == "Massloss.per")] <- "Open.Massloss"
colnames(GreenOpEx)[(names(GreenOpEx) == "Greenex$Massloss.per")] <- "Excl.Massloss"
GreenOpEx$Massloss..g..Ash.uncorrected <- NULL

#Create termite effect variable for green:
GreenOpEx$Termite.effect <- (GreenOpEx$Open.Massloss-GreenOpEx$Excl.Massloss)

length(GreenOpEx$Termite.effect[GreenOpEx$Termite.effect < 0]) #268 values are negative (excl>op)
#Above half of the litter has no termite effect 268/440 teabags.
LabileTermEff <- GreenOpEx
#Setting all negative values below zero:
LabileTermEff$Termite.effect[LabileTermEff$Termite.effect < 0] <- 0

#For red littertype dataset
length(Redop$Massloss.per)#440
length(Redex$Massloss.per)#440
#Joining the data:
RedOpEx <- cbind(Redop,Redex$Massloss.per)
#Renaming columns to avoid confusion:
colnames(RedOpEx)[(names(RedOpEx) == "Massloss.per")] <- "Open.Massloss"
colnames(RedOpEx)[(names(RedOpEx) == "Redex$Massloss.per")] <- "Excl.Massloss"
RedOpEx$Massloss..g..Ash.uncorrected <- NULL

#Create termite effect variable for Red:
RedOpEx$Termite.effect <- (RedOpEx$Open.Massloss-RedOpEx$Excl.Massloss)

length(RedOpEx$Termite.effect[RedOpEx$Termite.effect < 0]) #139 values are negative (excl>op)
#Below half of the litter has no termite effect 139/440 teabags --> Great!
RecalTermEff <- RedOpEx
#Setting all negative values below zero:
RecalTermEff$Termite.effect[RecalTermEff$Termite.effect < 0] <- 0

# Count zeroes by season 
RecalTermEff2<-RecalTermEff[!RecalTermEff$Region=="Intermediate",]
x0<-RecalTermEff2[!is.na(RecalTermEff2$Termite.effect),]
x1<-x0 %>% 
 group_by(Season) %>%
  summarise(Zero_bags = sum(Termite.effect==0)) # Why is this then 94? - include Nas?
x1
aggregate(RecalTermEff$Termite.effect, by=list(Season=RecalTermEff$Season,Landuse=RecalTermEff$Landuse),FUN=function(x) sum(x== 0, na.rm=T))
# Same result 94

x2<-x0 %>% 
  group_by(Season) %>%
  summarise(Tot_bags = sum(Termite.effect>.01))

100-((x2-x1)/x2*100) # Percent bags with zeroes - higher in wet season

# Averages of termite effect
aggregate(Termite.effect~Region+Season,RecalTermEff,mean)

# Without Seronera
RecalTermEff2<-RecalTermEff[!RecalTermEff$Region=="Intermediate",]
RedOpEx2<-RedOpEx[!RedOpEx$Region=="Intermediate",]
aggregate(Termite.effect~Landuse,RedOpEx2,mean)
((21.29934+22.87210)/2)-16.48753
aggregate(Termite.effect~Landuse,RecalTermEff2,mean)
aggregate(Termite.effect~Landuse,RecalTermEff2,median)
aggregate(Termite.effect~Landuse,RecalTermEff2,sd)
#1 Agriculture       22.67370
#2     Pasture       23.54969
#3        Wild       17.69902
((22.67370+23.54969)/2)-17.69902 # 5.4
aggregate(Termite.effect~Landuse,RecalTermEff2,min)
aggregate(Termite.effect~Landuse,RecalTermEff2,max)

aggregate(Termite.effect~Region+Landuse+Season,RecalTermEff2,mean)

ggplot(RecalTermEff2,aes(x=Termite.effect, fill=Season))+geom_density()+facet_wrap(~Region+Landuse)+theme_classic()

#Creating dataset for Vilde using the termite effect variable:
#In dryseason in block level
RecalTermEff_Dryseason <- droplevels(RecalTermEff[RecalTermEff$Season=="Dry",])
levels(RecalTermEff_Dryseason$Season)
RecalTermEff_Dryseason_Block <-aggregate(Termite.effect~Season+Region+Site+Landuse+Block, RecalTermEff_Dryseason, mean)
write.csv(write.csv(RecalTermEff_Dryseason_Block,file="Termites/RecalTermEff_Dryseason_Block.csv"))
#In wetseason in block level
RecalTermEff_Wetseason <- droplevels(RecalTermEff[RecalTermEff$Season=="Wet",])
levels(RecalTermEff_Wetseason$Season)
RecalTermEff_Wetseason_Block <-aggregate(Termite.effect~Season+Region+Site+Landuse+Block, RecalTermEff_Wetseason, mean)

#Setting all negative values below zero (did this previously for Vilde data, but do it again to create my own data):
RedOpEx$Termite.effect[RedOpEx$Termite.effect < 0] <- 0
GreenOpEx$Termite.effect[GreenOpEx$Termite.effect < 0] <- 0

#Create microbe effect variable for green:
GreenOpEx$Microbe.effect <- (GreenOpEx$Open.Massloss)
#Create microbe effect variable for red:
Microbe.effect.Red <- RedOpEx$Microbe.effect <- (RedOpEx$Open.Massloss)

#Agregatin the 4 varialbes
#MICROBE GREEN
SUM.Microbe.effect.green<-aggregate(Microbe.effect~Season+Region+Landuse,GreenOpEx,mean)
SUM.Microbe.effect.greenSE<-aggregate(Microbe.effect~Season+Region+Landuse,GreenOpEx,se)
SUM.Microbe.effect.green$SE<-SUM.Microbe.effect.greenSE$Microbe.effect
SUM.Microbe.effect.green$Decomposer <- "Microbe"
SUM.Microbe.effect.green$LD <- "Labile Microbe"
SUM.Microbe.effect.green$Littertype <- "Labile"
colnames(SUM.Microbe.effect.green)[(names(SUM.Microbe.effect.green)== "Microbe.effect")] <- "Massloss.per"
#MICROBE RED
SUM.Microbe.effect.red<-aggregate(Microbe.effect~Season+Region+Landuse,RedOpEx,mean)
SUM.Microbe.effect.redSE<-aggregate(Microbe.effect~Season+Region+Landuse,RedOpEx,se)
SUM.Microbe.effect.red$SE<-SUM.Microbe.effect.redSE$Microbe.effect
SUM.Microbe.effect.red$Decomposer <- "Microbe"
SUM.Microbe.effect.red$LD <- "Recalcitrant Microbe"
SUM.Microbe.effect.red$Littertype <- "Recalcitrant"
colnames(SUM.Microbe.effect.red)[(names(SUM.Microbe.effect.red)== "Microbe.effect")] <- "Massloss.per"

#TERMITE GREEN
SUM.Termite.effect.green<-aggregate(Termite.effect~Season+Region+Landuse,GreenOpEx,mean)
SUM.Termite.effect.greenSE<-aggregate(Termite.effect~Season+Region+Landuse,GreenOpEx,se)
SUM.Termite.effect.green$SE<-SUM.Termite.effect.greenSE$Termite.effect
SUM.Termite.effect.green$Decomposer <- "Termite"
SUM.Termite.effect.green$LD <- "Labile Termite"
SUM.Termite.effect.green$Littertype <- "Labile"
colnames(SUM.Termite.effect.green)[(names(SUM.Termite.effect.green)== "Termite.effect")] <- "Massloss.per"
#TERMITE RED
SUM.Termite.effect.red<-aggregate(Termite.effect~Season+Region+Landuse,RedOpEx,mean)
SUM.Termite.effect.redSE<-aggregate(Termite.effect~Season+Region+Landuse,RedOpEx,se)
SUM.Termite.effect.red$SE<-SUM.Termite.effect.redSE$Termite.effect
SUM.Termite.effect.red$Decomposer <- "Termite"
SUM.Termite.effect.red$LD <- "Recalcitrant Termite"
SUM.Termite.effect.red$Littertype <- "Recalcitrant"
colnames(SUM.Termite.effect.red)[(names(SUM.Termite.effect.red)== "Termite.effect")] <- "Massloss.per"
#Need to rbind the 4 variables to plot them:
TM.effect <- rbind(SUM.Microbe.effect.green,SUM.Microbe.effect.red,
                   SUM.Termite.effect.green,SUM.Termite.effect.red)

names(TM.effect)
aggregate(Massloss.per~Decomposer+Littertype,TM.effect,mean)
aggregate(Termite.effect~Landuse,RecalTermEff2,median)
aggregate(Termite.effect~Landuse,RecalTermEff2,sd)
#7  Agriculture    Microbe Recalcitrant    48.189215
#8      Pasture    Microbe Recalcitrant    47.206747
#9         Wild    Microbe Recalcitrant    45.113403
#10 Agriculture    Termite Recalcitrant    21.931850
#11     Pasture    Termite Recalcitrant    22.954480
#12        Wild    Termite Recalcitrant    20.995136

aggregate(Massloss.per~Landuse+Season+Region+Decomposer+Littertype,TM.effect,mean)
#46 Agriculture    Wet          Dry    Termite Recalcitrant    6.3254778
#47     Pasture    Wet          Dry    Termite Recalcitrant   14.9842328
#48        Wild    Wet          Dry    Termite Recalcitrant   31.6349679
31.6349679- 14.9842328
31.6349679- 6.3254778

aggregate(Massloss.per~Season+Decomposer+Littertype,TM.effect,mean)

#### BARPLOTTING - Termite effect #### 
# Three own graphs for each Landuse#
#Creating a fill factor:
TMMasslossMain$LD<-as.factor(with(TMMasslossMain, paste(Decomposer, Littertype, sep=" ")))
head(TMMasslossMain)
#Creating error bars for stacked barplot:
#Sort out each landuse into own dataframe
Agri <- droplevels(TMMasslossMain[TMMasslossMain$flanduse=="Agriculture",])
Pasture <- droplevels(TMMasslossMain[TMMasslossMain$flanduse=="Pasture",])
Wild <- droplevels(TMMasslossMain[TMMasslossMain$flanduse=="Wild",])

#AGRICULTURE#

#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- c("Litter Quality")

levels(Agri$LD)
levels(Agri$LD) <- c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite")
levels(Agri$LD)
levels(Agri$fseason)
levels(Agri$flanduse)
levels(Agri$Decomposer)
#Adjusting the upper SE for error bars for stacked barplot:
Agri$SE.up <- Agri$SE
Agri$SE.up[Agri$LD == "Labile Microbe"] <- with(Agri,SE[LD == "Labile Microbe"]+
                                                  #SE[LD == "Labile Termite"]+
                                                  Massloss.per[LD=="Labile Termite"])

Agri$SE.up[Agri$LD == "Recalcitrant Microbe"] <- with(Agri,SE[LD == "Recalcitrant Microbe"]+
                                                        #SE[LD == "Recalcitrant Termite"]+
                                                        Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
Agri$Mass.stop <- Agri$Massloss.per
Agri$Mass.stop[Agri$LD == "Labile Microbe"] <- with(Agri,Massloss.per[LD == "Labile Microbe"]+
                                                      Massloss.per[LD=="Labile Termite"])

Agri$Mass.stop[Agri$LD == "Recalcitrant Microbe"] <- with(Agri,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                            +Massloss.per[LD=="Recalcitrant Termite"])

#Plotting
AgriP <- ggplot(data=Agri,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
  geom_errorbar(position="identity",width=NA,lwd=1)+
  geom_bar(stat="identity",position="stack",width=0.9)+
  facet_wrap(~fseason+fregion,nrow=1)+
  scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
  scale_alpha_discrete(TitleDecomp,range=c(0.3,1))+
  xlab("")+ylab("Massloss (%)")+
  theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.background = element_rect(fill="transparent")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=14,color="black")
    ,axis.title.y=element_text(size=16,color="black", margin = margin(t = 0, r = 10, b = 0, l = 0))
  )
AgriP <-AgriP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0),position = "right")
AgriP <- AgriP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

AgriP

# ggsave("Termites/Main & CG experiment/Agriculture_Main_experiment.png",
#      width= 20, height = 15,units ="cm",bg ="transparent",
#     dpi = 600, limitsize = TRUE)

#PASTURE#
#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- c("Litter Quality")

levels(Pasture$LD)
levels(Pasture$LD) <- c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite")
levels(Pasture$LD)
levels(Pasture$fseason)
levels(Pasture$flanduse)
levels(Pasture$Decomposer)
#Adjusting the upper SE for error bars for stacked barplot:
Pasture$SE.up <- Pasture$SE
Pasture$SE.up[Pasture$LD == "Labile Microbe"] <- with(Pasture,SE[LD == "Labile Microbe"]+
                                                        #SE[LD == "Labile Termite"]+
                                                        Massloss.per[LD=="Labile Termite"])

Pasture$SE.up[Pasture$LD == "Recalcitrant Microbe"] <- with(Pasture,SE[LD == "Recalcitrant Microbe"]+
                                                              #SE[LD == "Recalcitrant Termite"]
                                                              +Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
Pasture$Mass.stop <- Pasture$Massloss.per
Pasture$Mass.stop[Pasture$LD == "Labile Microbe"] <- with(Pasture,Massloss.per[LD == "Labile Microbe"]+
                                                            Massloss.per[LD=="Labile Termite"])

Pasture$Mass.stop[Pasture$LD == "Recalcitrant Microbe"] <- with(Pasture,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                                  +Massloss.per[LD=="Recalcitrant Termite"])


PastureP <- ggplot(data=Pasture,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
  geom_errorbar(position="identity",width=NA,lwd=1)+
  geom_bar(stat="identity",position="stack",width=0.9)+
  facet_wrap(~fseason+fregion,nrow=1)+
  scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
  scale_alpha_discrete(TitleDecomp,range=c(0.3,1))+
  xlab("")+ylab("Massloss (%)")+
  theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.background = element_rect(fill="transparent")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=14,color="black")
    ,axis.title.y=element_text(size=16,color="black", margin = margin(t = 0, r = 10, b = 0, l = 0))
  )
PastureP <-PastureP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0))
PastureP <- PastureP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

PastureP

# ggsave("Termites/Main & CG experiment/Pasture_Main_experiment.png",
#        width= 20, height = 15,units ="cm",bg ="transparent",
#        dpi = 600, limitsize = TRUE)


#WILD#
#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- c("Litter Quality")

levels(Wild$LD)
levels(Wild$LD) <- c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite")
levels(Wild$LD)
levels(Wild$fseason)
levels(Wild$flanduse)
levels(Wild$Decomposer)
#Adjusting the upper SE for error bars for stacked barplot:
Wild$SE.up <- Wild$SE
Wild$SE.up[Wild$LD == "Labile Microbe"] <- with(Wild,SE[LD == "Labile Microbe"]+
                                                  #SE[LD == "Labile Termite"]+
                                                  Massloss.per[LD=="Labile Termite"])

Wild$SE.up[Wild$LD == "Recalcitrant Microbe"] <- with(Wild,SE[LD == "Recalcitrant Microbe"]+
                                                        #SE[LD == "Recalcitrant Termite"]
                                                        +Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
Wild$Mass.stop <- Wild$Massloss.per
Wild$Mass.stop[Wild$LD == "Labile Microbe"] <- with(Wild,Massloss.per[LD == "Labile Microbe"]+
                                                      Massloss.per[LD=="Labile Termite"])

Wild$Mass.stop[Wild$LD == "Recalcitrant Microbe"] <- with(Wild,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                            +Massloss.per[LD=="Recalcitrant Termite"])


WildP <- ggplot(data=Wild,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
  geom_errorbar(position="identity",width=NA,lwd=1)+
  geom_bar(stat="identity",position="stack",width=0.9)+
  facet_wrap(~fseason+fregion,nrow=1)+
  scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
  scale_alpha_discrete(TitleDecomp,range=c(0.3,1))+
  xlab("")+ylab("Massloss (%)")+
  theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.background = element_rect(fill="transparent")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=14,color="black")
    ,axis.title.y=element_text(size=16,color="black", margin = margin(t = 0, r = 10, b = 0, l = 0))
  )
WildP <-WildP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0))
WildP <- WildP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

WildP



#### Statistical models GLMM ####
#General massloss models, using Massloss.per as response varible####
####Dataprocessing####
#Separate littertypes
RecalMain <- droplevels(DataMain[DataMain$Littertype =="Rooibos",])
LabileMain <- droplevels(DataMain[DataMain$Littertype =="Green",])

#Adding data from buring in Seronera soil (local soil) - i.e an additional wild site in intermediate rainregion
LocalCGsoil2 <- DataCG[DataCG$Site=="Seronera",]
LabileLocalCGsoil2 <- LocalCGsoil2[LocalCGsoil2$Littertype=="Green",]
RecalLocalCGsoil2 <- LocalCGsoil2[LocalCGsoil2$Littertype=="Rooibos",]
##Removing 4-block design into 1 block with 4 replicates:
LabileLocalCGsoil2$Block<- c(1,2,3,4)
LabileLocalCGsoil2$Blockcode <- c("Int_W1","Int_W2","Int_W3","Int_W4")
RecalLocalCGsoil2$Block <- c(1,2,3,4)
RecalLocalCGsoil2$Blockcode <- c("Int_W1","Int_W2","Int_W3","Int_W4")

#RecalMain <- rbind.fill(RecalMain,RecalLocalCGsoil2)
#LabileMain <- rbind.fill(LabileMain,LabileLocalCGsoil2)

#Renaming some columns (RECAL):
names(RecalMain)
colnames(RecalMain)[(names(RecalMain)== "Sandcorr")] <- "Sand"
colnames(RecalMain)[(names(RecalMain)== "Claycorr")] <- "Clay"
colnames(RecalMain)[(names(RecalMain)== "Moisture..")] <- "Moisture"
colnames(RecalMain)[(names(RecalMain)== "Temperature..C.")] <- "Temp"
colnames(RecalMain)[(names(RecalMain)== "Rain.sum")] <- "Rain"
#Renaming some columns (LABILE):
names(LabileMain)
colnames(LabileMain)[(names(LabileMain)== "Sandcorr")] <- "Sand"
colnames(LabileMain)[(names(LabileMain)== "Claycorr")] <- "Clay"
colnames(LabileMain)[(names(LabileMain)== "Moisture..")] <- "Moisture"
colnames(LabileMain)[(names(LabileMain)== "Temperature..C.")] <- "Temp"
colnames(LabileMain)[(names(LabileMain)== "Rain.sum")] <- "Rain"

library(car)
MyVar2 <- c("Sand","C.N","Region","Rain")
corvif(LabileMain[,MyVar2])
# Temp and moisture cannot be in same model
# Moisture + Temperature cannot be in same model as season or region

#### GLMM with Beta distribution ####

#### Recal model ####
# Ensure mass loss values are between 0 and 1 (cannot be exactly zero and one so 0.01 and 99.99)
RecalMain$Massloss.per[RecalMain$Massloss.per>99.99]<-99.99
RecalMain$Massloss.per[RecalMain$Massloss.per<0.01]<-0.01
RecalMain$Massloss.perB<-RecalMain$Massloss.per/100
#### GLMM - glmmADMB pacakge #### Can also used glmmTMB
RecalMain2<-droplevels(RecalMain[!is.na(RecalMain$Massloss.per),])
RecalMain2$Massloss.per

RecalMain2$Site<-as.factor(RecalMain2$Site)
RecalMain2$Blockcode<-as.factor(RecalMain2$Blockcode)
RecalMain2$Plot<-as.factor(RecalMain2$Plot)


min(RecalMain2$Massloss.perB)
max(RecalMain2$Massloss.perB)

# Recalitrant litter model
RecalMainModFINAL2<- glmmTMB(Massloss.perB ~ Season+Region+Landuse+
                      Landuse:Treatment+Region:Treatment+Region:Landuse+
                      Season:Treatment+Season:Landuse+Season:Region+
                     Season:Region:Landuse+Season:Region:Treatment+
                    Season:Landuse:Treatment+
                      (1|Blockcode/Plot), 
                      family=beta_family(link="logit"), data=RecalMain2)

summary(RecalMainModFINAL2)
drop1(RecalMainModFINAL2, test="Chisq") # Nothing removed

#library(sjstats)
sjstats::icc(RecalMainModFINAL2)
#ICC (Plot:Blockcode): 0.4880
#ICC (Blockcode): 0.2648
sjstats::r2(RecalMainModFINAL2) #works when removing site
#    Marginal R2: 0.715
# Conditional R2: 0.929

#Inspect chosen model for homogeneity:
E1 <- resid(RecalMainModFINAL2, type ="pearson")
F1 <- fitted(RecalMainModFINAL2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
#hist(E1, nclass = 30) #OK

# Update recalitrant model
# Three-way
Recalmod<- update(RecalMainModFINAL2, .~. -Season:Landuse:Treatment)
Recalmod2<- update(RecalMainModFINAL2, .~. -Season:Region:Treatment)
Recalmod3<- update(RecalMainModFINAL2, .~. -Season:Region:Landuse)

#Two-way
RecalMainModFINAL2b<- glmmTMB(Massloss.perB ~ Season+Region+Landuse+Treatment+Sand+Landuse:Treatment+Region:Treatment+Region:Landuse+ 
                                Season:Treatment+Season:Landuse+Season:Region+(1|Blockcode/Plot), family=beta_family(link="logit"), data=RecalMain2)
Recalmod4<- update(RecalMainModFINAL2b, .~. -Season:Region)
Recalmod5<- update(RecalMainModFINAL2b, .~. -Season:Landuse)
Recalmod6<- update(RecalMainModFINAL2b, .~. -Season:Treatment)
Recalmod7<- update(RecalMainModFINAL2b, .~. -Region:Landuse)
Recalmod8<- update(RecalMainModFINAL2b, .~. -Region:Treatment)
Recalmod9<- update(RecalMainModFINAL2b, .~. -Landuse:Treatment)

#One-way
RecalMainModFINAL2c<- glmmTMB(Massloss.perB ~ Season+Region+Landuse+Treatment+(1|Blockcode/Plot), family=beta_family(link="logit"), data=RecalMain2)
Recalmod10<- update(RecalMainModFINAL2c, .~. -Treatment)
Recalmod11<- update(RecalMainModFINAL2c, .~. -Landuse)
Recalmod12<- update(RecalMainModFINAL2c, .~. -Region)
Recalmod13<- update(RecalMainModFINAL2c, .~. -Season)

# ANOVA
anova(RecalMainModFINAL2,Recalmod)
anova(RecalMainModFINAL2,Recalmod2)
anova(RecalMainModFINAL2,Recalmod3)
anova(RecalMainModFINAL2b,Recalmod4)
anova(RecalMainModFINAL2b,Recalmod5)
anova(RecalMainModFINAL2b,Recalmod6)
anova(RecalMainModFINAL2b,Recalmod7)
anova(RecalMainModFINAL2b,Recalmod8)
anova(RecalMainModFINAL2c,Recalmod9)
anova(RecalMainModFINAL2c,Recalmod10)
anova(RecalMainModFINAL2c,Recalmod11)
anova(RecalMainModFINAL2c,Recalmod12)
anova(RecalMainModFINAL2c,Recalmod13)

#Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
#RecalMainModFINAL2 23 -362.01 -257.74 204.01  -408.01 15.734      2  0.0003832 *** #Season:Landuse:Treatment
#RecalMainModFINAL2 23 -362.01 -257.74 204.01  -408.01 5.4291      1     0.0198 * #Season:Region:Treatment
#RecalMainModFINAL2 23 -362.01 -257.74 204.01  -408.01 15.961      2  0.0003421 *** #Season:Region:Landuse
#RecalMainModFINAL2b 19 -341.39 -255.25 189.69  -379.39  38.3      1  6.066e-10 *** #Season:Region
#RecalMainModFINAL2b 19 -341.39 -255.25 189.69  -379.39 7.899      2    0.01926 * #Season:Landuse
#RecalMainModFINAL2b 19 -341.39 -255.25 189.69  -379.39 0.0352      1     0.8512 #Season:Treatment
#RecalMainModFINAL2b 19 -341.39 -255.25 189.69  -379.39 9.5404      2   0.008479 ** # Region:Landuse
#RecalMainModFINAL2b 19 -341.39 -255.25 189.69  -379.39 0.5793      1     0.4466 #Region:Treatment
#Recalmod9           17 -343.99 -266.91 188.99  -377.99 60.758      8   3.31e-10 *** #Landuse:Treatment
#RecalMainModFINAL2c  9 -299.23 -258.42 158.614  -317.23 132.78      1  < 2.2e-16 *** #Treatment
#RecalMainModFINAL2c  9 -299.23 -258.42 158.61  -317.23 1.6495      2     0.4383 #Landuse
#RecalMainModFINAL2c  9 -299.23 -258.42 158.61  -317.23 3.9462      1    0.04698 * #Region
#RecalMainModFINAL2c  9 -299.23 -258.42 158.61  -317.23 57.242      1  3.853e-14 *** #Season

# Within factors - Season x landuse x treatment
ref.grid.RecalMainModFINAL <- ref_grid(RecalMainModFINAL2) #at = list(Region = c("Dry", "Wet"))) #Want to remove Intermadiate in the grid as it can't be compared to the other landuses, except wild.
ref.grid.RecalMainModFINAL#See how the grid is looking. Check for correct factors and the emmeans of numerical variables. If testing between numerical variables, only the means or the low/high end of values can be specified. I.e contrasts vs trend. See emmeans vignette for more info.
#Check threeway first:
emmip(ref.grid.RecalMainModFINAL, Region~Season|Landuse+Treatment, type="response")#
emmeans.RecalMainModFINAL <- emmeans(ref.grid.RecalMainModFINAL, pairwise~Season*Region*Treatment|Landuse,type="response") #
emmeans.RecalMainModFINAL$contrasts
emmeans.RecalMainModFINAL$emmeans
emmeans.RecalMainModFINAL.pairs <- pairs(emmeans.RecalMainModFINAL,simple = "each", combine =TRUE)
plot(emmeans.RecalMainModFINAL, comparisons = FALSE)


# Roobios mass loss averages
aggregate(Massloss.per~Landuse+Region+Season,data=RecalMain2,mean)
aggregate(Massloss.per~Region+Season+Treatment,data=RecalMain2,mean)
aggregate(Massloss.per~Region+Season,data=RecalMain2,sd)
aggregate(Massloss.per~Landuse+Treatment,data=RecalMain2,sd)

#### Labile model ####
# Ensure mass loss values are between 0 and 1 (cannot be exactly zero and one so 0.01 and 99.99)
LabileMain$Massloss.per[LabileMain$Massloss.per>99.99]<-99.99
LabileMain$Massloss.per[LabileMain$Massloss.per<0.01]<-0.01
LabileMain$Massloss.perB<-LabileMain$Massloss.per/100
#### GLMM - glmmADMB pacakge #### Can also used glmmTMB
LabileMain2<-droplevels(LabileMain[!is.na(LabileMain$Massloss.per),])
LabileMain2$Massloss.per

LabileMain2$Site<-as.factor(LabileMain2$Site)
LabileMain2$Blockcode<-as.factor(LabileMain2$Blockcode)
LabileMain2$Plot<-as.factor(LabileMain2$Plot)

min(LabileMain2$Massloss.perB)
max(LabileMain2$Massloss.perB)

# Labile litter model
LabileMainModFINAL2 <- glmmTMB(Massloss.perB ~ Season+Region+Landuse+#Treatment+
                                 +Region:Landuse+ #Landuse:Treatment+Region:Treatment+
                                 +Season:Landuse+Season:Region+#Season:Treatment+
                                 Season:Region:Landuse+#Season:Region:Treatment+Season:Landuse:Treatment+
                                 (1|Blockcode/Plot), 
                               family=beta_family(link="logit"), data=LabileMain2)
summary(LabileMainModFINAL2)
drop1(LabileMainModFINAL2, test="Chisq")

#library(sjstats)
sjstats::icc(LabileMainModFINAL2)
#  ICC (Plot:Blockcode): 0.7362
#ICC (Blockcode): 0.3124
sjstats::r2(LabileMainModFINAL2) #works when removing site
#   Marginal R2: 0.932
#Conditional R2: 1.003

#Inspect chosen model for homogeneity:
E1 <- resid(LabileMainModFINAL2, type ="pearson")
F1 <- fitted(LabileMainModFINAL2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # OK
hist(E1, nclass = 30) #OK

# Update labile model with temperature
# Three-way
Labmod<- update(LabileMainModFINAL2, .~. - Season:Region:Landuse)

#Two-way
Labmod2<- update(Labmod, .~. -Season:Region)
Labmod3<- update(Labmod, .~. -Season:Landuse)
Labmod4<- update(Labmod, .~. -Region:Landuse)

#One-way
LabileMainModFINAL2b<- glmmTMB(Massloss.perB ~ Season+Region+Landuse+(1|Blockcode/Plot),family=beta_family(link="logit"), data=LabileMain2)
Labmod5<- update(LabileMainModFINAL2b, .~. -Landuse)
Labmod6<- update(LabileMainModFINAL2b, .~. -Region)
Labmod7<- update(LabileMainModFINAL2b, .~. -Season)

# ANOVA
anova(LabileMainModFINAL2,Labmod)
anova(Labmod,Labmod2)
anova(Labmod,Labmod3)
anova(Labmod,Labmod4)
anova(LabileMainModFINAL2b,Labmod5)
anova(LabileMainModFINAL2b,Labmod6)
anova(LabileMainModFINAL2b,Labmod7)


#                    Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
#LabileMainModFINAL2 15 -1335.2 -1267.1 682.62  -1365.2 51.077      2  8.106e-12 *** #Season:Region:Landuse
#Labmod  13 -1288.16 -1229.13 657.08 -1314.16 505.51      1  < 2.2e-16 *** # Season:Region
#Labmod  16 -1328.5 -1255.9 680.26  -1360.5 18.076      2  0.0001188 *** #Season:Landuse
#Labmod  13 -1288.2 -1229.1 657.08  -1314.2 10.478      2   0.005304 ** #Region:Landuse
#LabileMainModFINAL2b  8 -776.60 -740.27 396.30  -792.60 3.7595      2     0.1526 # Landuse
#LabileMainModFINAL2b  8 -776.60 -740.27 396.30  -792.60 48.327      1  3.607e-12 *** # Region
#LabileMainModFINAL2b  8 -776.60 -740.27 396.30  -792.60 537.86      1  < 2.2e-16 *** # Season

# Within factors - Season x landuse x treatment
ref.grid.LabileMainModFINAL2 <- ref_grid(LabileMainModFINAL2) #at = list(Region = c("Dry", "Wet"))) #Want to remove Intermadiate in the grid as it can't be compared to the other landuses, except wild.
ref.grid.LabileMainModFINAL2#See how the grid is looking. Check for correct factors and the emmeans of numerical variables. If testing between numerical variables, only the means or the low/high end of values can be specified. I.e contrasts vs trend. See emmeans vignette for more info.
#Check threeway first:
emmip(ref.grid.LabileMainModFINAL2, Region~Season|Landuse, type="response")#
emmeans.LabileMainModFINAL2 <- emmeans(ref.grid.LabileMainModFINAL2, pairwise~Season*Region|Landuse,type="response") #
emmeans.LabileMainModFINAL2$contrasts
emmeans.LabileMainModFINAL2$emmeans
emmeans.LabileMainModFINAL2.pairs <- pairs(emmeans.LabileMainModFINAL2L,simple = "each", combine =TRUE)
plot(emmeans.LabileMainModFINAL2, comparisons = FALSE)

aggregate(Massloss.per~Landuse+Region+Season,data=LabileMain2,mean)
aggregate(Massloss.per~Region+Season,data=LabileMain2,mean)
aggregate(Massloss.per~Region+Season,data=LabileMain2,sd)

aggregate(Temp~Landuse+Region+Season,data=DataMainC,mean)

#########################################################################################################################
#### Moisture,Temp, Sand, CN model ####
#########################################################################################################################

DataMain$Site<-as.factor(DataMain$Site)
DataMain$Blockcode<-as.factor(DataMain$Blockcode)
DataMain$Plot<-as.factor(DataMain$Plot)

colnames(DataMain)[(names(DataMain)== "Temperature..C.")] <- "Temp"
colnames(DataMain)[(names(DataMain)== "Moisture..")] <- "Moisture"
colnames(DataMain)[(names(DataMain)== "Sandcorr")] <- "Sand"

#### Moisture model ####
min(DataMain$Moisture,na.rm=T)
DataMainM<-DataMain[!is.na(DataMain$Moisture),]
DataMainM$Moisture
MoistureModFINAL2 <- glmmTMB(Moisture~ Season+Region+Landuse+Temp+Sand+#C.N+
                               Region:Landuse+Season:Landuse+Season:Region+
                               Season:Region:Landuse+
                                (1|Blockcode/Plot), data=DataMainM)
summary(MoistureModFINAL2)
drop1(MoistureModFINAL2, test="Chisq")

sjstats::icc(MoistureModFINAL2 )
#ICC (Plot:Blockcode): 0.3825
#ICC (Blockcode): 0.2019
sjstats::r2(MoistureModFINAL2) #works when removing site
# Marginal R2: 0.842
# Conditional R2: 0.934

plot(Moisture~Sand,DataMainM) # Negative
aggregate(Moisture~Season,DataMainM,mean)

#Inspect chosen model for homogeneity:
E1 <- resid(MoistureModFINAL2, type ="pearson")
F1 <- fitted(MoistureModFINAL2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
hist(E1, nclass = 30) # 1 negative fitted value - rest ok

# Update Moisture model with moisture
# Three-way
MoistMod<- update(MoistureModFINAL2, .~. -  Season:Region:Landuse)

#Two-way
MoistMod2<- update(MoistMod, .~. -Season:Region)
MoistMod3<- update(MoistMod, .~. -Season:Landuse)
MoistMod4<- update(MoistMod, .~. -Region:Landuse)

#One-way
MoistureModFINAL2b<- glmmTMB(Moisture~ Season+Region+Landuse+Temp+Sand+(1|Blockcode/Plot), data=DataMainM)
MoistMod5<- update(MoistureModFINAL2b, .~. -Sand)
MoistMod6<- update(MoistureModFINAL2b, .~. -Temp)
MoistMod7<- update(MoistureModFINAL2b, .~. -Landuse)
MoistMod8<- update(MoistureModFINAL2b, .~. -Region)
MoistMod9<- update(MoistureModFINAL2b, .~. -Season)

# ANOVA
anova(MoistureModFINAL2,MoistMod)
anova(MoistMod,MoistMod2)
anova(MoistMod,MoistMod3)
anova(MoistMod,MoistMod4)
anova(MoistureModFINAL2b,MoistMod5)
anova(MoistureModFINAL2b,MoistMod6)
anova(MoistureModFINAL2b,MoistMod7)
anova(MoistureModFINAL2b,MoistMod8)
anova(MoistureModFINAL2b,MoistMod9)

#                  Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#MoistureModFINAL2 17 7051.1 7140.8 -3508.6   7017.1 14.006      2  0.0009093 *** #Season:Region:Landuse
#MoistMod  15 7061.1 7140.2 -3515.6   7031.1 294.32      1  < 2.2e-16 *** #Season:Region
#MoistMod  15 7061.1 7140.2 -3515.6   7031.1 46.834      2  6.763e-11 *** #Season:Landuse
#MoistMod  15 7061.1 7140.2 -3515.6   7031.1 8.6512      2    0.01323 * #Region:Landuse
#MoistureModFINAL2b 10 7363.3 7416.0 -3671.7   7343.3 6.8657      1   0.008786 ** #Sand
#MoistureModFINAL2b 10 7363.3 7416.0 -3671.7   7343.3 901.66      1  < 2.2e-16 ***#Temp
#MoistureModFINAL2b 10 7363.3 7416.0 -3671.7   7343.3 3.8459      2     0.1462 #Landuse
#MoistureModFINAL2b 10 7363.3 7416.0 -3671.7   7343.3 10.982      1  0.0009202 *** #Region
#MoistureModFINAL2b 10 7363.3 7416.0 -3671.7   7343.3 1813.8      1  < 2.2e-16 *** #Season

MeanMoist<-aggregate(Moisture~Season+Region+Landuse,DataMainM,mean)
ggplot(DataMainM,aes(y=Moisture, x=Landuse))+geom_jitter(colour="light grey",alpha=.5,size=2)+
  geom_point(data=MeanMoist, colour="black", fill="black",size=4.5)+
  facet_wrap(~Season+Region)+theme_classic()

xyplot(Moisture~Massloss.per|Littertype*Season,DataMainM)
plot(Moisture~Temp,DataMainM)


# Within factors - Season x landuse x treatment
ref.grid.MoistureModFINAL <- ref_grid(MoistureModFINAL2) #at = list(Region = c("Dry", "Wet"))) #Want to remove Intermadiate in the grid as it can't be compared to the other landuses, except wild.
ref.grid.MoistureModFINAL#See how the grid is looking. Check for correct factors and the emmeans of numerical variables. If testing between numerical variables, only the means or the low/high end of values can be specified. I.e contrasts vs trend. See emmeans vignette for more info.
#Check threeway first:
emmip(ref.grid.MoistureModFINAL, Landuse~Region|Season, type="response")#
emmeans.MoistureModFINAL <- emmeans(ref.grid.MoistureModFINAL, pairwise~Landuse*Region|Season,type="response") #
emmeans.MoistureModFINAL$contrasts
emmeans.MoistureModFINAL$emmeans
emmeans.MoistureModFINAL.pairs <- pairs(emmeans.MoistureModFINAL,simple = "each", combine =TRUE)
plot(emmeans.MoistureModFINAL, comparisons = FALSE)

bwplot(Moisture~Landuse|Season*Region,DataMainM)
xyplot(Moisture~Temp|Landuse*Season*Region,DataMainM)
xyplot(Moisture~C.N|Landuse*Season,DataMainM)


with(DataMainM, {interaction.plot(Region,Landuse,Moisture,
                                  xlab = "Region",
                                  ylab = "Moisture",
                                  main="Soil moisture",
                                  fun=mean)})

with(DataMainM, {interaction.plot(Season,Region,Moisture,
                                  xlab = "Season",
                                  ylab = "Moisture",
                                  main="Soil Moisture",
                                  fun=mean)})


#### Temperature model ####
DataMainC<-DataMain[!is.na(DataMain$Temp),]
TempModFINAL2 <- glmmTMB(Temp~  Season+Region+Landuse+Moisture+C.N+#Sand+
                           Region:Landuse+Season:Landuse+Season:Region+
                           Season:Region:Landuse+
                               (1|Blockcode/Plot),  data=DataMainC)

summary(TempModFINAL2)
drop1(TempModFINAL2, test="Chisq")

sjstats::icc(TempModFINAL2)
# ICC (Plot:Blockcode): 0.3368
#ICC (Blockcode): 0.3818
sjstats::r2(TempModFINAL2) #works when removing site
#    Marginal R2: 0.808
# Conditional R2: 0.946

xyplot(Temp~Moisture|Landuse*Season, DataMainC)
plot(Temp~C.N, DataMainC) # positive
aggregate(Temp~Season,DataMainC,mean)
aggregate(Temp~Landuse,DataMainC,mean)
29.35208-27.88667
29.35208-27.22417
aggregate(Temp~Landuse,DataMainC,sd)
#Inspect chosen model for homogeneity:
E1 <- resid(TempModFINAL2, type ="pearson")
F1 <- fitted(TempModFINAL2 )

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)# Very good

# Update Moisture model with moisture
# Three-way
TempMod<- update(TempModFINAL2, .~. -  Season:Region:Landuse)

#Two-way
TempMod2<- update(TempMod, .~. -Season:Region)
TempMod3<- update(TempMod, .~. -Season:Landuse)
TempMod4<- update(TempMod, .~. -Region:Landuse)

#One-way
TempModFINAL2b<- glmmTMB(Temp~ Season+Region+Landuse+Moisture+C.N+(1|Blockcode/Plot), data=DataMain)
TempMod5<- update(TempModFINAL2b, .~. -C.N)
TempMod6<- update(TempModFINAL2b, .~. -Moisture)
TempMod7<- update(TempModFINAL2b, .~. -Landuse)
TempMod8<- update(TempModFINAL2b, .~. -Region)
TempMod9<- update(TempModFINAL2b, .~. -Season)

# ANOVA
anova(TempModFINAL2,TempMod)
anova(TempMod,TempMod2)
anova(TempMod,TempMod3)
anova(TempMod,TempMod4)
anova(TempModFINAL2b,TempMod5)
anova(TempModFINAL2b,TempMod6)
anova(TempModFINAL2b,TempMod7)
anova(TempModFINAL2b,TempMod8)
anova(TempModFINAL2b,TempMod9)

#              Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#TempModFINAL2 17 4336.7 4426.4 -2151.4   4302.7 82.719      2  < 2.2e-16 *** #Season:Region:Landuse
#TempMod  15 4415.5 4494.5 -2192.7   4385.5 168.97      1  < 2.2e-16 *** #Season:Region
#TempMod  15 4415.5 4494.5 -2192.7   4385.5 95.544      2  < 2.2e-16 *** #Season:Landuse
#TempMod  15 4415.5 4494.5 -2192.7   4385.5 12.015      2   0.002461 ** #Region:Landuse
#TempModFINAL2b 10 4626.6 4679.4 -2303.3   4606.6 10.572      1   0.001148 ** #C.N
#TempModFINAL2b 10 4626.6 4679.4 -2303.3   4606.6 971.99      1  < 2.2e-16 *** #Moisture
#TempModFINAL2b 10 4626.6 4679.4 -2303.3   4606.6 6.4319      2    0.04012 * #Landuse
#TempModFINAL2b 10 4626.6 4679.4 -2303.3   4606.6 24.557      1  7.216e-07 *** #Region
#TempModFINAL2b 10 4626.6 4679.4 -2303.3   4606.6 290.24      1  < 2.2e-16 *** #Season

# Landuse significant temperature
library(multcomp)
bwplot(Temp~Landuse,DataMainC)

MeanTemp<-aggregate(Temp~Season+Region+Landuse,DataMain,mean)
ggplot(DataMain,aes(y=Temp, x=Landuse))+geom_jitter(colour="light grey",alpha=.5,size=2)+
  geom_point(data=MeanTemp, colour="black", fill="black",size=4.5)+
  facet_wrap(~Season+Region)+theme_classic()

DataMainC<-DataMain[!is.na(DataMain$Temp),]
with(DataMainC, {interaction.plot(Region,Landuse,Temp,
                                  xlab = "Region",
                                  ylab = "Temp",
                                  main="Productivity exclosure",
                                  fun=mean)})

with(DataMainC, {interaction.plot(Season,Region,Temp,
                                  xlab = "Season",
                                  ylab = "Moisture",
                                  main="Productivity exclosure",
                                  fun=mean)})


#########################################################################################################################
#### Summary of spot measurements and logger - Soil moisture and temperature ####
#########################################################################################################################

########################################################################
# Temperature and moisture logger for TBI incubation
########################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Moisture.temp.loggers/")

TinyTagTemp<-read.csv("Soil.temp.logger.long.csv")
DeltaMoist<-read.csv("Soil.moist.logger.long.csv")

head(TinyTagTemp)
head(DeltaMoist)
dim(TinyTagTemp) # 117842     15
dim(DeltaMoist) #30560    15
names(TinyTagTemp)
names(DeltaMoist)

tail(DeltaMoist)

#### ANDERS TEABAG INCUBATION TIMES ######
# Import data TBI incubation
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/")
SerEbio<-read.csv(file="ANDERS_MSc_Incubationtime_For_Workshop.csv", sep=",",header=TRUE)
names(SerEbio)

# Creating shared block id
TinyTagTemp$Area_landuse<-as.factor(with(TinyTagTemp, paste(area,landuse, sep="_")))
DeltaMoist$Area_landuse<-as.factor(with(DeltaMoist, paste(area,landuse, sep="_")))
SerEbio$Area_landuse<-as.factor(with(SerEbio, paste(area,landuse, sep="_")))

levels(TinyTagTemp$Area_landuse)<-c("Makao_agriculture","Makao_pasture","MakaoWMA_wild","MaswaGR_wild",      
                                    "Makao_pasture","Mwantimba_agriculture", "Mwantimba_pasture","Ololosokwan_pasture",
                                    "Seronera_wild","SNP_wild" )
levels(DeltaMoist$Area_landuse)<-c("Makao_agriculture","Makao_pasture","MakaoWMA_wild","MaswaGR_wild",      
                                   "MaswaGR_wild_illegal","Mwantimba_agriculture", "Mwantimba_pasture",
                                   "Seronera_wild","SNP_wild" )
levels(SerEbio$Area_landuse)<-c( "SNP_wild","Makao_agriculture","Makao_pasture","MaswaGR_wild","Mwantimba_agriculture",
                                 "Mwantimba_pasture", "Seronera_wild" )

# Remove non shared area and land-uses
TinyTagTemp<-droplevels(TinyTagTemp[!TinyTagTemp$Area_landuse=="MakaoWMA_wild" & !TinyTagTemp$Area_landuse=="MaswaGR_wild_illegal" & #!TinyTagTemp$Area_landuse=="MaswaGR_wild_illegal" &
                                      !TinyTagTemp$Area_landuse=="Ololosokwan_pasture",])

DeltaMoist<-droplevels(DeltaMoist[!DeltaMoist$Area_landuse=="MakaoWMA_wild" & !DeltaMoist$Area_landuse=="MaswaGR_wild_illegal" ,])# & !DeltaMoist$Area_landuse=="MaswaGR_wild_illegal",])

DeltaMoist$area

names(DeltaMoist)
DeltaMoist$area # 5 levels
DeltaMoist$Landuse<-DeltaMoist$landuse
levels(DeltaMoist$Landuse)<-c("Agriculture" ,"Pasture", "Wild")

dim(TinyTagTemp) # 99396    16
dim(DeltaMoist) #26316    19

#### Logger moisture and temperature through seasons ####

names(TinyTagTemp)
names(DeltaMoist)

# Remove outliers for moisture for plotcode TZ_SS_SW105
dim(DeltaMoist)
DeltaMoist$plotcode
DeltaMoist<-DeltaMoist[!DeltaMoist$plotcode=="TZ_SS_SW105" | ! DeltaMoist$Moisture.m3.m3 <0.1,]
DeltaMoist<-DeltaMoist[!DeltaMoist$plotcode=="TZ_SS_SW105" | ! DeltaMoist$Moisture.m3.m3 >0.5,]
dim(DeltaMoist) # 26316    19

# Remove outlier TZ_SS_MP127 > 37C
dim(TinyTagTemp)
TinyTagTemp<-TinyTagTemp[!TinyTagTemp$plotcode=="TZ_SS_MP127" | ! TinyTagTemp$temperature.C >37,]
dim(TinyTagTemp) # 99396    16

# Seperate datasets DRY SEASON and WET SEASON
# Wet season: 26/01/2017 - 27/03/2017
# Dry season: 20/07/2017 - 03/10/2017

DeltaMoist$date2<-as.Date(DeltaMoist$date,"%d.%m.%Y")

DeltaMoistWET<-DeltaMoist[DeltaMoist$date2>"2017-01-26" & DeltaMoist$date2<"2017-03-27", ]
DeltaMoistDRY<-DeltaMoist[DeltaMoist$date2>"2017-07-20" & DeltaMoist$date2<"2017-10-03", ]

DeltaMoistWET$area

dim(DeltaMoist) # 26316    19
dim(DeltaMoistWET) #3137   19
dim(DeltaMoistDRY) #2948   19
DeltaMoistWET$Season<-"Wet Season"
DeltaMoistDRY$Season<-"Dry Season"

# Caclulate outliers in Dry and Wet Region during study period
DeltaMoistWET$Region<-DeltaMoistWET$area
DeltaMoistDRY$Region<-DeltaMoistDRY$area
DeltaMoist$Region<-DeltaMoist$area
levels(DeltaMoistWET$Region)<-c("Dry Region","Dry Region","Wet Region","Intermediate Region","Wet Region")
levels(DeltaMoistDRY$Region)<-c("Dry Region","Dry Region","Wet Region","Intermediate Region","Wet Region")
levels(DeltaMoist$Region)<-c("Dry Region","Dry Region","Wet Region","Intermediate Region","Wet Region")
#DeltaMoistWETsub<-droplevels(DeltaMoistWET[!DeltaMoistWET$area=="Seronera",])
#DeltaMoistDRYsub<-droplevels(DeltaMoistDRY[!DeltaMoistDRY$area=="Seronera",])
#DeltaMoistsub<-droplevels(DeltaMoist[!DeltaMoist$area=="Seronera",])

Dp<-DeltaMoistDRYsub[DeltaMoistDRYsub$Region=="Dry Region" & DeltaMoistDRYsub$landuse=="pasture", ]
Wp<-DeltaMoistWETsub[DeltaMoistWETsub$Region=="Wet Region" & DeltaMoistWETsub$landuse=="pasture", ]
Ww<-DeltaMoistWETsub[DeltaMoistWETsub$Region=="Wet Region" & DeltaMoistWETsub$landuse=="wild", ]

min(Dp$Moisture.m3.m3, na.rm=T) # 0.36
min(Wp$Moisture.m3.m3, na.rm=T) # 0.03
min(Ww$Moisture.m3.m3, na.rm=T) # 0.039

# Remove outliers
DeltaMoistDRYsub<-DeltaMoistDRY[!DeltaMoistDRY$Region=="Dry Region" | !DeltaMoistDRY$landuse=="pasture" | !DeltaMoistDRY$Moisture.m3.m3<.037, ]
DeltaMoistWETsub<-DeltaMoistWET[!DeltaMoistWET$Region=="Wet Region" | !DeltaMoistWET$landuse=="pasture" | !DeltaMoistWET$Moisture.m3.m3<.04,  ]
DeltaMoistWETsub<-DeltaMoistWET[!DeltaMoistWET$Region=="Wet Region" | !DeltaMoistWET$landuse=="wild" | !DeltaMoistWET$Moisture.m3.m3<.04,  ]

DeltaMoistsub<-DeltaMoist
DeltaMoistsub<-DeltaMoistsub[!DeltaMoistsub$Region=="Dry Region" | !DeltaMoistsub$landuse=="pasture" | !DeltaMoistsub$Moisture.m3.m3<.037, ]
DeltaMoistsub<-DeltaMoistsub[!DeltaMoistsub$Region=="Wet Region" | !DeltaMoistsub$landuse=="pasture" | !DeltaMoistsub$Moisture.m3.m3<.04,  ]
DeltaMoistsub<-DeltaMoistsub[!DeltaMoistsub$Region=="Wet Region" | !DeltaMoistsub$landuse=="wild" | !DeltaMoistsub$Moisture.m3.m3<.04,  ]

# Combine seasons
DeltaMoist2<-rbind(DeltaMoistWETsub,DeltaMoistDRYsub)
DeltaMoist2$Region
# Check patterns - looks OK - fault with pasture wet region - not data after end July
summary(is.na(DeltaMoist2$Moisture.m3.m3))
DeltaMoist3<-DeltaMoist2
DeltaMoist3<-DeltaMoist3[!is.na(DeltaMoist3$Moisture.m3.m3),]
DeltaMoistsub2<-DeltaMoistsub[!is.na(DeltaMoistsub$Moisture.m3.m3),]
summary(is.na(DeltaMoist3$Moisture.m3.m3))
max(DeltaMoist3$Moisture.m3.m3)
max(DeltaMoistsub2$Moisture.m3.m3)
# remove wet pasture wet season - no records...logger stolen
DeltaMoist3<-DeltaMoist3[!DeltaMoist3$Region=="Wet Region" | !DeltaMoist3$landuse=="pasture" |!DeltaMoist3$date2>"2017-03-27", ]

# Mean moisture - wet season and dry season
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/")
TempMoistFull <-read.csv('Termites/TempMoistFull.csv', sep=',',dec='.')
names(TempMoistFull)

TempMoistFull$date2<-as.Date(TempMoistFull$date,"%d/%m/%Y")
MeanDate<-aggregate(date2~landuse+Region+placement+Season,TempMoistFull, mean)
MeanM<-aggregate(moist~landuse+Region+placement+Season,TempMoistFull, mean)
MeanMsd<-aggregate(moist~landuse+Region+placement+Season,TempMoistFull,na.rm=T, sd)

library(lubridate)
TempMoistFull$time2<-as.POSIXct(as.character(TempMoistFull$time),format="%H:%M",tz="Africa/Nairobi")
class(TempMoistFull$time2)
class(TempMoistFull$time)
summary(is.na(TempMoistFull$time2))
min(TempMoistFull$time2, na.rm=T)
max(TempMoistFull$time2, na.rm=T)

MeanM$date2<-MeanDate$date2
MeanM$sd<-MeanMsd$moist
MeanM$SeUp<-MeanM$moist+MeanM$sd
MeanM$SeLo<-MeanM$moist-MeanM$sd

levels(MeanM$Region)<-c("Dry Region", "Intermediate Region", "Wet Region")

# Conditional subtractions
DeltaMoistsub2$region_code<-as.factor(with(DeltaMoistsub2, paste(landuse, Region, sep="-")))
levels(DeltaMoistsub2$region_code)
#[1] "agriculture-Dry Region"   "agriculture-Wet Region"   "pasture-Dry Region"       "pasture-Wet Region"      
#[5] "wild-Dry Region"          "wild-Intermediate Region" "wild-Wet Region"

# Min moisture
DeltaMoistsub2 %>% 
  group_by(Region,landuse) %>%
  summarise(Moisture.m3.m3_min = min(Moisture.m3.m3))
#1 Dry Region          agriculture              0.146
#2 Dry Region          pasture                  0.116
#3 Dry Region          wild                     0.084
#4 Wet Region          agriculture              0.056
#5 Wet Region          pasture                  0.04 
#6 Wet Region          wild                     0.04 
#7 Intermediate Region wild                     0.032

#DeltaMoistsub3<-DeltaMoistsub2 %>%
#  group_by(region_code) %>%
#  mutate(Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "agriculture-Dry Region"] - 0.146,
#         Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "pasture-Dry Region"] - 0.116,
#         Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Dry Region"] - 0.084,
#         Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "agriculture-Wet Region" ]-0.056,
#         Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "pasture-Wet Region" ] - 0.04,
#         Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Wet Region"] - 0.04,
#         Moisture.m3.m3Correct = DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Intermediate Region"] - 0.032)

# Adjusting for baseline moistures
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "agriculture-Dry Region"]<-DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "agriculture-Dry Region"] - 0.146
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "pasture-Dry Region"]<-DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "pasture-Dry Region"] - 0.11
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Dry Region"] <-DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Dry Region"] - 0.084
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "agriculture-Wet Region" ]<-DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "agriculture-Wet Region" ]-0.056
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "pasture-Wet Region" ] <-DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "pasture-Wet Region" ] - 0.04
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Wet Region"] <- DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Wet Region"] - 0.04
DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Intermediate Region"] <-DeltaMoistsub2$Moisture.m3.m3[DeltaMoistsub2$region_code == "wild-Intermediate Region"] - 0.092


# Scaling factors
# Change scaling facotr...
scaleFactorM <-  max(MeanM$moist,na.rm=T)/mean(DeltaMoistsub2$Moisture.m3.m3,na.rm=T)
scaleFactorM

levels(DeltaMoistsub2$Region)<-c("Mesic region", "Wet region", "Mesic-wet region")
levels(MeanM$Region)<-c("Mesic region", "Mesic-wet region", "Wet region")
levels(DeltaMoistsub$landuse)[DeltaMoistsub$area=="Seronera"]

max(DeltaMoistsub2$Moisture.m3.m3)
# Soil moisture logger through time
pM<-ggplot(data=DeltaMoistsub2,aes(x=date2,y=Moisture.m3.m3))
pM<-pM+geom_rect(aes(xmin =as.Date("2017-01-26"), xmax =as.Date("2017-03-27"), ymin=0,ymax=max(DeltaMoistsub2$Moisture.m3.m3)),fill = "light grey", colour="grey")
pM<-pM+geom_rect(aes(xmin =as.Date("2017-07-21"), xmax =as.Date("2017-10-03"), ymin=0,ymax=max(DeltaMoistsub2$Moisture.m3.m3)),fill = NA, colour="grey")
pM<-pM+scale_y_continuous(limits=c(0,max(DeltaMoistsub2$Moisture.m3.m3)),sec.axis = sec_axis(~ .*scaleFactorM, breaks = c(0,50,100), labels = c(0,50,100), name="Soil moisture (%)" ))
pM<-pM+geom_point(fill="white", colour="grey50", shape=21, size=1.5)
pM<-pM+geom_errorbar(data=MeanM,aes(y=moist/scaleFactorM,ymax=SeUp/scaleFactorM,ymin=SeLo/scaleFactorM),colour="black",width=.1)
pM<-pM+geom_point(data=MeanM,aes(y=moist/scaleFactorM),size=3,colour="black",fill="black",shape=22,stroke=1,show.legend=F)
pM<-pM+facet_wrap(~landuse+Region,ncol=2)
pM<-pM+scale_x_date(date_breaks = "3 month", date_labels = "%b", limits=c(as.Date("2017-01-01"),max=as.Date("2017-12-31")))
pM<-pM+ylab(expression(paste("Soil moisture (",m^-3," ",m^-3,")"))) + xlab(" Month")
pM<-pM+theme_classic()
pM
  
# Average Temp and mosisture
aggregate(moist~Season+Region+landuse,MeanM,mean)

# Seasons - 
aggregate(temperature.C~Season,MeanT,mean)
29.40288-27.18085 # warmer in dry season
aggregate(moist~Season,MeanM,mean)
7.227545-20.499242 # -13.2717
# SOIL TEMPERATURE
# Seperate datasets DRY SEASON and WET SEASON
# Wet season: 26/01/2017 - 27/03/2017
# Dry season: 20/07/2017 - 03/10/2017

names(TinyTagTemp)
TinyTagTemp$date2<-as.Date(TinyTagTemp$date,"%d.%m.%Y")

# Landuses
levels(TinyTagTemp$landuse)<-c("agriculture",  "pasture","wild" , "pasture") # Logger changed location

TinyTagTempWET<-TinyTagTemp[TinyTagTemp$date2>"2017-01-26" & TinyTagTemp$date2<"2017-03-27", ]
TinyTagTempDRY<-TinyTagTemp[TinyTagTemp$date2>"2017-07-20" & TinyTagTemp$date2<"2017-10-03", ]

dim(TinyTagTemp) #  99396    18
dim(TinyTagTempWET) # 16906    19
dim(TinyTagTempDRY) # 15978    19
TinyTagTempWET$Season<-"Wet Season"
TinyTagTempDRY$Season<-"Dry Season"

# Variable names
TinyTagTempWET$Region<-TinyTagTempWET$area
TinyTagTempDRY$Region<-TinyTagTempDRY$area
levels(TinyTagTempWET$Region)<-c("Dry Region","Dry Region","Wet Region","Intermediate Region","Wet Region")
levels(TinyTagTempDRY$Region)<-c("Dry Region","Dry Region","Wet Region","Intermediate Region","Wet Region")
#TinyTagTempWETsub<-droplevels(TinyTagTempWET[!TinyTagTempWET$area=="Seronera",])
#TinyTagTempDRYsub<-droplevels(TinyTagTempDRY[!TinyTagTempDRY$area=="Seronera",])

# Combine seasons
TinyTagTemp2<-rbind(TinyTagTempWET,TinyTagTempDRY)

# Variable names main dataset
TinyTagTemp$Region<-TinyTagTemp$area
levels(TinyTagTemp$Region)<-c("Dry Region","Dry Region","Wet Region","Intermediate Region","Wet Region")
TinyTagSero<-droplevels(TinyTagTemp[TinyTagTemp$area=="Seronera",])
levels(TinyTagSero$plotcode)
TinyTagTempsub<-TinyTagTemp[!TinyTagTemp$plotcode=="TZ_SS_SW103" & !TinyTagTemp$plotcode== "TZ_SS_SW105",]
#TinyTagTempsub<-droplevels(TinyTagTempsub[!TinyTagTempsub$Region=="Intermediate Region",])
# Tiny tag "TZ_SS_SW103" and "TZ_SS_SW105" are diretly under tree canopies - thus need 

# Mean temperature - wet season and dry season
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/")
TempMoistFull <-read.csv('Termites/TempMoistFull.csv', sep=',',dec='.')
names(TempMoistFull)

TempMoistFull$date2<-as.Date(TempMoistFull$date,"%d/%m/%Y")
MeanDate<-aggregate(date2~landuse+Region+placement+Season,TempMoistFull, mean)
MeanT<-aggregate(temperature.C~landuse+Region+placement+Season,TempMoistFull, mean)
MeanTsd<-aggregate(temperature.C~landuse+Region+placement+Season,TempMoistFull, sd)

MeanT$date2<-MeanDate$date2
MeanT$sd<-MeanTsd$temperature.C
levels(MeanT$Region)<-c("Dry Region", "Intermediate Region", "Wet Region")

# Soil temperature logger through time
TinyTagTemp3<-TinyTagTemp2[!is.na(TinyTagTemp2$temperature.C),]
TinyTagTempNA<-TinyTagTempsub[!is.na(TinyTagTempsub$temperature.C),]
MaxTempC<-max(MeanT$temperature.C)+MeanT$sd

levels(TinyTagTempNA$Region)<-c("Mesic region", "Wet region", "Mesic-wet region")
levels(MeanT$Region)<-c("Mesic region", "Mesic-wet region", "Wet region")

pT<-ggplot(data=TinyTagTempNA,aes(x=date2,y=temperature.C))
pT<-pT+geom_rect(aes(xmin =as.Date("2017-01-26"), xmax =as.Date("2017-03-27"), ymin=18,ymax=max(MaxTempC)),fill = "light grey", colour="grey")
pT<-pT+geom_rect(aes(xmin =as.Date("2017-07-21"), xmax =as.Date("2017-10-03"), ymin=18,ymax=max(MaxTempC)),fill = NA, colour="grey")
pT<-pT+geom_point(fill="white", colour="grey50",shape=21, size=1.5)
pT<-pT+geom_errorbar(data=MeanT,aes(x=date2,y=temperature.C,ymax=temperature.C+sd, ymin=temperature.C-sd), colour="black",width=.1)
pT<-pT+geom_point(data=MeanT,fill="black", colour="black",shape=22, size=3.5, stroke=1)
pT<-pT+facet_wrap(~landuse+Region, ncol=2)
pT<-pT+scale_x_date(date_breaks = "3 month", date_labels = "%b", limits=c(as.Date("2017-01-01"),max=as.Date("2017-12-31")))
pT<-pT+scale_y_continuous(limits=c(18,max(MaxTempC)))
pT<-pT+ylab(expression(paste("Soil temperature (",degree,"C)"))) + xlab("Month")
pT<-pT+theme_classic()
pT

# Cacluate mean, sd, min and sd Moisture and Temperature

library(dplyr)
summary(is.na(DeltaMoist2$Moisture.m3.m3))
DeltaMoist2<-DeltaMoist2[!is.na(DeltaMoist2$Moisture.m3.m3),]
# Landuse, region, season mean moisture
# Mean moisture
DeltaMoist2 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_mean = mean(Moisture.m3.m3))

# Min moisture
DeltaMoist2 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_min = min(Moisture.m3.m3))

# Min moisture
DeltaMoist2 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_max = max(Moisture.m3.m3, na.rm=T))

# Sd moisture
DeltaMoist2 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_sd = sd(Moisture.m3.m3, na.rm=T))

# Sum moisture by date
DeltaMoist2 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_sum = sum(Moisture.m3.m3, na.rm=T))

PrecipSumDate<-DeltaMoist2 %>% 
  group_by(Region,Season,landuse,date) %>%
  summarise(Moisture.m3.m3_sum = sum(Moisture.m3.m3, na.rm=T))

#library(MESS)
#DeltaMoist2 %>%
#  group_by(Region,Season,landuse) %>%
#  summarise(AUC = auc(date2,Moisture.m3.m3, type = "spline"))


PrecipSumDate %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_sum2 = mean(Moisture.m3.m3_sum))

PrecipSumDate %>% 
  group_by(Region,Season,landuse) %>%
  summarise(Moisture.m3.m3_sd2 = sd(Moisture.m3.m3_sum))

# Temperature

# Landuse, region, season mean temp
# Mean temp
names(TinyTagTemp3)
TinyTagTemp3 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(temperature.C_mean = mean(temperature.C)*100)

# Min temp
TinyTagTemp3 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(temperature.C_min = min(temperature.C)*100)

# Max temp
TinyTagTemp3%>% 
  group_by(Region,Season,landuse) %>%
  summarise(temperature.C_max = max(temperature.C)*100)

# Sd moisture
TinyTagTemp3 %>% 
  group_by(Region,Season,landuse) %>%
  summarise(temperature.C_sd = sd(temperature.C)*100)

#### SUMMARY LOGGER DATA #### 
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/")
# Import summary
TinyD<-read.csv(file="TBI.loggerSummary.csv", sep=",",header=TRUE)
names(TinyD)
dim(TinyD) #14 17
head(TinyD)
#### Moisture,Temp, Sand, CN model ####
DataMain$Site<-as.factor(DataMain$Site)
DataMain$Blockcode<-as.factor(DataMain$Blockcode)
DataMain$Plot<-as.factor(DataMain$Plot)

colnames(DataMain)[(names(DataMain)== "Temperature..C.")] <- "Temp"
colnames(DataMain)[(names(DataMain)== "Moisture..")] <- "Moisture"
colnames(DataMain)[(names(DataMain)== "Sandcorr")] <- "Sand"

#### Moisture - hand-held and logger ####
min(DataMain$Moisture,na.rm=T)
DataMainM<-DataMain[!is.na(DataMain$Moisture),]

TinyD$Landuse<-TinyD$landuse
TinyD$Season<-TinyD$season
TinyD$Region<-TinyD$rain.region
levels(DataMainM$Landuse)<-c("Agriculture","Pasture", "Wild")
levels(TinyD$Landuse)<-c("Agriculture","Pasture", "Wild")

TinyD2<-droplevels(TinyD[!TinyD$rain.region=="Intermediate",])
TinyDs<-droplevels(TinyD[TinyD$rain.region=="Intermediate",])

# Upper and lower SDx
TinyD2$SeUp<-TinyD2$Moist.3.m3_meanCorrected+TinyD2$Moist.3.m3_sd
TinyD2$SeLo<-TinyD2$Moist.3.m3_meanCorrected-TinyD2$Moist.3.m3_sd

# Soil moisture - spot measurements and loggers
MeanMoist<-aggregate(Moisture~Season+Region+Landuse,DataMainM,mean)
MeanMoistsd<-aggregate(Moisture~Season+Region+Landuse,DataMainM,sd)
MeanMoist$sd<-MeanMoistsd$Moisture
MeanMoist$SeUp<-MeanMoist$Moisture+MeanMoist$sd
MeanMoist$SeLo<-MeanMoist$Moisture-MeanMoist$sd

scaleFactor <- mean(MeanMoist$Moisture,na.rm=T) / mean(TinyD2$Moist.3.m3_meanCorrected,na.rm=T)
scaleFactor 

#DeltaMoist$Moisture<-DeltaMoist$Moisture.m3.m3*scaleFactor
#DeltaMoist = DeltaMoist[seq(1, nrow(DeltaMoist), 10), ]

levels(MeanMoist$Season)<-c("Dry Season","Wet Season")
levels(DataMainM$Season)<-c("Dry Season","Wet Season")
levels(TinyD2$Season)<-c("Dry Season","Wet Season")

levels(MeanMoist$Region)<-c("Dry Region","Wet Region")
levels(DataMainM$Region)<-c("Dry Region","Wet Region")
levels(TinyD2$Region)<-c("Dry Region","Wet Region")

SM<-ggplot(MeanMoist,aes(y=Moisture, x=Landuse))
SM<-SM+scale_y_continuous(limits=c(-4,48),sec.axis = sec_axis(~ ./scaleFactor, breaks = c(0,.05,.1,.15,.2), labels = c(0,.05,.1, .15,.2), name=(expression(paste("Diurnal logger soil moisture (",m^-3," ",m^-3,")"))) ))
#SM<-SM+geom_jitter(data=DeltaMoist,shape=22,colour="grey", fill="white",alpha=.65,size=1.5)
SM<-SM+geom_jitter(data=DataMainM,colour="dark grey",alpha=.65,size=1.5)
SM<-SM+geom_errorbar(aes(ymax=SeUp,ymin=SeLo),width=.1,position = position_nudge(x = -0.1))
SM<-SM+ geom_point(colour="black", fill="black",size=3.5,stroke=1,position = position_nudge(x = -0.1))
SM<-SM+geom_errorbar(data=TinyD2,aes(y=Moist.3.m3_meanCorrected*scaleFactor,ymax=SeUp*scaleFactor,ymin=SeLo*scaleFactor),width=.1,position = position_nudge(x = 0.1))
SM<-SM+geom_point(data=TinyD2,aes(y=Moist.3.m3_meanCorrected*scaleFactor),size=3.5,colour="black",fill="white",shape=22,stroke=1,show.legend=F,position = position_nudge(x = 0.1))
SM<-SM+facet_wrap(~Season+Region)
SM<-SM+xlab("Land-use")+ylab("Daytime spot measure soil moisture (%)")
SM<-SM+theme_classic()
SM

#### Temperature - hand-held and logger ####
min(DataMain$Temp,na.rm=T)

TinyD$Landuse<-TinyD$landuse
TinyD$Season<-TinyD$season
TinyD$Region<-TinyD$rain.region
levels(DataMainM$Landuse)<-c("Agriculture","Pasture", "Wild")
levels(TinyD$Landuse)<-c("Agriculture","Pasture", "Wild")

TinyD2<-droplevels(TinyD[!TinyD$rain.region=="Intermediate",])
TinyDs<-droplevels(TinyD[TinyD$rain.region=="Intermediate",])

# Upper and lower SD
names(TinyD2)
TinyD2$TSeUp<-TinyD2$Temp.C_mean+TinyD2$Temp.C_sd
TinyD2$TSeLo<-TinyD2$Temp.C_mean-TinyD2$Temp.C_sd

# Soil moisture - spot measurements and loggers
MeanTemp<-aggregate(Temp~Season+Region+Landuse,DataMain,mean)
MeanTempsd<-aggregate(Temp~Season+Region+Landuse,DataMain,sd)
MeanTemp$sd<-MeanTempsd$Temp
MeanTemp$SeUp<-MeanTemp$Temp+MeanTemp$sd
MeanTemp$SeLo<-MeanTemp$Temp-MeanTemp$sd

scaleFactorT <- mean(MeanTemp$Temp,na.rm=T)/mean(TinyD2$Temp.C_mean,na.rm=T)
scaleFactorT 

MeanTemp$Temp
TinyD2$Temp.C_mean
TinyD2$Temp.C_

levels(MeanTemp$Season)<-c("Dry Season","Wet Season")
levels(DataMain$Season)<-c("Dry Season","Wet Season")
levels(TinyD2$Season)<-c("Dry Season","Wet Season")

levels(MeanTemp$Region)<-c("Dry Region","Wet Region")
levels(DataMain$Region)<-c("Dry Region","Wet Region")
levels(TinyD2$Region)<-c("Dry Region","Wet Region")

TM<-ggplot(MeanTemp,aes(y=Temp, x=Landuse))
TM<-TM+scale_y_continuous(limits=c(20,36),sec.axis = sec_axis(~ ./1, breaks = c(20,25,30,35), labels = c(20,25,30,35), name=(expression(paste("Diurnal logger soil temperature (",degree,"C)"))) ))
TM<-TM+geom_jitter(data=DataMain,colour="dark grey",alpha=.5,size=1.5)
TM<-TM+geom_errorbar(aes(ymax=SeUp,ymin=SeLo),width=.1,position = position_nudge(x = -0.1))
TM<-TM+geom_point(colour="black", fill="black",size=3.5,stroke=1,position = position_nudge(x = -0.1))
TM<-TM+geom_errorbar(data=TinyD2,aes(y=Temp.C_mean*1,ymax=TSeUp*1,ymin=TSeLo*1),width=.1,position = position_nudge(x = 0.1))
TM<-TM+geom_point(data=TinyD2,aes(y=Temp.C_mean*1),size=3.5,colour="black",fill="white",shape=22,stroke=1,show.legend=F,position = position_nudge(x = 0.1))
TM<-TM+facet_wrap(~Season+Region)
TM<-TM+xlab("Land-use")+ylab(expression(paste("Daytime spot measure soil temperature (",degree,"C)")))
TM<-TM+theme_classic()
TM

# Spot measurements are warmer...
ComboT<-left_join(TinyD2,MeanTemp, by=c("Season","Region","Landuse"))
head(ComboT)
mean(((ComboT$Temp-ComboT$Temp.C_mean)/ComboT$Temp*100), na.rm=T) # 8% warmer than mean spot measure
sd(((ComboT$Temp-ComboT$Temp.C_mean)/ComboT$Temp*100), na.rm=T) # 11%
max(((ComboT$Temp-ComboT$Temp.C_mean)/ComboT$Temp*100), na.rm=T) # 24% 

#########################################################################################################################
#### CN model model ####
CNModFINAL2 <- glmmTMB(C.N~Season+Region+Landuse+
                         +Region:Landuse+Season:Landuse+Season:Region+
                         Season:Region:Landuse+
                         (1|Blockcode), data=DataMain)

summary(CNModFINAL2)
drop1(CNModFINAL2, test="Chisq")

sjstats::icc(CNModFINAL2)
# ICC (Blockcode): 0.5582
sjstats::r2(CNModFINAL2) #works when removing site
#     Marginal R2: 0.364
# Conditional R2: 0.719

bwplot(C.N~Landuse|Region*Season, DataMain)

#Inspect chosen model for homogeneity:
E1 <- resid(CNModFINAL2, type ="pearson")
F1 <- fitted(CNModFINAL2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)# OK

# Update Moisture model with moisture
# Three-way
CNMod<- update(CNModFINAL2, .~. -  Season:Region:Landuse)

#Two-way
CNMod2<- update(CNMod, .~. -Season:Region)
CNMod3<- update(CNMod, .~. -Season:Landuse)
CNMod4<- update(CNMod, .~. -Region:Landuse)

#One-way
CNModFINAL2b<- glmmTMB(C.N~ Season+Region+Landuse+(1|Blockcode), data=DataMain)
CNMod5<- update(CNModFINAL2b, .~. -Landuse)
CNMod6<- update(CNModFINAL2b, .~. -Region)
CNMod7<- update(CNModFINAL2b, .~. -Season)

# ANOVA
anova(CNModFINAL2,CNMod)
anova(CNMod,CNMod2)
anova(CNMod,CNMod3)
anova(CNMod,CNMod4)
anova(CNModFINAL2b,CNMod5)
anova(CNModFINAL2b,CNMod6)
anova(CNModFINAL2b,CNMod7)

#            Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
#CNModFINAL2 14 3970.9 4045.6 -1971.4   3942.9 142.5      2  < 2.2e-16 *** #Season:Region:Landuse
#CNMod  12 4109.4 4173.4 -2042.7   4085.4 8.5734      1   0.003411 ** #Season:Region
#CNMod  12 4109.4 4173.4 -2042.7   4085.4 120.88      2  < 2.2e-16 *** #Season:Landuse
#CNMod  12 4109.4 4173.4 -2042.7   4085.4 8.6814      2    0.01303 * #Region:Landuse
#CNModFINAL2b  7 4236.8 4274.2 -2111.4   4222.8 4.3037      2     0.1163 #Landuse
#CNModFINAL2b  7 4236.8 4274.2 -2111.4   4222.8 1.4786      1      0.224 #Region
#CNModFINAL2b  7 4236.8 4274.2 -2111.4   4222.8 80.93      1  < 2.2e-16 *** #Season

#### Sand model #### 
#Do not have different values for seasons #
SandModFINAL2 <- glmmTMB(Sand~ Region+Landuse+Region:Landuse+
                           (1|Site), data=DataMain)

summary(SandModFINAL2)
drop1(SandModFINAL2, test="Chisq")

####################################################################################################################
#### COMMON GARDEN EXP ####
#Dataprocessing - getting data ready for modelling####
DataCG<-droplevels(Fulldata[Fulldata$Experiment=="CG",]) # Only commongarden data
DataMain<-droplevels(Fulldata[Fulldata$Experiment=="Main",]) #Only landuse experiement data
LocalCGsoil3 <- DataCG[DataCG$Site=="Seronera",]
#Removing 4-block design into 1 block with 4 replicates in Seronera local soil:
LocalCGsoil3$Block <- 1
LocalCGsoil3$Blockcode <- "Int_W1"

DataCGexLocal<-droplevels(DataCG[DataCG$Site!="Seronera",]) # Only commongarden data without local soil

#Add local soil from CG to Main experiment:
DataMain <- rbind.fill(DataMain,LocalCGsoil3)
#Add adjusted blockcode naming in local soil to CG:
DataCG <- rbind.fill(DataCGexLocal,LocalCGsoil3)
#Checking factor levels of the added blockcode:
DataCG$Blockcode <- as.factor(DataCG$Blockcode)
levels(DataCG$Blockcode) #OK
DataMain$Blockcode <- as.factor(DataMain$Blockcode)
levels(DataMain$Blockcode) #OK

#Then, want to combine the two data set by left_join.
#Problem: CG data has 224 obs (on block level), whilce main exp. has 1568 obs (on plot level).
#Need to aggregate Main to block level, first:
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

DataMainSummary<-aggregate(cbind(Massloss.per,Massloss..g.,Moisture..,Temperature..C.,Rain.sum, C.N, Sandcorr, Claycorr)~Season+Landuse+Region+Blockcode+Treatment+Littertype,DataMain,mean)
#DataMainSummaryse <- aggregate(cbind(Massloss.per,Massloss..g.,Moisture..,Temperature..C.,Rain.sum, C.N, Sandcorr, Claycorr)~Season+Landuse+Region+Blockcode+Treatment+Littertype,DataMain, se)
#DataMainSummary$SE <- DataMainSummaryse$
length(DataMainSummary$Massloss.per) #200

Regionlvl <-  levels(DataCG$Region) #Adjusting correct factor level order
levels(DataMain$Region) <- Regionlvl #Adjusting correct level order
Landuselvl <- levels(DataMain$Landuse) #Adjusting correct level order
levels(DataCG$Landuse) <- Landuselvl #Adjusting correct level order

names(DataMainSummary)
names(DataCG)
Blockcodelvl <- levels(DataCG$Blockcode)
levels(DataMainSummary$Blockcode) <- Blockcodelvl

DataMCG <- left_join(DataMainSummary,DataCG,by=c("Season","Treatment","Littertype","Blockcode"))

#Creating variables for difference between Main (x) and CG (y):
DataMCG$MainCGdiff <- DataMCG$Massloss.per.x-DataMCG$Massloss.per.y
DataMCG$Moistdiff <- DataMCG$Moisture...x -DataMCG$Moisture...y
DataMCG$Tempdiff <- DataMCG$Temperature..C..x -DataMCG$Temperature..C..y

#Renaming some columns:
names(DataMCG)
colnames(DataMCG)[(names(DataMCG) == "C.N.x")] <-"C.N"
colnames(DataMCG)[(names(DataMCG)== "Sandcorr.x")] <- "Sand"
colnames(DataMCG)[(names(DataMCG)== "Claycorr.x")] <- "Clay"
colnames(DataMCG)[(names(DataMCG)== "Moisture...x")] <- "Moisture"
colnames(DataMCG)[(names(DataMCG)== "Temperature..C..x")] <- "Temp"
colnames(DataMCG)[(names(DataMCG)== "Landuse.x")] <- "Landuse"
colnames(DataMCG)[(names(DataMCG)== "Region.x")] <- "Region"
colnames(DataMCG)[(names(DataMCG)== "Rain.sum.x")] <- "Rain"

#Creating dataset for each littertype:
RecalDataMCG <- droplevels(DataMCG[DataMCG$Littertype =="Rooibos",])
LabileDataMCG <- droplevels(DataMCG[DataMCG$Littertype =="Green",])

# Drop Seronera - intermediate rainfall
#RecalDataMCG <- droplevels(RecalDataMCG[!RecalDataMCG$Region =="Intermediate",])
#LabileDataMCG<- droplevels(LabileDataMCG[!LabileDataMCG$Region =="Intermediate",])

#Ensuring factors are factors:
RecalDataMCG$Season <- as.factor(RecalDataMCG$Season)
RecalDataMCG$Region <- as.factor(RecalDataMCG$Region)
RecalDataMCG$Landuse <- as.factor(RecalDataMCG$Landuse)
RecalDataMCG$Treatment <- as.factor(RecalDataMCG$Treatment)
LabileDataMCG$Season <- as.factor(LabileDataMCG$Season)
LabileDataMCG$Region <- as.factor(LabileDataMCG$Region)
LabileDataMCG$Landuse <- as.factor(LabileDataMCG$Landuse)
LabileDataMCG$Treatment <- as.factor(LabileDataMCG$Treatment)

####################################################################################################################

#COMMON GARDEN MODELLING####

#### Labile CG model analysis####

GlobalLabileMCGMod <- glmmTMB(MainCGdiff ~Season+Region+#Landuse+Treatment+
                               # Region:Landuse+#Landuse:Treatment+Region:Treatment+
                              #  Season:Treatment+Season:Landuse+Season:Region+
                              #  Season:Region:Landuse+#Season:Region:Treatment+
                                #Season:Landuse:Treatment+
                                (1|Blockcode), data=LabileDataMCG)

summary(GlobalLabileMCGMod)
drop1(GlobalLabileMCGMod, test="Chisq")

r.squaredGLMM(GlobalLabileMCGMod)
#           R2m       R2c
#[1,] 0.6816161 0.6816162

sjstats::icc(GlobalLabileMCGMod)
#  ICC (Blockcode): 0.1701
sjstats::r2(GlobalLabileMCGMod) #works when removing site
#     Marginal R2: 0.800
# Conditional R2: 0.834

plot(MainCGdiff~Moisture,LabileDataMCG)
xyplot(MainCGdiff~Moisture|Landuse*Region*Season,LabileDataMCG)

# Update Labile CG vs main experiment model
#GLabMod<- update(GlobalLabileMCGMod, .~. -Treatment)
#GLabMod1<- update(GlobalLabileMCGMod, .~. -Landuse)
GLabMod2<- update(GlobalLabileMCGMod, .~. -Region)
GLabMod3<- update(GlobalLabileMCGMod, .~. -Season)

# ANOVA
#anova(GlobalLabileMCGMod,GLabMod)
#anova(GlobalLabileMCGMod,GLabMod1)
anova(GlobalLabileMCGMod,GLabMod2)
anova(GlobalLabileMCGMod,GLabMod3)

#                   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#GlobalLabileMCGMod  6 931.09 947.40 -459.55   919.09 42.658      2  5.457e-10 *** # Region
#GlobalLabileMCGMod  6  931.09  947.4 -459.55   919.09 82.271      1  < 2.2e-16 *** # Season

#Inspect chosen model for homogeneity:
E1 <- resid(GlobalLabileMCGMod, type ="pearson")
F1 <- fitted(GlobalLabileMCGMod)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)# Very good


CGLabileMainr2<-rbind(
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Season+(1|Blockcode), data=LabileDataMCG))/ r.squaredGLMM(GlobalLabileMCGMod),
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Region+(1|Blockcode), data=LabileDataMCG))/ r.squaredGLMM(GlobalLabileMCGMod), 
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Landuse+(1|Blockcode), data=LabileDataMCG))/ r.squaredGLMM(GlobalLabileMCGMod),
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Treatment+(1|Blockcode), data=LabileDataMCG))/r.squaredGLMM(GlobalLabileMCGMod))

#  #r.squaredGLMM(glmmTMB(MainCGdiff ~ Temp+(1|Blockcode/Plot), data=LabileDataMCG))/r.squaredGLMM(GlobalLabileMCGMod),

CGLabileMainr2b<-as.data.frame(CGLabileMainr2)
CGLabileMainr2b$terms<-c("Season","Region","Landuse","Treatment")
CGLabileMainr2b$terms<- factor(CGLabileMainr2b$terms, levels = CGLabileMainr2b$terms[order(CGLabileMainr2b$R2m)])
CGLabileMainr2b$R2m
ggplot(CGLabileMainr2b, aes(y=terms, x=R2m))+geom_point(size=3)+theme_classic()

boxplot(MainCGdiff ~ Season,LabileDataMCG)
boxplot(MainCGdiff ~ Region,LabileDataMCG)

####################################################################################################################

#### Recal CG model analysis####

GlobalRecalMCGMod <- glmmTMB(MainCGdiff ~Season+Treatment+#Region+Landuse+
                                # Region:Landuse+#Landuse:Treatment+Region:Treatment+
                                #  Season:Treatment+Season:Landuse+Season:Region+
                                #  Season:Region:Landuse+#Season:Region:Treatment+
                                #Season:Landuse:Treatment+
                                (1|Blockcode), data=RecalDataMCG)


summary(GlobalRecalMCGMod)
drop1(GlobalRecalMCGMod, test="Chisq")

r.squaredGLMM(GlobalRecalMCGMod)
#            R2m       R2c
#[1,] 0.1318157 0.1318374

# Update Recal CG vs main experiment model
GRecMod<- update(GlobalRecalMCGMod, .~. -Treatment)
#GRecMod1<- update(GlobalRecalMCGMod, .~. -Landuse)
#GRecMod2<- update(GlobalRecalMCGMod, .~. -Region)
GRecMod3<- update(GlobalRecalMCGMod, .~. -Season)

# ANOVA
anova(GlobalRecalMCGMod,GRecMod)
#anova(GlobalRecalMCGMod,GRecMod1)
#anova(GlobalRecalMCGMod,GRecMod2)
anova(GlobalRecalMCGMod,GRecMod3)

#                  Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# GlobalRecalMCGMod  5 1002.2 1015.8 -496.11   992.22 4.5104      1    0.03369 * # Treatment
#GlobalRecalMCGMod  5 1002.2 1015.8 -496.11   992.22 11.36      1  0.0007505 *** # Season

#Inspect chosen model for homogeneity:
E1 <- resid(GlobalRecalMCGMod, type ="pearson")
F1 <- fitted(GlobalRecalMCGMod)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)# ALL good


CGRecalMainr2<-rbind(#CGLabileMain<-r.squaredGLMM(GlobalLabileMCGMod),,
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Season+ (1|Blockcode), data=RecalDataMCG))/r.squaredGLMM(GlobalRecalMCGMod), 
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Region+(1|Blockcode), data=RecalDataMCG))/r.squaredGLMM(GlobalRecalMCGMod), 
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Landuse+(1|Blockcode), data=RecalDataMCG))/r.squaredGLMM(GlobalRecalMCGMod), 
  r.squaredGLMM(glmmTMB(MainCGdiff ~ Treatment+(1|Blockcode), data=RecalDataMCG))/r.squaredGLMM(GlobalRecalMCGMod))

CGRecalMainr2b<-as.data.frame(CGRecalMainr2)
CGRecalMainr2b$terms<-c("Season","Region","Landuse","Treatment")
CGRecalMainr2b$terms<- factor(CGRecalMainr2b$terms, levels = CGRecalMainr2b$terms[order(CGRecalMainr2b$R2m)])
CGRecalMainr2b$R2m
ggplot(CGRecalMainr2b, aes(y=terms, x=R2m))+geom_point()+theme_classic()

boxplot(MainCGdiff ~ Season,RecalDataMCG)
boxplot(MainCGdiff ~ Treatment,RecalDataMCG)

##########################################################################################

##########################################################################################
#### INLA # Bayesian approach####
#Spatial position of the sampling locations:
##########################################################################################
library(lattice)
names(RecalMain2)
par(mfrow=c(1,1), mar=c(1,1,1,1))
#coordinates(SppSNPs) <- c("Lat", "Lon")
loc2 <- cbind(RecalMain2S$Lat, RecalMain2S$Long)

#Not in the book:
xyplot(loc2[,2] ~ loc2[,1],
       aspext = "iso")

# REMOVE SERONERA 
RecalMain2S<-droplevels(RecalMain2[!RecalMain2$Site=="Seronera",])

# Create a grid mesh
mesh5a <- inla.mesh.2d(loc2, max.edge=c(10, 10), cutoff = .0001) # 1 or 10 m m?

#Install package splancs # Shape files to define boundaries
library(splancs)
zzdomain2 <- inla.nonconvex.hull(loc2)
mesh6a <- inla.mesh.2d(boundary = zzdomain2, max.edge=c(20, 20), cutoff = 0.001)
# NEed to know what the value of the cut-off equates to

# Plot mesh
plot(mesh5a, asp=1)
points(loc2,col=2,pch=16, cex=.5)

#And mesh is mesh3 from here onwards
mesh5a$n #  598 # cut off= 0.0001
# 333 # cut = 0.001
# Tell INLA which sampling locations match the points
# on the mesh
A1      <- inla.spde.make.A(mesh5a, loc = loc2) # Tells INLA where the sampling locations
# Values 0 and 1 #one equals sampling location
# Also need to do for the covariates - but covariates are not always at the same position

#Define the Matern correlation on the mesh
spde2   <- inla.spde2.matern(mesh5a, alpha = 2) # Quantify distance between points
# Will be explained on the next Powerpoint slide


# Section 10.7.3
# Set up the model. 
# Create a data frame with an intercept and the covariate

N2 <- nrow(RecalMain2S)
X2 <- data.frame(Intercept = rep(1,N2), 
                 Season = RecalMain2S$Season,
                 Region=RecalMain2S$Region,
                 Landuse=RecalMain2S$Landuse,
                 Treatment = RecalMain2S$Treatment,
                 C.N=RecalMain2S$C.N,
                 Temp=RecalMain2S$Temp,
                 Sand=RecalMain2S$Sand) # Covariates
str(X2)
#Tell INLA that the covariates are sampled at the same
#sampling locations.	                       
stk.e1 <- inla.stack(
  tag = "est",
  data = list(y = RecalMain2S$Massloss.perB),  # Y vaiable
  A = list(A1,1),      #This is the confusing bit # Sampling locations      
  effects = list(                 
    s = 1:spde2$n.spde,       #Spatial field  
    X2))                      #Covariates

dim(inla.stack.A(stk.e1)) #688 782
########################################

#10.7.4 Executing inla
# With and without spatial dependecy
f4a <- y ~ -1 + Intercept + f(Temp,model='rw2') +Season+Region+Landuse+C.N+Sand+Treatment+
  #Treatment:Sand+Treatment:C.N+Landuse:Temp+Landuse:C.N+
  Landuse:Treatment+Region:Treatment+
  +Region:Landuse+Season:Treatment+Season:Landuse+Season:Region+
    Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment

f5a <- y ~ -1 + Intercept + f(Temp,model='rw2') +Season+Region+Landuse+#C.N+Sand+Treatment+
  #Treatment:Sand+Treatment:C.N+Landuse:Temp+Landuse:C.N+
  Landuse:Treatment+Region:Treatment+
  +Region:Landuse+Season:Treatment+Season:Landuse+Season:Region+
  Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment +  f(s, model=spde2)

# With and without smoother
f5a <- y ~ -1 + Intercept + f(Temp,model='rw2') +Season+Region+Landuse+#C.N+Sand+Treatment+
  #Treatment:Sand+Treatment:C.N+Landuse:Temp+Landuse:C.N+
  Landuse:Treatment+Region:Treatment+
  +Region:Landuse+Season:Treatment+Season:Landuse+Season:Region+
  Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment

f5a2 <-y ~ -1 + Intercept + +Season+Region+Landuse+C.N+Sand+Treatment+
  #Treatment:Sand+Treatment:C.N+Landuse:Temp+Landuse:C.N+
  Landuse:Treatment+Region:Treatment+
  +Region:Landuse+Season:Treatment+Season:Landuse+Season:Region+
  Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment +  f(s, model=spde2)

# Beta model with random structure and nothing

f6a <- Massloss.perB ~ +Season+Region+Landuse+C.N+Sand+Treatment+Temp+
  #Treatment:Sand+Treatment:C.N+Landuse:Temp+Landuse:C.N+
  Landuse:Treatment+Region:Treatment+
  +Region:Landuse+Season:Treatment+Season:Landuse+Season:Region+
  Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+
  f(Site, model = "iid")+f(Blockcode, model = "iid")+f(Plot, model = "iid")

f6aN <- Massloss.perB ~ +Season+Region+Landuse+C.N+Sand+Treatment+Temp+
  #Treatment:Sand+Treatment:C.N+Landuse:Temp+Landuse:C.N+
  Landuse:Treatment+Region:Treatment+
  +Region:Landuse+Season:Treatment+Season:Landuse+Season:Region+
  Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment

I4a <- inla(f4a,
            family = "beta", 
            data=inla.stack.data(stk.e1),
            control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE),
            control.predictor = list(A = inla.stack.A(stk.e1)))


I5a <- inla(f5a,
            family = "beta", 
            data=inla.stack.data(stk.e1),
            control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE),
            control.predictor = list(A = inla.stack.A(stk.e1)))

I5a2 <- inla(f5a2,
             family = "Gaussian", 
             data=inla.stack.data(stk.e1),
             control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE),
             control.predictor = list(A = inla.stack.A(stk.e1)))

I6a <- inla(f6a,
             family = "beta", 
            data=RecalMain2,
            control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE))

I6a2 <- inla(f6a,
            family = "beta", 
            data=RecalMain2S,
            control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE))

I6aN <- inla(f6aN,
            family = "beta", 
            data=RecalMain2,
            control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE))

I6a2N <- inla(f6aN,
             family = "beta", 
             data=RecalMain2S,
             control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE))


c(I4a$dic$dic, I5a$dic$dic,I5a2$dic$dic,I6a$dic$dic,I6a2$dic$dic,I6aN$dic$dic,I6a2N$dic$dic)
# Poisson contrasts
# Spatial model - not best on - Beta with random structure

#Global model...pvalue histogram....
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
pval<-rep(NA, nrow=(RecalMain2))
for(i in 1:nrow(RecalMain2)){
  pval[i]<-inla.pmarginal(q= RecalMain2$Massloss.perB[i],
                          marginal=I5a$marginals.fitted.values[[i]])
}
hist(pval) # Not good

par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
pval<-rep(NA, nrow=(RecalMain2S))
for(i in 1:nrow(RecalMain2S)){
  pval[i]<-inla.pmarginal(q= RecalMain2S$Massloss.perB[i],
                          marginal=I6a$marginals.fitted.values[[i]])
}
hist(pval) # Stil not good


# Good model
summary(I5a)
summary(I6a2)

# Brinla summaries
library(brinla)
bri.hyperpar.summary(I5a)
bri.hyperpar.plot(I5a)

bri.hyperpar.summary(I6a)
bri.hyperpar.plot(I6a)

# Posterior distribution fixed effects - cannot trust
bri.fixed.plot(I5a2)
bri.fixed.plot(I6a)
bri.fixed.plot(I6aN)

#Section 10.7.5: Plot the spatial random field
#Get the posterior mean of the spatial field
mu.srf2 <- I5a2$summary.random$s$mean
gproj2 <- inla.mesh.projector(mesh5a,
                              xlim = range(loc2[,1]),
                              ylim = range(loc2[,2])) 

g.mu2 <- inla.mesh.project(gproj2, mu.srf2)


#Figure 10.8
library(lattice)
library(gridExtra)
lattice.options(default.theme =
                  standard.theme(color = TRUE)) 

levelplot(g.mu2, 
          scales = list(draw = TRUE),
          xlab = "Longitude", 
          ylab = "Latitude",
          main = "Mean spatial random field")
# Add sample points grid.points(..1,..1, pch=2)

# This is the residual spatially correlated term.  


#Posterior mean values and 95% CI
Betas <- I6a$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas, digits = 2)
# Similar Sd for source - large - not strong component

# plot betas - compare model with and without rainfall
Betas$mod<-"RecalMain"
Betas$names <- rownames(Betas)
colnames(Betas)[3]<-'SeLo'
colnames(Betas)[4]<-'SeUp'
pp<-ggplot(Betas, aes(x=names, y=mean,ymin=SeLo, ymax=SeUp, fill=mod,colour=mod))
pp<- pp+geom_hline(yintercept =0, colour="grey")
pp<- pp+ geom_errorbar(width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
pp<- pp+ geom_point(size = 2, stroke=.5,position=position_dodge(width=.65)) 
pp<- pp+ scale_y_continuous(limits=c(-7.5,7)) 
pp<-pp+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
pp


#Calculate residuals
X <- model.matrix(~ 1 + Fire.freq+Source.activities +Flora.name+Lon, data = SppSNPs)
Fit.5a2<- X %*% Betas[,"mean"]  #X * posterior mean parameters

# Extra beta with dependecy
Bmean<-(Betas[,"mean"])
Bmean # 14 for fixed factors
str(X) # 13 # Why 13 - including incept
#Residuals from the INLA model: Y - fit
# Incl long as spatial component

E1.inla <- SppSNPs$Cover - Fit.5a2

#Plot fire freq versus the residuals
par(mfrow = c(1,1),mar = c(5,5,2,2))
plot(x = SppSNPs$Fire.freq, 
     y = E1.inla,
     xlab = "Fire freq",
     ylab = "INLA residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

#Information on the sigma
#By default INLA gives the precision # = 1 / variance
I5a2$summary.hyperpar               # Precision

#Convert them into sigma
1 / sqrt(I5a2$summary.hyperpar[,1]) # sigma
#0.5798671       NaN 0.5197873 # Random component - sptail matrix



