#LOADING DATA Anders Sundsdal MSc####
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
library(qpcR)
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
#require(devtools)
#install_version("lme4", version = "1.1-18-1", repos = "https://cran.r-project.org")

#setwd("~/Master data/GitHub_ABS/AfricanBioServices-Vegetation-and-soils")
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

#Readjust and merge data####
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
Fulldata$Site.ID <- as.factor(with(Fulldata, ifelse(Site %in% c("Makao") &
                                      Landuse == "Pasture", 
                                      "Makao.p",
                                    ifelse(Site %in% c("Makao") &
                                      Landuse == "Agriculture",
                                      "Makao.ag",
                                    ifelse(Site %in% c("Mwantimba") &
                                      Landuse == "Pasture",
                                      "Mwantimba.p",
                                    ifelse(Site %in% c("Mwantimba") &
                                       Landuse == "Agriculture",
                                       "Mwantimba.ag",
                                    ifelse(Site == "Handajega","Handajega",
                                    ifelse(Site == "Maswa", "Maswa",
                                    ifelse(Site == "Seronera","Seronera","WRONG")))))))))
levels(Fulldata$Site.ID)                          
table(Fulldata$Site.ID,Fulldata$Landuse) #OK

Fulldata$X <- NULL
Fulldata$Blockcode.y <-  NULL
colnames(Fulldata)[(names(Fulldata) == "Blockcode.x")] <- "Blockcode"
Fulldata$CGBlock.y <- NULL
Fulldata$CGBlock <- NULL
Fulldata$CGBlock.x <- NULL

write.csv(Fulldata,file="Termites/Fulldata.csv")

###Loading Fulldata
Fulldata <- read.csv("Termites/Fulldata.csv")




#DATA EXPLORATION####
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?
# I Are the variables Norm.distributed?
# Alain Zuur - data exploration functions
source(file="C:/Users/ansun/Google Drive/09Fremdrift Master/Kjekke R ting/Data exploration/HighstatLibV10.R")
# A Missing values? ####

colSums(is.na(Fulldata))
#Missing 96 Inital teabags:
#OK since this is one plot in dry season in each site that
#we're not used due to shortage of rooibos teabags.

#Missing 156 Final ash corected weights (from LOI): If we substract the 96 unused plots,
#then 60 teabag where not retrived in field.
#After checking rawdatafile: 19 not retrived in dry season and  37 in wet season=56.
#So 4 not accounted for.
#Not too bad

#Missing 156 obs in Massloss.g: This corresponds to final ash corrected weight,
#OK.


#Missing 96 obs in MOISTURE and 96 obs in TEMPERATURE,
#this corresponds to the missing initial teabags due to less plots in dry season,
#so OK, but...
data.table(aggregate(Temperature..C.~Blockcode+Season,Fulldata, function(x) {sum(is.na(x))}, na.action = NULL))
data.table(aggregate(Moisture.. ~Blockcode+Season,Fulldata, function(x) {sum(is.na(x))}, na.action = NULL))
#Both climate variables has same number of observations, BUT! THese two variables
#consist of a mean between two points in time for each season on plot level.
#And moisture variable did not have data for wet season Dry_P1 to Dry_P4 due to no battery left :( .
#So the moisture variale for these  blocks are only based on the first measure before it broke...
#Therefore, the temp variable are more "robust" and complete.
#But could check if these moisture varables (Dry_P1-P4) are different from the other variables based on both time points:
boxplot(Moisture.. ~Blockcode+Season,data=Fulldata) #Nope, so maybe OK.
#But we see that Dry_Ag2 and Ag3 are quite moist compared to the other Dry_Ag.
boxplot(Temperature..C. ~Blockcode+Season,data=Fulldata)
boxplot(Moisture.. ~Landuse,data=Fulldata)


which(is.na(Fulldata$Massloss.per))
sum(is.na(Fulldata$Massloss.per))
#Removing rows which consist of NAs in "Massloss.per" column
#Data <- Fulldata[complete.cases(Fulldata[,40]),]
#summary(Data)

# B Outliers in Y / Outliers in X####

#Ashed final weight
dotchart(Fulldata$Ashed.final.corrected.weight..tea.only..g.)
plot(Fulldata$Ashed.final.corrected.weight..tea.only..g.)
#identify(Fulldata$Ashed.final.corrected.weight..tea.only..g)
Fulldata[671,] # Maybe too high? Need to check massloss.g variable

#Outlier in ashed data % (should be between 0-100):
dotchart(Fulldata$Ashed.final.subsample.percentage.....) #
plot(Fulldata$Ashed.final.subsample.percentage.....)

#Checking outliers in initial and final weights:
#Inital weight:
dotchart(Fulldata$Initial.weight.tea.only..g)
plot(Fulldata$Initial.weight.tea.only..g)
#identify(Fulldata$Initial.weight.tea.only..g.)

#Checking for massloss outlier
dotchart(Fulldata$Massloss..g. ) #
plot(Fulldata$Massloss..g.)
#identify(Fulldata$Massloss..g.)


#Checking for Massloss per outlier
dotchart(Fulldata$Massloss.per)
plot(Fulldata$Massloss.per)
#identify(Fulldata$Massloss.per)
#How many are 100% massloss?
nrow(subset(Fulldata,Massloss.per == 100))
nrow(subset(Fulldata,Massloss.per >80))
#44 obs. is 100% massloss
#726 is over 60%
#159 is over 80%

#Checkig outliers in temperature and moisture:
dotchart(Fulldata$Temperature..C.) #Looks ok
plot(Fulldata$Temperature..C.)
#identify(Data$Temperature..C.)
dotchart(Fulldata$Moisture..) #Looks ok
plot(Fulldata$Moisture..)

#Checking outliers in Soil variables:
#C:N
dotchart(Soildata$C.N) #One large outlier 
plot(Soildata$C.N)
#identify(Soildata$C.N)
Soildata[39,] #19.9, not sure if remove or not,
#for now, we'll let it be.
boxplot(C.N ~Blockcode+Season,data=Soildata)
#Here we see that Wet_W3 in dry seaon is
#very different from other sites across season aswell
#Clay
dotchart(Soildata$Claycorr)  
plot(Soildata$Claycorr)
#identify(Soildata$Claycorr)
#Sand
dotchart(Soildata$Sandcorr) 
plot(Soildata$Sandcorr)
#identify(Soildata$Sandcorr)


# C Collinearity X ####
Signdata <- rcorr(as.matrix(Data))
           
###Checking Collinearity among variables###
#Now, checking correlation
names(Fulldata)
#Soil variables
MySoil<-c("Massloss.per","C.N","Claycorr","Siltcorr","Sandcorr")
#pairs(Fulldata[,MySoil], lower.panel= panel.cor)

#Climate variables
MyEnv<-c("Massloss.per","Rain.sum","Moisture..","Temperature..C.")
pairs(Fulldata[,MyEnv], lower.panel= panel.cor)

#Choosen variables:
MyVarTot <- c("C.N","Claycorr","Sandcorr",
           "Moisture..","Temperature..C.","Rain.sum")
#pairs(Fulldata[,MyVarTot], lower.panel= panel.cor)

MyVar1 <- c("C.N","Claycorr","Temperature..C.",
            "Rain.sum" )
#pairs(Fulldata[,MyVar1], lower.panel= panel.cor)
MyVar2 <- c("Massloss.per","C.N","Sandcorr",
            "Temperature..C.","Rain.sum")
#pairs(Fulldata[,MyVar2], lower.panel= panel.cor)
MyVar3 <- c("C.N","Claycorr","Moisture..")
#pairs(Fulldata[,MyVar3], lower.panel= panel.cor)
MyVar4 <- c("C.N","Moisture..","Temperature..C.")
MyVar5 <- c("C.N","Moisture..","Sandcorr","Temperature..C.")
MyVar6 <- c("C.N","Moisture..","Temperature..C.","Rain.sum")
MyVar7 <- c("C.N","Moisture..","Sandcorr","Temperature..C.","Rain.sum")

#CORRELATION RESULTS:

#Silt - clay: 0.8
#Silt - sand: -0.9
#Sand - Clay: -1.0
#--> Can't use these together in model

#Moisture - temp: -0.6
#Rain - temp not: ~0 #OK, can use together
#Rain-Moist: 0.6

#C:N - temp: 0.3 
#Clay - temp: -0.3

#Sand - moisture: -0.4
#Sand - temp: 0.3

#Clay - moisture: 0.4

#Checking for variance inflation factor.
#Want as low as possible,
#because we want to be able to get P-values as low as possible.
#High VIF increases the P-value. High VIF= high level of correlation.
corvif(Fulldata[,MyVarTot]) #High for Sand and Clay
# GVIF
# C.N              1.097046
# Claycorr        13.080490
# Sandcorr        13.979418
# Moisture..       3.723475
# Temperature..C.  2.148949
# Rain.sum         2.328552

corvif(Fulldata[,MyVar1])#OK
corvif(Fulldata[,MyVar2])#OK
corvif(Fulldata[,MyVar3])#OK
corvif(Fulldata[,MyVar4])#No texture or rain variable
# GVIF
# C.N             1.09585
# Moisture..      1.53766
# Temperature..C. 1.64781
corvif(Fulldata[,MyVar5])#Include texture, not a high increase in inflation.
# GVIF
# C.N             1.096648
# Moisture..      1.640716
# Sandcorr        1.192411
# Temperature..C. 1.677015
corvif(Fulldata[,MyVar6]) #Include only rain, higher infaltion compared to only sand.
# GVIF
# C.N             1.096198
# Moisture..      2.926265
# Temperature..C. 2.131238
# Rain.sum        1.903446
corvif(Fulldata[,MyVar7]) #Inlcude both rain and texture, high infaltion in moisture.
# GVIF
# C.N             1.096709
# Moisture..      3.696862
# Sandcorr        1.430227
# Temperature..C. 2.146452
# Rain.sum        2.283072

#Should probably not include rain if I want texture and moisture in a model.


#Checking for covariance among factors with boxplot:
#If overlapping, they don't covary=OK

names(Fulldata)
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))

#Checking Moisture:
boxplot( Moisture.. ~Region+Season  , 
        xlab = "Moisture",
        ylab = "Region"  ,
        data = Fulldata)
        # Looks like more variable and moist in Dry region...
        #Not good.

boxplot( Moisture.. ~Blockcode  , 
         xlab = "Moisture",
         ylab = "Blockcode"  ,
         data = Fulldata) #OK, I think

boxplot( Moisture.. ~Landuse  , 
         xlab = "Moisture",
         ylab = "Blockcode"  ,
         data = Fulldata) #OK

#Checking Rainfall:
boxplot(Rain.sum ~Region+Season  , 
         xlab = "Moisture",
         ylab = "Region"  ,
         data = Fulldata)
# Dry region in dry season: Really dry
#Intermediate, same amount of rainfall both season
#No difference in rainfall in wet vs dry regions in wet season.
#THere is a pattern here, but the categories in Region are not
#really following whats happening for the amount of Rain
#Should only use Rain.sum.

boxplot( Rain.sum ~Blockcode +Season  , 
         xlab = "Moisture",
         ylab = "Blockcode"  ,
         data = Fulldata) #Pattern here, not OK.

boxplot( Rain.sum ~Landuse  , 
         xlab = "Moisture",
         ylab = "Blockcode"  ,
         data = Fulldata) #OK
#Maybe thinking of not having Rgion and Season
#in model and only use Rain.sum

boxplot(Temperature..C.~Landuse,data=Fulldata) #Overlapping - not covarying
boxplot(Temperature..C.~Region,data=Fulldata) #Overlaping - not covarying
boxplot(Temperature..C.~Season,data=Fulldata) #Overlapping - not covarying

boxplot(Moisture.. ~Landuse,data=Fulldata) #Overlapping
boxplot(Moisture.. ~Region,data=Fulldata)
boxplot(Moisture.. ~Season,data=Fulldata) #Covarying

boxplot(Sandcorr ~Treatment,data=Fulldata) #Overlapping
boxplot(Sandcorr ~Littertype,data=Fulldata) #Overlapping
boxplot(Sandcorr ~Landuse,data=Fulldata) #Overlapping
boxplot(Sandcorr ~Region,data=Fulldata) #Overlapping
boxplot(Sandcorr ~Season,data=Fulldata) #Overlapping

boxplot(Claycorr ~Treatment,data=Fulldata) #Overlapping
boxplot(Claycorr ~Littertype,data=Fulldata) #Overlapping
boxplot(Claycorr ~Landuse,data=Fulldata) #Overlapping
boxplot(Claycorr ~Region,data=Fulldata) #Overlapping
boxplot(Claycorr ~Season,data=Fulldata) #Overlapping

boxplot(C.N ~Treatment,data=Fulldata) #Overlapping
boxplot(C.N ~Littertype,data=Fulldata) #Overlapping
boxplot(C.N ~Landuse,data=Fulldata) #Overlapping
boxplot(C.N ~Region,data=Fulldata) #Overlapping
boxplot(C.N ~Season,data=Fulldata) #Overlapping

boxplot(Rain.sum ~Region,data=Fulldata)


#SEPARATING EXP'S####
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

names(Data)

#Creating summaries (aggregate dataset)####
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
# Main experiment means and standard error (exclude blocks) # From Stu: Need to seperate out Agricutlure in Makao and Mwantimba(WHY?)
names(DataMain)
#Creating means by landuse (excluding blocks)
DataMainmean1<-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, DataMain, mean)
DataMainse1 <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, DataMain, se)
#Creating new column with the SE in the Mainmean dataset.
DataMainmean1$SE <- DataMainse1$Massloss.per 

#CODE HERE NOT WORKING NOW.Termite effect and microbe effect variable for Main####
Greenop<-DataMain[DataMain$Littertype=="Green" & DataMain$Treatment=="Open",] # Only Green Open data
Greenex<-DataMain[DataMain$Littertype=="Green" & DataMain$Treatment=="Exclosed",] # Only Green Open data
Redop<-DataMain[DataMain$Littertype=="Rooibos" & DataMain$Treatment=="Open",] # Only Green Open data
Redex<-DataMain[DataMain$Littertype=="Rooibos" & DataMain$Treatment=="Exclosed",] # Only Green Open data
 

#Creating dataframe with termite and microbe effect variable=Exlosed and Termite effect=Open minus exclosed):
GreendataT <- Greenop
GreendataM <- Greenop

GreendataM$Massloss.per <- Greenex$Massloss.per
GreendataT$Massloss.per <- abs(Greenop$Massloss.per-Greenex$Massloss.per)
GreendataT$Massloss.per <- (Greenop$Massloss.per-Greenex$Massloss.per)
plot(GreendataT$Massloss.per)
GreendataT$Massloss.per[GreendataT$Massloss.per<0] <- 0

ReddataT <- Redop
ReddataM <- Redop
ReddataM$Massloss.per<-Redex$Massloss.per
ReddataT$Massloss.per<- (Redop$Massloss.per-Redex$Massloss.per)
plot(ReddataT$Massloss.per)
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
write.csv(write.csv(RecalTermEff_Wetseason_Block,file="Termites/RecalTermEff_Wetseason_Block.csv"))



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

#GRAPH SIMPLIFIED TERMITE EFFECT (SEASON & LANDUSE)####
#First, just excude Seronera:
TM.effectMain <- droplevels(TM.effect[TM.effect$Region!="Intermediate",])

#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- "Litter Quality"
TM.effectMain$LD <- factor(TM.effectMain$LD,levels=c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite"))
levels(TM.effectMain$LD)

#Housekeeping
TM.effectMain$Season <- as.factor(TM.effectMain$Season)
levels(TM.effectMain$Season)
levels(TM.effectMain$Landuse)
TM.effectMain$Decomposer <- as.factor(TM.effectMain$Decomposer)
levels(TM.effectMain$Decomposer)
TM.effectMain$Littertype <- as.factor(TM.effectMain$Littertype)
levels(TM.effectMain$Littertype)
TM.effectMain$Region <- as.factor(TM.effectMain$Region)
levels(TM.effectMain$Region)

#Adjusting the upper SE for error bars for stacked barplot:
TM.effectMain$SE.up <- TM.effectMain$SE
TM.effectMain$SE.up[TM.effectMain$LD == "Labile Microbe"] <- with(TM.effectMain,SE[LD == "Labile Microbe"]+
                                                                    Massloss.per[LD=="Labile Termite"])

TM.effectMain$SE.up[TM.effectMain$LD == "Recalcitrant Microbe"] <- with(TM.effectMain,SE[LD == "Recalcitrant Microbe"]+
                                                                   Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
TM.effectMain$Mass.stop <- TM.effectMain$Massloss.per
TM.effectMain$Mass.stop[TM.effectMain$LD == "Labile Microbe"] <- with(TM.effectMain,Massloss.per[LD == "Labile Microbe"]+
                                                      Massloss.per[LD=="Labile Termite"])

TM.effectMain$Mass.stop[TM.effectMain$LD == "Recalcitrant Microbe"] <- with(TM.effectMain,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                            +Massloss.per[LD=="Recalcitrant Termite"])

#Plotting
TM.effectMainP <- ggplot(data=TM.effectMain,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
  #geom_errorbar(position="identity",width=NA,lwd=1)+
  geom_bar(stat="identity",width=0.9)+
  facet_wrap(Region~Season+Landuse,nrow=2)+
  scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
  scale_alpha_discrete(TitleDecomp,range=c(0.3,1,0.3,1))+
  xlab("")+ylab("Mass loss (%)")+
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
TM.effectMainP <-TM.effectMainP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80,100), labels = c(0,20,40,60,80,100), expand=c(0,0))
TM.effectMainP <- TM.effectMainP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

TM.effectMainP

#BARPLOTTING - three own grafs for each Landuse####
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
AgriP <-AgriP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0))
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

# ggsave("Termites/Main & CG experiment/Wild_Main_experiment.png",
#        width= 20, height = 15,units ="cm",bg ="transparent",
#        dpi = 600, limitsize = TRUE)

#Graphing: CGvsMain experiment ####
#But first sorting the commongarden data and main experiemnt for GGplot:

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
# Main experiment means and standard error (include blocks) # From Stu: Need to seperate out Agricutlure in Makao and Mwantimba(WHY?)
names(DataMain)
#Creating means by landuse (excluding blocks)
DataMainmean<-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, DataMain, mean)
DataMainse <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, DataMain, se)
#Creating new column with the SE in the Mainmean dataset.
DataMainmean$SE <- DataMainse$Massloss.per 
# Fill by termite * landuse = empty = absence, filled = prescence 
DataMainmean$tea.hole<-as.factor(with(DataMainmean, paste(flittertype, ftreatment, sep=" ")))
levels(DataMainmean$tea.hole)
#levels(DataMainmean$fregion)<-c("Dry region","Wet region")
DataMainmean$fregion <- factor(DataMainmean$fregion)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(DataMainmean$fregion)
levels(DataMainmean$fseason)

#Common garden means and standard error (Exclude blocks)
names(DataCG)
DataCGmean <- aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, DataCG, mean)
DataCGse <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, DataCG, se)
#Creating new column with the SE in the CGmean dataset.
DataCGmean$SE <- DataCGse$Massloss.per
# Fill by termite * landuse = empty = absence, filled = prescence 
DataCGmean$tea.hole<-as.factor(with(DataCGmean, paste(flittertype, ftreatment, sep=" ")))
levels(DataCGmean$tea.hole)
levels(DataCGmean$fregion)
levels(DataCGmean$fseason)

#Need to add Intermediate into Maindata set. To have the local soil appear on the 1:1 line in the upcoming graph.
IntermediateCG<-DataCGmean[DataCGmean$fregion=="Intermediate",] #extracting rows with "intermediate" from CG dataset

DataMainmean<-rbind(DataMainmean,IntermediateCG) #Putting in the intermediate data into main dataset.
levels(DataMainmean$fregion)

#Putting CG means and SE with Main means and Se in the same dataset
names(DataMainmean)
names(DataCGmean)
#Renaming colums to restrict merging of the Massloss and SE from the two experiments, when using merge() later.
colnames(DataMainmean)[6]<-"massloss.perMain"
colnames(DataMainmean)[7]<-"SEMain"
colnames(DataCGmean)[6]<-"massloss.perCG"
colnames(DataCGmean)[7]<-"SECG"
#Now that we have same amount of observations in both CG and Main dataset, we combine the massloss columns to create on dataset:
MainCG <- merge(DataMainmean,DataCGmean)

#Want to reorder tea.hole to have a nicer legend:
MainCG$tea.hole <- ordered(MainCG$tea.hole, levels=c("Green Exclosed", "Rooibos Exclosed","Green Open","Rooibos Open"))
levels(MainCG$tea.hole)

#Now, ready for graphing: Main experiment vs CG
names(MainCG)

colnames(MainCG)[1]<-"Season"
MainCGplot<-ggplot(MainCG, aes(x=massloss.perMain, y=massloss.perCG, fill=tea.hole,color=flittertype,shape=flanduse)) +
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_errorbar(aes(ymin = massloss.perCG-SECG,ymax = massloss.perCG+SECG),show.legend=F) + 
  geom_errorbarh(aes(xmin = massloss.perMain-SEMain,xmax = massloss.perMain+SEMain),show.legend=F) +
  geom_point(size=4.5,stroke=1.5, show.legend=T) +
  facet_wrap(~Season, scale ="fixed", labeller=labeller(Season = c(`Wet`= "Wet Season", `Dry`="Dry Season"))) +
  scale_color_manual(values=c("green4","orangered3")) +
  scale_fill_manual(values=c("green4","orangered3","white","white"))+
  scale_shape_manual(values=c(21,23,24,22)) + 
  guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
  scale_x_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Main experiment mass loss (%)") +  ylab("Common garden mass loss (%)") +
 
  theme(rect = element_rect(fill ="transparent")
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
        ,axis.text.x = element_text(size=12,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.line.y = element_blank()
        ,axis.line.x = element_blank()
        ,plot.margin = unit(c(15,45,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1.5, "lines")
        ,legend.background = element_rect(fill = "transparent")
        ,legend.title=element_blank()
        ,legend.position = c(1.1, 0.50)
        ,legend.spacing.y = unit(-0.8, "mm")
        ,legend.key.height=unit(7.5,"mm")
        ,legend.key.width=unit(7.5,"mm")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.key.size = unit(7,"mm")
        ,legend.text=element_text(size=11,color="black"))+
  
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  5, yend = 18,
           linetype = "dashed", color = "white",size = 1.15) +
  annotate(geom = "segment", x = 5, xend = 18, y =  -Inf, yend = -Inf,
           linetype = "dashed", color = "white",size = 1.15)



MainCGplot
#ggsave("Termites/Main & CG experiment/CommongardenvsMain.png",
 #     width= 30, height = 15,units ="cm",bg ="transparent",
  #   dpi = 600, limitsize = TRUE)

#|####
#ANALYSIS####
#|####
#MAIN LANDUSE EXPERIMENT####

#Want to answer the following questions:
#1. Do litter decomposition rates differ across major land-uses
#   in the Serengeti savannah ecosystem? 
#2. Does temporal and spatial climate effects influence the
#decomposition of litter?

#General massloss models, using Massloss.per as response varible####
####Dataprocessing####
#Separate littertypes
RecalMain <- droplevels(DataMain[DataMain$Littertype =="Rooibos",])
LabileMain <- droplevels(DataMain[DataMain$Littertype =="Green",])

#Adding data from buring in Seronera soil (local soil) - i.e an additional wild site in intermediate rainregion
LocalCGsoil2 <- DataCG[DataCG$Site=="Seronera",]
LabileLocalCGsoil2 <- LocalCGsoil2[LocalCGsoil2$Littertype=="Green",]
RecalLocalCGsoil2 <- LocalCGsoil2[LocalCGsoil2$Littertype=="Rooibos",]
#Removing 4-block design into 1 block with 4 replicates:
 LabileLocalCGsoil2$Block <- 1
 LabileLocalCGsoil2$Blockcode <- "Int_W1"
 RecalLocalCGsoil2$Block <- 1
 RecalLocalCGsoil2$Blockcode <- "Int_W1"
 
 RecalMain <- rbind.fill(RecalMain,RecalLocalCGsoil2)
 LabileMain <- rbind.fill(LabileMain,LabileLocalCGsoil2)

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


levels(RecalMain$blockdesign.num)
levels(LabileMain$Site)
LabileMain$Blockcode <- as.factor(LabileMain$Blockcode)
LabileMain$Blockcode <- droplevels(LabileMain$Blockcode)
levels(LabileMain$Blockcode)
levels(LabileMain$Region)
LabileMain$Region <- droplevels(as.factor(LabileMain$Region))
LabileMain$Plot <- as.factor(LabileMain$Plot)
levels(LabileMain$Plot)

#Labile Models - general massloss across season and landuse####

#Labile Model 1 - No moisture, no rain.
#Have to reduce the amount of predictors in model for dredging.
GlobalLabileMainMod <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
                              Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment-
                              C.N:Temp-C.N:Sand-Temp:Sand+
                         (1|Site/Blockcode/Plot), na.action=na.omit,REML = F,data=LabileMain)

summary(GlobalLabileMainMod)
anova(GlobalLabileMainMod) 
AIC(GlobalLabileMainMod) #5003.159
drop1(GlobalLabileMainMod,test="Chisq") #

# Season:Region:Landuse     2 5012.9 13.773  0.001021 ** K 
# Season:Region:Treatment   2 5003.7  4.563  0.102152 R  
# Season:Landuse:Treatment  2 5000.0  0.848  0.654428  R  
# Region:Landuse:Treatment  2 4999.2  0.057  0.971705 R

LabileMainMod1 <- update(GlobalLabileMainMod, .~. -Region:Landuse:Treatment-Season:Landuse:Treatment-Season:Region:Treatment)
anova(LabileMainMod1) 
AIC(LabileMainMod1) #4996.243
drop1(LabileMainMod1,test="Chisq")
#Season:Region:Landuse  2 5005.8 13.513  0.001163 ** Keep, but remove now to test other 2-way factors
LabileMainMod2 <- update(LabileMainMod1, .~. -Season:Region:Landuse)
AIC(LabileMainMod2) #5005.757 #REMEMBER I REMOVED A SIGN 3WAY.
drop1(LabileMainMod2,test="Chisq")
# Season:Region      2 5299.6 297.832 < 2.2e-16 *** #KEEP
#   Season:Landuse     2 5045.7  43.933 2.885e-10 ***#KEEP
#   Season:Treatment   1 5004.6   0.880 0.3483138    
# Season:C.N         1 5003.8   0.006 0.9364322    KEEP due to sign threeway
# Season:Temp        1 5004.1   0.331 0.5649130    KEEP due to sign threeway
# Season:Sand        1 5098.4  94.659 < 2.2e-16 *** #KEEP
#   Region:Landuse     2 5013.6  11.823 0.0027075 ** #KEEP
#   Region:Treatment   2 5001.9   0.134 0.9351854    
# Region:C.N         2 5002.5   0.792 0.6731375    KEEP due to sign threeway
# Region:Temp        2 5007.6   5.887 0.0526865 .  #KEEP
# Region:Sand        2 5002.0   0.250 0.8825828    KEEP due to sign threeway
# Landuse:Treatment  2 5002.8   1.053 0.5907519    
# Landuse:C.N        2 5028.4  26.644 1.638e-06 *** #KEEP
#   Landuse:Temp       2 5017.4  15.678 0.0003942 ***#KEEP
#   Landuse:Sand       2 5004.0   2.273 0.3209287  KEEP due to sign threeway  
# Treatment:C.N      1 5004.2   0.444 0.5051996    
# Treatment:Temp     1 5003.8   0.001 0.9791816    
# Treatment:Sand     1 5003.8   0.006 0.9402220  
LabileMainMod3 <- update(LabileMainMod2, .~. -Treatment:Sand-Treatment:Temp-Treatment:C.N-Landuse:Treatment-
                           Region:Treatment-Season:Treatment)
AIC(LabileMainMod3) #4992.419 #REMEMBER I REMOVED A SIGN 3WAY.
drop1(LabileMainMod3,test="Chisq")
LabileMainMod3a <- update(LabileMainMod3, .~. +Season:Region:Landuse) #Adding SIGN 3Way
AIC(LabileMainMod3a) #Global: 4983.039 #SIGN 3way added.
drop1(LabileMainMod3a,test="Chisq")

# Treatment              1 4981.1  0.038  0.846216    #REMOVE
# Season:C.N             1 4981.2  0.145  0.703130    
# Season:Temp            1 4986.1  5.036  0.024829 *  
#   Season:Sand            1 5069.5 88.438 < 2.2e-16 ***
#   Region:C.N             2 4979.5  0.419  0.810876    
# Region:Temp            2 4988.1  9.080  0.010676 *  
#   Region:Sand            2 4979.2  0.118  0.942564    
# Landuse:C.N            2 5010.9 31.815 1.234e-07 ***
#   Landuse:Temp           2 4988.8  9.763  0.007586 ** 
#   Landuse:Sand           2 4981.2  2.173  0.337337    
# Season:Region:Landuse  2 4992.4 13.380  0.001243 ** 

LabileMainMod4a <- update(LabileMainMod3a, .~. -Treatment)
AIC(LabileMainMod4a)#4981.077
drop1(LabileMainMod4a,test="Chisq")
# Season:C.N             1 4979.2  0.146  0.702359    #Remove
# Season:Temp            1 4984.1  5.042  0.024741 *  
#   Season:Sand            1 5067.5 88.450 < 2.2e-16 ***
#   Region:C.N             2 4977.5  0.419  0.810884    #Remove
# Region:Temp            2 4986.2  9.079  0.010676 *  
#   Region:Sand            2 4977.2  0.118  0.942607    #Remove
# Landuse:C.N            2 5008.9 31.814 1.235e-07 ***
#   Landuse:Temp           2 4986.8  9.763  0.007586 ** 
#   Landuse:Sand           2 4979.3  2.173  0.337378    #Remove
# Season:Region:Landuse  2 4990.5 13.391  0.001236 ** 

LabileMainMod5a <- update(LabileMainMod4a, .~. -Season:C.N-Region:C.N-Region:Sand-Landuse:Sand)
AIC(LabileMainMod5a)#4970.082
drop1(LabileMainMod5a,test="Chisq")# All sign, so now need to get P-values for the single terms.
#But first, want to add some terms first to check:
LabileMainMod6a <- update(LabileMainMod5a, .~. +C.N:Temp)
anova(LabileMainMod6a,LabileMainMod5a) #C.N:Temp NS
LabileMainMod7a <- update(LabileMainMod5a, .~. +C.N:Sand)
anova(LabileMainMod7a,LabileMainMod5a) #C.N:Sand NS
LabileMainMod8a <- update(LabileMainMod5a, .~. +Temp:Sand)
anova(LabileMainMod8a,LabileMainMod5a) #Temp:Sand NS
LabileMainMod9a <- update(LabileMainMod5a, .~. +Landuse:Sand)
anova(LabileMainMod9a,LabileMainMod5a) # Landuse:Sand NS
LabileMainMod10a <- update(LabileMainMod5a, .~. +Treatment)
anova(LabileMainMod10a,LabileMainMod5a) # Treatment NS


#Generating P-values from best model
LabileMainMod5aA<- update(LabileMainMod5a, .~. -Season:Region:Landuse)
LabileMainMod5ab <- update(LabileMainMod5aA, .~.-Landuse:Temp)
LabileMainMod5ac <- update(LabileMainMod5aA, .~.-Landuse:C.N)
LabileMainMod5ad <- update(LabileMainMod5aA, .~.-Region:Temp)
LabileMainMod5ae <- update(LabileMainMod5aA, .~.-Region:Landuse)
LabileMainMod5af <- update(LabileMainMod5aA, .~.-Season:Sand)
LabileMainMod5ag <- update(LabileMainMod5aA, .~.-Season:Temp)
LabileMainMod5ah <- update(LabileMainMod5aA, .~.-Season:Landuse)
LabileMainMod5ai <- update(LabileMainMod5aA, .~.-Season:Region)
LabileMainMod5aJ <- update(LabileMainMod5aA, .~.-Landuse:Temp-Landuse:C.N-Region:Temp-
                             Region:Landuse-Season:Sand-Season:Temp-Season:Landuse-Season:Region)
LabileMainMod5ak <- update(LabileMainMod5aJ, .~.-Sand)
LabileMainMod5al <- update(LabileMainMod5aJ, .~.-Temp)
LabileMainMod5am <- update(LabileMainMod5aJ, .~.-C.N)
LabileMainMod5an <- update(LabileMainMod5aJ, .~.-Landuse)
LabileMainMod5ao <- update(LabileMainMod5aJ, .~.-Region)
LabileMainMod5ap <- update(LabileMainMod5aJ, .~.-Season)

anova(LabileMainMod5aA,LabileMainMod5a)  #0.0009554 *** #Season:Region:Landuse
anova(LabileMainMod5ab,LabileMainMod5aA) #6.059e-05 ***#Landuse:Temp
anova(LabileMainMod5ac,LabileMainMod5aA) #2.337e-09 *** Landuse:C.N
anova(LabileMainMod5ad,LabileMainMod5aA) #0.1076 . Region:Temp #NS - But important for the model (See drop1 output)
anova(LabileMainMod5ae,LabileMainMod5aA) #0.002427 ** **Region:Landuse
anova(LabileMainMod5af,LabileMainMod5aA) #2.2e-16 ***Season:Sand
anova(LabileMainMod5ag,LabileMainMod5aA) #0.3997 Season:Temp - NS, but need this to have the threeway
anova(LabileMainMod5ah,LabileMainMod5aA) #1.304e-10 *** Season:Landuse
anova(LabileMainMod5ai,LabileMainMod5aA) #1 Season:Region - NS, but need this to have the threeway
anova(LabileMainMod5ak,LabileMainMod5aJ) # 2.2e-16 *** Sand
anova(LabileMainMod5al,LabileMainMod5aJ) # 2.2e-16 *** Temp
anova(LabileMainMod5am,LabileMainMod5aJ) # 1.913e-07 *** C.N
anova(LabileMainMod5an,LabileMainMod5aJ) # 0.01129 * Landuse
anova(LabileMainMod5ao,LabileMainMod5aJ) # 1.391e-05 *** Region
anova(LabileMainMod5ap,LabileMainMod5aJ) # 2.2e-16 *** Season


LabileMainModFINAL <- lmer(Massloss.per ~ Season+Region+Landuse+C.N+Temp+Sand+Season:Region+Season:Landuse+Season:Temp+Season:Sand+
                             Region:Landuse+Region:Temp+Landuse:C.N+Landuse:Temp+
                             Season:Region:Landuse+
                             (1|Site/Blockcode/Plot), na.action=na.omit,REML = T,data=LabileMain)

LabileMainModFINAL.sumary <- summary(LabileMainModFINAL)$coef
write.csv(LabileMainModFINAL.sumary,file="Termites/Model_estimates_LabileMainMod.csv")

coef(summary(LabileMainModFINAL))

#Inspect chosen model for homogeneity:
E1 <- resid(LabileMainModFINAL, type ="pearson")
F1 <- fitted(LabileMainModFINAL)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5) #Looks OK
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)

hist(E1, nclass = 30) #OK



#Checking for non linearity
xyplot(E1 ~ C.N | Landuse+Season,
       data = LabileMain, ylab = "Residuals",
       xlab = "Landuse",
       panel = function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

xyplot(E1 ~ Sand | Landuse+Season,
       data = LabileMain, ylab = "Residuals",
       xlab = "Landuse",
       panel = function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

xyplot(E1 ~ Temp | Landuse+Season,
       data = LabileMain, ylab = "Residuals",
       xlab = "Landuse",
       panel = function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

#Here we see that the line is not entirely straight - So the linear mixed effect is wrong, i.e effect of C.N,Sand and Temp is not linear

library(itsadug)
plot(acf_resid(LabileMainModFINAL), type="b",alpha=0.05)
abline(c(0,0), lty = 2, col = 1)




library(mgcv)
M1 <- gamm(Massloss.per ~ Season+Region+Landuse+C.N+Temp+Sand+Season:Region+Season:Landuse+Season:Temp+Season:Sand+
             Region:Landuse+Region:Temp+Landuse:C.N+Landuse:Temp+
             Season:Region:Landuse+
             (1|Site/Blockcode/Plot),
             random = list(Site/Blockcode/Plot =~ 1), data = LabileMain)

#Exploring interaction levels, what drives the interactions to be significant?####
#Interesting interactions to look at:
#Threeway: Season:Region:Landuse
#Twoway:Landuse:Temp, Landuse:C.N, Season:Sand
#emmeans
#ref.grid.LabileMainModFINAL <- ref_grid(LabileMainModFINAL) #at = list(Region = c("Dry", "Wet"))) #Want to remove Intermadiate in the grid as it can't be compared to the other landuses, except wild.
#ref.grid.LabileMainModFINAL#See how the grid is looking. Check for correct factors and the emmeans of numerical variables. If testing between numerical variables, only the means or the low/high end of values can be specified. I.e contrasts vs trend. See emmeans vignette for more info.
#str(ref.grid.LabileMainModFINAL)
#Check threeway first:
#emmip(ref.grid.LabileMainModFINAL, ~Region*Landuse|Season, type="response")#Can see here that the regional differences is greatly pronounce in dry season. And thatdry region is most different in this season. In wet season, dry and wet region is similar (following rainfall) and that wet season is the most dissimilar. 
#emmeans.LabileMainModFINAL <- emmeans(ref.grid.LabileMainModFINAL, pairwise~Season*Region*Landuse,type="response") #
#emmeans.LabileMainModFINAL$contrasts #Get contrast between factors (linear). This is somewhat similar to pairs()
#emmeans.LabileMainModFINAL$emmeans #Get emmeans of factors.
#emmeans.LabileMainModFINAL.pairs <- pairs(emmeans.LabileMainModFINAL,simple = "each", combine =TRUE) # Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which simple effect is being displayed. 
#write.csv(emmeans.LabileMainModFINAL.pairs$emmeans,file="Termites/Emmeans_contrast_Threeway_LabileMainMod.csv")
#plot(emmeans.LabileMainModFINAL, comparisons = FALSE) #Comparisons summarized graphically. The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them ,if comparison=TRUE. If an arrow from one mean overlaps an arrow from another group, the difference is not significant

#Now look at the two-way interaction Landuse:C.N. I use emmtrends here due to usage of covariate. Also cov.reduce=range is used so we'll be using not only the means (one value) of the covariate but the min and max.
# emtrends.LabileMainModFINAL <- emtrends(LabileMainModFINAL, pairwise ~ Landuse|Region, var = "C.N",type="response")
# emmip(LabileMainModFINAL, Landuse~C.N|Region, cov.reduce = range, type="response")
# emtrends.LabileMainModFINAL.pairs <- pairs(emtrends.LabileMainModFINAL,simple = "each", combine =TRUE) # Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which simple effect is being displayed. 
# plot(emtrends.LabileMainModFINAL, cov.reduce = range,comparisons = FALSE) #Comparisons summarized graphically. The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them ,if comparison=TRUE. If an arrow from one mean overlaps an arrow from another group, the difference is not significant

#Now look at the two-way interaction Landuse:Temp. I use emmtrends here due to usage of covariate. Also cov.reduce=range is used so we'll be using not only the means (one value) of the covariate but the min and max.
# emtrends.LabileMainModFINAL <- emtrends(LabileMainModFINAL, pairwise ~ Landuse|Region, var = "Temp",type="response")
# emmip(LabileMainModFINAL, Landuse~Temp|Region*Season, cov.reduce = range, type="response")
# emtrends.LabileMainModFINAL.pairs <- pairs(emtrends.LabileMainModFINAL,simple = "each", combine =TRUE) # Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which simple effect is being displayed. 
# plot(emtrends.LabileMainModFINAL, cov.reduce = range,comparisons = FALSE) #Comparisons summarized graphically. The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them ,if comparison=TRUE. If an arrow from one mean overlaps an arrow from another group, the difference is not significant


#|####
#Predicted and observed values on graph####
#### Sketch fitted values #
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE

LabileMainModFINAL <- lmer(Massloss.per ~ Season+Region+Landuse+C.N+Temp+Sand+Season:Region+Season:Landuse+
                             #+Season:Temp+Season:Sand+
                             #Region:Landuse+Region:Temp+Landuse:C.N+Landuse:Temp+
                             # Season:Region:Landuse+
                             (1|Site/Blockcode/Plot), na.action=na.omit,REML = T,data=LabileMain)

#A:Specify covariate values for predictions - Labile
Data2Labile <- expand.grid(Season=levels(LabileMain$Season), #Specify which terms are used in the model. Specify levels for factors and min-max for numeric values. Specify length for each numeric var (how many predictions are created)
                           Region=levels(LabileMain$Region),
                           Landuse=levels(LabileMain$Landuse),
                           C.N = seq(min(LabileMain$C.N), max(LabileMain$C.N), length=12),
                           Temp = seq(min(LabileMain$Temp,na.rm=T), max(LabileMain$Temp,na.rm=T), length=12),
                           Sand = seq(min(LabileMain$Sand), max(LabileMain$Sand), length=12))

#B. Create X matrix with expand.grid
X1 <- model.matrix(~ Season+Region+Landuse+C.N+Temp+Sand+Season:Region+Season:Landuse, data = Data2Labile)
head(X1)

#C. Calculate predicted values
Data2Labile$Pred <- X1 %*% fixef(LabileMainModFINAL) 

#D. Calculate standard errors (SE) for predicted values
#Data2Labile$SE <- sqrt(  diag(X1 %*% vcov(LabileMainModFINAL) %*% t(X1))  ) #Hash out f not using as it takes some time to run.
memory.limit() #Need to allocate more memory to R to be able to do this with length of 25. Scaled down to max of 12, since only ~8gb is given.


#And using the Pred and SE values, we can calculate
#a 95% confidence interval
Data2Labile$SeUp <- Data2Labile$Pred + 1.96 * Data2Labile$SE
Data2Labile$SeLo <- Data2Labile$Pred - 1.96 * Data2Labile$SE

#E. Plot predicted values
names(Data2Labile) #NB! Some predicted values are negativ mass loss. This does not makes sense...
colnames(Data2Labile)[3]<-"Landuse"
colnames(Data2Labile)[7] <- "Massloss.per"

#Sorting predicted data (Means, SE)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
Data2Labile.sum <-aggregate(Massloss.per~Season+Region+Landuse, data=Data2Labile, mean)
colnames(Data2Labile.sum)[4] <- "Massloss.per"
Data2Labile.sum.se<-aggregate(Massloss.per~Season+Region+Landuse, data=Data2Labile, se)
Data2Labile.sum$SE<-Data2Labile.sum.se$SE

#Sorting observed data (Means, SE)
#LabileMain.sum <- aggregate(Massloss.per~Season+Region+Landuse, data=LabileMain, mean)
LabileMain.sum <-aggregate(cbind(Massloss.per,C.N,Temp,Sand)~Season+Region+Landuse, data=LabileMain, mean)
LabileMain.sum.se<-aggregate(Massloss.per~Season+Region+Landuse, data=LabileMain, se)
LabileMain.sum$SE<-LabileMain.sum.se$Massloss.per

#### Plot observed data versus prediction #####

Mainp <- ggplot(data=LabileMain.sum, aes(x=Landuse,y=Massloss.per))
Mainp <- Mainp+ geom_errorbar(data=Data2Labile.sum, aes(ymin=Massloss.per-SE, ymax=Massloss.per+SE), colour="green4",width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
Mainp <- Mainp+ geom_point(data=Data2Labile.sum,size=3,stroke=1.2,colour="green4",fill="green4",position=position_dodge(width=.35),show.legend=T) 
Mainp <- Mainp+geom_errorbar(aes(ymin=Massloss.per-SE, ymax=Massloss.per+SE),width=.5,lwd=1,show.legend=F)
Mainp <- Mainp+geom_point(position=position_dodge(width=.65),size=3)
Mainp <- Mainp+ facet_grid(Region ~ Season, scale ="fixed", labeller=labeller(Region = c(`Dry`= "Dry Region", `Wet`="Wet Region",`Intermediate`="Intermediate Region"),Season = c(`Wet`= "Wet Season", `Dry`="Dry Season")))
Mainp <- Mainp+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
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
                     ,axis.text.x = element_text(size=10,color="black",
                                                 margin=margin(2.5,2.5,2.5,2.5,"mm"))
                     ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
                     ,axis.ticks.length=unit(-1.5, "mm")
                     #,axis.line.y = element_blank()
                     ,axis.line.x = element_blank()
                     ,plot.margin = unit(c(8,50,5,5), "mm")
                     ,strip.background = element_rect(fill="transparent",colour=NA)
                     ,strip.text.x = element_text(size = 14,colour = "black")
                     ,strip.text.y = element_text(size = 14,colour = "black")
                     ,panel.spacing = unit(1, "lines")
                     ,legend.background = element_rect(fill = "transparent")
                     ,legend.title=element_blank()
                     ,legend.position = c(1.3,0.5)
                     ,legend.spacing.y = unit(-0.8, "mm")
                     ,legend.key.height=unit(7.5,"mm")
                     ,legend.key.width=unit(7.5,"mm")
                     ,legend.key = element_rect(colour = NA, fill = NA)
                     ,legend.key.size = unit(7,"mm")
                     ,legend.text=element_text(size=12,color="black"))
Mainp <- Mainp+
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15)

Mainp
#ggsave("Termites//Main & CG experiment/Mainlabile_predvsobs.png",
#      width= 20, height = 15,units ="cm",bg ="transparent",
#     dpi = 600, limitsize = TRUE)



#Recal Model - general massloss across season and landuse####
#Recal Model 1 - No moisture, no rain.
RecalMain$Season <- as.factor(RecalMain$Season)
RecalMain$Region <- as.factor(RecalMain$Region)
RecalMain$Landuse <- as.factor(RecalMain$Landuse)
RecalMain$Site<-as.factor(RecalMain$Site)
RecalMain$Blockcode<-as.factor(RecalMain$Blockcode)
RecalMain$Plot<-as.factor(RecalMain$Plot)

summary(RecalMain)
table(RecalMain$Massloss.per)
hist(RecalMain$Massloss.per)

GlobalRecalMainMod <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
                              Season:Region:Landuse+
                             Season:Region:Treatment+ #Runs without this
                             Season:Landuse:Treatment+ 
                             Region:Landuse:Treatment- #Runs without this
                              C.N:Temp-C.N:Sand-Temp:Sand+
                             (1|Site/Blockcode/Plot), na.action=na.omit,REML = F,data=RecalMain)
#Data is not enough to handle four three-ways with REML. So removing one of the two possible threeways.
GlobalRecalMainMod1 <- update(GlobalRecalMainMod, .~. -Region:Landuse:Treatment)
GlobalRecalMainMod2 <- update(GlobalRecalMainMod, .~. -Season:Region:Treatment)
anova(GlobalRecalMainMod1,GlobalRecalMainMod) #0.001985 **
anova(GlobalRecalMainMod2,GlobalRecalMainMod) #0.0001552 ***
AIC(GlobalRecalMainMod1)# 6405.566
AIC(GlobalRecalMainMod2)# 6410.663
#Choosing to remove Region:Landuse:Treatmentfrom model based on P-value and AIC, and start selection from there:

drop1(GlobalRecalMainMod1,test="Chisq")
# Season:C.N                1 6405.0  1.4596 0.2269975    #REMOVE
# Season:Temp               1 6404.9  1.3005 0.2541247    #REMOVE
# Season:Sand               1 6403.9  0.3022 0.5824758    #REMOVE
# Region:C.N                2 6402.6  1.0735 0.5846583    #REMOVE
# Region:Temp               2 6402.2  0.6778 0.7125380    #REMOVE
# Region:Sand               2 6402.1  0.5184 0.7716693    #REMOVE
# Landuse:C.N               2 6406.7  5.1091 0.0777254 .  
# Landuse:Temp              2 6410.7  9.1429 0.0103427 *  
#   Landuse:Sand              2 6402.2  0.6806 0.7115675    #REMOVE
# Treatment:C.N             1 6406.3  2.7767 0.0956448 .  
# Treatment:Temp            1 6403.6  0.0585 0.8088799    #REMOVE
# Treatment:Sand            1 6406.4  2.8293 0.0925592 .  
# Season:Region:Landuse     2 6406.6  5.0031 0.0819560 .  #Try to remove this? START HERE <- 
# Season:Region:Treatment   2 6416.8 15.2771 0.0004815 ***
#   Season:Landuse:Treatment  2 6412.7 11.1200 0.0038487 ** 

RecalMainMod1a <- update(GlobalRecalMainMod1, .~. -Treatment:Temp -Landuse:Sand-
                           Region:Sand-Region:Temp-Region:C.N-Season:Sand-
                           Season:Temp-Season:C.N)
AIC(RecalMainMod1a) #6388.022
drop1(RecalMainMod1a,test="Chisq") #All values that could be removed are significant.
# Landuse:C.N               2 6390.9  6.7905 0.0335326 *  
#   Landuse:Temp              2 6391.1  6.9662 0.0307116 *  
#   Treatment:C.N             1 6389.1  2.9951 0.0835154 .  #Could remove, but does not improve AIC
# Treatment:Sand            1 6389.1  2.9647 0.0851015 .  #Could remove, but does not improve AIC
# Season:Region:Landuse     2 6397.1 13.0190 0.0014892 ** 
#   Season:Region:Treatment   2 6400.4 16.2858 0.0002908 ***
#   Season:Landuse:Treatment  2 6395.1 11.0180 0.0040501 ** 

#Trying to remove the non sign at 0.05 and see if model improves:
RecalMainMod2a <- update(RecalMainMod1a, .~. -Treatment:Sand-Treatment:C.N)
AIC(RecalMainMod2a) #6389.604 - NOT IMPROVED OVER PREVIOUs MODEL
drop1(RecalMainMod2a,test="Chisq")
# Sand                      1 6397.3  9.6387 0.0019052 ** 
#   Landuse:C.N               2 6392.3  6.6486 0.0359985 *  
#   Landuse:Temp              2 6392.6  6.9081 0.0316166 *  
#   Season:Region:Landuse     2 6398.6 12.8947 0.0015847 ** 
#   Season:Region:Treatment   2 6401.8 16.1353 0.0003135 ***
#   Season:Landuse:Treatment  2 6398.7 13.0649 0.0014554 ** 


#Testing if adding back some variables I think could be important are now sign or not:
RecalMainMod1a1 <- update(RecalMainMod1a, .~. +C.N:Temp)
anova(RecalMainMod1a1,RecalMainMod1a) #C.N:Temp NS
RecalMainMod1a2 <- update(RecalMainMod1a, .~. +C.N:Sand)
anova(RecalMainMod1a2,RecalMainMod1a) #C.N:Sand NS
RecalMainMod1a3 <- update(RecalMainMod1a, .~. +Temp:Sand)
anova(RecalMainMod1a3,RecalMainMod1a) #Temp:Sand NS
RecalMainMod1a4 <- update(RecalMainMod1a, .~. +Treatment:C.N)
anova(RecalMainMod1a4,RecalMainMod1a) #Treatment:C.N NS
RecalMainMod1a5 <- update(RecalMainMod1a, .~. +Treatment:Temp)
anova(RecalMainMod1a5,RecalMainMod1a) #Treatment:Temp NS
#Testing if adding back some variables I think could be important are now sign or not:
RecalMainMod2a1 <- update(RecalMainMod2a, .~. +C.N:Temp)
anova(RecalMainMod1a1,RecalMainMod2a) #C.N:Temp BORDERLINE
RecalMainMod2a2 <- update(RecalMainMod2a, .~. +C.N:Sand)
anova(RecalMainMod1a2,RecalMainMod2a) #C.N:Sand NS
RecalMainMod2a3 <- update(RecalMainMod2a, .~. +Temp:Sand)
anova(RecalMainMod1a3,RecalMainMod2a) #Temp:Sand NS BORDERLINE
RecalMainMod2a4 <- update(RecalMainMod2a, .~. +Treatment:C.N)
anova(RecalMainMod1a4,RecalMainMod2a) #Treatment:C.N NS BORDERLINE
RecalMainMod2a5 <- update(RecalMainMod2a, .~. +Treatment:Temp)
anova(RecalMainMod2a5,RecalMainMod2a) #Treatment:Temp NS

#TRYING TO INCREASE BEST MODEL OF THE MODEL 2a:
RecalMainMod2a6 <- update(RecalMainMod2a, .~. +C.N:Temp+Treatment:Temp)
AIC(RecalMainMod2a6) #6391.802 - EVEN WORSE...OK GOING BACK TO MODEL 1a
drop1(RecalMainMod2a6,test="Chisq") #NOPE, the additioning of the two terms, not better for model

#Going back to GlobalRecalMainMod1 and trying to remove the borderline NS threeway and test from there:
RecalMainMod3a <- update(GlobalRecalMainMod1, .~. -Season:Region:Landuse)
AIC(RecalMainMod3a) #6406.492
drop1(RecalMainMod3a,test="Chisq")
# Season:C.N                1 6408.8  4.2214 0.0399174 *  
#   Season:Temp               1 6404.7  0.1157 0.7336918    
# Season:Sand               1 6410.4  5.8748 0.0153589 *  
#   Region:Landuse            2 6413.3 10.7154 0.0047117 ** 
#   Region:C.N                2 6403.4  0.8604 0.6503679    
# Region:Temp               2 6403.2  0.6168 0.7346225    
# Region:Sand               2 6402.9  0.2816 0.8686498    
# Landuse:C.N               2 6409.0  6.4353 0.0400495 *  
#   Landuse:Temp              2 6410.2  7.6117 0.0222406 *  
#   Landuse:Sand              2 6404.3  1.7118 0.4248908    
# Treatment:C.N             1 6407.3  2.7570 0.0968304 .  
# Treatment:Temp            1 6404.6  0.0697 0.7917725    
# Treatment:Sand            1 6407.4  2.8264 0.0927280 .  
# Season:Region:Treatment   2 6417.6 15.0550 0.0005381 ***
#   Season:Landuse:Treatment  2 6413.7 11.1544 0.0037831 ** 
RecalMainMod3a1 <- update(RecalMainMod3a, .~. -Treatment:Temp-Landuse:Sand-Region:Sand-Region:Temp-Region:C.N-
                            Season:Temp)
AIC(RecalMainMod3a1) #6390.274
drop1(RecalMainMod3a1, test="Chisq")
# Season:C.N                1 6392.5  4.1905 0.0406508 *  
#   Season:Sand               1 6393.6  5.2528 0.0219109 *  
#   Region:Landuse            2 6399.7 13.3391 0.0012690 ** 
#   Landuse:C.N               2 6392.5  6.1611 0.0459342 *  
#   Landuse:Temp              2 6392.8  6.4170 0.0404177 *  
#   Treatment:C.N             1 6391.3  2.9855 0.0840117 .  
# Treatment:Sand            1 6391.3  2.9202 0.0874795 .  
# Season:Region:Treatment   2 6402.5 16.1277 0.0003147 ***
#   Season:Landuse:Treatment  2 6397.5 11.1542 0.0037835 ** 
RecalMainMod3a2 <- update(RecalMainMod3a1, .~. -Treatment:C.N-Treatment:Sand)
AIC(RecalMainMod3a2) #6391.805
drop1(RecalMainMod3a2, test="Chisq")
# Season:C.N                1 6394.1  4.1927 0.0405983 *  
#   Season:Sand               1 6395.1  5.1882 0.0227401 *  
#   Region:Landuse            2 6401.2 13.3169 0.0012831 ** 
#   Landuse:C.N               2 6393.9  5.9794 0.0503021 .  
# Landuse:Temp              2 6394.2  6.3595 0.0415957 *  
#   Season:Region:Treatment   2 6403.9 15.9816 0.0003386 ***
#   Season:Landuse:Treatment  2 6401.1 13.2113 0.0013527 ** 
RecalMainMod3a3 <- update(RecalMainMod3a2, .~. -Landuse:C.N)
AIC(RecalMainMod3a3) #6393.785
drop1(RecalMainMod3a3, test="Chisq") #All significant, but not a better model than previous.
# Season:C.N                1 6399.5  7.6462 0.0056892 ** 
#   Season:Sand               1 6397.4  5.5466 0.0185165 *  
#   Region:Landuse            2 6402.7 12.7974 0.0016637 ** 
#   Landuse:Temp              2 6396.0  6.1035 0.0472752 *  
#   Season:Region:Treatment   2 6405.7 15.8640 0.0003591 ***
#   Season:Landuse:Treatment  2 6403.1 13.2040 0.0013576 ** 


#The best model, and generating P-values:
RecalMainMod1ab <- update(RecalMainMod1a, .~. -Season:Landuse:Treatment)
RecalMainMod1ac <- update(RecalMainMod1a, .~. -Season:Region:Treatment)
RecalMainMod1ad <- update(RecalMainMod1a, .~. -Season:Region:Landuse)
RecalMainMod1aE <- update(RecalMainMod1a, .~. -Season:Landuse:Treatment-
                            Season:Region:Treatment-Season:Region:Landuse)
RecalMainMod1af <- update(RecalMainMod1aE, .~. -Treatment:Sand)
RecalMainMod1ag <- update(RecalMainMod1aE, .~. -Treatment:C.N)
RecalMainMod1ah <- update(RecalMainMod1aE, .~. - Landuse:Temp)
RecalMainMod1ai <- update(RecalMainMod1aE, .~. - Landuse:C.N)
RecalMainMod1aj <- update(RecalMainMod1aE, .~. - Landuse:Treatment)
RecalMainMod1ak <- update(RecalMainMod1aE, .~. - Region:Treatment)
RecalMainMod1al <- update(RecalMainMod1aE, .~. - Region:Landuse)
RecalMainMod1am <- update(RecalMainMod1aE, .~. - Season:Treatment)
RecalMainMod1an <- update(RecalMainMod1aE, .~. - Season:Landuse)
RecalMainMod1ao <- update(RecalMainMod1aE, .~. - Season:Region)
RecalMainMod1aP <- update(RecalMainMod1aE, .~. - Treatment:Sand-Treatment:C.N-Landuse:Temp-
                            Landuse:C.N-Landuse:Treatment- Region:Treatment-
                            -Region:Landuse-Season:Treatment-Season:Landuse-
                            Season:Region)
RecalMainMod1aq <- update(RecalMainMod1aP, .~. -  Season)
RecalMainMod1ar <- update(RecalMainMod1aP, .~. -  Region)
RecalMainMod1as <- update(RecalMainMod1aP, .~. -  Landuse)
RecalMainMod1at <- update(RecalMainMod1aP, .~. -  Treatment)
RecalMainMod1au <- update(RecalMainMod1aP, .~. -  C.N)
RecalMainMod1av <- update(RecalMainMod1aP, .~. -  Temp)
RecalMainMod1aw <- update(RecalMainMod1aP, .~. -  Sand)


anova(RecalMainMod1ab,RecalMainMod1a)# 0.004051 ** Season:Landuse:Treatment
anova(RecalMainMod1ac,RecalMainMod1a)# 0.0002909 *** Season:Region:Treatment
anova(RecalMainMod1ad,RecalMainMod1a)# 0.00149 ** Season:Region:Landuse
anova(RecalMainMod1af,RecalMainMod1aE)# 0.08624 . Treatment:Sand    NS - Effect not sign, but important for model
anova(RecalMainMod1ag,RecalMainMod1aE) # 0.03512 Treatment.C.N
anova(RecalMainMod1ah,RecalMainMod1aE)# 0.2149 Landuse:Temp     NS - Effect not sign, but important for model
anova(RecalMainMod1ai,RecalMainMod1aE)# 0.007003 ** Landuse:C.N
anova(RecalMainMod1aj,RecalMainMod1aE)# 0.8755 Landuse:Treatment  NS  - Needed for threeway
anova(RecalMainMod1ak,RecalMainMod1aE)# 0.1828 Region:Treatment   NS   - Needed for threeway
anova(RecalMainMod1al,RecalMainMod1aE)# 0.009045 ** Region:Landuse  - Needed for threeway
anova(RecalMainMod1am,RecalMainMod1aE)# 0.01887 * Season:Treatment - Needed for threeway
anova(RecalMainMod1an,RecalMainMod1aE)# 0.2557 Season:Landuse     NS   - Needed for threeway
anova(RecalMainMod1ao,RecalMainMod1aE)# 1.767e-12 *** Season:Region - Needed for threeway
anova(RecalMainMod1aq,RecalMainMod1aP)# 2.747e-08 *** Season
anova(RecalMainMod1ar,RecalMainMod1aP)# 2.2e-16 *** Region
anova(RecalMainMod1as,RecalMainMod1aP)# 1 Landuse               NS   - Needed for two- and threeway
anova(RecalMainMod1at,RecalMainMod1aP)# 2.2e-16 *** Treatment
anova(RecalMainMod1au,RecalMainMod1aP)# 0.7848 C.N              NS  -   - Needed for sign twoway
anova(RecalMainMod1av,RecalMainMod1aP)# 0.001983 ** Temp
anova(RecalMainMod1aw,RecalMainMod1aP)# 0.000358 *** Sand

#FINAL model:
RecalMain$Site<-as.factor(RecalMain$Site)
RecalMain$Blockcode<-as.factor(RecalMain$Blockcode)
RecalMain$Plot<-as.factor(RecalMain$Plot)
RecalMain$Treatment<-as.factor(RecalMain$Treatment)

RecalMainModFINAL <- lmer(Massloss.per ~ Season+Region+Landuse+Treatment+C.N+Temp+Sand+
                            Treatment:Sand+Treatment:C.N+Landuse:Temp+
                            Landuse:C.N+Landuse:Treatment+Region:Treatment+
                            +Region:Landuse+Season:Treatment+Season:Landuse+
                            Season:Region+
                            Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+
                             (1|Site/Blockcode/Plot), na.action=na.omit,REML = T, data=RecalMain)

summary(RecalMainModFINAL)
anova(RecalMainModFINAL)
coef(summary(RecalMainModFINAL))

#Inspect chosen model for homogeneity:
E1 <- resid(RecalMainModFINAL, type ="pearson")
F1 <- fitted(RecalMainModFINAL)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)

hist(E1, nclass = 30) 

#### Standadrized temp, C:N and Sand  variables -  remove resdiual negative slope #####
RecalMain$Temp.std <- (RecalMain$Temp - mean(RecalMain$Temp, na.rm=T)) / sd(RecalMain$Temp,na.rm=T)
RecalMain$Sand.std <- (RecalMain$Sand - mean(RecalMain$Sand, na.rm=T)) / sd(RecalMain$Sand,na.rm=T)
RecalMain$C.N.std <- (RecalMain$C.N - mean(RecalMain$C.N,na.rm=T)) / sd(RecalMain$C.N, na.rm=T)

RecalMainModFINAL2 <- lmer(Massloss.per ~ Season+Region+Landuse+Treatment+C.N.std +Temp.std +Sand.std +
                            Treatment:Sand.std +Treatment:C.N.std +Landuse:Temp.std +
                            Landuse:C.N.std +Landuse:Treatment+Region:Treatment+
                            +Region:Landuse+Season:Treatment+Season:Landuse+
                            Season:Region+
                            Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+
                            (1|Site/Blockcode/Plot), na.action=na.omit,REML = T, data=RecalMain)

summary(RecalMainModFINAL2)
anova(RecalMainModFINAL2)

#Inspect chosen model for homogeneity:
E1 <- resid(RecalMainModFINAL2, type ="pearson")
F1 <- fitted(RecalMainModFINAL2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)

#### GLMM with Beta distribution ####
# Ensure mass loss values are between 0 and 1 (cannot be exactly zero and one so 0.01 and 99.99)
RecalMain$Massloss.per[RecalMain$Massloss.per>99.99]<-99.99
RecalMain$Massloss.per[RecalMain$Massloss.per<0.01]<-0.01
RecalMain$Massloss.perB<-RecalMain$Massloss.per/100
#### GLMM - glmmADMB pacakge #### Can also used glmmTMB
library(glmmADMB)

RecalMain2<-RecalMain[!is.na(RecalMain$Massloss.per),]
RecalMain2$Massloss.per

RecalMain2$Site<-as.factor(RecalMain2$Site)
RecalMain2$Blockcode<-as.factor(RecalMain2$Blockcode)
RecalMain2$Plot<-as.factor(RecalMain2$Plot)


min(RecalMain2$Massloss.perB)
max(RecalMain2$Massloss.perB)
#RecalMainModFINAL2 <- glmmadmb(Massloss.perB ~ Season+Region+Landuse+C.N+Temp+Sand+
                            Treatment:Sand+Treatment:C.N+Landuse:Temp+
                            Landuse:C.N+Landuse:Treatment+Region:Treatment+
                            #+Region:Landuse+Season:Treatment+Season:Landuse+
                           # Season:Region+
                          #  Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+
                            (1|Site/Blockcode/Plot), 
                           # admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5),
                            family="beta", data=RecalMain2)


#RecalMainModFINAL2 <-glmmTMB(Massloss.perB ~ Season+Region+Landuse+C.N+Temp+Sand+
#                               (1|Site/Blockcode/Plot),
#                family=list(family="beta",link="logit"),
#                data = RecalMain2)

summary(RecalMainModFINAL2)


#Exploring interaction levels, what drives the interactions to be significant?####
#Interesting interactions to look at:
#Threeway: Season:Region:Landuse, Season:Region:Treatment, Season:Landuse:Treatment
#Twoway: Treatment:C.N, Landuse:C.N, 
#emmeans
#First up:Season:Region:Landuse

#ref.grid.RecalMainModFINAL <- ref_grid(RecalMainModFINAL) #at = list(Region = c("Dry", "Wet"))) #Want to remove Intermadiate in the grid as it can't be compared to the other landuses, except wild.
#ref.grid.RecalMainModFINAL#See how the grid is looking. Check for correct factors and the emmeans of numerical variables. If testing between numerical variables, only the means or the low/high end of values can be specified. I.e contrasts vs trend. See emmeans vignette for more info.
#Check threeway first:
#emmip(ref.grid.RecalMainModFINAL, Landuse~Region|Season+Treatment, type="response")#
#emmeans.RecalMainModFINAL <- emmeans(ref.grid.RecalMainModFINAL, pairwise~Season*Region*Landuse|Treatment,type="response") #
#emmeans.RecalMainModFINAL$contrasts #Get contrast between factors (linear). This is somewhat similar to pairs()
#emmeans.RecalMainModFINAL$emmeans #Get emmeans of factors.
#emmeans.RecalMainModFINAL.pairs <- pairs(emmeans.RecalMainModFINAL,simple = "each", combine =TRUE) # Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which simple effect is being displayed. 
#write.csv(emmeans.RecalMainModFINAL.pairs$emmeans,file="Termites/Emmeans_contrast_Threeway_RecalMainMod.csv")
#plot(emmeans.RecalMainModFINAL, comparisons = FALSE) #Comparisons summarized graphically. The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them ,if comparison=TRUE. If an arrow from one mean overlaps an arrow from another group, the difference is not significant



#Predicted and observed values on graph####
#### Sketch fitted values #
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE

RecalMainModFINAL <- lmer(Massloss.per ~ Season+Region+Landuse+C.N+Temp+Sand+Treatment
                            Treatment:Sand+Treatment:C.N+Landuse:Temp+
                            Landuse:C.N+Landuse:Treatment+Region:Treatment+
                            +Region:Landuse+Season:Treatment+Season:Landuse+
                            Season:Region+
                            Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+
                            (1|Site/Blockcode/Plot), na.action=na.omit,REML = T, data=RecalMain)

#A:Specify covariate values for predictions - Recal
Data2Recal <- expand.grid(Season=levels(RecalMain$Season), #Specify which terms are used in the model. Specify levels for factors and min-max for numeric values. Specify length for each numeric var (how many predictions are created)
                           Region=levels(RecalMain$Region),
                           Landuse=levels(RecalMain$Landuse),
                          Treatment=levels(RecalMain$Treatment),
                           C.N = seq(min(RecalMain$C.N), max(RecalMain$C.N), length=12),
                           Temp = seq(min(RecalMain$Temp,na.rm=T), max(RecalMain$Temp,na.rm=T), length=12),
                           Sand = seq(min(RecalMain$Sand), max(RecalMain$Sand), length=12))

#B. Create X matrix with expand.grid
X1 <- model.matrix(~ Season+Region+Landuse+C.N+Temp+Sand+
                     Treatment:Sand+Treatment:C.N+Landuse:Temp+
                     Landuse:C.N+Landuse:Treatment+Region:Treatment+
                     +Region:Landuse+Season:Treatment+Season:Landuse+
                     Season:Region+
                     Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment,
                   data = Data2Recal)
head(X1)

#C. Calculate predicted values
Data2Recal$Pred <- X1 %*% fixef(RecalMainModFINAL) 

#D. Calculate standard errors (SE) for predicted values
#Data2Recal$SE <- sqrt(  diag(X1 %*% vcov(RecalMainModFINAL) %*% t(X1))  ) #Hash out if not using. Takes some time to run!
memory.limit() #Need to allocate more memory to R to be able to do this with length of 25. Scaled down to max of 12, since only ~8gb is given.


#And using the Pred and SE values, we can calculate
#a 95% confidence interval
Data2Recal$SeUp <- Data2Recal$Pred + 1.96 * Data2Recal$SE
Data2Recal$SeLo <- Data2Recal$Pred - 1.96 * Data2Recal$SE

#E. Plot predicted values
names(Data2Recal) #NB! Some predicted values are negativ mass loss. This does not makes sense...
colnames(Data2Recal)[3]<-"Landuse"
colnames(Data2Recal)[7] <- "Massloss.per"

#Sorting predicted data (Means, SE)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
Data2Recal.sum <-aggregate(Massloss.per~Season+Region+Landuse, data=Data2Recal, mean)
colnames(Data2Recal.sum)[4] <- "Massloss.per"
Data2Recal.sum.se<-aggregate(Massloss.per~Season+Region+Landuse, data=Data2Recal, se)
Data2Recal.sum$SE<-Data2Recal.sum.se$SE

#Sorting observed data (Means, SE)
#RecalMain.sum <- aggregate(Massloss.per~Season+Region+Landuse, data=RecalMain, mean)
RecalMain.sum <-aggregate(cbind(Massloss.per,C.N,Temp,Sand)~Season+Region+Landuse, data=RecalMain, mean)
RecalMain.sum.se<-aggregate(Massloss.per~Season+Region+Landuse, data=RecalMain, se)
RecalMain.sum$SE<-RecalMain.sum.se$Massloss.per

#### Plot observed data versus prediction #####

Mainp <- ggplot(data=RecalMain.sum, aes(x=Landuse,y=Massloss.per))
Mainp <- Mainp+ geom_errorbar(data=Data2Recal.sum, aes(ymin=Massloss.per-SE, ymax=Massloss.per+SE), colour="green4",width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
Mainp <- Mainp+ geom_point(data=Data2Recal.sum,size=3,stroke=1.2,colour="green4",fill="green4",position=position_dodge(width=.35),show.legend=T) 
Mainp <- Mainp+geom_errorbar(aes(ymin=Massloss.per-SE, ymax=Massloss.per+SE),width=.5,lwd=1,show.legend=F)
Mainp <- Mainp+geom_point(position=position_dodge(width=.65),size=3)
Mainp <- Mainp+ facet_grid(Region ~ Season, scale ="fixed", labeller=labeller(Region = c(`Dry`= "Dry Region", `Wet`="Wet Region",`Intermediate`="Intermediate Region"),Season = c(`Wet`= "Wet Season", `Dry`="Dry Season")))
Mainp <- Mainp+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
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
                     ,axis.text.x = element_text(size=10,color="black",
                                                 margin=margin(2.5,2.5,2.5,2.5,"mm"))
                     ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
                     ,axis.ticks.length=unit(-1.5, "mm")
                     #,axis.line.y = element_blank()
                     ,axis.line.x = element_blank()
                     ,plot.margin = unit(c(8,50,5,5), "mm")
                     ,strip.background = element_rect(fill="transparent",colour=NA)
                     ,strip.text.x = element_text(size = 14,colour = "black")
                     ,strip.text.y = element_text(size = 14,colour = "black")
                     ,panel.spacing = unit(1, "lines")
                     ,legend.background = element_rect(fill = "transparent")
                     ,legend.title=element_blank()
                     ,legend.position = c(1.3,0.5)
                     ,legend.spacing.y = unit(-0.8, "mm")
                     ,legend.key.height=unit(7.5,"mm")
                     ,legend.key.width=unit(7.5,"mm")
                     ,legend.key = element_rect(colour = NA, fill = NA)
                     ,legend.key.size = unit(7,"mm")
                     ,legend.text=element_text(size=12,color="black"))
Mainp <- Mainp+
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15)

Mainp



#Termite effect models, using the difference between treatment (op-excl)####
####Preliminary dataprocessing####
names(LabileTermEff) #Data has both CG and Main experiment - Need to seperate
names(RecalTermEff) #Data has both CG and Main experiment - Need to seperate

#Seperate experiments
LabileTermEff.Main <- LabileTermEff[LabileTermEff$Experiment=="Main",]
RecalTermEff.Main <- RecalTermEff[RecalTermEff$Experiment=="Main",]
#Extracting Local soil (Seronera) and adding it to the Main datasets:
LocalCGsoil.T.E.Labile <- LabileTermEff[LabileTermEff$Site=="Seronera",] #The sites are from 1-4, but actually they are just 1 when comparing to the other sites.
LocalCGsoil.T.E.Recal <- RecalTermEff[RecalTermEff$Site=="Seronera",]
#So replacing site numbering to only 1.
LocalCGsoil.T.E.Labile$Block <- 1
LocalCGsoil.T.E.Recal$Block  <- 1
LocalCGsoil.T.E.Labile$Blockcode <- "Int_W1"
LocalCGsoil.T.E.Recal$Blockcode <- "Int_W1"

LabileTermEff.Main <- rbind(LabileTermEff.Main,LocalCGsoil.T.E.Labile)
RecalTermEff.Main <- rbind(RecalTermEff.Main,LocalCGsoil.T.E.Recal)

#Renaming some columns (RECAL):
names(RecalTermEff.Main)
colnames(RecalTermEff.Main)[(names(RecalTermEff.Main)== "Sandcorr")] <- "Sand"
colnames(RecalTermEff.Main)[(names(RecalTermEff.Main)== "Claycorr")] <- "Clay"
colnames(RecalTermEff.Main)[(names(RecalTermEff.Main)== "Moisture..")] <- "Moisture"
colnames(RecalTermEff.Main)[(names(RecalTermEff.Main)== "Temperature..C.")] <- "Temp"
colnames(RecalTermEff.Main)[(names(RecalTermEff.Main)== "Rain.sum")] <- "Rain"
#Renaming some columns (LABILE):
names(LabileTermEff.Main)
colnames(LabileTermEff.Main)[(names(LabileTermEff.Main)== "Sandcorr")] <- "Sand"
colnames(LabileTermEff.Main)[(names(LabileTermEff.Main)== "Claycorr")] <- "Clay"
colnames(LabileTermEff.Main)[(names(LabileTermEff.Main)== "Moisture..")] <- "Moisture"
colnames(LabileTermEff.Main)[(names(LabileTermEff.Main)== "Temperature..C.")] <- "Temp"
colnames(LabileTermEff.Main)[(names(LabileTermEff.Main)== "Rain.sum")] <- "Rain"

#Block needs to be a unique number - not repeated across blocks (I think blockcode already do this but anyway...)

LabileTermEff.Main$blockdesign.num<-as.factor(with(LabileTermEff.Main, paste(Season,Region,Landuse,Blockcode, sep="")))
LabileTermEff.Main$blockdesign.num<-as.numeric(LabileTermEff.Main$blockdesign.num)
LabileTermEff.Main$blockdesign.num<-as.factor(LabileTermEff.Main$blockdesign.num)
table(LabileTermEff.Main$blockdesign.num, LabileTermEff.Main$Landuse)

RecalTermEff.Main$blockdesign.num<-as.factor(with(RecalTermEff.Main, paste(Season,Region,Landuse,Blockcode, sep="")))
RecalTermEff.Main$blockdesign.num<-as.numeric(RecalTermEff.Main$blockdesign.num)
RecalTermEff.Main$blockdesign.num<-as.factor(RecalTermEff.Main$blockdesign.num)
table(RecalTermEff.Main$blockdesign.num, RecalTermEff.Main$Landuse)


##Labile Model - Termite effect massloss####
#Singular fit issue:
LabileT.E.Mod <- lmer(Termite.effect~Season+Landuse+Region+Sand+Rain+Temp+
                        (1|Site/Blockcode/Plot),na.action=na.omit,REML = FALSE, data=LabileTermEff.Main)

#Recal Model - Termite effect massloss####
#Singular fit issue:
RecalT.E.Mod <- lmer(Termite.effect~Season+Landuse+Region+Sand+Rain+Temp+
                       (1|Site/Blockcode/Plot),na.action=na.omit,REML = FALSE, data=RecalTermEff.Main)





#Graphing: Main experiment - decomposition across landuse #### 
# Fill by termite * landuse = empty = absence, filled = prescence 
DataMainmean1$tea.hole<-as.factor(with(DataMainmean1, paste(flittertype, ftreatment, sep=" ")))
levels(DataMainmean1$tea.hole)
#levels(DataMainmean$fregion)<-c("Dry region","Wet region")
DataMainmean1$fregion <- as.factor(DataMainmean1$fregion)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(DataMainmean1$fregion)
levels(DataMainmean1$fseason)
Mainexp <- DataMainmean1
#Now, ready for graphing: Main experiment massloss against landuse
#Want to reorder tea.hole to have a nicer legend:
Mainexp$tea.hole <- ordered(Mainexp$tea.hole, levels=c("Green Exclosed", "Rooibos Exclosed","Green Open","Rooibos Open"))
levels(Mainexp$tea.hole)

colnames(Mainexp)[1]<-"Season"
colnames(Mainexp)[2]<-"Region"
names(Mainexp)

#Point plot Main exp####
Mainp <- ggplot(data=Mainexp, aes(x=flanduse,y=Massloss.per,
                                  ymin=(Massloss.per-SE),
                                  ymax=(Massloss.per+SE),
                                  fill = tea.hole,
                                  color = flittertype,
                                  shape=flanduse)
)+
  
  geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F) + 
  geom_point(size=5,stroke=1.2,position=position_dodge(width=.35),show.legend=T)+ 
  facet_grid(Region ~ Season, scale ="fixed", labeller=labeller(Region = c(`Dry`= "Dry Region", `Wet`="Wet Region"),
                                                                Season = c(`Wet`= "Wet Season", `Dry`="Dry Season")))+
  scale_color_manual(values=c("green4", "orangered3"))+
  scale_fill_manual(values=c("green4","orangered3","white","white"))+
  scale_shape_manual(values=c(21,23,24))+
  guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
  scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Land-use")+
  ylab("Mass loss (%)"
  )+
  
  theme(rect = element_rect(fill ="transparent")
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
        ,axis.text.x = element_text(size=10,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        #,axis.line.y = element_blank()
        ,axis.line.x = element_blank()
        ,plot.margin = unit(c(8,50,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 14,colour = "black")
        ,strip.text.y = element_text(size = 14,colour = "black")
        ,panel.spacing = unit(1, "lines")
        ,legend.background = element_rect(fill = "transparent")
        ,legend.title=element_blank()
        ,legend.position = c(1.3,0.5)
        ,legend.spacing.y = unit(-0.8, "mm")
        ,legend.key.height=unit(7.5,"mm")
        ,legend.key.width=unit(7.5,"mm")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.key.size = unit(7,"mm")
        ,legend.text=element_text(size=12,color="black")
        
  )+
  
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15)

Mainp
#ggsave("Termites//Main & CG experiment/Mainexp.png",
#      width= 20, height = 15,units ="cm",bg ="transparent",
#     dpi = 600, limitsize = TRUE)


#|####
#COMMON GARDEN EXP####
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

write.csv(write.csv(DataMain,file="Termites/Maindata.csv")) #Main with Seronera
write.csv(write.csv(DataCG,file="Termites/CGdata.csv"))

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
#range0to1 <- function(x){(x-min(x))/(max(x)-min(x))}
#DataMCG$Massloss.per.xratio <- range0to1(DataMCG$Massloss.per.x)
#DataMCG$Massloss.per.yratio <- range0to1(DataMCG$Massloss.per.y)
DataMCG$MainCGratio <- DataMCG$Massloss.per.x-DataMCG$Massloss.per.y

#hist(DataMCG$Massloss.per.xratio)
#hist(DataMCG$Massloss.per.yratio)
#plot(DataMCG$Massloss.per.yratio)

#Checking the distribution of massloss in Main and CG
par(mfrow=c(1,1))
qplot(DataMCG$Season,DataMCG$Massloss.per.x) #Main
qplot(DataMCG$Season,DataMCG$Massloss.per.y) #CG
qplot(DataMCG$Season,DataMCG$MainCGdiff)
qplot(DataMCG$Season,DataMCG$MainCGratio)

histogram(~  Massloss.per.x| Season+Littertype, data = DataMCG,xlab="Main experiment")
histogram(~  Massloss.per.y| Season+Littertype, data = DataMCG, xlab="Common Garden")

histogram(~  Massloss.per.x| Season+Treatment, data = DataMCG,xlab="Main experiment")
histogram(~  Massloss.per.y| Season+Treatment, data = DataMCG, xlab="Common Garden")


summary(DataMCG)

histogram(~  Massloss.per.x+Massloss.per.y| Season, data = LabileDataMCG)
histogram(~  Massloss.per.x+Massloss.per.y| Season, data = RecalDataMCG)

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

#Ensuring factors are factors:
RecalDataMCG$Season <- as.factor(RecalDataMCG$Season)
RecalDataMCG$Region <- as.factor(RecalDataMCG$Region)
RecalDataMCG$Landuse <- as.factor(RecalDataMCG$Landuse)
RecalDataMCG$Treatment <- as.factor(RecalDataMCG$Treatment)
LabileDataMCG$Season <- as.factor(LabileDataMCG$Season)
LabileDataMCG$Region <- as.factor(LabileDataMCG$Region)
LabileDataMCG$Landuse <- as.factor(LabileDataMCG$Landuse)
LabileDataMCG$Treatment <- as.factor(LabileDataMCG$Treatment)

#COMMON GARDEN MODELLING####
#Labilemodel analysis####
GlobalLabileMCGMod <- lmer(MainCGdiff ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2
                           +Season:Landuse:Treatment+Season:Landuse:Region+
                             Landuse:Region:Treatment+Season:Region:Treatment+
                             (1|Site/Blockcode), na.action=na.omit, REML=F, data =LabileDataMCG)
summary(GlobalLabileMCGMod)                         
AIC(GlobalLabileMCGMod)# 773.2234
drop1(GlobalLabileMCGMod,test="Chisq")
# Season:C.N                1 771.65  0.4279 0.5130239    
# Season:Temp               1 771.25  0.0227 0.8802988    
# Season:Sand               1 785.24 14.0178 0.0001811 ***
#   Region:C.N                1 771.67  0.4515 0.5016040    
# Region:Temp               1 784.74 13.5163 0.0002365 ***
#   Region:Sand               1 771.24  0.0156 0.9005052    
# Landuse:C.N               2 769.31  0.0882 0.9568677    
# Landuse:Temp              2 783.76 14.5378 0.0006969 ***
#   Landuse:Sand              2 776.00  6.7753 0.0337878 *  
#   Treatment:C.N             1 771.38  0.1544 0.6943222    
# Treatment:Temp            1 771.28  0.0595 0.8072838    
# Treatment:Sand            1 771.28  0.0612 0.8046842    
# C.N:Temp                  1 772.29  1.0634 0.3024423    
# C.N:Sand                  1 771.36  0.1334 0.7148878    
# Temp:Sand                 1 776.66  5.4405 0.0196751 *  
#   Season:Landuse:Treatment  2 769.39  0.1657 0.9205081  #REMOVE  
# Season:Region:Landuse     2 771.18  1.9539 0.3764606    #REMOVE
# Region:Landuse:Treatment  2 770.28  1.0575 0.5893542    #REMOVE
# Season:Region:Treatment   2 770.98  1.7604 0.4146967 #REMOVE
LabileMCGMod1 <- update(GlobalLabileMCGMod, .~. -Season:Landuse:Treatment-Season:Region:Landuse-
                          Region:Landuse:Treatment-Season:Region:Treatment)
AIC(LabileMCGMod1)# 762.1489
drop1(LabileMCGMod1,test="Chisq")
# Season:Region      1 825.73 65.582 5.574e-16 ***
#   Season:Landuse     2 759.51  1.358   0.50703    #REMOVE
# Season:Treatment   1 765.91  5.756   0.01643 *  
#   Season:Temp        1 760.70  0.550   0.45829    #REMOVE
# Season:Sand        1 786.38 26.230 3.032e-07 ***
#   Season:C.N         1 760.28  0.133   0.71547    #REMOVE
# Region:Landuse     2 777.82 19.675 5.342e-05 ***
#   Region:Treatment   2 758.74  0.592   0.74377    #REMOVE
# Region:Temp        1 776.68 16.527 4.796e-05 ***
#   Region:Sand        1 760.68  0.528   0.46757    #REMOVE
# Region:C.N         1 760.42  0.273   0.60129    #REMOVE
# Landuse:Treatment  2 758.25  0.097   0.95285    #REMOVE
# Landuse:Temp       2 776.62 18.475 9.732e-05 ***
#   Landuse:Sand       2 766.35  8.200   0.01657 *  
#   Landuse:C.N        2 758.63  0.481   0.78627   #REMOVE 
# Treatment:Temp     1 760.59  0.437   0.50877    #REMOVE
# Treatment:Sand     1 760.18  0.032   0.85815    #REMOVE
# Treatment:C.N      1 760.55  0.405   0.52470    #REMOVE
# Temp:Sand          1 766.18  6.027   0.01408 *  
#   Temp:C.N           1 761.76  1.611   0.20437    #REMOVE
# Sand:C.N           1 760.15  0.000   0.98268 #REMOVE

LabileMCGMod2 <- update(LabileMCGMod1, .~. -Sand:C.N-Temp:C.N-Treatment:C.N-Treatment:Sand-Treatment:Temp-
                          Landuse:C.N-Landuse:Treatment-Region:C.N-Region:Sand-Region:Treatment-
                          Season:C.N-Season:Temp-Season:Landuse)
AIC(LabileMCGMod2)# 735.2236
drop1(LabileMCGMod2,test="Chisq")
# C.N               1 733.64   0.418  0.517979   #REMOVE 
# Season:Region     1 849.00 115.774 < 2.2e-16 ***
#   Season:Treatment  1 738.99   5.771  0.016294 *  
#   Season:Sand       1 772.24  39.020 4.195e-10 ***
#   Region:Landuse    2 761.72  30.492 2.392e-07 ***
#   Region:Temp       1 773.27  40.046 2.481e-10 ***
#   Landuse:Temp      2 782.13  50.910 8.812e-12 ***
#   Landuse:Sand      2 739.94   8.713  0.012821 *  
#   Temp:Sand         1 742.55   9.329  0.002255 ** 
LabileMCGMod3 <- update(LabileMCGMod2, .~. -C.N)
AIC(LabileMCGMod3)# 733.6415
drop1(LabileMCGMod3,test="Chisq") #All sign, but still rank deficient
summary(LabileMCGMod3)

#Exctract P-values from best model for all terms:
LabileMCGMod3a <- update(LabileMCGMod3, .~. -Season:Region)
LabileMCGMod3b <- update(LabileMCGMod3, .~. -Season:Treatment )
LabileMCGMod3c <- update(LabileMCGMod3, .~. -Season:Sand )
LabileMCGMod3d <- update(LabileMCGMod3, .~. -Region:Landuse)
LabileMCGMod3e <- update(LabileMCGMod3, .~. -Region:Temp)
LabileMCGMod3f <- update(LabileMCGMod3, .~. -Landuse:Temp)
LabileMCGMod3g <- update(LabileMCGMod3, .~. -Landuse:Sand)
LabileMCGMod3h <- update(LabileMCGMod3, .~. -Temp:Sand)
LabileMCGMod3I <- update(LabileMCGMod3, .~. -Season:Region-Season:Treatment-Season:Sand-Region:Landuse-
                           Region:Temp-Landuse:Temp-Landuse:Sand-Temp:Sand) #REMOVE ALL 2ways
LabileMCGMod3j <- update(LabileMCGMod3I, .~. -Season)
LabileMCGMod3k <- update(LabileMCGMod3I, .~. -Region)
LabileMCGMod3l <- update(LabileMCGMod3I, .~. -Landuse)
LabileMCGMod3m <- update(LabileMCGMod3I, .~. -Treatment)
LabileMCGMod3n <- update(LabileMCGMod3I, .~. -Temp)
LabileMCGMod3o <- update(LabileMCGMod3I, .~. -Sand)

anova(LabileMCGMod3a,LabileMCGMod3) #Season:Region 2.2e-16 ***
anova(LabileMCGMod3b,LabileMCGMod3) #Season:Treatment 0.01675 *
anova(LabileMCGMod3c,LabileMCGMod3) #Season:Sand 2.31e-10 ***
anova(LabileMCGMod3d,LabileMCGMod3) #Region:Landuse 4.574e-08 ***
anova(LabileMCGMod3e,LabileMCGMod3) #Region:Temp 3.653e-11 ***
anova(LabileMCGMod3f,LabileMCGMod3) #Landuse:Temp 9.561e-13 ***
anova(LabileMCGMod3g,LabileMCGMod3) #Landuse:Sand 0.008727 **
anova(LabileMCGMod3h,LabileMCGMod3) #Temp:Sand 0.000803 ***
anova(LabileMCGMod3j,LabileMCGMod3I) #Season 3.279e-15 ***
anova(LabileMCGMod3k,LabileMCGMod3I) #Region 2.787e-05 ***
anova(LabileMCGMod3l,LabileMCGMod3I) #Landuse 0.03737 *
anova(LabileMCGMod3m,LabileMCGMod3I) #Treatment 0.5615
anova(LabileMCGMod3n,LabileMCGMod3I) #Temp 0.007109 **
anova(LabileMCGMod3o,LabileMCGMod3I) #Sand 0.0008505 ***

#See here that Treatment is not significant, can try and remove the sign two way and then treatment.
# LabileMCGMod4 <- update(LabileMCGMod3, .~. -Season:Treatment)
# AIC(LabileMCGMod4)# 737.3641
# drop1(LabileMCGMod4,test="Chisq") #
# LabileMCGMod5 <- update(LabileMCGMod4, .~. -Treatment)
# AIC(LabileMCGMod5)# 738.2697 - NOPE NOT BETTER THEN PREVIUOS

#Final Model
LabileMCGModFINAL <- lmer(MainCGdiff ~ Season+Region+Landuse+Treatment+Temp+Sand+
                            Season:Region+Season:Treatment+Season:Sand+Region:Landuse+
                            Region:Temp+Landuse:Temp+Landuse:Sand+Temp:Sand+
                            (1|Site/Blockcode), na.action=na.omit, REML=T, data =LabileDataMCG)
summary(LabileMCGModFINAL)
#Inspect chosen model for homogeneity:
E1 <- resid(LabileMCGModFINAL, type ="pearson")
F1 <- fitted(LabileMCGModFINAL)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
#identify(F1, E1)
hist(E1, nclass = 25) 

#Strange that there are a lot of neg values and that treatment:Season is significant

# Interaction plot
#Why is treatment.season sign for labile litter? #Looking a interacion plot below:
#CG lost more in open than in exclosed treatment during dry season.
#Seronera was the wettest area during dry season. So this makes sense!
#Maybe leaching is greatest in open teabags, which could be the case when there is a lot of rain.
with(LabileDataMCG, {interaction.plot(Season,Treatment,MainCGdiff,
                                         xlab = "Season",
                                         ylab = "Mass loss difference",
                                         fun=mean)})

#emmeans
ref.gridLabileMCGModFINAL <- ref_grid(LabileMCGModFINAL)
ref.gridLabileMCGModFINAL #See how the grid is looking. Check for correct factors and the emmeans of numerical variables. If testing between numerical variables, only the means or the low/high end of values can be specified. I.e contrasts vs trend. See emmeans vignette for more info.
emmip(LabileMCGModFINAL, Treatment~Season|Region, type="response") #Can see here that there is an interaction effect of treatment:season
emmeans.LabileMCGModFINAL <- emmeans(LabileMCGModFINAL, pairwise~Treatment|Season|Region,type="response") #Positive values=More main massloss means less massloss in CG
emm.sum.LabileMCGModFINAL<- summary(emmeansLabileMCGModFINAL)
emm.sum.LabileMCGModFINAL$contrasts #Get contrast between factors (linear). This is somewhat similar to pairs()
emm.sum.LabileMCGModFINAL$emmeans #Get emmeans of factors.

pairs(emmeans(LabileMCGModFINAL, ~Treatment*Season|Region),simple = "each", combine =TRUE) # Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE all contrasts are combined into one family:. The dots (.) in this result correspond to which simple effect is being displayed. 
plot(emmeans.LabileMCGModFINAL, comparisons = TRUE) #Comparisons summarized graphically. The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant

#Conlcusion on treatment:Season: There is an effect of treatment when season is dry. CG has higher massloss in dry season (due to more rain as seen in precip data).



#Recalcitrantmodel analysis####
GlobalRecalMCGMod <- lmer(MainCGdiff ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2
                          +Season:Landuse:Treatment+Season:Landuse:Region+
                            Landuse:Region:Treatment+Season:Region:Treatment+
                            (1|Site/Blockcode), na.action=na.omit, REML=F, data =RecalDataMCG)
summary(GlobalRecalMCGMod)                         
AIC(GlobalRecalMCGMod)# 1034.525
drop1(GlobalRecalMCGMod,test="Chisq")
# Season:Landuse:Treatment  2 1030.7  0.1874 0.9105547  #REMOVE  
# Season:Region:Landuse     2 1038.4  7.8421 0.0198204 *  
# Region:Landuse:Treatment  2 1033.0  2.4495 0.2938287    #REMOVE
# Season:Region:Treatment   2 1031.1  0.5450 0.7614901   #REMOVE
RecalMCGMod1a <- update(GlobalRecalMCGMod, .~. -Season:Landuse:Treatment-
                          Region:Landuse:Treatment-Season:Region:Treatment)
AIC(RecalMCGMod1a)# 1026.423
drop1(RecalMCGMod1a,test="Chisq")
# Season:Treatment       1 1026.5  2.0325 0.153971   #REMOVE
# Season:C.N             1 1024.5  0.0634 0.801255   #REMOVE
# Season:Temp            1 1032.6  8.1816 0.004232 **
#   Season:Sand            1 1025.9  1.5032 0.220174   #REMOVE
# Region:Treatment       2 1027.3  4.8875 0.086834 . 
# Region:C.N             1 1024.5  0.0662 0.796903   #REMOVE
# Region:Temp            1 1024.7  0.2561 0.612809   #REMOVE
# Region:Sand            1 1034.7 10.3016 0.001329 **
#   Landuse:Treatment      2 1025.5  3.1111 0.211071   #REMOVE
# Landuse:C.N            2 1022.5  0.0601 0.970379   #REMOVE
# Landuse:Temp           2 1026.6  4.1896 0.123096   #REMOVE
# Landuse:Sand           2 1027.3  4.9188 0.085488 . 
# Treatment:C.N          1 1024.6  0.1812 0.670302   REMOVE
# Treatment:Temp         1 1024.8  0.3943 0.530047   #REMOVE
# Treatment:Sand         1 1027.9  3.4983 0.061432 . 
# C.N:Temp               1 1024.7  0.3001 0.583813   #REMOVE
# C.N:Sand               1 1025.6  1.1702 0.279363   #REMOVE
# Temp:Sand              1 1028.1  3.6375 0.056493 . 
# Season:Region:Landuse  2 1031.0  8.5478 0.013927 * 
RecalMCGMod2a <- update(RecalMCGMod1a, .~. -Season:Treatment-Season:C.N-Season:Sand-Region:C.N-
                          Region:Temp-Landuse:Treatment-Landuse:C.N-Landuse:Temp-Treatment:Temp-
                          Treatment:C.N-C.N:Temp-C.N:Sand)
AIC(RecalMCGMod2a)# 1012.718
drop1(RecalMCGMod2a,test="Chisq")
# C.N                    1 1011.2  0.4466 0.503963   #REMOVE
# Season:Temp            1 1019.4  8.7207 0.003146 **
#   Region:Treatment       2 1010.6  1.9372 0.379614 #REMOVE  
# Region:Sand            1 1016.5  5.8019 0.016009 * 
#   Landuse:Sand           2 1012.1  3.4278 0.180161   #REMOVE
# Treatment:Sand         1 1012.6  1.9048 0.167537   #REMOVE
# Temp:Sand              1 1015.2  4.4611 0.034674 * 
#   Season:Region:Landuse  2 1022.0 13.3007 0.001294 **
RecalMCGMod3a <- update(RecalMCGMod2a, .~. -C.N-Region:Treatment-Landuse:Sand-
                          Treatment:Sand)
AIC(RecalMCGMod3a)# 1006.938
drop1(RecalMCGMod3a,test="Chisq")
# Treatment              1 1010.7  5.7223 0.016751 * 
#   Season:Temp            1 1012.3  7.3802 0.006595 **
#   Region:Sand            1 1007.9  2.9673 0.084963 . 
# Temp:Sand              1 1007.2  2.2528 0.133372   #REMOVE
# Season:Region:Landuse  2 1015.9 12.9211 0.001564 **
RecalMCGMod4a <- update(RecalMCGMod3a, .~. -Temp:Sand)
AIC(RecalMCGMod4a)# 1007.191 #Worse than previous
drop1(RecalMCGMod4a,test="Chisq")
# Treatment              1 1010.8  5.6267 0.017689 * 
# Season:Temp            1 1010.8  5.5873 0.018091 * 
# Region:Sand            1 1006.0  0.7702 0.380154   #REMOVE
# Season:Region:Landuse  2 1015.9 12.6644 0.001778 **
RecalMCGMod5a <- update(RecalMCGMod4a, .~. -Region:Sand)
AIC(RecalMCGMod5a)# 1005.962 #Better! Whoho!
drop1(RecalMCGMod5a,test="Chisq")
# Treatment              1 1009.6  5.6449 0.017506 * 
#   Sand                   1 1005.8  1.8214 0.177141  #REMOVE 
# Season:Temp            1 1009.7  5.7522 0.016468 * 
#   Season:Region:Landuse  2 1014.6 12.6419 0.001798 **
RecalMCGMod6a <- update(RecalMCGMod5a, .~. -Sand)
AIC(RecalMCGMod6a)# 1005.783
drop1(RecalMCGMod6a,test="Chisq") #ALL SIGN

#Now, want to test better fit if including some of the removed terms:
RecalMCGMod7a <- update(RecalMCGMod6a, .~. +Region:Treatment)
anova(RecalMCGMod7a,RecalMCGMod6a) #Region:Treatment not sign
AIC(RecalMCGMod7a)# Orig.Best: 1005.783. Now: 1008.773 #Not better

RecalMCGMod8a <- update(RecalMCGMod6a, .~. +Sand+Region:Sand)
anova(RecalMCGMod8a,RecalMCGMod6a) #Sand+Region:Sand not sign
AIC(RecalMCGMod8a)# Orig.Best: 1005.783. Now: 1007.191 #Not better

RecalMCGMod9a <- update(RecalMCGMod6a, .~. +Sand+Landuse:Sand)
anova(RecalMCGMod9a,RecalMCGMod6a) #Sand+Landuse:Sand not sign
AIC(RecalMCGMod9a)# Orig.Best: 1005.783. Now: 1009.192 #Not better

RecalMCGMod10a <- update(RecalMCGMod6a, .~. +Sand+Treatment:Sand)
anova(RecalMCGMod10a,RecalMCGMod6a) #Sand+Treatment:Sand not sign
AIC(RecalMCGMod10a)# Orig.Best: 1005.783. Now: 1007.172 #Not better

RecalMCGMod10a <- update(RecalMCGMod6a, .~. +Sand+Treatment:Sand)
anova(RecalMCGMod10a,RecalMCGMod6a) #Sand+Treatment:Sand not sign
AIC(RecalMCGMod10a)# Orig.Best: 1005.783. Now: 1007.172 #Not better

RecalMCGMod11a <- update(RecalMCGMod6a, .~. +Sand+Temp:Sand)
anova(RecalMCGMod11a,RecalMCGMod6a) #Sand+Temp:Sand not sign
AIC(RecalMCGMod11a)# Orig.Best: 1005.783. Now: 1007.906 #Not better

RecalMCGMod12a <- update(RecalMCGMod6a, .~. +Landuse:Treatment)
anova(RecalMCGMod12a,RecalMCGMod6a) #SLanduse:Treatment not sign
AIC(RecalMCGMod12a)# Orig.Best: 1005.783. Now: 1009.32 #Not better

#OK, landing on the best model, and generating p-values:
AIC(RecalMCGMod6a)
RecalMCGMod6aB <- update(RecalMCGMod6a, .~. - Season:Region:Landuse) #Without 3ways
RecalMCGMod6ac <- update(RecalMCGMod6aB, .~. - Region:Landuse)
RecalMCGMod6ad <- update(RecalMCGMod6aB, .~. - Season:Temp)
RecalMCGMod6ae <- update(RecalMCGMod6aB, .~. - Season:Landuse)
RecalMCGMod6af <- update(RecalMCGMod6aB, .~. - Season:Region)
RecalMCGMod6aG <- update(RecalMCGMod6aB, .~. - Region:Landuse- Season:Temp-
                           Season:Landuse-Season:Region) #Without two ways
RecalMCGMod6ah <- update(RecalMCGMod6aG, .~. -Temp)
RecalMCGMod6ai <- update(RecalMCGMod6aG, .~. - Treatment)
RecalMCGMod6aj <- update(RecalMCGMod6aG, .~. - Landuse)
RecalMCGMod6ak <- update(RecalMCGMod6aG, .~. - Region)
RecalMCGMod6al <- update(RecalMCGMod6aG, .~. - Season)

anova(RecalMCGMod6aB,RecalMCGMod6a)# Season:Region:Landuse 0.0007802 *** 
anova(RecalMCGMod6ac,RecalMCGMod6aB)#Region:Landuse 0.9338
anova(RecalMCGMod6ad,RecalMCGMod6aB)# Season:Temp 0.3137
anova(RecalMCGMod6ae,RecalMCGMod6aB)#Season:Landuse 0.274
anova(RecalMCGMod6af,RecalMCGMod6aB)# Season:Region 0.07958 .
anova(RecalMCGMod6ah,RecalMCGMod6aG)# Temp 0.4085
anova(RecalMCGMod6ai,RecalMCGMod6aG)# Treatment 0.03319 *
anova(RecalMCGMod6aj,RecalMCGMod6aG)# Landuse 0.5582
anova(RecalMCGMod6ak,RecalMCGMod6aG)# Region 0.4597
anova(RecalMCGMod6al,RecalMCGMod6aG)# Season 0.00675 **

RecalMCGModFINAL <- lmer(MainCGdiff ~ Season+Region+Landuse+Treatment+C.N+Temp+
                      +Region:Landuse+Season:Temp+Season:Landuse+Season:Region+
                        Season:Region:Landuse+
                        (1|Site/Blockcode), na.action=na.omit, REML=T, data =RecalDataMCG)

#Inspect chosen model for homogeneity:
E1 <- resid(RecalMCGModFINAL, type ="pearson")
F1 <- fitted(RecalMCGModFINAL)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
#identify(F1, E1)
hist(E1, nclass = 25) 


#Predicted and observed values on graph####
#### Sketch fitted values #
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE
LabileMCGModFINAL <- lmer(MainCGdiff ~ Season+Region+Landuse+Treatment+Temp+Sand+
                            Season:Region+#Season:Treatment+
                            Season:Sand+#Region:Landuse+
                            #Region:Temp+
                            Landuse:Temp+Landuse:Sand+
                            Temp:Sand+
                            (1|Site/Blockcode), na.action=na.omit, REML=T, data =LabileDataMCG)

RecalMCGModFINAL <- lmer(MainCGdiff ~ Season+Region+Landuse+Treatment+C.N+Temp+
                           #+Region:Landuse+
                           Season:Temp+
                           Season:Landuse+Season:Region+
                           #Season:Region:Landuse+
                           (1|Site/Blockcode), na.action=na.omit, REML=T, data =RecalDataMCG)

#A:Specify covariate values for predictions
Data2LabileMCG <- expand.grid(Season=levels(RecalMain$Season), #Specify which terms are used in the model. Specify levels for factors and min-max for numeric values. Specify length for each numeric var (how many predictions are created)
                          Region=levels(RecalMain$Region),
                          Landuse=levels(RecalMain$Landuse),
                          Treatment=levels(RecalMain$Treatment),
                          Temp = seq(min(RecalMain$Temp,na.rm=T), max(RecalMain$Temp,na.rm=T), length=12),
                          Sand = seq(min(RecalMain$Sand), max(RecalMain$Sand), length=12))



Data2RecalMCG <- expand.grid(Season=levels(RecalMain$Season), #Specify which terms are used in the model. Specify levels for factors and min-max for numeric values. Specify length for each numeric var (how many predictions are created)
                              Region=levels(RecalMain$Region),
                              Landuse=levels(RecalMain$Landuse),
                              Treatment=levels(RecalMain$Treatment),
                              Temp = seq(min(RecalMain$Temp,na.rm=T), max(RecalMain$Temp,na.rm=T), length=12),
                              C.N = seq(min(RecalMain$C.N), max(RecalMain$C.N), length=12))

#B. Create X matrix with expand.grid
CG_L <- model.matrix(~ Season+Region+Landuse+Treatment+Temp+Sand+
                       Season:Region+#Season:Treatment+
                       Season:Sand+#Region:Landuse+
                       #Region:Temp+
                       Landuse:Temp+Landuse:Sand+
                       Temp:Sand,
                       data = Data2LabileMCG)
head(CG_L)

CG_R <- model.matrix(~ Season+Region+Landuse+Treatment+C.N+Temp+
                       #+Region:Landuse+
                       Season:Temp+
                       Season:Landuse+Season:Region,
                       #Season:Region:Landuse+,
                       data = Data2RecalMCG)
head(CG_R)

#C. Calculate predicted values
Data2LabileMCG$Pred <- CG_L %*% fixef(LabileMCGModFINAL) #Error: non-conformable arguments when having rank deficiency. need to reduce model.
Data2RecalMCG$Pred <- CG_R %*% fixef(RecalMCGModFINAL) ##Error: non-conformable arguments when having rank deficiency. need to reduce model.

#D. Calculate standard errors (SE) for predicted values
Data2LabileMCG$SE <- sqrt(  diag(CG_L %*% vcov(LabileMCGModFINAL) %*% t(CG_L))  )
Data2RecalMCG$SE <- sqrt(  diag(CG_R %*% vcov(RecalMCGModFINAL) %*% t(CG_R))  )

memory.limit() #Need to allocate more memory to R to be able to do this with length of 25. Scaled down to max of 12, since only ~8gb is given.


#And using the Pred and SE values, we can calculate
#a 95% confidence interval
Data2LabileMCG$SeUp <- Data2LabileMCG$Pred + 1.96 * Data2LabileMCG$SE
Data2LabileMCG$SeLo <- Data2LabileMCG$Pred - 1.96 * Data2LabileMCG$SE

Data2RecalMCG$SeUp <- Data2RecalMCG$Pred + 1.96 * Data2RecalMCG$SE
Data2RecalMCG$SeLo <- Data2RecalMCG$Pred - 1.96 * Data2RecalMCG$SE

#E. Plot predicted values
names(Data2LabileMCG) 
names(Data2RecalMCG)
colnames(Data2LabileMCG)[7] <- "MainCGdiff"
colnames(Data2RecalMCG)[7] <- "MainCGdiff"

#Sorting predicted data (Means, SE)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
Data2LabileMCG.sum <-aggregate(MainCGdiff~Season+Region+Landuse, data=Data2LabileMCG, mean)
colnames(Data2LabileMCG.sum)[4] <- "MainCGdiff"
Data2LabileMCG.sum.se<-aggregate(MainCGdiff~Season+Region+Landuse, data=Data2LabileMCG, se)
Data2LabileMCG.sum$SE<-Data2LabileMCG.sum.se$V1

Data2RecalMCG.sum <-aggregate(MainCGdiff~Season+Region+Landuse, data=Data2RecalMCG, mean)
colnames(Data2RecalMCG.sum)[4] <- "MainCGdiff"
Data2RecalMCG.sum.se<-aggregate(MainCGdiff~Season+Region+Landuse, data=Data2RecalMCG, se)
Data2RecalMCG.sum$SE<-Data2RecalMCG.sum.se$V1

#Sorting observed data (Means, SE)
LabileDataMCG.sum <-aggregate(cbind(MainCGdiff,C.N,Temp,Sand)~Season+Region+Landuse, data=LabileDataMCG, mean)
LabileDataMCG.sum.se<-aggregate(MainCGdiff~Season+Region+Landuse, data=LabileDataMCG, se)
LabileDataMCG.sum$SE<-LabileDataMCG.sum.se$MainCGdiff

RecalDataMCG.sum <-aggregate(cbind(MainCGdiff,C.N,Temp,Sand)~Season+Region+Landuse, data=RecalDataMCG, mean)
RecalDataMCG.sum.se<-aggregate(MainCGdiff~Season+Region+Landuse, data=RecalDataMCG, se)
RecalDataMCG.sum$SE<-RecalDataMCG.sum.se$MainCGdiff

#Plot CG ####



#|####
#STUFF REMOVED####
#|####
#Stuff removed from orig script, but kept here in case it's needed:####
#Labile Model - general massloss across season and landuse, Drop1 script#### 
LabileMainMod <- lmer(Massloss.per~Season+Landuse+Region+
                        Treatment+
                        Sand+Temp+C.N+
                        #Season:Landuse+Season:Treatment+Season:Sand+Season:Temp+
                        #Season:C.N+Landuse:Treatment+Landuse:Sand+Landuse:Temp+Landuse:C.N+
                        #Treatment:Sand+Treatment:Temp+Treatment:C.N+
                        #Sand:Temp+Sand:C.N+
                        #Temp:C.N+
                        (1|Site/Blockcode/Plot), na.action=na.omit,REML = F,data=LabileMain)

summary(LabileMainMod)
anova(LabileMainMod)
drop1(LabileMainMod,test="Chisq")
#First step in model selecion, drop1() output:
#
#Model:
#   Massloss.per ~ Season + Landuse + Treatment + Sand + Rain + Temp + 
#   C.N + Season:Landuse + Season:Treatment + Season:Sand + Season:Rain + 
#   Season:Temp + Season:C.N + Landuse:Treatment + Landuse:Sand + 
#   Landuse:Rain + Landuse:Temp + Landuse:C.N + Treatment:Sand + 
#   Treatment:Rain + Treatment:Temp + Treatment:C.N + Sand:Rain + 
#   Sand:Temp + Sand:C.N + Rain:Temp + Rain:C.N + Temp:C.N + 
#   (Site.ID | blockdesign.num)
# Df    AIC    LRT Pr(Chi)  
# <none>               5362.4                 
# Season:Landuse     2 5360.8 2.4228 0.29778  --> Remove
# Season:Treatment   1 5363.1 2.6489 0.10362  
# Season:Sand        1 5366.0 5.5492 0.01849 *
#   Season:Rain        1 5360.5 0.0668 0.79599  
# Season:Temp        1 5360.4 0.0035 0.95297  --> Remove
# Season:C.N         1 5360.8 0.4041 0.52500  --> Remove
# Landuse:Treatment  2 5361.7 3.3007 0.19198  
# Landuse:Sand       2 5361.2 2.7853 0.24841  --> Remove
# Landuse:Rain       2 5363.3 4.8696 0.08761 .
# Landuse:Temp       2 5359.5 1.0505 0.59140  --> Remove
# Landuse:C.N        2 5362.7 4.2601 0.11883  
# Treatment:Sand     1 5361.5 1.0606 0.30308  --> Remove
# Treatment:Rain     1 5360.9 0.5401 0.46237  --> Remove
# Treatment:Temp     1 5360.8 0.3525 0.55273  --> Remove
# Treatment:C.N      1 5361.2 0.8055 0.36944  --> Remove
# Sand:Rain          1 5361.8 1.4382 0.23043  --> Remove
# Sand:Temp          1 5363.8 3.4111 0.06476 .
# Sand:C.N           1 5360.8 0.4367 0.50874  --> Remove
# Rain:Temp          1 5362.7 2.2659 0.13225  
# Rain:C.N           1 5364.1 3.6590 0.05577 .
# Temp:C.N           1 5361.3 0.9381 0.33276  --> Remove
##
# Model:
#   Massloss.per ~ Season + Landuse + Treatment + Sand + Rain + Temp + 
#   C.N + Season:Treatment + Season:Sand + Season:Rain + Landuse:Treatment + 
#   Landuse:Rain + Landuse:C.N + Sand:Temp + Rain:Temp + Rain:C.N + 
#   (Site.ID | blockdesign.num)
# Df    AIC     LRT   Pr(Chi)    
# <none>               5347.1                      
# Season:Treatment   1 5347.4  2.3654 0.1240501 --> Remove   
# Season:Sand        1 5366.0 20.8875 4.871e-06 ***
#   Season:Rain        1 5349.8  4.7048 0.0300789 *  
#   Landuse:Treatment  2 5345.1  2.0755 0.3542505    --> Remove
# Landuse:Rain       2 5358.5 15.4328 0.0004455 ***
#   Landuse:C.N        2 5351.7  8.6280 0.0133800 *  
#   Sand:Temp          1 5346.8  1.7583 0.1848327    --> Remove
# Rain:Temp          1 5349.3  4.2191 0.0399706 *  
#   Rain:C.N           1 5347.6  2.5607 0.1095503 --> Remove
# #
# Model:
#   Massloss.per ~ Season + Landuse + Treatment + Sand + Rain + Temp + 
#   C.N + Season:Sand + Season:Rain + Landuse:Rain + Landuse:C.N + 
#   Rain:Temp + (Site.ID | blockdesign.num)
# Df    AIC     LRT   Pr(Chi)    
# <none>          5345.5                      
# Treatment     1 5344.9  1.4302  0.231735   - 
# Season:Sand   1 5362.6 19.0370 1.282e-05 ***
#   Season:Rain   1 5347.3  3.8024  0.051179 .  ->Remove
# Landuse:Rain  2 5356.2 14.6710  0.000652 ***
#   Landuse:C.N   2 5348.5  6.9822  0.030467 *  
#   Rain:Temp     1 5346.6  3.0917  0.078693 .->Remove  
# #
# Model:
#   Massloss.per ~ Season + Landuse + Treatment + Sand + Rain + Temp + 
#   C.N + Season:Sand + Landuse:Rain + Landuse:C.N +
#   (Site.ID | blockdesign.num)
#             Df    AIC     LRT   Pr(Chi)    
# <none>          5348.7                  --> Worse AIC    
# Treatment     1 5348.1  1.4647 0.2261840    
# Temp          1 5356.7 10.0393 0.0015324 ** 
#   Season:Sand   1 5360.6 13.9392 0.0001888 ***
#   Landuse:Rain  2 5361.1 16.4388 0.0002694 ***
#   Landuse:C.N   2 5351.5  6.8213 0.0330204 *  



#Block needs to be a unique number - not repeated across blocks (I think blockcode already do this but anyway...)
LabileMain$blockdesign.num<-as.factor(with(LabileMain, paste(Site,Block,Landuse, sep="")))
LabileMain$blockdesign.num<-as.numeric(LabileMain$blockdesign.num)
LabileMain$blockdesign.num<-as.factor(LabileMain$blockdesign.num)
table(LabileMain$blockdesign.num, LabileMain$Site)

RecalMain$blockdesign.num<-as.factor(with(RecalMain, paste(Site,Block,Landuse, sep="")))
RecalMain$blockdesign.num<-as.numeric(RecalMain$blockdesign.num)
RecalMain$blockdesign.num<-as.factor(RecalMain$blockdesign.num)
table(RecalMain$blockdesign.num, RecalMain$Site)


#STUFF NOT SORTED####









                  
AndersTea2b <- update(AndersTea2, .~. -flittertype:ftreatment)
AndersTea2c <- update(AndersTea2, .~. -sand.per)
AndersTea2d <- update(AndersTea2a, .~. -fregion)
AndersTea2e <- update(AndersTea2a, .~. -flanduse)
AndersTea2f <- update(AndersTea2b, .~. -flittertype)
AndersTea2g <- update(AndersTea2b, .~. -ftreatment)

# Interaction results - when removed from model
anova(AndersTea2,AndersTea2a) # flanduse:fregion signficiant
anova(AndersTea2,AndersTea2b) # flittertype:ftreatment significant
anova(AndersTea2,AndersTea2c) # Sand signficiant
anova(AndersTea2a,AndersTea2d) # Rainfall region NS
anova(AndersTea2a,AndersTea2e) # Landuse singificant
anova(AndersTea2b,AndersTea2f) # Litter type significant
anova(AndersTea2b,AndersTea2g) # Treatment significant




drop1(Recalmodel,test="Chisq")
# Df    AIC     LRT   Pr(Chi)    
# <none>                      5667.4                      
# Landuse:Season:Treatment  2 5682.5 19.1388 6.983e-05 ***
#   Landuse:Region:Treatment  3 5673.6 12.2436  0.006594 ** 
#   Landuse:Season:Region     2 5667.7  4.2925  0.116923  


#   ---
#install.packages("lmerTest")
#install.packages("emmeans")
#library(lsmeans)
library(emmeans) 
library(lmerTest)


#Working with model with Recalcitrant littertype#
#Testing the threeway interaction Landuse:Season:Treatment

#Getting Estimated Marginal means of these three factors in all combinations
ref_grid(Recalmodel) #@ grid
emm.s.recalmod<- emmeans(Recalmodel,~Landuse:Season:Treatment)
        #Geting message: A nesting structure was detected in the fitted model:
        #Region %in% Landuse, Treatment %in% Landuse.
test(emm.s.recalmod)

#Testing the contrast among the factors by season. 
contrast(emm.s.recalmod,by="Season")
          #THis is testing if the different factors are different from 0 or not...or?
          #in wet season: Exclosed,Agriculture effect:P=0,1325.
          #In dry season: Open-wild effect: P=0,4896 
          #This means that this two are not different from 0...I think.
pairs(emm.s.recalmod,by="Season")
        #THis is testing if one factor is different from an other factor.
        #Testig all combinations within each season, since I've set by="Season"....I think
        #If thats true, then the output says:
            #DRY SEASON:
            #Exclosed,Agriculture and Exclosed,Pasture is not different:P=0.9979
            #Exclosed,Agriculture - Exclosed,Wild       P= 1.0000
            #Exclosed,Agriculture - Open,Wild          P= 0.9165
            #Etc....See output

#Plotting the emmeans for each comparison with its SE
plot(emm.s.recalmod, comparisons = TRUE)






#######################################
##STUART SCRIPT########################
########################################
#CHecking assumptions for the linearity#
########################################
E1 <- resid(Recalmodel, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F1 <- fitted(Recalmodel)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
# Fitted values both above and below 0, and it looks OK spread around 0.

#Checking for autocorrelation
library(itsadug)
plot(acf_resid(Recalmodel), type="b",alpha=0.05)
abline(c(0,0), lty = 2, col = 1)

####Modelling Landuse main experiment####

#### Provisional analysis####

# Rooibos Open only for mound effect
decomp1R<-decomp1[decomp1$littertype=="Rooibos",]
decomp1Ro<-decomp1R[decomp1R$treatment=="Open",]
droplevels(decomp1Ro)
decomp1Ro$treatment
plot(decomp1Ro$massloss.per~decomp1Ro$N.D.Mound_m, col=c(decomp1Ro$landuse))
abline(lm(decomp1Ro$massloss.per~decomp1Ro$N.D.Mound_m)) # Further away from mounds higher mass loss??

# Convert percentage to proportion 0 - 1
decomp1$Mass.loss.beta<-(decomp1$massloss.per/100)
decomp1Ro$Mass.loss.beta<-(decomp1Ro$massloss.per/100)
names(decomp1Ro)
plot(massloss.per~N.D.Mound_m, data=decomp1, col=c(flittertype), pch=c(decomp1$ftreatment))
plot(massloss.per~N.D.Mound_m, data=decomp1Ro, col=c(flittertype), pch=c(decomp1$ftreatment))

# Remove NAs, Common garden # Seronera
decomp1b<-decomp1[!is.na(decomp1$massloss.per),]
decomp1c<-decomp1b[decomp1b$fsite!="Seronera",]
decomp1c<-decomp1b[decomp1b$experiment!="common",]
decomp1d<-decomp1c[decomp1c$fregion!="Local",]
decomp1d<-droplevels(decomp1d)
dim(decomp1d) # 663 53

# Block needs to be a unique number - not repeated 
decomp1d$fblockdesign.num<-as.factor(with(decomp1d, paste(fregion,fsite,flanduse,fblockdesign, sep="")))
decomp1d$fblockdesign.num<-as.numeric(decomp1d$fblockdesign.num)
decomp1d$fblockdesign.num<-as.factor(decomp1d$fblockdesign.num)
table(decomp1d$fregion, decomp1d$flanduse) # OK balance


names(decomp1d)

# Mixed linear model
AndersTea<-lmer(massloss.per~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                  flanduse:fregion+fregion*flittertype*ftreatment+sand.per+flanduse:sand.per+tempC+
               (1|fblockdesign.num/fplot.id), na.action=na.omit, data = decomp1d)
summary(AndersTea)

# Check assumptions
par(mfrow=c(1,1))
plot(fitted(AndersTea),residuals(AndersTea)) # Not good rooibos huge variation and green small
abline(h = 0, lty = 2) 
abline(v = 0, lty = 2)
qqnorm(resid(AndersTea))
qqline(resid(AndersTea)) # Not good - higher loss - break from qqline

# Residuals to each covariate # Readjust with interactions..
plot(AndersTea,flittertype~resid(.),abline=0) # Rooibos larger resid error
plot(AndersTea,ftreatment~resid(.),abline=0) # Open larger residual error
plot(AndersTea,flanduse~resid(.),abline=0) # OK
plot(AndersTea,fregion~resid(.),abline=0) # OK


# Checking resids vs fit
E1 <- resid(AndersTea, type = "pearson")
F1 <- fitted(AndersTea)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)


# Mixed linear model with beta (0 -1)
# Beta model with glmmTMB
# Issues with beta distribution cannot be zero or 1 exactly...
decomp1d$Mass.loss.beta[decomp1d$Mass.loss.beta<0.01]<-0.01
min(decomp1d$Mass.loss.beta)
plot(decomp1d$Mass.loss.beta)

decomp1e<-decomp1d[!is.na(decomp1d$N.D.Mound_m),] # If included in model - NO NAS
dim(decomp1e)
#AndersTeaGMT<-glmmTMB(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
#                        flanduse:fregion+
#                        (1|fblockdesign.num/fplot.id),
#                family=list(family="beta",link="logit"),
#                data = decomp1d)


AndersTea<-glmmadmb(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                      flanduse:ftreatment+flanduse:fregion+sand.per+sand.per:flanduse+tempC+
                      fregion*flittertype*ftreatment+flanduse*flittertype*ftreatment+
                      (1|fblockdesign.num/fplot.id),
                family="beta",data = decomp1d)
                #admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))

# First tried: admb.opts=admbControl(shess=FALSE,noinit=FALSE)
# Second: admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5) # No improvement
# start=list(RE_sd=rep(1,2),RE_cor=rep(0,2))
# If everything fails = xtra.args="-ams 500000000 -simplex" 

summary(AndersTea)

#Deviance
# Opt 1
sum(residuals(AndersTea, type="pearson")^2) / df.residual(AndersTea) #0.7316665 # bad deviance

# Opt 2
#Option 2
#(100*(Null deviance-Residual deviance)/Null deviance)

#                   Null deviance - residual deviance
#Generalized R^2 = ------------------------------------#
#                           Null deviance
sum(resid(AndersTea))
-2*logLik(AndersTea) # 'log Lik.' -238.472 (df=9)
(-759.74--23.87073)/-759.74 # 0.9685804 # Good

# Remove factors - what is important
drop1(AndersTea,test="Chi")

#Mass.loss.beta ~ flittertype + ftreatment + flanduse + fregion + 
#Df     AIC    LRT Pr(>Chi)    
#<none>                                -673.25                    
#tempC                            1 -711.68 0.060   0.8065
#flanduse:fregion                 2 -709.88 3.860   0.1451
#flanduse:sand.per                2 -711.43 2.314   0.3144
#flittertype:ftreatment:fregion   1 -711.74 0.004   0.9496
#flittertype:ftreatment:flanduse  2 -713.07 0.668   0.7161 

# Drop tempC and littertype:treatment:rainfall region

AndersTea2<-glmmadmb(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                       flanduse:ftreatment+flanduse:fregion+sand.per+(1|fblockdesign.num/fplot.id),
                    family="beta",data = decomp1d)
summary(AndersTea2)

# Remove factors - what is important
drop1(AndersTea2,test="Chi")

#Df     AIC    LRT  Pr(>Chi) 
#sand.per                1 -666.53 19.084 1.251e-05 ***
#flittertype:ftreatment  1 -646.42 39.190 3.845e-10 ***
#flanduse:fregion        2 -676.79 10.824  0.004463 ** 

# Test significance of factors
AndersTea2a <- update(AndersTea2, .~. -flanduse:fregion)
AndersTea2b <- update(AndersTea2, .~. -flittertype:ftreatment)
AndersTea2c <- update(AndersTea2, .~. -sand.per)
AndersTea2d <- update(AndersTea2a, .~. -fregion)
AndersTea2e <- update(AndersTea2a, .~. -flanduse)
AndersTea2f <- update(AndersTea2b, .~. -flittertype)
AndersTea2g <- update(AndersTea2b, .~. -ftreatment)

# Interaction results - when removed from model
anova(AndersTea2,AndersTea2a) # flanduse:fregion signficiant
anova(AndersTea2,AndersTea2b) # flittertype:ftreatment significant
anova(AndersTea2,AndersTea2c) # Sand signficiant
anova(AndersTea2a,AndersTea2d) # Rainfall region NS
anova(AndersTea2a,AndersTea2e) # Landuse singificant
anova(AndersTea2b,AndersTea2f) # Litter type significant
anova(AndersTea2b,AndersTea2g) # Treatment significant

# Sand significant
plot(Mass.loss.beta~sand.per, decomp1d)
abline(lm(Mass.loss.beta~sand.per, decomp1d)) # Positive  - seems very marginal 
# Should check if colinear with landuse...


#### Predicted values from final model####
# Final model # Wthout sand for prediction
AndersTea2b<-glmmadmb(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                        flanduse:fregion+(1|fblockdesign.num/fplot.id),
                     family="beta",data = decomp1d)

# Plot predicted vs actual data
MyData5 <- expand.grid(flittertype = levels(decomp1d$flittertype),
                      ftreatment = levels(decomp1d$ftreatment),
                      flanduse = levels(decomp1d$flanduse),
                      fregion = levels(decomp1d$fregion))

#Create X matrix with expand.grid
X5 <- model.matrix(~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+flanduse:fregion, data =MyData5)
head(X5)

#Calculate predicted values from beta distribution
#var[Y_i] = P_i * (1 - Pi) / (1 + phi) # Beta

#Extract parameters and parameter covariance matrix
betas2    <- fixef(AndersTea2b)
Covbetas2 <- vcov(AndersTea2b)

#Calculate the fitted values in the predictor scale
MyData5$eta <- X5 %*% betas2
MyData5$Pi  <- exp(MyData5$eta ) / (1 + exp(MyData5$eta))

#Calculate the SEs on the scale of the predictor function
MyData5$se    <- sqrt(diag(X5 %*% Covbetas2 %*% t(X5)))
MyData5$SeUp  <- exp(MyData5$eta + 1.96 *MyData5$se) / 
  (1 + exp(MyData5$eta  + 1.96 *MyData5$se))
MyData5$SeLo  <- exp(MyData5$eta - 1.96 *MyData5$se) / 
  (1 + exp(MyData5$eta  - 1.96 *MyData5$se))

#E. Combined predicted betas with actual beta data
# Remove predicted stems for all species = ONLY Zea mayas stem
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
CG.x<-aggregate(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion, data=decomp1d, mean)
CG.se<-aggregate(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion, data=decomp1d, se)
MyData6<-cbind(MyData5,CG.x[5],CG.se[5])
colnames(MyData6)[7]<-"SE"
colnames(MyData6)[10]<-"Mass.loss"
colnames(MyData6)[11]<-"se"

# Convert Beta scaling back to percentage  *100
MyData6$Mass.loss<-MyData6$Mass.loss*100 
MyData6$se<-MyData6$se*100 
MyData6$SeUp<-MyData6$SeUp*100
MyData6$SeLo<-MyData6$SeLo*100

# Create second dataset for predictions
colnames(MyData5)[6]<-"Mass.loss"
MyData5$Mass.loss<-MyData5$Mass.loss*100 
MyData5$se<-MyData5$se*100 
MyData5$SeUp<-MyData5$SeUp*100
MyData5$SeLo<-MyData5$SeLo*100


# Fill by termite * landuse = empty = absence filled = prescence 
MyData6$tea.hole<-as.factor(with(MyData6, paste(flittertype, ftreatment, sep="")))
MyData5$tea.hole<-as.factor(with(MyData5, paste(flittertype, ftreatment, sep="")))


levels(MyData6$fregion)<-c("Dry region","Wet region")

# Mass loss by landuse - TBI WET SEASON
cp<- ggplot(data=MyData6, aes(y=Mass.loss,x=flanduse, shape=flanduse,
                              ymin=Mass.loss-se, ymax=Mass.loss+se, col=flittertype,fill=tea.hole))
#cp<- cp+ geom_errorbar(data=MyData5, aes(x=flanduse, ymin=SeLo , ymax=SeUp ),colour="dark grey", width=.1)
#cp<- cp+  geom_point(data = MyData5,aes(x =flanduse, y = Mass.loss),size = 3,
#                     color = "dark grey")
cp<- cp+ geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
cp<- cp+ geom_point(size=5,stroke=1.2,position=position_dodge(width=.35),show.legend=F) # Legend T on individual graph
cp<- cp+ facet_wrap(~fregion, scale ="fixed")
cp<- cp+ scale_color_manual(values=c("green4", "orangered3"))
cp<- cp+ scale_fill_manual(values=c("green4","white","orangered3","white"))
cp<- cp+scale_shape_manual(values=c(21,24,22))
cp<- cp+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
cp <- cp + xlab("Land-use") +  ylab("Mass loss (%)")  
cp <- cp + theme_bw() +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
        ,axis.text.x = element_text(size=12,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,legend.text=element_text(size=12,color="black")
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.ticks.x = element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="black", size = .75)
        ,plot.margin = unit(c(5,12,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1, "lines")
        ,legend.title=element_blank()
        #,legend.background = element_rect(fill = "transparent")
        ,legend.position = c(.99, .95)
        ,legend.spacing.y = unit(-0.5, "mm"))
cp <- cp + annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  5, yend = 18,
           linetype = "dashed", color = "white",size = 1.15) 
#cp <- cp + guides(colour=F, fill=F,shape = guide_legend(override.aes = list(shape=c(21,24,22),
#                                                                            size=3.2,fill="grey30",col="grey30", stroke=1.5)))

#cp2 <- cp +  geom_point(data = MyData6, aes(size=ftreatment, shape = NA), colour = "grey50")
#cp2 <- cp2 + guides(size=guide_legend("Source", override.aes=list(shape=c(21, 1), size=4.5,fill="grey30",col="grey30", stroke=1.5)))
#cp2 
cp

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/AndersMainLanduseWET.png",
       width= 25, height = 12,units ="cm", bg ="transparent",
       dpi = 600, limitsize = TRUE)