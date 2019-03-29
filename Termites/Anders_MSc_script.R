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
library(dplyr)
library(plyr)
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
                                                  #SE[LD == "Recalcitrant Termite"]
                                                  +Massloss.per[LD=="Recalcitrant Termite"])
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

summary(LabileMainModFINAL)

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

install.packages("mgcv")


library(mgcv)
M1 <- gamm(Massloss.per ~ Season+Region+Landuse+C.N+Temp+Sand+Season:Region+Season:Landuse+Season:Temp+Season:Sand+
             Region:Landuse+Region:Temp+Landuse:C.N+Landuse:Temp+
             Season:Region:Landuse+
             (1|Site/Blockcode/Plot),
             random = list(Site/Blockcode/Plot =~ 1), data = LabileMain)

# #DREDGING
# #Need to use na.action=na.fail, therfore removing Na's from data set:
# #Removing rows which consist of NAs in all variable used in model:
# LabileMain_NA_filtered <- LabileMain[complete.cases(LabileMain$Massloss.per,LabileMain$C.N,LabileMain$Sand,LabileMain$Temp),]
# summary(LabileMain_NA_filtered)
# which(is.na(LabileMain_NA_filtered$Massloss.per))
# #Changed data file to NA_filtered in LabileMainMod1 and set na.action=na.fail, and REML=F to use dredging.
# # LabileMainMod1 <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^3-C.N:Temp:Sand+
# #                          (1|Site/Blockcode/Plot), na.action=na.fail,REML = F,data=LabileMain_NA_filtered)
# LabileMainMod1 <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
#                          Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment-
#                          C.N:Temp-C.N:Sand-Temp:Sand+
#                          (1|Site/Blockcode/Plot), na.action=na.fail,REML = F,data=LabileMain_NA_filtered)
# 
# #modsetlmer_LabileMainMod1 <- dredge(LabileMainMod1,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
# model.sel(LabileMainMod1) #Model selection table giving AIC, deltaAIC and weighting
# modavglmer_LabileMainMod1<-model.avg(modsetlmer_LabileMainMod1) #Averages coefficient estimates across multiple models according to the weigthing from above
# importance(modavglmer_LabileMainMod1)#Importance of each variable
# write.table(importance(modavglmer_LabileMainMod1),file="Termites/Importance_LabileMain1.txt")
# summarymodmodavglmer_LabileMain1 <- summary(modavglmer_LabileMainMod1)#Estimated coefficients given weighting
# write.table(summary(modavglmer_LabileMainMod1)$coefmat.subset,file="Termites/SumCoef_LabileMain1.txt")


#Recal Model - general massloss across season and landuse####
#Recal Model 1 - No moisture, no rain.
GlobalRecalMainMod <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
                              Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment-
                              C.N:Temp-C.N:Sand-Temp:Sand+
                             (1|Site/Blockcode/Plot), na.action=na.omit,REML = F,data=RecalMain)

summary(GlobalRecalMainMod)
anova(GlobalRecalMainMod) 
AIC(GlobalRecalMainMod) #6397.122
drop1(GlobalRecalMainMod,test="Chisq")
# Season:Region:Landuse     2 6398.2  5.1029 0.0779695 .  #Remove 
# Season:Region:Treatment   2 6410.7 17.5412 0.0001552 ***
#   Season:Landuse:Treatment  2 6406.5 13.3614 0.0012549 ** 
#   Region:Landuse:Treatment  2 6405.6 12.4441 0.0019851 ** 

RecalMainMod1a <- update(GlobalRecalMainMod, .~. -Season:Region:Landuse)
AIC(RecalMainMod1a) #6398.225
drop1(RecalMainMod1a,test="Chisq") # All sign try to remove the two sign threeway and work from there.
# Season:Region:Treatment   2 6411.5 17.2709 0.0001777 ***
#   Season:Landuse:Treatment  2 6407.6 13.3923 0.0012357 ** 
#   Region:Landuse:Treatment  2 6406.6 12.3444 0.0020866 ** 
RecalMainMod1 <- update(RecalMainMod1a, .~. -Season:Region:Treatment-Season:Landuse:Treatment-Region:Landuse:Treatment)
AIC(RecalMainMod1) #6421.805
drop1(RecalMainMod1,test="Chisq")
# Season:Region      2 6439.4 21.5744 2.066e-05 ***
#   Season:Landuse     2 6420.8  2.9508  0.228688    #NS, BUT HAVE A THREEWAY WITH THESE, SO CANT REMOVE.
# Season:Treatment   1 6426.4  6.6190  0.010090 *  
#   Season:C.N         1 6423.8  4.0377  0.044495 *  
#   Season:Temp        1 6419.9  0.1049  0.746021  #REMOVE  
# Season:Sand        1 6425.5  5.6599  0.017357 *  
#   Region:Landuse     2 6428.8 10.9533  0.004183 ** 
#   Region:Treatment   2 6421.9  4.0731  0.130478    
# Region:C.N         2 6418.6  0.8324  0.659561    #REMOVE
# Region:Temp        2 6418.5  0.7154  0.699276    #REMOVE
# Region:Sand        2 6418.1  0.2803  0.869231    #REMOVE
# Landuse:Treatment  2 6418.1  0.2747  0.871676    
# Landuse:C.N        2 6424.3  6.4849  0.039069 *  
#   Landuse:Temp       2 6425.5  7.7407  0.020851 *  
#   Landuse:Sand       2 6419.6  1.8000  0.406576    #REMOVE
# Treatment:C.N      1 6423.7  3.8818  0.048811 *  
#   Treatment:Temp     1 6420.7  0.9428  0.331550    #REMOVE
# Treatment:Sand     1 6423.7  3.9104  0.047987 * 

RecalMainMod2 <- update(RecalMainMod1, .~. -Season:Temp-Region:C.N-Region:Temp-Region:Sand-Landuse:Sand-Treatment:Temp)
AIC(RecalMainMod2) #6406.593
RecalMainMod2a <- update(RecalMainMod2, .~. +Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment)
AIC(RecalMainMod2a) #6382.536
drop1(RecalMainMod2,test="Chisq") #Treatment:C:N now sign without threeways
# Season:Region      2 6442.4 39.835 2.238e-09 ***
#   Season:Landuse     2 6406.5  3.948  0.138877   #REMOVE?
# Season:Treatment   1 6410.2  5.643  0.017521 *  
#   Season:C.N         1 6408.7  4.063  0.043839 *  
#   Season:Sand        1 6409.6  4.990  0.025493 *  
#   Region:Landuse     2 6415.9 13.350  0.001262 ** 
#   Region:Treatment   2 6406.0  3.455  0.177717   #REMOVE? 
# Landuse:Treatment  2 6402.9  0.262  0.877150    #REMOVE?
# Landuse:C.N        2 6408.6  6.056  0.048410 *  
#   Landuse:Temp       2 6409.1  6.513  0.038515 *  
#   Treatment:C.N      1 6409.1  4.510  0.033695 *  
#   Treatment:Sand     1 6407.6  2.969  0.084888 .  
drop1(RecalMainMod2a,test="Chisq") #Treatment:C.N could be removed, when threeways included, but first trying to remove, one fo the two-way components in the threeways (so removing twoways and threeways)
# Season:C.N                1 6384.8  4.2206 0.0399353 *  
#   Season:Sand               1 6385.8  5.2811 0.0215587 *  
#   Landuse:C.N               2 6384.9  6.4062 0.0406360 *  
#   Landuse:Temp              2 6384.9  6.3659 0.0414636 *  
#   Treatment:C.N             1 6380.5  0.0000 0.9946324    
# Treatment:Sand            1 6391.5 10.9537 0.0009342 ***
#   Season:Region:Treatment   2 6395.2 16.7024 0.0002361 ***
#   Season:Landuse:Treatment  2 6392.2 13.6436 0.0010897 ** 
#   Region:Landuse:Treatment  2 6390.4 11.8166 0.0027168 ** 

#OOPS!! CAN I REMOVE THESE TERMS LIKE I DO BELOW? REMOVING 2 threeways, but keep the last 3-way even if I'm remove 2-way terms which includes a term in the kept 3-way?
RecalMainMod3a <- update(RecalMainMod2a, .~. -Season:Landuse-Landuse:Treatment-Season:Landuse:Treatment-Region:Landuse:Treatment)
AIC(RecalMainMod3a)
drop1(RecalMainMod3a,test="Chisq") #Removing two N:S two-ways, and hence coupled threeways


#DREDGING
#Need to use na.action=na.fail, therfore removing Na's from data set:
#Removing rows which consist of NAs in all variable used in model:
RecalMain_NA_filtered <- RecalMain[complete.cases(RecalMain$Massloss.per,RecalMain$C.N,RecalMain$Sand,RecalMain$Temp),]
summary(RecalMain_NA_filtered)
which(is.na(RecalMain_NA_filtered$Massloss.per))
#Changed data file to NA_filtered in RecalMainMod1 and set na.action=na.fail, and REML=F to use dredging.
# RecalMainMod1 <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
#               Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment+ 
#                         (1|Site/Blockcode/Plot), na.action=na.fail,REML = F,data=RecalMain_NA_filtered)

RecalMainMod1 <- lmer(Massloss.per ~ (Season+Region+Landuse+Treatment+C.N+Temp+Sand)^2+
                        Season:Region:Landuse+Season:Region:Treatment+Season:Landuse:Treatment+Region:Landuse:Treatment-
                        C.N:Temp-C.N:Sand-Temp:Sand+
                        (1|Site/Blockcode/Plot), na.action=na.fail,REML = F,data=RecalMain_NA_filtered)


modsetlmer_RecalMainMod1 <- dredge(RecalMainMod1,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(RecalMainMod1) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_RecalMainMod1<-model.avg(modsetlmer_RecalMainMod1) #Averages coefficient estimates across multiple models according to the weigthing from above
importance(modavglmer_RecalMainMod1)#Importance of each variable
write.table(importance(modavglmer_RecalMainMod1),file="Termites/Importance_RecalMain1.txt")
summarymodmodavglmer_RecalMain1 <- summary(modavglmer_RecalMainMod1)#Estimated coefficients given weighting
write.table(summary(modavglmer_RecalMainMod1)$coefmat.subset,file="Termites/SumCoef_RecalMain1.txt")

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
#|####
#COMMON GARDEN EXP####
#Dataprocessing - getting data ready for modelling####
DataCG<-droplevels(Fulldata[Fulldata$Experiment=="CG",]) # Only commongarden data
DataMain<-droplevels(Fulldata[Fulldata$Experiment=="Main",]) #Only landuse experiement data
LocalCGsoil <- DataCG[DataCG$Site=="Seronera",]

#Add local soil from CG to Main experiment:
DataMain <- rbind.fill(DataMain,LocalCGsoil)

write.csv(write.csv(DataMain,file="Termites/Maindata.csv"))
write.csv(write.csv(DataCG,file="Termites/CGdata.csv"))

#Then, want to combine the two data set by left_join.
#Problem: CG data has 224 obs (on block level), whilce main exp. has 1568 obs (on plot level).
#Need to aggregate Main to block level, first:
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error

DataMainSummary<-aggregate(cbind(Massloss.per,Massloss..g.,Moisture..,Temperature..C.,Rain.sum, C.N, Sandcorr, Claycorr)~Season+Landuse+Region+Blockcode+Treatment+Littertype,DataMain,mean)
#DataMainSummaryse <- aggregate(cbind(Massloss.per,Massloss..g.,Moisture..,Temperature..C.,Rain.sum, C.N, Sandcorr, Claycorr)~Season+Landuse+Region+Blockcode+Treatment+Littertype,DataMain, se)
#DataMainSummary$SE <- DataMainSummaryse$
length(DataMainSummary$Massloss.per) #224

Regionlvl <-  levels(DataCG$Region) #Adjusting correct factor level order
levels(DataMain$Region) <- Regionlvl #Adjusting correct level order
Landuselvl <- levels(DataMain$Landuse) #Adjusting correct level order
levels(DataCG$Landuse) <- Landuselvl #Adjusting correct level order

names(DataMainSummary)
names(DataCG)
Blockcodelvl <- levels(DataCG$Blockcode)
levels(DataMainSummary$Blockcode) <- Blockcodelvl

DataMCG <- left_join(DataMainSummary,DataCG,by=c("Season","Treatment","Littertype","Blockcode"))

#Creating variables for difference between CG (y) and Main (x):
DataMCG$MainCGdiff <- DataMCG$Massloss.per.x-DataMCG$Massloss.per.y
DataMCG$Moistdiff <- DataMCG$Moisture...x -DataMCG$Moisture...y
DataMCG$Tempdiff <- DataMCG$Temperature..C..x -DataMCG$Temperature..C..y
#Renaming some columns:
names(DataMCG)
colnames(DataMCG)[(names(DataMCG) == "C.N.x")] <-"CN"
colnames(DataMCG)[(names(DataMCG)== "Sandcorr.x")] <- "Sand"
colnames(DataMCG)[(names(DataMCG)== "Claycorr.x")] <- "Clay"
#colnames(DataMCG)[(names(DataMCG)== "Moisture...x")] <- "Moisture"
#colnames(DataMCG)[(names(DataMCG)== "Temperature..C..x")] <- "Temp"
colnames(DataMCG)[(names(DataMCG)== "Landuse.x")] <- "Landuse"
colnames(DataMCG)[(names(DataMCG)== "Region.x")] <- "Region"
colnames(DataMCG)[(names(DataMCG)== "Rain.sum.x")] <- "Rain"

#Creating dataset for each littertype:
RecalDataMCG <- droplevels(DataMCG[DataMCG$Littertype =="Rooibos",])
LabileDataMCG <- droplevels(DataMCG[DataMCG$Littertype =="Green",])

#Block needs to be a unique number - not repeated 
LabileDataMCG$blockdesign.num<-as.factor(with(LabileDataMCG, paste(Season,Blockcode,Treatment, sep="")))
LabileDataMCG$blockdesign.num<-as.numeric(LabileDataMCG$blockdesign.num)
LabileDataMCG$blockdesign.num<-as.factor(LabileDataMCG$blockdesign.num)
table(LabileDataMCG$Site.ID, LabileDataMCG$Landuse)

RecalDataMCG$blockdesign.num<-as.factor(with(RecalDataMCG, paste(Season,Treatment,Blockcode, sep="")))
RecalDataMCG$blockdesign.num<-as.numeric(RecalDataMCG$blockdesign.num)
RecalDataMCG$blockdesign.num<-as.factor(RecalDataMCG$blockdesign.num)
table(RecalDataMCG$Site.ID, RecalDataMCG$Landuse)
#COMMON GARDEN MODELLING####
#Labilemodel analysis####
names(LabileDataMCG)
levels(LabileDataMCG$Site.ID)
#Using Tempdiff instead of Temp variable
#Using Sand and not Clay
#First, writing up all combinations, then excluding first the ones that does not make sense to include (in interactions)
LabileFullMCGMod <- lmer(MainCGdiff ~ Season+Region+Landuse+Treatment+Tempdiff+Sand+CN+Rain+
                             #Season:Landuse+Season:Treatment+Season:Tempdiff+Season:Sand+Season:CN+Season:Rain+
                             #Treatment:Landuse+Treatment:Tempdiff+Treatment:Sand+Treatment:CN+
                             #Landuse:Treatment+Landuse:Tempdiff+Landuse:Sand+Landuse:CN+#Landuse:Rain+
                             #Tempdiff:Sand+Tempdiff:CN+Tempdiff:Rain+
                             #Sand:CN+#Sand:Rain+
                             #Season:Landuse:Treatment+Season:Landuse:Tempdiff+Season:Landuse:CN+#Season:Landuse:Sand+#Season:Landuse:Rain+
                             #Landuse:Treatment:Tempdiff+Landuse:Treatment:Sand+Landuse:Treatment:CN+#Landuse:Treatment:Rain+
                             #Treatment:Tempdiff:Sand+#Treatment:Tempdiff:CN+Treatment:Tempdiff:Rain+
                             #Tempdiff:Sand:CN+Tempdiff:Sand:Rain+
                             #Sand:CN:Rain+
                             (1|Site), na.action=na.omit, REML=F, data =LabileDataMCG)

LabileMCGMod <- lmer(MainCGdiff ~ Season+Landuse+Treatment+Tempdiff+Sand+CN+Rain+
                         Season:Landuse+Season:Treatment+Season:Tempdiff+
                       Season:Sand+Season:CN+Season:Rain+
                         Treatment:Landuse+Treatment:Tempdiff+Treatment:Sand+
                       Treatment:CN+
                         Landuse:Treatment+Landuse:Tempdiff+
                       Landuse:Sand+Landuse:CN+Landuse:Rain+
                         Tempdiff:Sand+Tempdiff:CN+Tempdiff:Rain+
                         Sand:CN+Sand:Rain+
                         #Season:Landuse:Treatment+
                   Season:Landuse:Tempdiff+
                     #Season:Landuse:CN+#
                     Season:Landuse:Sand+
                     Season:Landuse:Rain+
                         #Landuse:Treatment:Tempdiff+
                    #Landuse:Treatment:Sand+Landuse:Treatment:CN+Landuse:Treatment:Rain+
                        # Treatment:Tempdiff:Sand+Treatment:Tempdiff:CN+Treatment:Tempdiff:Rain+
                         #Tempdiff:Sand:CN+
                     Tempdiff:Sand:Rain+
                         Sand:CN:Rain+
                         (1|Site), na.action=na.omit, REML=F, data =LabileDataMCG)


summary(LabileMCGMod)
anova(LabileMCGMod) 
AIC(LabileMCGMod)# Orig (All possble interactions): 790.2116, new:767.9645 
drop1(LabileMCGMod,test="Chisq")
#How the selection scenario was done:
#First of all: I get warning of Singular fit and consider rescaling. But keep going anyway:
#Step 1 - Running full model and Drop1:
#Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
#   CN + Rain + Season:Landuse + Season:Treatment + Season:Tempdiff + 
#   Season:Sand + Season:CN + Season:Rain + Treatment:Landuse + 
#   Treatment:Tempdiff + Treatment:Sand + Treatment:CN + Landuse:Treatment + 
#   Landuse:Tempdiff + Landuse:Sand + Landuse:CN + Landuse:Rain + 
#   Tempdiff:Sand + Tempdiff:CN + Tempdiff:Rain + Sand:CN + Sand:Rain + 
#   Season:Landuse:Treatment + Season:Landuse:Tempdiff + Season:Landuse:CN + 
#   Season:Landuse:Sand + Season:Landuse:Rain + Landuse:Treatment:Tempdiff + 
#   Landuse:Treatment:Sand + Landuse:Treatment:CN + Landuse:Treatment:Rain + 
#   Treatment:Tempdiff:Sand + Treatment:Tempdiff:CN + Treatment:Tempdiff:Rain + 
#   Tempdiff:Sand:CN + Tempdiff:Sand:Rain + Sand:CN:Rain + (1 | 
#                                                             Site.ID)
# Df    AIC     LRT   Pr(Chi)    
# <none>                        790.21                      
# Season:Landuse:Treatment    2 786.28  0.0645 0.9682651 -->Remove
# Season:Landuse:Tempdiff     2 802.22 16.0127 0.0003333 ***
#   Season:Landuse:CN           2 787.67  1.4630 0.4811823  -->Remove  
# Season:Landuse:Sand         2 793.00  6.7928 0.0334944 *  
#   Season:Landuse:Rain         2 798.37 12.1622 0.0022856 ** 
#   Landuse:Treatment:Tempdiff  2 788.63  2.4209 0.2980687  -->Remove  
# Landuse:Treatment:Sand      2 786.38  0.1703 0.9183592  -->Remove  
# Landuse:Treatment:CN        2 786.51  0.3000 0.8607008  -->Remove  
# Landuse:Treatment:Rain      3 785.92  1.7127 0.6341079  -->Remove  
# Treatment:Tempdiff:Sand     1 788.51  0.2995 0.5841925  -->Remove  
# Treatment:Tempdiff:CN       1 790.61  2.3953 0.1217034  -->Remove  
# Treatment:Tempdiff:Rain     1 788.21  0.0009 0.9758912  -->Remove  
# Tempdiff:Sand:CN            1 788.23  0.0176 0.8945908  -->Remove  
# Tempdiff:Sand:Rain          1 802.54 14.3329 0.0001532 ***
#   Sand:CN:Rain                1 800.72 12.5107 0.0004046 ***

#Step 2: Remove N.S and run drop1 again:




#2. All threeways N.S:
#                         Df    AIC    LRT  Pr(Chi)
# Season:Landuse:Tempdiff  3 809.30 4.5446 0.208348   -->Remove
# Season:Landuse:CN        3 809.85 5.0996 0.164650   
# Landuse:Treatment:Sand   3 809.03 4.2825 0.232533  -->Remove
#
#3. Last threeway somewhat sign. AIC:
#                     Df    AIC    LRT  Pr(Chi)
# Season:Treatment    1 807.32 2.1224 0.14516  
# Season:Tempdiff     1 805.68 0.4774 0.48961  
# Season:Sand         1 808.28 3.0769 0.07941 .
# Season:Rain         1 805.22 0.0224 0.88093  
# Landuse:Treatment   3 802.47 1.2748 0.73513  
# Treatment:Tempdiff  1 807.44 2.2450 0.13405  
# Treatment:Sand      1 805.28 0.0827 0.77362  
# Treatment:CN        1 808.38 3.1783 0.07462 .
# Landuse:Tempdiff    3 808.52 7.3186 0.06241 .
# Landuse:Sand        3 804.34 3.1412 0.37036  
# Tempdiff:Sand       1 806.62 1.4206 0.23331  
# Tempdiff:CN         1 807.96 2.7600 0.09665 .
# Tempdiff:Rain       1 806.36 1.1608 0.28129  
# Sand:CN             1 805.31 0.1124 0.73739  
# Season:Landuse:CN   3 807.71 6.5068 0.08940 .
#--> Removing threeway to test twoway interactions
# 
#4. AIC:
#                     Df    AIC    LRT  Pr(Chi)
# Season:Landuse      3 814.46 12.7511 0.005207 **
# Season:Treatment    1 807.75  2.0399 0.153222   
# Season:Tempdiff     1 805.78  0.0776 0.780518   
# Season:Sand         1 814.63  8.9188 0.002823 **
# Season:CN           1 805.75  0.0388 0.843853    --> Remove
# Season:Rain         1 805.71  0.0010 0.974976    --> Remove
# Landuse:Treatment   3 802.93  1.2209 0.747984    --> Remove
# Treatment:Tempdiff  1 807.86  2.1579 0.141841    --> Remove
# Treatment:Sand      1 805.79  0.0789 0.778796    --> Remove
# Treatment:CN        1 808.77  3.0613 0.080178 . 
# Landuse:Tempdiff    3 804.60  2.8945 0.408184   --> Remove 
# Landuse:Sand        3 809.30  7.5947 0.055175 . 
# Landuse:CN          3 805.40  3.6955 0.296279  --> Remove  
# Tempdiff:Sand       1 806.46  0.7566 0.384405  --> Remove  
# Tempdiff:CN         1 807.25  1.5390 0.214761  --> Remove  
# Tempdiff:Rain       1 805.71  0.0041 0.948778 --> Remove   
# Sand:CN             1 805.73  0.0188 0.890984 --> Remove
# 
#5.Removed N.S two-ways, better AIC:790.8469. When I added three-way AIC: 800 and threeway was N.S. So removed:
#                     Df    AIC    LRT  Pr(Chi)
#Season:Landuse    3 796.76 11.9174 0.007671 **
# Season:Treatment  1 791.05  2.1989 0.138109   
# Season:Tempdiff   1 789.89  1.0480 0.305968   --> Remove
# Season:Sand       1 797.46  8.6130 0.003338 **
# Season:CN         1 789.09  0.2438 0.621440   --> Remove
# Season:Rain       1 789.70  0.8570 0.354567   --> Remove
# Treatment:CN      1 790.78  1.9281 0.164966   
# Landuse:Sand      3 795.73 10.8820 0.012381 * 
#--> Removing the most N.S, above P=20.
#
#6.AIC: 786.4553
#                   Df    AIC    LRT  Pr(Chi)
# Tempdiff          1 788.05   3.592 0.0580646 .  
# Rain              1 909.37 124.912 < 2.2e-16 ***
# Season:Landuse    3 797.51  17.057 0.0006880 ***
# Season:Treatment  1 786.65   2.194 0.1385375    --> Remove
# Season:Sand       1 799.57  15.111 0.0001014 ***
# Treatment:CN      1 786.38   1.924 0.1654536    --> Remove
# Landuse:Sand      3 791.79  11.338 0.0100296 *  

# 7. AIC: 787.1119
#                Df    AIC     LRT   Pr(Chi)    
# <none>            787.11                      
# Treatment       1 786.65   1.542  0.214331    --> Remove
# Tempdiff        1 788.64   3.532  0.060186 .  
# CN              1 786.18   1.066  0.301758    --> Remove
# Rain            1 907.02 121.907 < 2.2e-16 ***
# Season:Landuse  3 797.59  16.475  0.000906 ***
# Season:Sand     1 799.56  14.449  0.000144 ***
# Landuse:Sand    3 791.62  10.506  0.014719 * 

#6. Significant twoways, and some N.S covariates.
#Trying update() to see significance of these when a twoway are removed

LabileMCGMod1 <- update(LabileMCGMod, .~. -Season:Landuse)
LabileMCGMod2 <- update(LabileMCGMod, .~. -Season:Sand)
LabileMCGMod3 <- update(LabileMCGMod, .~. -Landuse:Sand) 

LabileMCGMod1a <- update(LabileMCGMod1,.~.-Treatment)
LabileMCGMod1b <- update(LabileMCGMod1,.~.-Tempdiff)
LabileMCGMod1c <- update(LabileMCGMod1,.~.-CN)
LabileMCGMod1d <- update(LabileMCGMod1,.~.-Rain)

LabileMCGMod2a <- update(LabileMCGMod2,.~.-Treatment)
LabileMCGMod2b <- update(LabileMCGMod2,.~.-Tempdiff)
LabileMCGMod2c <- update(LabileMCGMod2,.~.-CN)
LabileMCGMod2d <- update(LabileMCGMod2,.~.-Rain)

LabileMCGMod3a <- update(LabileMCGMod3,.~.-Treatment)
LabileMCGMod3b <- update(LabileMCGMod3,.~.-Tempdiff)
LabileMCGMod3c <- update(LabileMCGMod3,.~.-CN)
LabileMCGMod3d <- update(LabileMCGMod3,.~.-Rain)

#Significance of covariates when Season:Landuse removed:
anova(LabileMCGMod1,LabileMCGMod1a) #Treatment N.S --> Remove
anova(LabileMCGMod1,LabileMCGMod1b) #Tempdiff N.S 
anova(LabileMCGMod1,LabileMCGMod1c) #CN N.S --> Remove
anova(LabileMCGMod1,LabileMCGMod1d) #Rain Sign
#Significance of covariates when Season:Sand removed:
anova(LabileMCGMod2,LabileMCGMod2a) #Treatment  N.S --> Remove
anova(LabileMCGMod2,LabileMCGMod2b) #Tempdiff N.S 
anova(LabileMCGMod2,LabileMCGMod2c) #CN N.S --> Remove
anova(LabileMCGMod2,LabileMCGMod2d) #Rain Sign
#Significance of covariates when Landuse:Sand removed:
anova(LabileMCGMod3,LabileMCGMod3a) #Treatment N.S --> Remove
anova(LabileMCGMod3,LabileMCGMod3b) #Tempdiff Sign #SHould I remove the twoway or does this mean that the twoway has an interaction on this, so keep the two way?
anova(LabileMCGMod3,LabileMCGMod3c) #CN N.S --> Remove
anova(LabileMCGMod3,LabileMCGMod3d) #Rain Sign 


#New alternative selected models:
Alt1LabileMCGMod <- lmer(MainCGdiff ~ Season+Landuse+#Treatment
                           #Tempdiff
                           Sand+#CN+
                           Rain+
                       Season:Landuse+Season:Sand+Landuse:Sand+
                       (1|Site.ID), na.action=na.omit, REML=F, data =LabileDataMCG)

summary(Alt1LabileMCGMod)
anova(Alt1LabileMCGMod) 
AIC(Alt1LabileMCGMod)# Orig: 785.6975, new: 786.957, 
drop1(Alt1LabileMCGMod,test="Chisq")
#               Df    AIC     LRT   Pr(Chi)    
# <none>            785.70                      
# Tempdiff        1 786.96   3.260 0.0710100 .  --> Remove/Or change Temdiff to Temp variable
# Rain            1 903.60 119.906 < 2.2e-16 ***
# Season:Landuse  3 796.45  16.748 0.0007962 ***
# Season:Sand     1 797.64  13.943 0.0001885 ***
# Landuse:Sand    3 788.96   9.265 0.0259699 * 
#
#Using Temp variable instead, not better model:
#                   Df    AIC     LRT   Pr(Chi)    
# <none>               786.39                      
# Temperature..C..x  1 786.96   2.570 0.1089035    
# Rain               1 900.52 116.130 < 2.2e-16 ***
# Season:Landuse     3 796.48  16.093 0.0010851 ** 
# Season:Sand        1 798.76  14.369 0.0001502 ***
# Landuse:Sand       3 792.72  12.337 0.0063150 ** 
#---> Swithcing back to Tempdiff variable, but trying now to remove it.
#
#New model without Tempdiff. AIC: 786.957
# #Model:
# MainCGdiff ~ Season + Landuse + Sand + Rain + Season:Landuse + 
# Season:Sand + Landuse:Sand + (1 | Site.ID)
# Df    AIC     LRT   Pr(Chi)    
# <none>            786.96                      
# Rain            1 907.62 122.661 < 2.2e-16 ***
# Season:Landuse  3 794.48  13.524 0.0036298 ** 
# Season:Sand     1 797.73  12.773 0.0003516 ***
# Landuse:Sand    3 793.06  12.100 0.0070499 ** 
#
#Adding CN back into model, as I think it should be important:
#AIC=788.1711
# Model:
# MainCGdiff ~ Season + Landuse + Sand + CN + Rain + Season:Landuse + 
# Season:Sand + Landuse:Sand + (1 | Site.ID)
#                 Df    AIC     LRT   Pr(Chi)    
# <none>            788.17                      
# CN              1 786.96   0.786 0.3753433    --> Still not sign, Remove
# Rain            1 909.60 123.430 < 2.2e-16 ***
# Season:Landuse  3 795.01  12.834 0.0050098 ** 
# Season:Sand     1 799.17  12.996 0.0003122 ***
# Landuse:Sand    3 795.05  12.881 0.0049013 ** 
#
#Doing update again, to get p-vaues for all covariates. Problem: unsure how to get corect p-values
#when multiple interactions are signigifant.


#Checking distrubution of the modelled data. Problem: I dont know how to interpret this:
E1 <- resid(Alt1LabileMCGMod, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F1 <- fitted(Alt1LabileMCGMod)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # Several of the fitted values <0 #PROBLEM???
abline(h = 0, lty = 2, col = 1)

#Getting parameter estimates directly:
Alt1LabileMCGModA <- lmer(MainCGdiff ~ -1+Season + Landuse + Sand + Rain + Season:Landuse + 
                           Season:Sand + Landuse:Sand + 
                           (1| Site.ID),REML=T,data=LabileDataMCG)

summary(Alt1LabileMCGMod)

#Should also try and add back some variables to test.
####

#Recalcitrantmodel analysis####
names(LabileDataMCG)
levels(LabileDataMCG$Site.ID)
#Using Tempdiff instead of Temp variable
#Using Sand and not Clay
#First, writing up all combinations, then excluding first the ones that does not make sense to include (in interactions)
RecalFullMCGMod <- lmer(MainCGdiff ~ Season+Landuse+Treatment+Tempdiff+Sand+CN+Rain+
                           Season:Landuse+Season:Treatment+Season:Tempdiff+Season:Sand+Season:CN+#Season:Rain+
                           Treatment:Landuse+Treatment:Tempdiff+Treatment:Sand+Treatment:CN+
                          Landuse:Treatment+Landuse:Tempdiff+Landuse:Sand+Landuse:CN+#Landuse:Rain+
                          Tempdiff:Sand+Tempdiff:CN+Tempdiff:Rain+
                          Sand:CN+#Sand:Rain+
                          Season:Landuse:Treatment+Season:Landuse:Tempdiff+Season:Landuse:CN+
                          #Season:Landuse:Sand+#Season:Landuse:Rain+
                          Landuse:Treatment:Tempdiff+Landuse:Treatment:Sand+Landuse:Treatment:CN+
                          #Landuse:Treatment:Rain+
                          #Treatment:Tempdiff:Sand+#Treatment:Tempdiff:CN+Treatment:Tempdiff:Rain+
                          #Tempdiff:Sand:CN+Tempdiff:Sand:Rain+
                          #Sand:CN:Rain+
                           (1|Site.ID), na.action=na.omit, REML=F, data =RecalDataMCG)
RecalMCGMod <- lmer(MainCGdiff ~ #Season+#Landuse+
                      Treatment+#Tempdiff+
                      Sand+#CN+
                      Rain+
                          #Season:Landuse+#Season:Treatment+#Season:Tempdiff+
                      #Season:Sand+#Season:CN+#Season:Rain+
                          #Treatment:Landuse+#Treatment:Tempdiff+
                      #Treatment:Sand+
                     # Treatment:CN+
                          #Landuse:Treatment+#Landuse:Tempdiff+#Landuse:Sand+
                     # Landuse:CN+#Landuse:Rain+
                          #Tempdiff:Sand+Tempdiff:CN+Tempdiff:Rain+
                          #Sand:CN+#Sand:Rain+
                          #Season:Landuse:Treatment+Season:Landuse:Tempdiff+
                          #Season:Landuse:CN+
                          #Season:Landuse:Sand+#Season:Landuse:Rain+
                          #Landuse:Treatment:Tempdiff+Landuse:Treatment:Sand+Landuse:Treatment:CN+
                          #Landuse:Treatment:Rain+
                          #Treatment:Tempdiff:Sand+#Treatment:Tempdiff:CN+Treatment:Tempdiff:Rain+
                          #Tempdiff:Sand:CN+Tempdiff:Sand:Rain+
                          #Sand:CN:Rain+
                          (1|Site.ID), na.action=na.omit, REML=F, data =RecalDataMCG)


summary(RecalMCGMod) #Oops singular fit, not good. Need to reduce or adjust the model until singular fit is removed
AIC(RecalMCGMod)# Orig: 1042 New: 1021.285
drop1(RecalMCGMod,test="Chisq")
# Model:
#   MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
#   CN + Rain + Season:Landuse + Season:Treatment + Season:Tempdiff + 
#   Season:Sand + Season:CN + Treatment:Landuse + Treatment:Tempdiff + 
#   Treatment:Sand + Treatment:CN + Landuse:Treatment + Landuse:Tempdiff + 
#   Landuse:Sand + Landuse:CN + Tempdiff:Sand + Tempdiff:CN + 
#   Tempdiff:Rain + Sand:CN + Season:Landuse:Treatment + Season:Landuse:Tempdiff + 
#   Season:Landuse:CN + Landuse:Treatment:Tempdiff + Landuse:Treatment:Sand + 
#   Landuse:Treatment:CN + (1 | Site.ID)
#                             Df    AIC    LRT Pr(Chi)  
# <none>                        1043.0                 
# Season:Sand                 1 1047.5 6.5855 0.01028 *
#   Tempdiff:Sand               1 1041.5 0.5128 0.47394  
# Tempdiff:CN                 1 1042.2 1.2845 0.25707  
# Tempdiff:Rain               1 1042.4 1.4482 0.22881  
# Sand:CN                     1 1041.2 0.2200 0.63903  
# Season:Landuse:Treatment    2 1042.0 2.9902 0.22422  --> Remove
# Season:Landuse:Tempdiff     2 1042.1 3.1385 0.20820  --> Remove
# Season:Landuse:CN           2 1044.8 5.8871 0.05268 .
# Landuse:Treatment:Tempdiff  2 1040.3 1.3616 0.50622  --> Remove
# Landuse:Treatment:Sand      2 1039.5 0.5886 0.74506  --> Remove
# Landuse:Treatment:CN        2 1039.2 0.2064 0.90193  --> Remove

#Drop1 again:
#
#Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
#   CN + Rain + Season:Landuse + Season:Treatment + Season:Tempdiff + 
#   Season:Sand + Season:CN + Treatment:Landuse + Treatment:Tempdiff + 
#   Treatment:Sand + Treatment:CN + Landuse:Treatment + Landuse:Tempdiff + 
#   Landuse:Sand + Landuse:CN + Tempdiff:Sand + Tempdiff:CN + 
#   Tempdiff:Rain + Sand:CN + Season:Landuse:CN + (1 | Site.ID)
# Df    AIC    LRT  Pr(Chi)   
# <none>                1032.4                   
# Season:Treatment    1 1032.9 2.4901 0.114561   
# Season:Tempdiff     1 1031.8 1.4233 0.232853   
# Season:Sand         1 1037.2 6.7288 0.009487 **
#   Landuse:Treatment   2 1033.1 4.6607 0.097261 . 
# Treatment:Tempdiff  1 1031.2 0.7919 0.373524   
# Treatment:Sand      1 1034.5 4.1165 0.042466 * 
#   Treatment:CN        1 1033.7 3.2358 0.072045 . 
# Landuse:Tempdiff    2 1034.4 5.9532 0.050965 . 
# Landuse:Sand        2 1031.8 3.4179 0.181058   
# Tempdiff:Sand       1 1030.9 0.4425 0.505905   
# Tempdiff:CN         1 1031.3 0.8392 0.359632   
# Tempdiff:Rain       1 1030.6 0.1601 0.689049   
# Sand:CN             1 1030.8 0.3851 0.534910   
# Season:Landuse:CN   2 1034.2 5.8244 0.054355 . 
#--> Need to use update() to remove threeway and look at twoway significance:
#
#After doing update() (below) the new output of Drop1 is:
# Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
# CN + Rain + Season:Landuse + Season:Sand + Season:CN + Treatment:Landuse + 
# Treatment:Sand + Treatment:CN + Landuse:Treatment + Landuse:Tempdiff + 
# Landuse:CN + Season:Landuse:CN + (1 | Site.ID)
#                   Df    AIC    LRT Pr(Chi)  
# <none>               1029.8                 
# Rain               1 1031.3 3.4589 0.06291 .
# Season:Sand        1 1034.3 6.5199 0.01067 *
# Landuse:Treatment  2 1029.7 3.9111 0.14149
# Treatment:Sand     1 1031.0 3.2485 0.07149 .
# Treatment:CN       1 1032.7 4.9294 0.02640 *
# Landuse:Tempdiff   2 1029.9 4.1147 0.12780  
# Season:Landuse:CN  2 1030.8 5.0025 0.08198 .

#Doing drop1 again, without threeway Season:Landuse:CN:
#Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
# CN + Rain + Season:Landuse + Season:Sand + Season:CN + Treatment:Landuse + 
# Treatment:Sand + Treatment:CN + Landuse:Treatment + Landuse:Tempdiff + 
# Landuse:CN + (1 | Site.ID)
#                       Df    AIC    LRT Pr(Chi)  
# <none>               1030.8                 
# Rain               1 1032.7 3.8997 0.04829 *
# Season:Landuse     2 1029.9 3.1187 0.21028  -->Remove
# Season:Sand        1 1032.4 3.5712 0.05879 .
# Season:CN          1 1029.0 0.1667 0.68303  -->Remove
# Landuse:Treatment  2 1030.8 4.0060 0.13493  -->Remove
# Treatment:Sand     1 1032.2 3.3730 0.06627 .
# Treatment:CN       1 1033.8 4.9561 0.02600 *
# Landuse:Tempdiff   2 1030.5 3.6520 0.16105  -->Remove 
# Landuse:CN         2 1027.4 0.5788 0.74870 -->Remove

# #
# Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
# CN + Rain + Season:Sand + Treatment:Sand + Treatment:CN + 
# (1 | Site.ID)
#                 Df    AIC    LRT  Pr(Chi)   
# <none>            1023.6                   
# Landuse         2 1024.3 4.7331 0.093803 . 
# Tempdiff        1 1023.6 2.0362 0.153595   
# Rain            1 1028.2 6.6421 0.009959 **
# Season:Sand     1 1023.6 2.0157 0.155674   -->Remove
# Treatment:Sand  1 1024.4 2.8877 0.089258 . 
# Treatment:CN    1 1024.2 2.6308 0.104806  -->Remove

# Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
# CN + Rain + Treatment:Sand + (1 | Site.ID)
#                 Df    AIC    LRT  Pr(Chi)   
# <none>            1024.1                   
# Season          1 1022.1 0.0000 0.999781   
# Landuse         2 1024.7 4.6124 0.099640 . 
# Tempdiff        1 1024.5 2.4063 0.120850   
# CN              1 1026.4 4.3019 0.038069 * 
# Rain            1 1030.7 8.6040 0.003354 **
# Treatment:Sand  1 1024.4 2.2606 0.132702 --> Remove

# Removing last two-way interaction:
# Model:
# MainCGdiff ~ Season + Landuse + Treatment + Tempdiff + Sand + 
# CN + Rain + (1 | Site.ID)
#           Df    AIC    LRT  Pr(Chi)   
# <none>       1024.4                   
# Season     1 1022.4 0.0001 0.991041   -->Remove
# Landuse    2 1024.9 4.5167 0.104525   -->Remove
# Treatment  1 1026.1 3.6995 0.054427 . 
# Tempdiff   1 1024.7 2.3571 0.124711   -->Remove
# Sand       1 1026.2 3.8133 0.050846 . 
# CN         1 1026.5 4.1014 0.042847 * 
# Rain       1 1030.6 8.2464 0.004083 **

#
# Model (THIS IS THE FIRST WITHOUT "SINGULAR FIT" WARNING - GOOD!):
# MainCGdiff ~ Treatment + Sand + CN + Rain + (1 | Site.ID)
#            Df    AIC     LRT   Pr(Chi)    
# <none>       1021.3                      
# Treatment  1 1023.1  3.7774   0.05195 .  
# Sand       1 1022.5  3.1653   0.07522 .  
# CN         1 1021.9  2.6468   0.10376    -->This is now N.S. Could be due to interaction with one of the terms that were removed?
# Rain       1 1035.8 16.5207 4.812e-05 ***
#--> Trying to add one each of the variables separately to see if CN changes. My bet is Landuse or Treatment that is somewhat collinear.
# I want to test some inereraction and if its afecting covariates to be significant
# But, for now I'll remove CN from model:
# Model:
# MainCGdiff ~ Treatment + Sand + Rain + (1 | Site.ID)
#           Df    AIC     LRT  Pr(Chi)    
# <none>       1021.9                     
# Treatment  1 1023.7  3.7769  0.05197 .  
# Sand       1 1023.3  3.3946  0.06541 .  
# Rain       1 1036.6 16.7079 4.36e-05 ***
##Tried to remove Sand - the model gets worse AIC-wise.
#Will now try adding variable back and maybe twoway interaction that would make sense


#Best model so far:
#MainCGdiff ~ 
RecalMCGModBEST <- lmer(MainCGdiff~ Treatment + Sand + CN + Rain + (1 | Site.ID), na.action=na.omit, REML=T, data =RecalDataMCG)
summary(RecalMCGModBEST)

#Checking assumptions:
E1 <- resid(RecalMCGModBEST, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F1 <- fitted(RecalMCGModBEST)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # Several of the fitted values <0 #PROBLEM???
abline(h = 0, lty = 2, col = 1)


RecalMCGMod1 <- update(RecalMCGMod, .~. -Season:Landuse:CN)

RecalMCGMod1a <- update(RecalMCGMod1,.~.-Season:Treatment)
RecalMCGMod1b <- update(RecalMCGMod1,.~.-Season:Tempdiff)
RecalMCGMod1c <- update(RecalMCGMod1,.~.-Season:Sand)
RecalMCGMod1d <- update(RecalMCGMod1,.~.-Landuse:Treatment)
RecalMCGMod1e <- update(RecalMCGMod1,.~.-Treatment:Tempdiff)
RecalMCGMod1f <- update(RecalMCGMod1,.~.-Treatment:Sand )
RecalMCGMod1g <- update(RecalMCGMod1,.~.-Treatment:CN)
RecalMCGMod1h <- update(RecalMCGMod1,.~.-Landuse:Tempdiff)
RecalMCGMod1i <- update(RecalMCGMod1,.~.-Landuse:Sand)
RecalMCGMod1j <- update(RecalMCGMod1,.~.-Tempdiff:Sand)
RecalMCGMod1k <- update(RecalMCGMod1,.~.-Tempdiff:CN)
RecalMCGMod1l <- update(RecalMCGMod1,.~.-Tempdiff:Rain)
RecalMCGMod1m <- update(RecalMCGMod1,.~.-Sand:CN)

#Comparing each model when threeway removed and one two way removed:
anova(RecalMCGMod1,RecalMCGMod1a)#Season:Treatment N.S
anova(RecalMCGMod1,RecalMCGMod1b)#Season:Tempdiff N.S
anova(RecalMCGMod1,RecalMCGMod1c)#Season:Sand N.S
anova(RecalMCGMod1,RecalMCGMod1d)#Landuse:Treatment N.S
anova(RecalMCGMod1,RecalMCGMod1e)#Treatment:Tempdiff N.S
anova(RecalMCGMod1,RecalMCGMod1f)#Treatment:Sand N.S
anova(RecalMCGMod1,RecalMCGMod1g)#Treatment:CN N.s 0.06
anova(RecalMCGMod1,RecalMCGMod1h)#Landuse:Tempdiff N.S
anova(RecalMCGMod1,RecalMCGMod1i)#Landuse:Sand N.S
anova(RecalMCGMod1,RecalMCGMod1j)#Tempdiff:Sand N.S
anova(RecalMCGMod1,RecalMCGMod1k)#Tempdiff:CN N.S
anova(RecalMCGMod1,RecalMCGMod1l)#Tempdiff:Rain N.S
anova(RecalMCGMod1,RecalMCGMod1m)#Sand:CN N.S
#-->Removing all N.S two-ways


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