########################################################################
#### Serengeti turf trasnplant experiment ####
########################################################################
# Clean your R environment
rm(list=ls())
library(dplyr)
library(Matrix)
library(ggplot2)
library(glmmADMB)
library(glmmTMB)
library(multcomp)
library(lme4)
########################################################################
##### Aboveground plant cover, aboveground biomass and soil properties ####
#TP<-read.csv(file="Transplant experiment/Aboveground.survey.May2018.csv", sep=",",header=TRUE)
#TPbio<-read.csv(file="Transplant experiment/Aboveground.biomass.May2018.csv", sep=",",header=TRUE)
# House keeping factors
#TP$fCurrent.placement.of.the.turf<-as.factor(TP$Current.placement.of.the.turf)
#TPbio$fCurrent.placement.of.the.turf<-as.factor(TPbio$Current.placement.of.the.turf)
# Combine cover and biomass
#BioCover<-left_join(TP,TPbio,  by=c("fCurrent.placement.of.the.turf"))
#write.csv(BioCover, "Transplant experiment/BioCover.csv",row.names=F) #,sep = ",",dec = ".",col.names = TRUE, row.names=F)

# Combined specie and aboveground biomass
#TP<-read.csv(file="Transplant experiment/BioCover2.csv", sep=",",header=TRUE)
#TP[duplicated(TP$Current.placement.of.the.turf), ] # No dups

# Combined species and belowground soil properties
#bTP<-read.csv(file="Transplant experiment/Belowground.soil.May2018.csv", sep=",",header=TRUE)

# Seperate 5 cm and deep samples
# Duplicates by depth #
#bTP5cm<-bTP[bTP$Soil.depth=="5cm",] # No dups
#bTP10cm<-bTP[!bTP$Soil.depth=="5cm",]
#bTP5cm[duplicated(bTP5cm$Current.placement.of.the.turf), ] # Duplicates
#dim(bTP5cm) # 137  16
#CG_DRY_W_2_2
#bTP10cm[duplicated(bTP10cm$Current.placement.of.the.turf), ] # Duplicates
#dim(bTP10cm) #140  16
# CG_DRY_W_1_1
#SE_2_EX_4

#BioCoverSoil5cm<-left_join(TP,bTP5cm,  by=c("Current.placement.of.the.turf"))
#write.csv(BioCoverSoil5cm, "Transplant experiment/BioCoverSoil5cm.csv",row.names=F) #,sep = ",",dec = ".",col.names = TRUE, row.names=F)

########################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/")
# Combined species, aboveground biomass and 5 cm soil
TP<-read.csv(file="Transplant experiment/BioCoverSoil5cmNuts.csv", sep=",",header=TRUE)
TP[duplicated(TP$Current.placement.of.the.turf), ] # No dups

# House keeping factors for analysis
names(TP)
TP$fSite<-as.factor(TP$site)
TP$fBlock<-as.factor(TP$block)
TP$fTagsp<-as.factor(TP$Tagsp)
TP$fLanduse<-as.factor(TP$landuse)
TP$fregion<-as.factor(TP$region)

#### Process data ####
# Remove no data and control plots
names(TP)
dim(TP) # 152  67
TP2Cex<-TP[!is.na(TP$FilTagspCover) & ! TP$transplant=="Control" & !TP$treatment=="Exclosed",]
TP2Con<-TP[!is.na(TP$FilTagspCover) & ! TP$transplant=="Control",]
TP2<-TP[!is.na(TP$FilTagspCover),]
dim(TP2Cex) #   102  67 - No controls or exclosures
dim(TP2Con) # 118  67 - No controls
dim(TP2) # No NAs 139  67
TP2$FilTagspCover

# Convert to binary data - presence vs absence and probability of detection
TP2$FinalPresAb<-TP2$FilTagspCover
TP2$FinalPresAb[TP2$FinalPresAb>0]<-1

TP2Cry<-droplevels(TP2[!TP2$fTagsp=="Cry ori" & !TP2$fTagsp=="The tri" & !TP2$fTagsp=="Cyn dac",])
dim(TP2Cry) # 86 68

# Sample analysis
TP0<-TP[!is.na(TP$FilVegCover),]
dim(TP0) #139  67

########################################################################
#Data exploration
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?
########################################################################
source(file="/Users/anotherswsmith/Documents/AfricanBioServices/Data/Root decomp Serengeti/HighstatLibV10.R")

#Missing values
colSums(is.na(TP)) 
#Tree data has missing values
# Missing values for fauna.hole (percentage - not scanned)

# Check for outliers
dotchart(TP$FilTagspCover)
dotchart(TP$Target.weight.g)

# C Collinearity X
names(TP)
MyEnv<-c("FilVegH","rain.sum.mm", "Tot.Zero.days","Mean.ConscZero.days" ,        
       "Fire.freq2000.2016","Year.of.last.fire",
       "total_dungTot","total_dungMean", "HerbPRC")
pairs(TP[,MyEnv], lower.panel= panel.cor)

# Rainfall variables all related
# Fire variables related
# Height OK with variables
# Use either min or total

MyHerb<-c("wild_grazerTot","wild_grazerMean","live_broswerMean","live_broswerTot",        
"live_grazerTot","live_grazerMean","wild_broswerrMean","wild_broswerTot",
"total_dungTot","total_dungMean", "HerbPRC")
pairs(TP[,MyHerb], lower.panel= panel.cor)

# HerbPRC and total are related - higher on livestock
# Boxplot for factors
par(mfrow = c(1, 3), cex.lab = 1.5)
boxplot(FilVegH~ landuse, 
        xlab = "Landuse",
        ylab = "Veg height",
        data = TP) # Higher in wild, but overlaps

boxplot(FilVegH~ treatment, 
        xlab = "Treatment",
        ylab = "Veg height",
        data = TP) # Higher exclosure - colinear

boxplot(Year.of.last.fire~ landuse, 
        xlab = "Treatment",
        ylab = "Yr.of.last.fire",
        data = TP) # Fire history colinear with land-use

boxplot(total_dungTot~ landuse, 
        xlab = "Treatment",
        ylab = "total_dungTot",
        data = TP) # Total higher on pasture

########################################################################
#### Cover change with and without controls ####

names(TP2Con)
# Target species cover - with & without controls
TP2$CoverChange<-TP2$FilTagspCover-TP2$InitialTagSpcover
TP2Con$CoverChange<-TP2Con$FilTagspCover-TP2Con$InitialTagSpcover

# Summary of cover change with and without controls
CC<-aggregate(CoverChange~fTagsp+fLanduse,TP2,mean)
CCsd<-aggregate(CoverChange~fTagsp+fLanduse,TP2,sd)
CC$sd<-CCsd$CoverChange
CCcon<-aggregate(CoverChange~fTagsp+fLanduse,TP2Con,mean)
CCconsd<-aggregate(CoverChange~fTagsp+fLanduse,TP2Con,sd)
CCcon$sd<-CCconsd$CoverChange
CCcon$control<-"no control"
CC$control<-"with control"
CC<-rbind(CC,CCcon)

# Change cover 
CCbiom<-ggplot(CC,aes(y=CoverChange, x=fTagsp, colour=control, fill=control))
CCbiom<-CCbiom+geom_abline(intercept=0, slope=0, colour="black", linetype="dashed")
CCbiom<-CCbiom+geom_errorbar(aes(ymin=CoverChange-sd, ymax=CoverChange+sd),position=position_dodge(width=.6),width=.2,lwd=1.1, alpha=.95,show.legend=F)
CCbiom<-CCbiom+geom_point(position=position_dodge(width=.6),size=4.5)
CCbiom<-CCbiom+facet_wrap(~fLanduse)
CCbiom<-CCbiom+theme_classic()
CCbiom

# Summary of Final species cover
CCFin<-aggregate(FilVegCover~fTagsp+fLanduse,TP2,mean)
CCFinsd<-aggregate(FilVegCover~fTagsp+fLanduse,TP2,sd)
CCFin$sd<-CCFinsd$FilVegCover
CCFincon<-aggregate(FilVegCover~fTagsp+fLanduse,TP2Con,mean)
CCFinconsd<-aggregate(FilVegCover~fTagsp+fLanduse,TP2Con,sd)
CCFincon$sd<-CCFinconsd$FilVegCover
CCFincon$control<-"no control"
CCFin$control<-"with control"
CCFin<-rbind(CCFin,CCFincon)

# Final cover
CCbiomF<-ggplot(CCFin,aes(y=FilVegCover, x=fTagsp, colour=control, fill=control))
CCbiomF<-CCbiomF+geom_errorbar(aes(ymin=FilVegCover-sd, ymax=FilVegCover+sd),position=position_dodge(width=.6),width=.2,lwd=1.1, alpha=.95,show.legend=F)
CCbiomF<-CCbiomF+geom_point(position=position_dodge(width=.6),size=4.5)
CCbiomF<-CCbiomF+facet_wrap(~fLanduse)
CCbiomF<-CCbiomF+theme_classic()
CCbiomF

################################################################################################################################################
#### Model probability of occurence ####
names(TP2)
TP2$FinalPresAb
TPprob<- glmmadmb(FinalPresAb~fTagsp+fLanduse+treatment+fTagsp:fLanduse+
                    #Treatment:fTagsp
                     (1|fSite/fBlock), 
                        family="binomial",#zeroInflation = TRUE,
                        #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                          admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=500,imaxfn=500,maxph=5),
                        data=TP2)
summary(TPprob) # No difference in probabilty of species 
#drop1(TPprob,test="Chi")
summary(glht(TPprob, mcp(fTagsp="Tukey"))) 

#Model matrix
MyData <- expand.grid(fTagsp = levels(TP2$fTagsp),
                      fLanduse = levels(TP2$fLanduse),
                      Treatment = levels(TP2$treatment))
head(MyData)

#Convert the covariate values into an X matrix
Xp <- model.matrix(~ fTagsp+fLanduse+Treatment+fTagsp:fLanduse, data = MyData)

#Extract parameters and parameter covariance matrix
betas    <- fixef(TPprob)
Covbetas <- vcov(TPprob)

#Calculate the fitted values in the predictor scale
MyData$eta <- Xp %*% betas
MyData$Pi  <- exp(MyData$eta) / (1 + exp(MyData$eta))

#Calculate the SEs on the scale of the predictor function
MyData$se    <- sqrt(diag(Xp %*% Covbetas %*% t(Xp)))
MyData$SeUp  <- exp(MyData$eta + 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  + 1.96 *MyData$se))
MyData$SeLo  <- exp(MyData$eta - 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  - 1.96 *MyData$se))

MyData


# No difference in probabilty of species being present after trasnplant
# Species from pastures do worse in wildlife, most species do worse in pasture,
# Chl pyc does poorly in Seronera exclosures

################################################################################################################################################
#### Determine species differences in biomass ####
names(TP2)
TPbio<- lmer(Target.weight.g~fTagsp+Treatment+#fLanduse+Year.of.last.fire+
               fTagsp:Treatment+#fTagsp:fLanduse+
                 (1|fSite/fBlock),REML=T,
                  data=TP2)
anova(TPbio)
summary(TPbio) # No difference in cover - marginal
drop1(TPbio,test="Chi")

summary(glht(TPbio, mcp(fTagsp="Tukey"))) 

# Remove Seronera
TP2b<-TP2[!TP2$fSite=="Seronera",]
TPbio2<- lmer(Target.weight.g~fTagsp+total_dungTot+Year.of.last.fire+FilVegH+
                #fTagsp:total_dungTot+fTagsp:Year.of.last.fire+
               (1|fSite/fBlock),REML=T,
             data=TP2b)
anova(TPbio2)
summary(TPbio2) # No difference in cover - marginal
drop1(TPbio2,test="Chi")
plot(Target.weight.g~total_dungTot,TP2b)

TPbio3<- lmer(Target.weight.g~fTagsp+live_grazerTot+Year.of.last.fire+FilVegH+
                fTagsp:live_grazerTot+fTagsp:Year.of.last.fire+
                (1|fSite/fBlock),REML=T,
              data=TP2b)
anova(TPbio3)
summary(TPbio3) # No difference in cover - marginal
drop1(TPbio3,test="Chi")
plot(Target.weight.g~live_grazerTot,TP2b)

#### Graph biomass and probability ####

MeanBio<-aggregate(Target.weight.g~fTagsp+fLanduse+Treatment,TP2,mean)
MeanBio2<-aggregate(Target.weight.g~fTagsp+fLanduse+Treatment,TP2,sd)
MeanBio$Sd<-MeanBio2$Target.weight.g
MyData<-MyData[!MyData$fLanduse=="Pasture" | !MyData$Treatment=="Exclosed",]
MeanBio$Pi<-MyData$Pi

TPbiom<-ggplot(MeanBio,aes(y=Target.weight.g, x=fTagsp,colour=Treatment,fill=Treatment))
TPbiom<-TPbiom+geom_jitter(data=TP2, fill="light grey", alpha=.5)
TPbiom<-TPbiom+geom_point(stat="identity",aes(size=Pi,fill=Treatment))
TPbiom<-TPbiom+geom_errorbar(aes(ymin = Target.weight.g-Sd,ymax = Target.weight.g+Sd),width=.1,show.legend=F) 
TPbiom<-TPbiom+ylab("Biomass (g)")+xlab("Target species")
TPbiom<-TPbiom+facet_wrap(~fLanduse)
TPbiom<-TPbiom+scale_colour_manual(values=c("dark grey","black"))
TPbiom<-TPbiom+scale_y_continuous(limits=c(-5,80))
TPbiom<-TPbiom+theme_classic()
TPbiom

# Interaction plot
TP3<-TP2[!is.na(TP2$Target.weight.g),]
with(TP3, {interaction.plot(fLanduse,fTagsp,Target.weight.g,
                                  xlab = "Landuse",
                                  ylab = "Target Cover",
                                  fun=mean)})
bwplot(Target.weight.g~fLanduse|fTagsp,TP3)

# Species differences
summary(glht(TPbio, mcp(fTagsp="Tukey"))) 

# No difference in probabilty of species 

#Model matrix
MyData2 <- expand.grid(fTagsp = levels(TP2$fTagsp),
                       fLanduse = levels(TP2$fLanduse),
                       Treatment = levels(TP2$Treatment))
head(MyData2)

#Convert the covariate values into an X matrix
Xp2 <- model.matrix(~ fTagsp+Treatment+fLanduse+fTagsp:fLanduse+
                      fTagsp:Treatment, data = MyData2)

#Extract parameters and parameter covariance matrix
betas2    <- fixef(TPbio)
Covbetas2 <- vcov(TPbio)

#Calculate the fitted values in the predictor scale
MyData2$eta <- Xp %*% betas2

#Calculate the SEs on the scale of the predictor function
MyData2$se    <- sqrt(diag(Xp2 %*% Covbetas2 %*% t(Xp2)))
MyData2$SeUp  <- exp(MyData2$eta + 1.96 *MyData2$se) / 
  (1 + exp(MyData2$eta  + 1.96 *MyData$se))
MyData2$SeLo  <- exp(MyData2$eta - 1.96 *MyData2$se) / 
  (1 + exp(MyData2$eta  - 1.96 *MyData2$se))

MyData2

########################################################################
#### Soil properties ####

TP2c<-TP2[TP2$Trasnplant=="Control",] # Controls only...

#### Soil bulk density ####

BD<-aggregate(BD.gcm3~fTagsp+fLanduse,TP2, mean)
BDc<-aggregate(BD.gcm3~fTagsp+fLanduse,TP2c, mean)
BDSe<-aggregate(BD.gcm3~fTagsp+fLanduse,TP2, sd)
BDcSe<-aggregate(BD.gcm3~fTagsp+fLanduse,TP2c, sd)
BD$Sd<-BDSe$BD.gcm3
BDc$Sd<-BDcSe$BD.gcm3
BD$transplanted<-"trasnplanted"
BDc$transplanted<-"control"

BD<-rbind(BD, BDc)

TPBD<-ggplot(BD,aes(y=BD.gcm3, x=fLanduse, colour=transplanted, fill=transplanted))
TPBD<-TPBD+geom_point(stat="identity", size=4,position=position_dodge(width=.6))
TPBD<-TPBD+geom_errorbar(aes(ymin = BD.gcm3-Sd,ymax = BD.gcm3+Sd),position=position_dodge(width=.6),width=.1,show.legend=F) 
TPBD<-TPBD+ylab("Bulk density (g cm3)")+xlab("Target species")
TPBD<-TPBD+facet_wrap(~fTagsp)
#TPBD<-TPBD+scale_y_continuous(limits=c(-5,80))
TPBD<-TPBD+theme_classic()
TPBD


#### Ph ####

pH<-aggregate(pH~fTagsp+fLanduse,TP2, mean)
pHc<-aggregate(pH~fTagsp+fLanduse,TP2c, mean)
pHSe<-aggregate(pH~fTagsp+fLanduse,TP2, sd)
pHcSe<-aggregate(pH~fTagsp+fLanduse,TP2c, sd)
pH$Sd<-pHSe$pH
pHc$Sd<-pHcSe$pH
pH$transplanted<-"trasnplanted"
pHc$transplanted<-"control"

pH<-rbind(pH, pHc)

TPpH<-ggplot(pH,aes(y=pH, x=fLanduse, colour=transplanted, fill=transplanted))
TPpH<-TPpH+geom_point(stat="identity", size=4,position=position_dodge(width=.6))
TPpH<-TPpH+geom_errorbar(aes(ymin = pH-Sd,ymax = pH+Sd),position=position_dodge(width=.6),width=.1,show.legend=F) 
TPpH<-TPpH+ylab("pH")+xlab("Target species")
TPpH<-TPpH+facet_wrap(~fTagsp)
#TPBD<-TPBD+scale_y_continuous(limits=c(-5,80))
TPpH<-TPpH+theme_classic()
TPpH

#### Selecting Dig mac only ####
#### Select controls ####
TP2dig<-TP2[TP2$fTagsp=="Dig mac",]
TP2c<-TP2[TP2$Trasnplant=="Control",]
dim(TP2dig) # 36
dim(TP2c) # 21
NminSelect<-rbind(TP2dig,TP2c) 
dim(NminSelect)
TP2dig$Site_block<-as.factor(with(TP2dig, paste(fSite,fBlock, sep=".")))
BDdig<-aggregate(BD.gcm3~Site_block,TP2dig, mean)
57*280 # 15960 NOK ...
write.csv(NminSelect, "Transplant experiment/NminSelect.csv",row.names=F) #,sep = ",",dec = ".",col.names = TRUE, row.names=F)


################################################################################################################################################
# Plant Nitrogen concencentrations #
################################################################################################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")

MoveNuts<-read.csv("BiomassStacked2.csv")
names(MoveNuts)

TransNuts<-read.csv("BioCoverSoil5cmNuts2.csv")
names(TransNuts)

# Harvest H7 
names(MoveNuts)
MoveNutsH7<-droplevels(MoveNuts[MoveNuts$harvest=="H7" & !MoveNuts$pool=="other",])
dim(MoveNutsH7)
levels(MoveNutsH7$landuse)<-c("Pasture","Wild")


# Merge transplant and exclosure data sets
names(TransNuts)
names(MoveNutsH7)
MoveNutsH7$site<-MoveNutsH7$site.name
MyVars<-c("Plant.N","landuse","region","transplant","site", "block", "Tagsp")
TransNuts2<-TransNuts[MyVars]
MoveNutsH7b<-MoveNutsH7[MyVars]
TransNuts3<-rbind(TransNuts2,MoveNutsH7b)

# Nitrogen concentrations 

# Means Plant N
TransNutsDUNG<-aggregate(total_dungMean~landuse+region+transplant+Tagsp,TransNuts,mean)
TransNuts3mean<-aggregate(Plant.N~landuse+region+transplant+Tagsp,TransNuts3,mean)
TransNuts3sd<-aggregate(Plant.N~landuse+region+transplant+Tagsp,TransNuts3,sd)
#TransNuts3mean2<-left_join(TransNuts3mean,TransNutsDUNG,by=c("landuse","region","transplant","Tagsp"))
TransNuts3mean$sd<-TransNuts3sd$Plant.N

PN<-ggplot(TransNuts3mean, aes(y=Plant.N, x = landuse, shape=transplant,colour=Tagsp))
PN<-PN+geom_errorbar(aes(ymin=Plant.N-sd, ymax=Plant.N+sd),position=position_dodge(width=.5),width=.1,lwd=.9, alpha=.5,show.legend=F)
PN<-PN+geom_point(stat="identity", size=4.5,position=position_dodge(.5))
PN<-PN+facet_wrap(~region, ncol=5)
PN<-PN+theme_classic()
PN

# Modelling plant nitrogen 
TransNutsNA<-TransNuts3[!is.na(TransNuts3$Plant.N),]
dim(TransNutsNA) # 117   7 (77 62 without exclosures)

Nlm<-lmer(Plant.N~Tagsp+landuse+region+transplant+
            landuse:region+ transplant:region+
           landuse:region:transplant+
        (1|site/block),REML=T,TransNutsNA)

summary(Nlm)
plot(Nlm)

drop1(Nmixed,test="Chisq")

# Checking resids vs fit
E1 <- resid(Nlm, type = "pearson")
F1 <- fitted(Nlm)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)

################################################################################################################################################
#### Moveable exclosures - N through time ####
################################################################################################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")

#### DF dominant sp (stacked) ####
Datastack <- read.csv("BiomassStacked2.csv",header=T)
# Removing Ex2 - separate analysis
Datastack <- Datastack[Datastack$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Datastack <- Datastack[Datastack$harvest!="H0",] #removing H0                #280 obs
Datastack <- droplevels(Datastack)

# Creating factor variables
Datastack$landuse<-as.factor(Datastack$landuse)
Datastack$region<-as.factor(Datastack$region)
Datastack$site.name <- as.factor(Datastack$site.name)
Datastack$block<-as.factor(Datastack$block)
Datastack$treatment<-as.factor(Datastack$treatment)
Datastack$harvest<-as.factor(Datastack$harvest)
Datastack$site.id <- as.factor(Datastack$site.id)
Datastack$block.id.harvest <- as.factor(Datastack$block.id.harvest)

#Renaming total productivity and consumption columns
colnames(Datastack)[colnames(Datastack)=="productivity.total.g.m2.day"] <- "prodtot"
colnames(Datastack)[colnames(Datastack)=="productivity.total.g.m2.dayWEIGHTED"] <- "prodtot.per"

colnames(Datastack)[colnames(Datastack)=="consumption.total.g.m2.day"] <- "constot"
colnames(Datastack)[colnames(Datastack)=="consumption.total.g.m2.dayWEIGHTED"] <- "constot.per"

#Productivity and consumptions per harvest period
Datastack$prodsp.sum <- Datastack$prodsp*Datastack$growth.period
Datastack$conssp.sum <- Datastack$conssp*Datastack$growth.period

Datastack$prodspper.sum <- Datastack$prodsp.per*Datastack$growth.period
Datastack$consspper.sum <- Datastack$conssp.per*Datastack$growth.period

Datastack$prodtot.sum <- Datastack$prodtot*Datastack$growth.period
Datastack$constot.sum <- Datastack$constot*Datastack$growth.period
Datastack$prodtotper.sum <- Datastack$prodtot.per*Datastack$growth.period
Datastack$constotper.sum <- Datastack$constot.per*Datastack$growth.period

colnames(Datastack)[colnames(Datastack)=="sand.per"] <- "sand"

#Renaming levels in region, landuse and treatment columns
levels(Datastack$region)<-c("Dry Region","Intermediate Region","Wet Region")
levels(Datastack$landuse)<-c("pasture","wild")
levels(Datastack$treatment)<-c("exclosed","open")

#Rain per day for each harvest period
Datastack$rain.day <- Datastack$rain.sum/Datastack$growth.period

# Rdate create month column. default was (="%d.%m.%Y")
Rdate<-strptime(as.character(Datastack$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )# East African time #USE
class(Rdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
Datastack$Rdate<-Rdate# Add to the dataframe #
# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
Datastack$YrMonth<-format(as.Date(Rdate), "%Y-%m")

# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
Datastack$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(Datastack$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
Datastack$plot.code <- as.factor(with(Datastack,paste(region,landuse,block,treatment,pool,sep="_")))
levels(Datastack$plot.code) #80 levels

# Remove "OTHER" from pool
Datastack2<-Datastack[!Datastack$pool=="other",]

# Remove N outliers
NutsN<-droplevels(subset(Datastack2,Plant.N<3.1 & Plant.N>0.5 | is.na(Plant.N)))
NutsN<-droplevels(subset(NutsN,prodsp.per>-10 | is.na(prodsp.per)))

names(NutsN)
#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|Tagsp/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = NutsN)
corMatrix(cs1AR1.) #What does this give? 

Datastack2NA<-NutsN[!is.na(NutsN$prodsp.per) & !is.na(NutsN$Plant.N) ,]
names(Datastack2NA)
Datastack2NA$prodsp.per
Datastack2NA$Tagsp

#Productivity per species - corrected for percent cover
# LME with temporal auto-correlation (using nlme package)
NAP.lme <- lme(prodsp.per~landuse+poly(rain.sum.1,2)+poly(zero.days,2)+Plant.N+
                 Plant.N:landuse,
              # Plant.N:poly(rain.sum.1,2),#+
                # landuse:rain.sum.1+landuse:consec.NoRain.days,
               random=~1|Tagsp, method="ML",correlation=cs1AR1,data=Datastack2NA)
summary(NAP.lme)#for parameter estimates, don't use the p-values
anova(NAP.lme) #get F statistics and P-values
AIC(NAP.lme) 
drop1(NAP.lme, test="Chisq") # Nothing important

xyplot(prodsp.per~Plant.N|landuse,Datastack2NA)

# Remove NAs
Datastack2N<-NutsN[!is.na(NutsN$Plant.N),]

NAP.lme <- lme(Plant.N~landuse+poly(rain.sum.1,2)+poly(consec.NoRain.days,2),#+
               # landuse:rain.sum.1+landuse:consec.NoRain.days,
               random=~1|Tagsp, method="ML",correlation=cs1AR1,data=Datastack2N)
summary(NAP.lme)#for parameter estimates, don't use the p-values
anova(NAP.lme) #get F statistics and P-values
AIC(NAP.lme) #1185.861

xyplot(Plant.N~consec.NoRain.days|landuse,Datastack2N)
xyplot(Plant.N~rain.sum.1|landuse,Datastack2N)

plot(rain.sum.1~consec.NoRain.days,Datastack2N)


############################################
#### OLD SCRIPT ####
############################################

# SUA and NTNU N concentrations correlations
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")

MoveNuts<-read.csv("MoveEx.Nuts.csv")

names(MoveNuts)

plot(N.target.NTNU~N.target.SUA, MoveNuts)
identify(MoveNuts$N.target.NTNU~MoveNuts$N.target.SUA)
#[1]   9  50  89 168 173 179 202
MoveNuts[9,]
MoveNuts[50,]
MoveNuts[89,]
MoveNuts[168,]
MoveNuts[173,]
MoveNuts[179,]
MoveNuts[202,]

#9 DRY_W_1_H0 = Cyn.dac
#50 DRY_P_1_OP_H1 = Chl.pyc
#89 DRY_W_2_EX2_H2 = Cyn.dac
#168 WET_W_2_OP_H4 = The.tri
#173 WET_P_1_EX_H4 = Chr.ori 
#179 WET_P_4_EX_H4  =  Chr.ori
#202 SE_1_EX2_H4  = Dig.mac

########################################################################
#### END ####
########################################################################