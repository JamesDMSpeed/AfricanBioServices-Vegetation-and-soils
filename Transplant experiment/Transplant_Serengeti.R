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


# Combined species, aboveground biomass and 5 cm soil
TP<-read.csv(file="Transplant experiment/BioCoverSoil5cm.csv", sep=",",header=TRUE)
TP[duplicated(TP$Current.placement.of.the.turf), ] # No dups

# House keeping factors for analysis
TP$fSite<-as.factor(TP$Site)
TP$fBlock<-as.factor(TP$Block)
TP$fTagsp<-as.factor(TP$Tagsp)
TP$fLanduse<-as.factor(TP$Landuse)

#### Process data ####
# Remove no data and control plots
names(TP)
dim(TP) # 152  30
TP2Cex<-TP[!is.na(TP$FilTagspCover) & ! TP$Trasnplant=="Control" & !TP$Treatment=="Exclosed",]
TP2Con<-TP[!is.na(TP$FilTagspCover) & ! TP$Trasnplant=="Control",]
TP2<-TP[!is.na(TP$FilTagspCover),]
dim(TP2Cex) #  89 41 - No controls or exclosures
dim(TP2Con) # 118  41 - No controls
dim(TP2) # No NAs 139 41 
TP2$FilTagspCover

# Convert to binary data - presence vs absence and probability of detection
TP2$FinalPresAb<-TP2$FilTagspCover
TP2$FinalPresAb[TP2$FinalPresAb>0]<-1

TP2Cry<-droplevels(TP2[!TP2$fTagsp=="Cry ori" & !TP2$fTagsp=="The tri" & !TP2$fTagsp=="Cyn dac",])
dim(TP2Cry) #  96 30

# Sample analysis
TP0<-TP[!is.na(TP$FilVegCover),]
dim(TP0) 
(139+40)*68 #= 12172 #elemental CN analysis plant material
840*96 #= 80640
840*49 #= 41160

########################################################################
#### Cover change with and without controls ####

names(TP2Con)
TP2Con$CoverChange<-TP2Con$FilTagspCover-TP2Con$InitialTagSpcover
TP2$CoverChange<-TP2$FilTagspCover-TP2$InitialTagSpcover

CC<-aggregate(CoverChange~fTagsp+fLanduse,TP2,mean)
CCcon<-aggregate(CoverChange~fTagsp+fLanduse,TP2Con,mean)

CC

########################################################################
#### Model probability of occurence ####
TPprob<- glmmadmb(FinalPresAb~fTagsp+fLanduse+fTagsp:fLanduse+(1|fSite/fBlock), 
                        family="binomial",#zeroInflation = TRUE,
                        #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                        data=TP2)
summary(TPprob) # No difference in probabilty of species 
drop1(TPprob,test="Chi")
summary(glht(TPprob, mcp(fTagsp="Tukey"))) 

#Model matrix
MyData <- expand.grid(fTagsp = levels(TP2$fTagsp),
                      fLanduse = levels(TP2$fLanduse))
head(MyData)

#Convert the covariate values into an X matrix
Xp <- model.matrix(~ fTagsp+fLanduse+fTagsp:fLanduse, data = MyData)

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
#fTagsp        eta        Pi        se      SeUp      SeLo
#1 Chl pyc  0.5108753 0.6250116 0.4216500 0.7920471 0.4217585
#2 Chr ori -0.4053894 0.4000182 0.7142230 0.7299734 0.1412112 # Cryo orien - low 
#3 Cyn dac  0.9985056 0.7307647 0.7657373 0.9240963 0.3769911 # Cynodon higher 
#4 Dig mac  0.4519652 0.6111064 0.6852178 0.8575382 0.2908908
#5 The tri  0.5306943 0.6296450 0.7177266 0.8740705 0.2939970

# No difference in probabilty of species being present after trasnplant

#### Determine species differences in biomass ####
names(TP2)
TPbio<- lmer(Target.weight.g~fTagsp+fLanduse+fTagsp:fLanduse+
                 (1|fSite/fBlock),REML=T,
                  data=TP2)
anova(TPbio)
summary(TPbio) # No difference in cover - marginal
drop1(TPbio,test="Chi")

summary(glht(TPbio, mcp(fTagsp="Tukey"))) 


#### Graph biomass and probability ####

MeanBio<-aggregate(Target.weight.g~fTagsp+fLanduse,TP2,mean)
MeanBio2<-aggregate(Target.weight.g~fTagsp+fLanduse,TP2,sd)
MeanBio$Sd<-MeanBio2$Target.weight.g
MeanBio$Pi<-MyData$Pi

TPbiom<-ggplot(MeanBio,aes(y=Target.weight.g, x=fTagsp))
TPbiom<-TPbiom+geom_jitter(data=TP2, fill="light grey", alpha=.5)
TPbiom<-TPbiom+geom_point(stat="identity",aes(size=Pi),position=position_dodge(width=.6))
TPbiom<-TPbiom+geom_errorbar(aes(ymin = Target.weight.g-Sd,ymax = Target.weight.g+Sd),width=.1,show.legend=F) 
TPbiom<-TPbiom+ylab("Biomass (g)")+xlab("Target species")
TPbiom<-TPbiom+facet_wrap(~fLanduse)
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
summary(glht(TPcover, mcp(fTagsp="Tukey"))) 

# No difference in probabilty of species 

#Model matrix
MyData <- expand.grid(fTagsp = levels(TP2$fTagsp))
head(MyData)

#Convert the covariate values into an X matrix
Xp <- model.matrix(~ fTagsp, data = MyData)

#Extract parameters and parameter covariance matrix
betas    <- fixef(TPcover)
Covbetas <- vcov(TPcover)

#Calculate the fitted values in the predictor scale
MyData$eta <- Xp %*% betas

#Calculate the SEs on the scale of the predictor function
MyData$se    <- sqrt(diag(Xp %*% Covbetas %*% t(Xp)))
MyData$SeUp  <- exp(MyData$eta + 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  + 1.96 *MyData$se))
MyData$SeLo  <- exp(MyData$eta - 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  - 1.96 *MyData$se))

MyData

########################################################################
#### Soil properties ####

TP2dig<-TP2[TP2$fTagsp=="Dig mac",]
dim(TP2dig) # 36
names(TP2dig)
TP2dig$Site_block<-as.factor(with(TP2dig, paste(fSite,fBlock, sep=".")))
BDdig<-aggregate(BD.gcm3~Site_block,TP2dig, mean)
# 18 samples
18*280 #= 5040 NOK...
36*280 # 10080 NOK ...


#### Soil bulk density ####

BD<-aggregate(BD.gcm3~fTagsp+fLanduse,TP2, mean)
BDSe<-aggregate(BD.gcm3~fTagsp+fLanduse,TP2, sd)
BD$Sd<-BDSe$BD.gcm3

TPBD<-ggplot(BD,aes(y=BD.gcm3, x=fTagsp))
TPBD<-TPBD+geom_point(stat="identity", size=4,position=position_dodge(width=.6))
TPBD<-TPBD+geom_errorbar(aes(ymin = BD.gcm3-Sd,ymax = BD.gcm3+Sd),width=.1,show.legend=F) 
TPBD<-TPBD+ylab("Bulk density (g cm3)")+xlab("Target species")
TPBD<-TPBD+facet_wrap(~fLanduse)
#TPBD<-TPBD+scale_y_continuous(limits=c(-5,80))
TPBD<-TPBD+theme_classic()
TPBD


########################################################################
#### END ####
########################################################################