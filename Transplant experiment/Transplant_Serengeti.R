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

########################################################################
#### Model probability of occurence ####
names(TP2)
TP2$FinalPresAb
TPprob<- glmmadmb(FinalPresAb~fTagsp+fLanduse+Treatment+fTagsp:fLanduse+
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
                      Treatment = levels(TP2$Treatment))
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
#fTagsp        eta        Pi        se      SeUp      SeLo
#1 Chl pyc  0.5108753 0.6250116 0.4216500 0.7920471 0.4217585
#2 Chr ori -0.4053894 0.4000182 0.7142230 0.7299734 0.1412112 # Cryo orien - low 
#3 Cyn dac  0.9985056 0.7307647 0.7657373 0.9240963 0.3769911 # Cynodon higher 
#4 Dig mac  0.4519652 0.6111064 0.6852178 0.8575382 0.2908908
#5 The tri  0.5306943 0.6296450 0.7177266 0.8740705 0.2939970

# No difference in probabilty of species being present after trasnplant

#### Determine species differences in biomass ####
names(TP2)
TPbio<- lmer(Target.weight.g~fTagsp+Treatment+fLanduse+fTagsp:fLanduse+
               fTagsp:Treatment+
                 (1|fSite/fBlock),REML=T,
                  data=TP2)
anova(TPbio)
summary(TPbio) # No difference in cover - marginal
drop1(TPbio,test="Chi")

summary(glht(TPbio, mcp(fTagsp="Tukey"))) 


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


########################################################################
#### END ####
########################################################################