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

TP2Con$FinalPresAb<-TP2Con$FilTagspCover
TP2Con$FinalPresAb[TP2Con$FinalPresAb>0]<-1

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

################################################################################################################################################
#### Model probability of occurence ####
names(TP2)
TP2$FinalPresAb
names(TP2)
TPprob<- glmmadmb(FinalPresAb~fTagsp+fLanduse+region+#treatemnt+transplant
                    fTagsp:region+ 
                    fTagsp:fLanduse+ 
                     (1|fSite/fBlock), 
                        family="binomial",#zeroInflation = TRUE,
                        #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                          admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=500,imaxfn=500,maxph=5),
                        data=TP2)
summary(TPprob) # No difference in probabilty of species 
#drop1(TPprob,test="Chi")
summary(glht(TPprob, mcp(fTagsp="Tukey"))) 

# RANK DEFICIENCY - CANNOT TEST THIS WITH JUST TRANSPLANT PLOTS...NEED INCLUDE CONTROLS...
# RANK DEFICIENCY - CANNOT TEST INTERACTIONS SPP x REGION and SPP X LANDUSE


#Model matrix
MyData <- expand.grid(fTagsp = levels(TP2$fTagsp),
                      fLanduse = levels(TP2$fLanduse),
                      #treatment = levels(TP2$treatment),
                     # transplant= levels(TP2$transplant),
                      region = levels(TP2$region))
head(MyData)

#Convert the covariate values into an X matrix
Xp <- model.matrix(~ fTagsp+fLanduse+region+#treatemnt+transplant
                     fTagsp:region+ 
                     fTagsp:fLanduse, data = MyData)

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
MyData<-as.data.frame(MyData)
# No difference in probabilty of species being present after trasnplant
# Species from pastures do worse in wildlife, most species do worse in pasture,
# Chl pyc does poorly in Seronera exclosures

##########################################
# GRAPH: Probability of survival
##########################################
# Means probability
TransPmean<-aggregate(Pi~fLanduse+region+transplant+fTagsp,MyData,mean)
TransPsd<-aggregate(Pi~fLanduse+region+transplant+fTagsp,MyData,sd)
#TransNuts3mean2<-left_join(TransNuts3mean,TransNutsDUNG,by=c("landuse","region","transplant","Tagsp"))
TransPmean$sd<-TransPsd$V1
colnames(TransPmean)[5]<-"Pi"

# Relevel so that other
levels(TransPmean$fTagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
TransPmean$Tagsp<- factor(TransPmean$fTagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
TransPmean$spp.code<-as.factor(with(TransPmean, paste(fTagsp,transplant,sep="_")))

TPs<-ggplot(TransPmean,aes(y=Pi,x=fLanduse, shape=transplant, colour=fTagsp,fill=spp.code)) # group = grouping vector for lines
TPs<-TPs+geom_errorbar(data=TransPmean,aes(ymin=Pi-sd, ymax=Pi+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPs<-TPs+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPs<-TPs+ylab("Probability (%)")+xlab("Land-use")
TPs<-TPs+facet_wrap(~region, ncol=5)
TPs<-TPs+scale_shape_manual(values=c(22,21))
TPs<-TPs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPs<-TPs+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TPs<-TPs+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=13,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=13,color="black")
        ,legend.title = element_text(size=13,color="black")
        ,strip.text = element_text(size=13,color="black")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
TPs<-TPs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                          col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TPs

################################################################################################################################################
#### Combine Moveable exclosure + transplant ####
################################################################################################################################################

# Use moveable exclosure data as CONTROL 
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")

MoveNuts<-read.csv("BiomassStacked2.csv")
names(MoveNuts)

TransNuts<-read.csv("BioCoverSoil5cmNuts2.csv")
names(TransNuts)

# Harvest H7 ONLY and TARGET species ONLY
MoveNutsH7<-droplevels(MoveNuts[MoveNuts$harvest=="H7" & !MoveNuts$pool=="other",])
dim(MoveNutsH7)
levels(MoveNutsH7$landuse)<-c("Pasture","Wild")

# Convert setup cover and harvest cover to same name as Transplant dataset
names(MoveNutsH7)
#[23] "target.sp.cover.setup" 
#[27] "target.sp.cover.harvest.."   
names(TransNuts)
#[19] "InitialTagSpcover" 
#[21] "FilTagspCover"

colnames(MoveNutsH7)[23]<-"InitialTagSpcover" 
colnames(MoveNutsH7)[27]<-"FilTagspCover"

# Convert biomass to standard kg m-2
# Transplant 25 cm diameter # ((pi/4)*.25^2)
# Moveable exclosures 60 x 60 cm # .6*.6
MoveNutsH7$Biomass.g.m2<-(MoveNutsH7$biomass.total.g/0.36)/1000
TransNuts$Biomass.g.m2<-(TransNuts$Target.weight.g/((pi/4)*.25^2))/1000  

# Merge transplant and exclosure data sets
names(TransNuts)
names(MoveNutsH7)
MoveNutsH7$site<-MoveNutsH7$site.name
MyVars<-c("Plant.N","landuse","region","transplant","site", "block", "Tagsp","treatment","InitialTagSpcover","FilTagspCover","Biomass.g.m2")
TransNuts2<-TransNuts[MyVars]
MoveNutsH7b<-MoveNutsH7[MyVars]
TransNuts3<-rbind(TransNuts2,MoveNutsH7b)

# Rename levels for rainfall
levels(as.factor(TransNuts$rain.sum.mm))
aggregate(rain.sum.mm~region,TransNuts,mean) #DRY 1175.309,SE 1440.664, WET 1843.961
levels(TransNuts3$region)<-c("Dry \n (1175 mm)","Intermediate \n (1140 mm)","Wet \n (1840 mm)")


########################################################################
#### Plant cover changes ####
########################################################################

# Target species cover - with & without controls
TransNuts3$CoverChange<-TransNuts3$FilTagspCover-TransNuts3$InitialTagSpcover
TransNuts3$CoverChange<-TransNuts3$FilTagspCover-TP2Con$InitialTagSpcover

# Summary of cover change with and without controls
CC<-aggregate(CoverChange~landuse+region+transplant+Tagsp,TransNuts3,mean)
CCsd<-aggregate(CoverChange~landuse+region+transplant+Tagsp,TransNuts3,sd)
CC$sd<-CCsd$CoverChange

# Relevel so that other
levels(CC$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
CC$Tagsp<- factor(CC$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
CC$spp.code<-as.factor(with(CC, paste(Tagsp,transplant,sep="_")))

# Plant cover change  graph
CCbiom<-ggplot(CC,aes(y=CoverChange,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
CCbiom<-CCbiom+geom_hline(yintercept=0,linetype="dashed")
CCbiom<-CCbiom+geom_errorbar(data=CC,aes(ymin=CoverChange-sd, ymax=CoverChange+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
CCbiom<-CCbiom+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
CCbiom<-CCbiom+ylab("Change in plant cover (%)")+xlab("Land-use")
CCbiom<-CCbiom+facet_wrap(~region, ncol=5)
CCbiom<-CCbiom+scale_shape_manual(values=c(22,21))
CCbiom<-CCbiom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
CCbiom<-CCbiom+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
CCbiom<-CCbiom+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=13,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=13,color="black")
        ,legend.title = element_text(size=13,color="black")
        ,strip.text = element_text(size=13,color="black")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
CCbiom<-CCbiom+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                          col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
CCbiom


######################################################
#### Plant cover change modelling ####
######################################################

TransNuts3NA<-TransNuts3[!is.na(TransNuts3$CoverChange),]
dim(TransNuts3NA) # 187  12 (77 62 without exclosures)

CClm<-lmer(CoverChange~landuse+region+transplant+Tagsp+#treatment+
            landuse:region+ transplant:landuse+
            Tagsp:landuse+transplant:region+
            transplant:Tagsp+
            landuse:region:transplant+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNuts3NA)

summary(CClm)
drop1(CClm, test="Chisq")

# Model averaging
modsetlmer_CClm <- dredge(CClm,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_CClm) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_CClm<-model.avg(modsetlmer_CClm)
importance(modavglmer_modsetlmer_CClm)
CCimportance<-as.data.frame(importance(modavglmer_modsetlmer_CClm))
summary(modavglmer_modsetlmer_CClm)#Estimated coefficients given weighting
confint.CC <- confint(modavglmer_modsetlmer_CClm)
coef.CC <- summary(modavglmer_modsetlmer_CClm)$coefmat.subset       
CChange<- as.data.frame(cbind(confint.CC , coef.CC ))
CChange$terms<-dimnames(CChange)[[1]]
colnames(CChange)[1]<-"lowerCI"
colnames(CChange)[2]<-"upperCI"

# Coef
CCcoef<-ggplot(CChange, aes(x=terms, y=Estimate))
CCcoef<-CCcoef+geom_hline(yintercept=0,linetype="dashed")
CCcoef<-CCcoef+geom_errorbar(data=CChange,aes(ymin=lowerCI, ymax=upperCI),stat = "identity",width=.2,lwd=1,show.legend=F)
CCcoef<-CCcoef+geom_point(size=5, stroke=1.25)
CCcoef<-CCcoef+coord_flip()
CCcoef<-CCcoef+theme_classic()
CCcoef

# Reduced model
CClm<-lmer(CoverChange~landuse+transplant+Tagsp+#region+#treatment+
            # landuse:region+ #transplant:landuse+
             Tagsp:landuse+#transplant:region+
             transplant:Tagsp+
             #landuse:region:transplant+
             (1|site/block),
           na.action=na.fail,
           REML=T,TransNuts3NA)

summary(CClm)
drop1(CClm, test="Chisq")

# Checking resids vs fit
E1 <- resid(CClm, type = "pearson")
F1 <- fitted(CClm)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)

# Reduce model
confint.CC <- confint(CClm)
coef.CC <- summary(CClm)$coef  
CChange<- as.data.frame(cbind(confint.CC[5:18,] , coef.CC[2:15] ))
CChange$terms<-dimnames(CChange)[[1]]
colnames(CChange)[1]<-"lowerCI"
colnames(CChange)[2]<-"upperCI"
colnames(CChange)[3]<-"Estimate"

# Coef
CCcoef<-ggplot(CChange, aes(x=terms, y=Estimate))
CCcoef<-CCcoef+geom_hline(yintercept=0,linetype="dashed")
CCcoef<-CCcoef+geom_errorbar(data=CChange,aes(ymin=lowerCI, ymax=upperCI),stat = "identity",width=.2,lwd=1,show.legend=F)
CCcoef<-CCcoef+geom_point(size=5, stroke=1.25)
CCcoef<-CCcoef+coord_flip()
CCcoef<-CCcoef+theme_classic()
CCcoef

########################################################################
#### Final plant cover  ####
########################################################################

# Summary of Final targt species cover
CCFin<-aggregate(FilTagspCover~landuse+region+transplant+Tagsp,TransNuts3,mean)
CCFinsd<-aggregate(FilTagspCover~landuse+region+transplant+Tagsp,TransNuts3,sd)
CCFin$sd<-CCFinsd$FilTagspCover

# Relevel so that other
levels(CCFin$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
CCFin$Tagsp<- factor(CCFin$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
CCFin$spp.code<-as.factor(with(CCFin, paste(Tagsp,transplant,sep="_")))

# Plant cover change  graph
CCbiomF<-ggplot(CCFin,aes(y=FilTagspCover,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
CCbiomF<-CCbiomF+geom_hline(yintercept=0,linetype="dashed")
CCbiomF<-CCbiomF+geom_errorbar(data=CCFin,aes(ymin=FilTagspCover-sd, ymax=FilTagspCover+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
CCbiomF<-CCbiomF+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
CCbiomF<-CCbiomF+ylab("Final target species cover (%)")+xlab("Land-use")
CCbiomF<-CCbiomF+facet_wrap(~region, ncol=5)
CCbiomF<-CCbiomF+scale_shape_manual(values=c(22,21))
CCbiomF<-CCbiomF+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
CCbiomF<-CCbiomF+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
CCbiomF<-CCbiomF+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=13,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=13,color="black")
        ,legend.title = element_text(size=13,color="black")
        ,strip.text = element_text(size=13,color="black")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
CCbiomF<-CCbiomF+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                      colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
CCbiomF


######################################################
#### Final target species cover  modelling ####
######################################################

TransNuts4NA<-TransNuts3[!is.na(TransNuts3$FilTagspCover),]
dim(TransNuts4NA) # 187  12 (77 62 without exclosures)

CClmFin<-lmer(FilTagspCover~landuse+region+transplant+Tagsp+#treatment+
             landuse:region+ transplant:landuse+
             Tagsp:landuse+transplant:region+
             transplant:Tagsp+
             landuse:region:transplant+
             (1|site/block),
           na.action=na.fail,
           REML=T,TransNuts4NA)

summary(CClmFin)
drop1(CClmFin, test="Chisq")

# Model averaging
modsetlmer_CClmFin <- dredge(CClmFin,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_CClmFin) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_CClmFin<-model.avg(modsetlmer_CClmFin)
importance(modavglmer_modsetlmer_CClm)
CCimportanceFin<-as.data.frame(importance(modavglmer_modsetlmer_CClmFin))
summary(modavglmer_modsetlmer_CClmFin)#Estimated coefficients given weighting
confint.CCFin <- confint(modavglmer_modsetlmer_CClmFin)
coef.CCFin <- summary(modavglmer_modsetlmer_CClmFin)$coefmat.subset       
CCFin<- as.data.frame(cbind(confint.CCFin , coef.CCFin))
CCFin$terms<-dimnames(CCFin)[[1]]
colnames(CCFin)[1]<-"lowerCI"
colnames(CCFin)[2]<-"upperCI"


# Reduced model
CClmFin<-lmer(FilTagspCover~landuse+transplant+Tagsp+#region+#treatment+
              transplant:landuse+#landuse:region+
             #Tagsp:landuse+#transplant:region+
             #transplant:Tagsp+
             #landuse:region:transplant+
             (1|site/block),
           na.action=na.fail,
           REML=T,TransNuts4NA)

summary(CClmFin)
drop1(CClmFin, test="Chisq")

# Checking resids vs fit
E1 <- resid(CClmFin, type = "pearson")
F1 <- fitted(CClmFin)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)


# Reduce model
confint.CCFin <- confint(CClmFin)
coef.CCFin <- summary(CClm)$coef  
CChange<- as.data.frame(cbind(confint.CC[5:18,] , coef.CC[2:15] ))
CChange$terms<-dimnames(CChange)[[1]]
colnames(CChange)[1]<-"lowerCI"
colnames(CChange)[2]<-"upperCI"
colnames(CChange)[3]<-"Estimate"

# Coef
CCcoef<-ggplot(CChange, aes(x=terms, y=Estimate))
CCcoef<-CCcoef+geom_hline(yintercept=0,linetype="dashed")
CCcoef<-CCcoef+geom_errorbar(data=CChange,aes(ymin=lowerCI, ymax=upperCI),stat = "identity",width=.2,lwd=1,show.legend=F)
CCcoef<-CCcoef+geom_point(size=5, stroke=1.25)
CCcoef<-CCcoef+coord_flip()
CCcoef<-CCcoef+theme_classic()
CCcoef


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

# Means Plant biomass
TransBiomean<-aggregate(Target.weight.g~fLanduse+region+transplant+fTagsp,MyData,mean)
TransBiosd<-aggregate(Target.weight.g~fLanduse+region+transplant+fTagsp,MyData,sd)
TransBiomean$sd<-TransBiosd$Target.weight.g


# Treatment code for filling symbols
TransPmean$spp.code<-as.factor(with(TransPmean, paste(fTagsp,transplant,sep="_")))

TPs<-ggplot(TransPmean,aes(y=Pi,x=fLanduse, shape=transplant, colour=fTagsp,fill=spp.code)) # group = grouping vector for lines
TPs<-TPs+geom_errorbar(data=TransPmean,aes(ymin=Pi-sd, ymax=Pi+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPs<-TPs+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPs<-TPs+ylab("Probability (%)")+xlab("Land-use")
TPs<-TPs+facet_wrap(~region, ncol=5)
TPs<-TPs+scale_shape_manual(values=c(22,21))
TPs<-TPs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPs<-TPs+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TPs<-TPs+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=13,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=13,color="black")
        ,legend.title = element_text(size=13,color="black")
        ,strip.text = element_text(size=13,color="black")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
TPs<-TPs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                          col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TPs

################################################################################################################################################
# Plant Nitrogen concencentrations #
################################################################################################################################################


##########################################
# GRAPH: Nitrogen concentrations 
##########################################
# Means Plant N
TransNutsDUNG<-aggregate(total_dungMean~landuse+region+transplant+Tagsp,TransNuts,mean)
TransNuts3mean<-aggregate(Plant.N~landuse+region+transplant+Tagsp,TransNuts3,mean)
TransNuts3sd<-aggregate(Plant.N~landuse+region+transplant+Tagsp,TransNuts3,sd)
#TransNuts3mean2<-left_join(TransNuts3mean,TransNutsDUNG,by=c("landuse","region","transplant","Tagsp"))
TransNuts3mean$sd<-TransNuts3sd$Plant.N

# Relevel so that other
levels(TransNuts3mean$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
TransNuts3mean$Tagsp<- factor(TransNuts3mean$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
TransNuts3mean$spp.code<-as.factor(with(TransNuts3mean, paste(Tagsp,transplant,sep="_")))

# Plant Nitrogen concentrations graph
TNs<-ggplot(TransNuts3mean,aes(y=Plant.N,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
TNs<-TNs+geom_errorbar(data=TransNuts3mean,aes(ymin=Plant.N-sd, ymax=Plant.N+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TNs<-TNs+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TNs<-TNs+ylab("Plant nitrogen concentration (%)")+xlab("Land-use")
TNs<-TNs+facet_wrap(~region, ncol=5)
TNs<-TNs+scale_shape_manual(values=c(22,21))
TNs<-TNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TNs<-TNs+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TNs<-TNs+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=13,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=13,color="black")
        ,legend.title = element_text(size=13,color="black")
        ,strip.text = element_text(size=13,color="black")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
TNs<-TNs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                             col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TNs


PN<-ggplot(TransNuts3mean, aes(y=Plant.N, x = landuse, shape=transplant,colour=Tagsp))
PN<-PN+geom_errorbar(aes(ymin=Plant.N-sd, ymax=Plant.N+sd),position=position_dodge(width=.5),width=.1,lwd=.9, alpha=.5,show.legend=F)
PN<-PN+geom_point(stat="identity", size=4.5,position=position_dodge(.5))
PN<-PN+facet_wrap(~region, ncol=5)
PN<-PN+theme_classic()
PN

#####################################################################
##### Plant nitrogen - modelling ####
#####################################################################

TransNutsNA<-TransNuts3[!is.na(TransNuts3$Plant.N),]
dim(TransNutsNA) # 117   7 (77 62 without exclosures)

Nlm<-lmer(Plant.N~landuse+region+transplant+Tagsp+#treatment+
            landuse:region+ transplant:landuse+
            Tagsp:landuse+transplant:region+
            transplant:Tagsp+
           landuse:region:transplant+
         (1|site/block),
         na.action=na.fail,
         REML=T,TransNutsNA)

summary(Nlm)
drop1(Nlm, test="Chisq")

# Model averaging
modsetlmer_Nlm <- dredge(Nlm,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_Nlm) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_Nlm<-model.avg(modsetlmer_Nlm)
importance(modavglmer_modsetlmer_Nlm)
PlantNimportance<-as.data.frame(importance(modavglmer_modsetlmer_Nlm))
plot(PlantNimportance)           


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


########################################################################
#### Transplant soil properties ####
########################################################################

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
               random=~1|Tagsp, method="ML",correlation=cs1AR1,data=Datastack2N)
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


#### Point graph - each species N compared to other #####
dim(Datastack2N)
Datastack$transplant
names(Datastack2N)
SpeciesN<-aggregate(Plant.N~Tagsp+landuse+treatment,Datastack2,mean)
SpeciesNSE<-aggregate(N.conc.adj~target.sp.+landuse+treatment,TargetNavgYm,sd)
SpeciesN$SE<-SpeciesNSE$N.conc.adj

# Relevel so that other
SpeciesN$target.sp.<- factor(SpeciesN$target.sp., levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda","Other"))

# Treatment code for filling symbols
SpeciesN$spp.code<-as.factor(with(SpeciesN, paste(target.sp.,treatment,sep="_")))
TNs<-ggplot(SpeciesN,aes(y=N.conc.adj,x=landuse, colour=target.sp.,fill=spp.code, shape=treatment)) # group = grouping vector for lines
TNs<-TNs+geom_errorbar(data=SpeciesN,aes(ymin=N.conc.adj-SE, ymax=N.conc.adj+SE),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TNs<-TNs+geom_point(position=position_dodge(width=.45),size=5, stroke=1)
TNs<-TNs+ylab("Plant nitrogen concentration (%)")+xlab("Land-use")
TNs<-TNs+scale_shape_manual(values=c(22,21))
TNs<-TNs+scale_fill_manual(values=c("chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","grey50","white","orangered3","white"))
TNs<-TNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3","grey50"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TNs<-TNs+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,legend.text = element_text(size=12,color="black")
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm"))
TNs<-TNs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1)),
                colour = guide_legend("Dominant species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3", "grey50"),
                                                                             col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3", "grey50"), stroke=1)) )
TNs


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