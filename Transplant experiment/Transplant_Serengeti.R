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

# Missing data
dim(TP) # Full design 152 plots + 20 = 172 FULL DESIGN 

# Not found
summary(is.na(TP$weight.g))
TPnofind<-TP[!is.na(TP$weight.g),]
# Missing target species
dim(TPnofind[TPnofind$FilTagspCover==0,])
# 53 have zero cover

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

################################################################################################################################################
#### Combine Moveable exclosure + transplant ####
################################################################################################################################################

# Use moveable exclosure data as CONTROL 
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/")

MoveNuts<-read.csv("BiomassStacked2.csv")
names(MoveNuts)

TransNuts<-read.csv("BioCoverSoil5cmNuts2.csv")
names(TransNuts)

# Harvest H7 ONLY and TARGET species ONLY and no exclosures
names(MoveNuts)
MoveNuts$treatment
MoveNutsH7<-droplevels(MoveNuts[MoveNuts$harvest=="H7" & !MoveNuts$pool=="other" & ! MoveNuts$treatment=="EX" & ! MoveNuts$treatment=="EX2",])
dim(MoveNutsH7) #20 57 # 20 additional points
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
MyVars<-c("Plant.N","landuse","region","transplant","site", "block", "Tagsp","treatment","InitialTagSpcover","FilTagspCover","Biomass.g.m2","Plant.CN")
TransNuts2<-TransNuts[MyVars]
MoveNutsH7b<-MoveNutsH7[MyVars]
TransNuts3<-rbind(TransNuts2,MoveNutsH7b)

# Rename levels for rainfall
levels(as.factor(TransNuts$rain.sum.mm))
aggregate(rain.sum.mm~region,TransNuts,mean) #DRY 1175.309,SE 1440.664, WET 1843.961
levels(TransNuts3$region)<-c("Dry \n (1175 mm)","Intermediate \n (1440 mm)","Wet \n (1840 mm)")

# Rename treatment b/c MoveNuts has different name #"Exclosed" "Open"     "OP"  
levels(TransNuts3$treatment)<-c("Exclosed", "Open","Open") 

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
CCbiom<-CCbiom+ylab("Change in relative cover (%)")+xlab("") #+xlab("Land-use")
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
        ,axis.title.x=element_text(size=14,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.x =  element_blank()
        ,axis.text.y = element_text(size=13,margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=14,color="black")
        ,legend.title = element_text(size=14,color="black")
        ,strip.text = element_text(size=14,color="black")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
#CCbiom<-CCbiom+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
#                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
#                                                                         col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )

CCbiom<-CCbiom+guides(fill=F,linetype=F,shape = F, colour=F)
CCbiom


# Differenes in target species control site vs. moved site
# Chl pyc
# Control =  28.285714
# Transplant same site = 4.666667
sum(-11.000000,-30.875000,3.500000,-18.000000)/4 #-14.09375
  
# Chr ori
# Control = 11.714286
# Trasnplant same site= 10.333333
sum(-18.750000,-25.333333,20.250000,-15.75000)/4 # -9.895833

# Chr ori
# Control =9.714286
# Trasnplant same site = 10.666667
sum(-13.750000,-33.750000, 3.666667,-3.250000)/4 #-11.77083


#landuse                    region transplant   Tagsp CoverChange
#19    Wild Intermediate \n (1440 mm)    Control Dig mac    1.250000
#20 Pasture          Dry \n (1175 mm) Transplant Dig mac  -16.000000
#21    Wild          Dry \n (1175 mm) Transplant Dig mac   -3.250000
#22 Pasture          Wet \n (1840 mm) Transplant Dig mac  -17.750000
#23    Wild          Wet \n (1840 mm) Transplant Dig mac   -9.375000
#24    Wild          Wet \n (1840 mm)    Control The tri    0.875000
#25 Pasture          Dry \n (1175 mm) Transplant The tri  -50.000000
#26    Wild          Dry \n (1175 mm) Transplant The tri  -40.666667
#27    Wild Intermediate \n (1440 mm) Transplant The tri   28.500000
#28 Pasture          Wet \n (1840 mm) Transplant The tri   -4.800000
#29    Wild          Wet \n (1840 mm) Transplant The tri  -18.250000

######################################################
#### Plant cover change modelling ####
######################################################

# Remove NAS
TransNuts3NA<-TransNuts3[!is.na(TransNuts3$CoverChange),]
dim(TransNuts3NA) # 159  12(77 62 without exclosures)

# Remove Seronera
names(TransNuts3NA)
TransNuts3NA<-droplevels(TransNuts3NA[!TransNuts3NA$region=="Intermediate \n (1440 mm)",])

CClm<-lmer(CoverChange~landuse+region+transplant+Tagsp+#Treatment+
             landuse:region+ Tagsp:landuse+
              transplant:Tagsp+
             transplant:Tagsp:region+
             transplant:Tagsp:landuse+
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
CClm<-lmer(CoverChange~Tagsp+transplant+region+#landuse+#transplant+#region+#treatment+
              #Tagsp:landuse+ #landuse:region+
             #transplant:Tagsp+
             transplant:Tagsp:region+
             #transplant:Tagsp:landuse+
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
dim(coef.CC)
confint.CC <- confint(CClm)
coef.CC <- summary(CClm)$coef  
CChange<- as.data.frame(cbind(confint.CC[5:22,] , coef.CC[2:19] ))
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

#Pairwise contrasts *ghlt* and *lsmeans*
library(multcomp)
library(emmeans)

# Species
summary(glht(CClm, mcp(Tagsp="Tukey"))) # Nothing
emm.s.recalmod <- emmeans(CClm,~Tagsp)
pairs(emm.s.recalmod) # Nothing singificantly different

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

# Convert lower SD to zero 
CCFin$sdHi<-CCFin$sd+CCFin$FilTagspCover
CCFin$sdLo<-CCFin$FilTagspCover-CCFin$sd
CCFin$sdLo[CCFin$sdLo<0]<--3

# Plant cover change  graph
CCbiomF<-ggplot(CCFin,aes(y=FilTagspCover,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
CCbiomF<-CCbiomF+geom_errorbar(data=CCFin,aes(ymin=sdLo, ymax=sdHi),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
CCbiomF<-CCbiomF+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
CCbiomF<-CCbiomF+ylab("Final relative cover (%)")+xlab("")
CCbiomF<-CCbiomF+facet_wrap(~region, ncol=5)
CCbiomF<-CCbiomF+scale_shape_manual(values=c(22,21))
CCbiomF<-CCbiomF+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
CCbiomF<-CCbiomF+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
CCbiomF<-CCbiomF+scale_y_continuous(limits=c(-3,102),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),expand=c(0,0))
CCbiomF<-CCbiomF+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.title.x=element_text(size=14,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.x =  element_blank()
        ,axis.text.y = element_text(size=13,margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=14,color="black")
        ,legend.title = element_text(size=14,color="black")
        ,strip.text = element_blank()
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
CCbiomF<-CCbiomF+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                      colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
CCbiomF<-CCbiomF+guides(fill=F,linetype=F,shape = F, colour=F)
CCbiomF

######################################################
#### Final target species cover  modelling ####
######################################################

TransNuts4NA<-TransNuts3[!is.na(TransNuts3$FilTagspCover),]
dim(TransNuts4NA) # 159  12 (77 62 without exclosures)

CClmFin<-lmer(FilTagspCover~landuse+region+transplant+Tagsp+treatment+
             landuse:region+Tagsp:landuse+
             transplant:Tagsp+
               transplant:Tagsp:region+
               transplant:Tagsp:landuse+
             (1|site/block),
           na.action=na.fail,
           REML=T,TransNuts4NA)

summary(CClmFin)
drop1(CClmFin, test="Chisq")

# Model averaging
modsetlmer_CClmFin <- dredge(CClmFin,trace=2) 
model.sel(modsetlmer_CClmFin) 
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
CClmFin<-lmer(FilTagspCover~transplant+Tagsp+region+#+landuse+#treatment+
                #landuse:region+Tagsp:landuse+
                transplant:Tagsp+
             transplant:Tagsp:region+
              # transplant:Tagsp:landuse+
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
dim(coef.CCFin)
confint.CCFin <- confint(CClmFin)
coef.CCFin <- summary(CClmFin)$coef  
CCFin<- as.data.frame(cbind(confint.CCFin[5:22,] , coef.CCFin[2:19] ))
CCFin$terms<-dimnames(CCFin)[[1]]
colnames(CCFin)[1]<-"lowerCI"
colnames(CCFin)[2]<-"upperCI"
colnames(CCFin)[3]<-"Estimate"

# Coef
CCcoefF<-ggplot(CCFin, aes(x=terms, y=Estimate))
CCcoefF<-CCcoefF+geom_hline(yintercept=0,linetype="dashed")
CCcoefF<-CCcoefF+geom_errorbar(data=CCFin,aes(ymin=lowerCI, ymax=upperCI),stat = "identity",width=.2,lwd=1,show.legend=F)
CCcoefF<-CCcoefF+geom_point(size=5, stroke=1.25)
CCcoefF<-CCcoefF+coord_flip()
CCcoefF<-CCcoefF+theme_classic()
CCcoefF

# Interaction plots transplant and target species 
with(TransNuts4NA, {interaction.plot(transplant,Tagsp,FilTagspCover,
                                      xlab = "Treatment",
                                      ylab = "Final species cover (%)",
                                      fun=mean)})

# Emmeans contrast three-way interaction
CClmFinFINAL <- emmeans(CClmFin, pairwise~region,type="response") #Creating emmeans across the factor levels in the interaction.
CClmFinFINAL$emmeans
CClmFinFINAL.pairs <- pairs(CClmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CClmFinFINAL.pairs$emmeans
plot(CClmFinFINAL, comparisons = FALSE)

################################################################################################################################################
#### Standing biomass ####
################################################################################################################################################

#### Graph biomass ####
MeanBio<-aggregate(Biomass.g.m2~landuse+region+transplant+Tagsp+treatment,TransNuts3,mean)
MeanBio2<-aggregate(Biomass.g.m2~landuse+region+transplant+Tagsp+treatment,TransNuts3,sd)
MeanBio$sd<-MeanBio2$Biomass.g.m2

MeanB<-aggregate(Biomass.g.m2~landuse,TransNuts3,mean)
MeanB

# Relevel so that other
levels(MeanBio$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
MeanBio$Tagsp<- factor(MeanBio$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
MeanBio$spp.code<-as.factor(with(MeanBio, paste(Tagsp,transplant,sep="_")))

# Plant cover change  graph
TPbiom<-ggplot(MeanBio,aes(y=Biomass.g.m2,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code, alpha=treatment)) # group = grouping vector for lines
TPbiom<-TPbiom+geom_errorbar(data=MeanBio,aes(ymin=Biomass.g.m2-sd, ymax=Biomass.g.m2+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPbiom<-TPbiom+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPbiom<-TPbiom+ylab(expression(paste("Aboveground biomass (kg ",m^-2,")")))+xlab("")
TPbiom<-TPbiom+facet_wrap(~region, ncol=5)
TPbiom<-TPbiom+scale_shape_manual(values=c(22,21))
TPbiom<-TPbiom+scale_alpha_manual(values=c(.33,1))
TPbiom<-TPbiom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPbiom<-TPbiom+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
TPbiom<-TPbiom+scale_y_continuous(limits=c(-.1,1.85),breaks=c(0,.5,1.,1.5),labels=c(0,.5,1.,1.5),expand=c(0,0))
TPbiom<-TPbiom+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.title.x=element_text(size=14,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.x =  element_blank()
        ,axis.text.y = element_text(size=13,margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=14,color="black")
        ,legend.title = element_text(size=14,color="black")
        ,strip.text = element_blank()
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
TPbiom<-TPbiom+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                      alpha = guide_legend("Exclosures",override.aes = list(shape=c(21), size=3.75,alpha=c(0.33,1),fill="grey30",col="grey30", stroke=1.25)),
                        colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                  col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TPbiom

######################################################
#### Plant Biomass  modelling ####
######################################################

TransNuts5NA<-TransNuts3[!is.na(TransNuts3$Biomass.g.m2),]
dim(TransNuts5NA) # 97 12 (77 62 without exclosures)

TPbio<-lmer(Biomass.g.m2~landuse+region+transplant+Tagsp+treatment+
              landuse:region+Tagsp:landuse+
              transplant:Tagsp+
              transplant:Tagsp:region+
              #transplant:Tagsp:landuse+
                (1|site/block),
              na.action=na.fail,
              REML=T,TransNuts5NA)

summary(TPbio)
drop1(TPbio, test="Chisq")

# Model averaging
modsetlmer_TPbio <- dredge(TPbio,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_TPbio) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_TPbio<-model.avg(modsetlmer_TPbio)
importance(modavglmer_modsetlmer_TPbio)
TPbioimportance<-as.data.frame(importance(modavglmer_modsetlmer_TPbio))
summary(modavglmer_modsetlmer_TPbio)#Estimated coefficients given weighting
confint.TPbio <- confint(modavglmer_modsetlmer_TPbio)
coef.TPbio <- summary(modavglmer_modsetlmer_TPbio)$coefmat.subset       
CCTPbio<- as.data.frame(cbind(confint.TPbio , coef.TPbio))
CCTPbio$terms<-dimnames(CCTPbio)[[1]]
colnames(CCTPbio)[1]<-"lowerCI"
colnames(CCTPbio)[2]<-"upperCI"


# Reduced model
TPbio<-lmer(Biomass.g.m2~region+transplant+Tagsp+treatment+#landuse+
             # landuse:region+Tagsp:landuse+
              transplant:Tagsp+
              transplant:Tagsp:region+
                (1|site/block),
              na.action=na.fail,
              REML=T,TransNuts5NA)

summary(TPbio)
drop1(TPbio, test="Chisq")

# Checking resids vs fit
E1 <- resid(TPbio, type = "pearson")
F1 <- fitted(TPbio)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Not good = very strongly connical 
# Need to try Gamma distribution- will not work with rank deficiencies?

# Reduce model
dim(coef.TPbio)
confint.TPbio <- confint(TPbio)
coef.TPbio <- summary(TPbio)$coef  
CCTPbio<- as.data.frame(cbind(confint.CCFin[5:22,] , coef.CCFin[2:19] ))
CCTPbio$terms<-dimnames(CCTPbio)[[1]]
colnames(CCTPbio)[1]<-"lowerCI"
colnames(CCTPbio)[2]<-"upperCI"
colnames(CCTPbio)[3]<-"Estimate"

# Coef
CCcoefTPbio<-ggplot(CCTPbio, aes(x=terms, y=Estimate))
CCcoefTPbio<-CCcoefTPbio+geom_hline(yintercept=0,linetype="dashed")
CCcoefTPbio<-CCcoefTPbio+geom_errorbar(data=CCTPbio,aes(ymin=lowerCI, ymax=upperCI),stat = "identity",width=.2,lwd=1,show.legend=F)
CCcoefTPbio<-CCcoefTPbio+geom_point(size=5, stroke=1.25)
CCcoefTPbio<-CCcoefTPbio+coord_flip()
CCcoefTPbio<-CCcoefTPbio+theme_classic()
CCcoefTPbio

# Emmeans contrast three-way interaction
TPbioFINAL <- emmeans(TPbio, pairwise~region,type="response") #Creating emmeans across the factor levels in the interaction.
TPbioFINAL$emmeans
TPbioFINAL.pairs <- pairs(TPbioFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
TPbioFINAL.pairs$emmeans
plot(TPbioFINAL, comparisons = FALSE)


###########################################################
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
TNs<-TNs+ylab("Leaf nitrogen concentration (%)")+xlab("")
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
        ,axis.title.x=element_text(size=14,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.x =  element_blank()
        ,axis.text.y = element_text(size=13,margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=14,color="black")
        ,legend.title = element_text(size=14,color="black")
        ,strip.text = element_blank()
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
#TNs<-TNs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
#                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
#                                                                             col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TNs<-TNs+guides(fill=F,linetype=F,shape =F, colour=F)
TNs

#####################################################################
##### Plant nitrogen - modelling ####
#####################################################################

TransNutsNA<-TransNuts3[!is.na(TransNuts3$Plant.N),]
dim(TransNutsNA) # 93 12 (77 62 without exclosures)

Nlm<-lmer(Plant.N~landuse+region+transplant+Tagsp+treatment+
            landuse:region+Tagsp:landuse+
            transplant:Tagsp+
            transplant:Tagsp:region+
            #transplant:Tagsp:landuse+
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

# Reduced model
Nlm<-lmer(Plant.N~landuse+region+transplant+Tagsp+#treatment+
            landuse:region+#Tagsp:landuse+
            transplant:Tagsp+
            #transplant:Tagsp:region+
            #transplant:Tagsp:landuse+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNutsNA)

summary(Nlm)
drop1(Nlm, test="Chisq")

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
abline(h = 0, lty = 2, col = 1) # Very good

# Reduce model
dim(coef.Nlm )
confint.Nlm <- confint(Nlm)
coef.Nlm <- summary(Nlm)$coef  
CCNlm<- as.data.frame(cbind(confint.Nlm[5:17,] , coef.Nlm[2:14] ))
CCNlm$terms<-dimnames(CCNlm)[[1]]
colnames(CCNlm)[1]<-"lowerCI"
colnames(CCNlm)[2]<-"upperCI"
colnames(CCNlm)[3]<-"Estimate"

# Coef
CCcoefNlm<-ggplot(CCNlm, aes(x=terms, y=Estimate))
CCcoefNlm<-CCcoefNlm+geom_hline(yintercept=0,linetype="dashed")
CCcoefNlm<-CCcoefNlm+geom_errorbar(data=CCNlm,aes(ymin=lowerCI, ymax=upperCI),stat = "identity",width=.2,lwd=1,show.legend=F)
CCcoefNlm<-CCcoefNlm+geom_point(size=5, stroke=1.25)
CCcoefNlm<-CCcoefNlm+coord_flip()
CCcoefNlm<-CCcoefNlm+theme_classic()
CCcoefNlm

# Interaction plot transplant x species
with(TransNutsNA, {interaction.plot(transplant,Tagsp,Plant.N,
                                     xlab = "Treatment",
                                     ylab = "Plant.N (%)",
                                     fun=mean)})



TransNuts5NA<-TransNuts3[!is.na(TransNuts3$Biomass.g.m2) & !is.na(TransNuts3$Plant.N),]
dim(TransNuts5NA) # 90 13 (77 62 without exclosures)

NlmBio<-lmer(Biomass.g.m2~Plant.N+
              # Plant.N:Tagsp+
          # Plant.N:region+
            #  Plant.N:landuse+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNuts5NA)

summary(NlmBio)
drop1(NlmBio, test="Chisq")


mod <- nls(Biomass.g.m2 ~ exp(a + b * Plant.N), data = TransNuts5NA, start = list(a = 0, b = 0))
summary(mod)
coef(mod)
av<-seq(0.5,2.5,.05)
A.nls<-predict(mod, list(Plant.N.cm=av))
A.nls
plot(Biomass.g.m2~Plant.N, data =TransNuts5NA)
lines(av,A.nls,lty = 1, lwd =1.75, col = "black")


# Plot predicted vs actual data
MyData5 <- expand.grid(Plant.N=seq(min(TransNuts5NA$Plant.N),max(TransNuts5NA$Plant.N), length=25))

#Create X matrix with expand.grid
X5 <- model.matrix(~Plant.N, data =MyData5)
head(X5)

#Calculate predicted values from beta distribution
#var[Y_i] = P_i * (1 - Pi) / (1 + phi) # Beta

#Extract parameters and parameter covariance matrix
betas2    <- fixef(NlmBio)
Covbetas2 <- vcov(NlmBio)

#Calculate the fitted values in the predictor scale
MyData5$eta <- X5 %*% betas2
MyData5$Pi  <- exp(MyData5$eta ) / (1 + exp(MyData5$eta))

#NLS

Covbetas2 <- vcov(mod)
MyData5$eta <- X5 %*% coef(mod)
MyData5$Pi  <- exp(MyData5$eta ) / (1 + exp(MyData5$eta))


#Calculate the SEs on the scale of the predictor function
MyData5$se    <- sqrt(diag(X5 %*% Covbetas2 %*% t(X5)))
MyData5$SeUp  <- exp(MyData5$eta + 1.96 *MyData5$se) / 
  (1 + exp(MyData5$eta  + 1.96 *MyData5$se))
MyData5$SeLo  <- exp(MyData5$eta - 1.96 *MyData5$se) / 
  (1 + exp(MyData5$eta  - 1.96 *MyData5$se))

colnames(MyData5)[3]<-"Biomass.g.m2"


# Plant Biomass and N conc
TransNuts3$spp.code<-as.factor(with(TransNuts3, paste(Tagsp,treatment,sep="_")))
TNBio<-ggplot(TransNuts3,aes(y=Biomass.g.m2,x=Plant.N)) # group = grouping vector for lines
TNBio<-TNBio+geom_point(aes(shape=transplant, colour=Tagsp, fill=Tagsp),size=3.5, stroke=1.25)
TNBio<-TNBio+ylab(expression(paste("Aboveground biomass (kg ",m^-2,")")))+xlab("Leaf nitrogen concentration (%)")
#TNBio<-TNBio+facet_wrap(~region, ncol=5)
TNBio<-TNBio+geom_line(data=MyData5,aes(y=Biomass.g.m2,x=Plant.N), colour="black",size=1)
TNBio<-TNBio+scale_shape_manual(values=c(22,21))
TNBio<-TNBio+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TNBio<-TNBio+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TNBio<-TNBio+theme_bw()+
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
TNBio<-TNBio+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                    colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                              col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TNBio

# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggdensity(TransNuts3, "Plant.N",color="Tagsp", fill = "Tagsp")+scale_fill_manual("Tagsp",values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))+
  scale_colour_manual("Tagsp",values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
yplot <- ggdensity(TransNuts3, "Biomass.g.m2", color="Tagsp",fill = "Tagsp") +scale_fill_manual("Tagsp",values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))+
  scale_colour_manual("Tagsp",values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))+ rotate()

# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()

detach(package:egg)

# Arranging the plot
ggarrange(xplot, NULL, TNBio, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, .75), heights = c(.75, 2),
          common.legend = TRUE)

#### CARBON TO NITROGEN RATIO ####

# Means Plant CN
names(TransNuts3)
TransCNmean<-aggregate(Plant.CN~landuse+region+transplant+Tagsp,TransNuts3,mean)
TransCNsd<-aggregate(Plant.CN~landuse+region+transplant+Tagsp,TransNuts3,sd)
#TransNuts3mean2<-left_join(TransNuts3mean,TransNutsDUNG,by=c("landuse","region","transplant","Tagsp"))
TransCNmean$sd<-TransCNsd$Plant.CN

# Relevel so that other
levels(TransCNmean$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
TransCNmean$Tagsp<- factor(TransCNmean$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
TransCNmean$spp.code<-as.factor(with(TransCNmean, paste(Tagsp,transplant,sep="_")))

# Plant Carbon-to-Nitrogen ratio graph
TCNs<-ggplot(TransCNmean,aes(y=Plant.CN,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
TCNs<-TCNs+geom_errorbar(data=TransCNmean,aes(ymin=Plant.CN-sd, ymax=Plant.CN+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TCNs<-TCNs+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TCNs<-TCNs+ylab("C:N ratio")+xlab("Land-use")
TCNs<-TCNs+facet_wrap(~region, ncol=5)
TCNs<-TCNs+scale_shape_manual(values=c(22,21))
TCNs<-TCNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TCNs<-TCNs+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TCNs<-TCNs+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=14,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.title.x=element_text(size=14,color="black")
        ,axis.text.x=element_text(size=14,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=14,color="black")
        ,legend.title = element_text(size=14,color="black")
        ,strip.text = element_blank()
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
#TCNs<-TCNs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
#                colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
#                                                                          col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TCNs<-TCNs+guides(fill=F,linetype=F,shape =F, colour=F)
TCNs


#### Combining aboveground graphs ####

# Extra legend from legend plot
library(grid)
library(gridExtra)
library(ggpubr)
library(egg)
legend <- get_legend(TPbiom)
arrangeGrob(legend)

filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/", "Transplant.Aboveground", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=18, height=37.6, res=400, unit="cm")
egg::ggarrange(CCbiom, CCbiomF, TPbiom, TNs, TCNs, ncol=1) #common.legend = T,legend="right")
dev.off()

#p3 <- arrangeGrob(legend2,legend3,legend,pLiv,pWild,dp,bp, ncol=3, nrow=3,widths=c(1,1,1), heights=c(.25,1,2),layout_matrix = cbind(c(1,4,7), c(2,5,7),c(3,6,7)))
#grid.arrange(p3)

#p1 <- arrangeGrob(CCbiom, CCbiomF, TPbiom+ theme(legend.position="none"), TNs, TCNs,legend, ncol=2, nrow=5,widths=c(1,1), heights=c(1,1,1,1,1))
#layout_matrix = cbind(c(1,NA), c(2,NA),c(3,6), c(4,NA), c(5,NA))) #common.legend = T)
#grid.arrange(p1)


########################################################################
#### Transplant soil properties ####
########################################################################


levels(TP2$region)<-c("Dry \n (1175 mm)","Intermediate \n (1440 mm)","Wet \n (1840 mm)")

#### Soil bulk density ####

# Graph soil bulk density
BD<-aggregate(BD.gcm3~landuse+region+transplant+Tagsp,TP2, mean)
BDSe<-aggregate(BD.gcm3~landuse+region+transplant+Tagsp,TP2, sd)
BD$sd<-BDSe$BD.gcm3

# Relevel so that other
levels(BD$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
BD$Tagsp<- factor(BD$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
BD$spp.code<-as.factor(with(BD, paste(Tagsp,transplant,sep="_")))

# Soil bulk density graph
TPBD<-ggplot(BD,aes(y=BD.gcm3,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
TPBD<-TPBD+geom_errorbar(data=BD,aes(ymin=BD.gcm3-sd, ymax=BD.gcm3+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPBD<-TPBD+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPBD<-TPBD+ylab(expression(paste("Soil bulk density (g ",cm^-3,")")))+xlab("Land-use")
TPBD<-TPBD+facet_wrap(~region, ncol=5)
TPBD<-TPBD+scale_shape_manual(values=c(22,21))
TPBD<-TPBD+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPBD<-TPBD+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
TPBD<-TPBD+theme_bw()+
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
TPBD<-TPBD+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                      colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TPBD

#### Soil  pH ####

# Graph soil bulk density
pH<-aggregate(pH~landuse+region+transplant+Tagsp,TP2, mean)
pHSe<-aggregate(pH~landuse+region+transplant+Tagsp,TP2, sd)
pH$sd<-pHSe$pH

# Relevel so that other
levels(pH$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
pH$Tagsp<- factor(pH$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
pH$spp.code<-as.factor(with(pH, paste(Tagsp,transplant,sep="_")))


# Soil pH  graph
TPpH<-ggplot(pH,aes(y=pH,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
TPpH<-TPpH+geom_errorbar(data=pH,aes(ymin=pH-sd, ymax=pH+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPpH<-TPpH+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPpH<-TPpH+ylab("pH")+xlab("Land-use")
TPpH<-TPpH+facet_wrap(~region, ncol=5)
TPpH<-TPpH+scale_shape_manual(values=c(22,21))
TPpH<-TPpH+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPpH<-TPpH+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
TPpH<-TPpH+theme_bw()+
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
TPpH<-TPpH+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                  colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                            col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TPpH


names(TP2)
MyEnv<-c("Target.weight.g","FilTagspCover","Plant.N" ,        
         "BD.gcm3", "pH")
pairs(TP2[,MyEnv], lower.panel= panel.cor)

plot(Plant.N~pH,col=c(Tagsp),TP2)

TP2NA<-TP2[!is.na(TP2$Plant.N),]
pHlm<-lmer(pH~landuse+region+transplant+Tagsp+pH+
            # pH:region+pH:landuse+pH:transplant+
             #pH:Tagsp+
            (1|site/block),
          na.action=na.fail,
          REML=T,TP2NA)
summary(pHlm)
drop1(pHlm, test="Chisq")


################################################################################################################################################
#### OLD SCRIPT ####
################################################################################################################################################


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