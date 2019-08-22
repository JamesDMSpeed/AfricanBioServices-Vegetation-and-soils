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
library(MuMIn)
library(emmeans)
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
dotchart(TP$Plant.N)

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
#TPprob<- glmmadmb(FinalPresAb~fTagsp+fLanduse+region+#treatemnt+transplant
#                    fTagsp:region+ 
#                    fTagsp:fLanduse+ 
#                     (1|fSite/fBlock), 
#                        family="binomial",#zeroInflation = TRUE,
#                        #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
#                          admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=500,imaxfn=500,maxph=5),
#                        data=TP2)
#summary(TPprob) # No difference in probabilty of species 
#drop1(TPprob,test="Chi")
#summary(glht(TPprob, mcp(fTagsp="Tukey"))) 

# RANK DEFICIENCY - CANNOT TEST THIS WITH JUST TRANSPLANT PLOTS...NEED INCLUDE CONTROLS...
# RANK DEFICIENCY - CANNOT TEST INTERACTIONS SPP x REGION and SPP X LANDUSE


#Model matrix
#MyData <- expand.grid(fTagsp = levels(TP2$fTagsp),
#                      fLanduse = levels(TP2$fLanduse),
#                      #treatment = levels(TP2$treatment),
#                     # transplant= levels(TP2$transplant),
#                      region = levels(TP2$region))
#head(MyData)

#Convert the covariate values into an X matrix
#Xp <- model.matrix(~ fTagsp+fLanduse+region+#treatemnt+transplant
#                     fTagsp:region+ 
#                     fTagsp:fLanduse, data = MyData)

#Extract parameters and parameter covariance matrix
#betas    <- fixef(TPprob)
#Covbetas <- vcov(TPprob)

#Calculate the fitted values in the predictor scale
#MyData$eta <- Xp %*% betas
#MyData$Pi  <- exp(MyData$eta) / (1 + exp(MyData$eta))

#Calculate the SEs on the scale of the predictor function
#MyData$se    <- sqrt(diag(Xp %*% Covbetas %*% t(Xp)))
#MyData$SeUp  <- exp(MyData$eta + 1.96 *MyData$se) / 
 # (1 + exp(MyData$eta  + 1.96 *MyData$se))
#MyData$SeLo  <- exp(MyData$eta - 1.96 *MyData$se) / 
#  (1 + exp(MyData$eta  - 1.96 *MyData$se))

#MyData
#MyData<-as.data.frame(MyData)
# No difference in probabilty of species being present after trasnplant
# Species from pastures do worse in wildlife, most species do worse in pasture,
# Chl pyc does poorly in Seronera exclosures

################################################################################################################################################
#### Combine Moveable exclosure + transplant ####
################################################################################################################################################

# Use moveable exclosure data as CONTROL 
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Transplant experiment/")

MoveNuts<-read.csv("BiomassStacked2.csv")
names(MoveNuts)

TransNuts<-read.csv("BioCoverSoil5cmNuts.csv")
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

#[24]"total.veg.cover.setup" 
#
names(TransNuts)
#[19] "InitialTagSpcover" 
#[21] "FilTagspCover"

colnames(MoveNutsH7)[23]<-"InitialTagSpcover" 
colnames(MoveNutsH7)[27]<-"FilTagspCover"

MoveNutsH7$biomass.total.g # Total biomass
MoveNutsH7$biomass.sp # Target species biomass

# Convert biomass to standard kg m-2
# Transplant 25 cm diameter # ((pi/4)*.25^2)
# Moveable exclosures 60 x 60 cm # .6*.6
MoveNutsH7$Biomass.g.m2<-(MoveNutsH7$biomass.sp/((0.36/100)*MoveNutsH7$FilTagspCover))/1000
TransNuts$Biomass.g.m2<-(TransNuts$Target.weight.g/((pi/4)*.25^2))/1000  

ggplot(data=MoveNutsH7, aes(x=biomass.sp))+geom_histogram()
MoveNutsH7[MoveNutsH7$biomass.sp<5,] # 4 samples < 5 grams

# Merge transplant and exclosure data sets
names(TransNuts)
names(MoveNutsH7)
MoveNutsH7$site<-MoveNutsH7$site.name
MyVars<-c("Plant.N","landuse","region","transplant","site", "block", "Tagsp","treatment","InitialTagSpcover","FilTagspCover","Biomass.g.m2","Plant.CN")
TransNuts2<-TransNuts[MyVars]
MoveNutsH7b<-MoveNutsH7[MyVars]
TransNuts3<-rbind(TransNuts2,MoveNutsH7b)

dim(TransNuts2)
dim(MoveNutsH7b)

# Rename levels for rainfall
levels(as.factor(TransNuts$rain.sum.mm))
aggregate(rain.sum.mm~region,TransNuts,mean) #DRY 1175.309,SE 1440.664, WET 1843.961

# Change Seronera = wet region 
levels(TransNuts3$region)<-c("Mesic","Wet","Wet")

# Rename treatment b/c MoveNuts has different name #"Exclosed" "Open"     "OP"  
levels(TransNuts3$treatment)<-c("Exclosed", "Open","Open") 

#write.csv(TransNuts3,"/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Transplant experiment/TransMove.csv",row.names = FALSE, sep=",")

################################################################################################################################################
#### Recovery of transplant plots ####
################################################################################################################################################

# Transplant experiment only
TPNA<-TP %>% group_by(transplant,fregion, fLanduse, fTagsp) %>% summarise_each(funs(sum(is.na(.))))
TPNA<-TP %>% group_by(transplant,fregion, fLanduse, fTagsp) %>% filter(is.na(FilTagspCover)) %>% summarize(N = n())
TPNA
TPzeros<-TP %>% group_by(transplant,fregion, fLanduse, fTagsp) %>% filter(FilTagspCover==0) %>% summarize(N = n())
TPfull<-TP %>% group_by(transplant,fregion, fLanduse, fTagsp) %>% summarize(N = n())
TPzeros
print(tbl_df(TPzeros), n=30)
print(tbl_df(TPfull), n=30)

Merg1<-merge(TPfull,TPNA, by=c("transplant","fregion", "fLanduse", "fTagsp"), all=T)
Merg2<-merge(Merg1,TPzeros, by=c("transplant","fregion", "fLanduse", "fTagsp"), all=T)
colnames(Merg2)<-c("transplant","fregion", "fLanduse", "fTagsp", "Full", "Missing", "Zero")

# Full desing
# Missing plots
sum(Merg2$Full) # 152 + 20 = 172
sum(Merg2$Missing, na.rm=T)
100-(13/172*100) # 7.6% missing # 92% recovery

sum(Merg2$Zero, na.rm=T) # 55 
55/172*100

# Transplant + Marit's data
TPNA<-TransNuts3 %>%group_by(transplant,region, landuse, Tagsp) %>% filter(is.na(FilTagspCover)) %>% summarize(N = n())
TPzeros<-TransNuts3 %>% group_by(transplant,region, landuse, Tagsp) %>% filter(FilTagspCover==0) %>% summarize(N = n())
TPfull<-TransNuts3 %>% group_by(transplant,region, landuse, Tagsp) %>% summarize(N = n())
TPzeros
print(tbl_df(TPzeros), n=30)
print(tbl_df(TPfull), n=30)

# Initial plant cover averages
mean(TransNuts3$InitialTagSpcover) #  24.68605
sd(TransNuts3$InitialTagSpcover)
se(TransNuts3$InitialTagSpcover)
aggregate(InitialTagSpcover~transplant,TransNuts3,mean)
aggregate(InitialTagSpcover~transplant,TransNuts3,sd)
aggregate(InitialTagSpcover~transplant,TransNuts3,se)

########################################################################
#### Plant cover changes ####
########################################################################

# Target species cover - with & without controls
TransNuts3$CoverChange<-TransNuts3$FilTagspCover-TransNuts3$InitialTagSpcover

# Remove Marit's plant cover change data
TransNuts3cc<-TransNuts3[1:152,]

# Summary of cover change with and without controls
CC<-aggregate(CoverChange~landuse+region+transplant+Tagsp,TransNuts3cc,mean)
CCsd<-aggregate(CoverChange~landuse+region+transplant+Tagsp,TransNuts3cc,sd)
CC$sd<-CCsd$CoverChange

aggregate(CoverChange~transplant,TransNuts3cc,mean)
aggregate(CoverChange~transplant,TransNuts3cc,sd)
aggregate(CoverChange~transplant,TransNuts3cc,se)

# Relevel species
levels(CC$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
CC$Tagsp<- factor(CC$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

#Rename rainfall regions
levels(CC$region)<-c("Mesic region", "Wet region")

# Treatment code for filling symbols
CC$spp.code<-as.factor(with(CC, paste(Tagsp,transplant,sep="_")))

# Plant cover change  graph
CCbiom<-ggplot(CC,aes(y=CoverChange,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
CCbiom<-CCbiom+geom_hline(yintercept=0,linetype="dashed")
CCbiom<-CCbiom+geom_errorbar(data=CC,aes(ymin=CoverChange-sd, ymax=CoverChange+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
CCbiom<-CCbiom+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
CCbiom<-CCbiom+ylab("Change in plant cover (%)")+xlab("") #+xlab("Land-use")
CCbiom<-CCbiom+facet_wrap(~region, ncol=5)
CCbiom<-CCbiom+scale_shape_manual(values=c(22,21))
CCbiom<-CCbiom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
CCbiom<-CCbiom+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
CCbiom<-CCbiom+ggtitle("(a) Change in plant cover")
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

# Remove Marit's plant cover change data
TransNuts3cc<-TransNuts3[1:152,]
dim(TransNuts3cc) # 152

# Remove NAS
TransNuts3NA<-TransNuts3cc[!is.na(TransNuts3cc$CoverChange),]
dim(TransNuts3NA) # 139  13
TransNuts3NA$CoverChange
plot(TransNuts3NA$CoverChange)

# Cover change model
CClm<-lmer(CoverChange~landuse+region+transplant+Tagsp+treatment+
              Tagsp:landuse+ Tagsp:region+
              transplant:Tagsp+ landuse:region+
            # transplant:landuse:region+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNuts3NAcc)

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
CClm<-lmer(CoverChange~transplant+Tagsp+#region+landuse+treatment+
            # Tagsp:landuse+ Tagsp:region+
             transplant:Tagsp+ #landuse:region+
             (1|site/block),
           na.action=na.fail,
           REML=T,TransNuts3NA)
summary(CClm) # Chloris differ from Digitraria
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

# Generate model p-values - change in plant cover
CClm2 <- update(CClm, .~. -transplant:Tagsp)
CClm3 <- update(CClm2, .~. -transplant)
CClm4 <- update(CClm2, .~. -Tagsp)

anova(CClm,CClm2)
anova(CClm2,CClm3)
anova(CClm2,CClm4)

#      Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#CClm2  4 1112.3 1123.3 -552.17   1104.3                             
#CClm  13 1340.4 1378.6 -657.21   1314.4 9.6027      4    0.04768 * # Transplant x species
#CClm2  9 1342.0 1368.4 -662.01   1324.0 8.3108      1   0.003941 ** # Transplant
#CClm2  9 1342.0 1368.4 -662.01   1324.0 11.289      4     0.0235 * # Species

# Remove control plots
TransNuts3NAcc<-droplevels(TransNuts3[!TransNuts3$transplant=="Control",])
dim(TransNuts3NAcc) # 159  13
mean(TransNuts3NAcc$CoverChange, na.rm=T)
sd(TransNuts3NAcc$CoverChange, na.rm=T)

# Emmeans contrast three-way interaction
CClmFinFINAL <- emmeans(CClm, pairwise~Tagsp:transplant,type="response") #Creating emmeans across the factor levels in the interaction.
CClmFinFINAL$emmeans
CClmFinFINAL.pairs <- pairs(CClmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CClmFinFINAL.pairs$emmeans
plot(CClmFinFINAL, comparisons = FALSE)

# transplant Tagsp      contrast               estimate    SE    df t.ratio p.value
# Control    .          Chrysocloa - Digitaria    63.65 20.75 109.5  3.068  0.0680 
#Control    .          Digitaria - Themeda      -43.04 19.01 103.8 -2.265  0.6405

CClmFinFINAL <- emmeans(CClm, pairwise~Tagsp,type="response") #Creating emmeans across the factor levels in the interaction.
CClmFinFINAL$emmeans
CClmFinFINAL.pairs <- pairs(CClmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CClmFinFINAL.pairs$emmeans
plot(CClmFinFINAL, comparisons = FALSE)

# Species differences 
#Chrysocloa - Digitaria    34.89 10.91 115  3.197  0.0179
#Digitaria - Themeda      -27.02  9.99 115 -2.706  0.0786 

aggregate(CoverChange~Tagsp,TransNuts3NA,mean)
########################################################################
#### Final plant cover  ####
########################################################################

# Summary of Final targt species cover
CCFin<-aggregate(FilTagspCover~landuse+region+transplant+Tagsp,TransNuts3,mean)
CCFinsd<-aggregate(FilTagspCover~landuse+region+transplant+Tagsp,TransNuts3,sd)
CCFin$sd<-CCFinsd$FilTagspCover

# Chloris plant cover outside of its 'home' region
TransNuts3PD<-TransNuts3[!TransNuts3$landuse=="Pasture" | !TransNuts3$region=="Mesic", ]
aggregate(FilTagspCover~Tagsp+transplant,TransNuts3PD,mean)
aggregate(FilTagspCover~Tagsp+transplant,TransNuts3PD,sd)

# Themeda plant cover outside of its 'home' region
TransNuts3WW<-TransNuts3[!TransNuts3$landuse=="Wild" | !TransNuts3$region=="Wet", ]
aggregate(FilTagspCover~Tagsp+transplant,TransNuts3WW,mean)
aggregate(FilTagspCover~Tagsp+transplant,TransNuts3PD,sd)

# All other species - besides Chloris
TransNuts3Cp<-TransNuts3[!TransNuts3$Tagsp=="Chl pyc",]
mean(TransNuts3Cp$FilTagspCover, na.rm=T)
sd(TransNuts3Cp$FilTagspCover, na.rm=T)

# Relevel so that other
levels(CCFin$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
CCFin$Tagsp<- factor(CCFin$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
CCFin$spp.code<-as.factor(with(CCFin, paste(Tagsp,transplant,sep="_")))

# Convert lower SD to zero 
CCFin$sdHi<-CCFin$sd+CCFin$FilTagspCover
CCFin$sdLo<-CCFin$FilTagspCover-CCFin$sd
CCFin$sdLo[CCFin$sdLo<0]<--3

# Final plant cover graph
CCbiomF<-ggplot(CCFin,aes(y=FilTagspCover,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
CCbiomF<-CCbiomF+geom_errorbar(data=CCFin,aes(ymin=sdLo, ymax=sdHi),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
CCbiomF<-CCbiomF+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
CCbiomF<-CCbiomF+ylab("Plant cover (%)")+xlab("")
CCbiomF<-CCbiomF+facet_wrap(~region, ncol=5)
CCbiomF<-CCbiomF+scale_shape_manual(values=c(22,21))
CCbiomF<-CCbiomF+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
CCbiomF<-CCbiomF+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
CCbiomF<-CCbiomF+scale_y_continuous(limits=c(-3,102),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),expand=c(0,0))
CCbiomF<-CCbiomF+ggtitle("(b) Final plant cover")
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
#### Final target species cover modelling ####
######################################################

TransNuts4NA<-TransNuts3[!is.na(TransNuts3$FilTagspCover),]
dim(TransNuts4NA) # 159  13 (77 62 without exclosures)

CClmFin<-lmer(FilTagspCover~region+transplant+Tagsp+landuse+treatment+
             Tagsp:landuse+Tagsp:region+
              transplant:Tagsp+landuse:region+
               transplant:landuse:region+
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
#TransNuts4NA$FilTagspCover[TransNuts4NA$FilTagspCover==0]<-1
TransNuts4NA$FilTagspCover
CClmFin<-glmmTMB(FilTagspCover~transplant+Tagsp+landuse+#region+treatment+
                   Tagsp:landuse+#Tagsp:region+
                transplant:Tagsp+#landuse:region+
                 # transplant:landuse:region+
                (1|site/block),
                family=list(family="nbinom2",link="log"),
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
abline(h = 0, lty = 2, col = 1) # Looks great - follows nbiom

# Model deviance
summary(CClmFin)
-2*logLik(CClmFin) # 'log Lik.' 1064.893 (df=18)
(1064.893-0.332)/1064.893# 0.9996882 

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
CClmFinFINAL <- emmeans(CClmFin, pairwise~Tagsp:landuse,type="response") #Creating emmeans across the factor levels in the interaction.
CClmFinFINAL$emmeans
CClmFinFINAL.pairs <- pairs(CClmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CClmFinFINAL.pairs$emmeans
plot(CClmFinFINAL, comparisons = FALSE)

# landuse Tagsp   contrast             ratio       SE  df t.ratio p.value
#Wild    .       Chl pyc / Chr ori 1.04e-02 1.16e-02 156 -4.082  0.0018 
#Wild    .       Chl pyc / Cyn dac 1.69e-02 1.81e-02 156 -3.814  0.0049 
#Wild    .       Chl pyc / Dig mac 2.24e-02 2.14e-02 156 -3.986  0.0026 
#Wild    .       Chl pyc / The tri 7.85e-03 7.86e-03 156 -4.839  0.0001 
#.       Chl pyc Pasture / Wild    1.33e+02 1.60e+02 156  4.091  0.0017 

CClmFinFINAL <- emmeans(CClmFin, pairwise~Tagsp:transplant,type="response") #Creating emmeans across the factor levels in the interaction.
CClmFinFINAL$emmeans
CClmFinFINAL.pairs <- pairs(CClmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CClmFinFINAL.pairs$emmeans
plot(CClmFinFINAL, comparisons = FALSE)

#transplant Tagsp   contrast             ratio     SE  df t.ratio p.value
#Transplant .       Chl pyc / The tri    0.105 0.0660 156 -3.580  0.0115 

# Generate model p-values - change in plant cover
CClmFin2 <- update(CClmFin, .~. -transplant:Tagsp)
CClmFin3 <- update(CClmFin, .~. -Tagsp:landuse)
CClmFin4 <- update(CClmFin2, .~. -Tagsp:landuse)
CClmFin5 <- update(CClmFin4, .~. -Tagsp)
CClmFin6 <- update(CClmFin4, .~. -transplant)
CClmFin7 <- update(CClmFin4, .~. -landuse)

anova(CClmFin,CClmFin2) #transplant:Tagsp
anova(CClmFin,CClmFin3) #Tagsp:landuse
anova(CClmFin4,CClmFin5) #Tagsp
anova(CClmFin4,CClmFin6)
anova(CClmFin4,CClmFin7)

#      Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#CClmFin  18 1100.9 1156.1 -532.45   1064.9 10.495      4    0.03287 * # transplant:Tagsp
#CClmFin  18 1100.9 1156.1 -532.45   1064.9 33.196      4  1.089e-06 *** # Tagsp:landuse
#CClmFin4 10 1127.7 1158.4 -553.84   1107.7 7.7626      4     0.1007# Spp
#CClmFin4 10 1127.7 1158.4 -553.84   1107.7 8.0417      1   0.004571 ** transplant 
#CClmFin4 10 1127.7 1158.4 -553.84   1107.7 0.7911      1     0.3738 # Landuse


################################################################################################################################################
#### Final plant biomass ####
################################################################################################################################################
levels(TransNuts3$treatment)<-c("Exclosed","Open","Open")

#### Graph biomass ####
MeanBio<-aggregate(Biomass.g.m2~landuse+region+transplant+Tagsp+treatment,TransNuts3,na.rm=TRUE,mean)
MeanBio2<-aggregate(Biomass.g.m2~landuse+region+transplant+Tagsp+treatment,na.rm=TRUE,TransNuts3,sd)
MeanBio$sd<-MeanBio2$Biomass.g.m2

MeanB<-aggregate(Biomass.g.m2~landuse,TransNuts3,mean)
MeanB

# Relevel so that other
levels(MeanBio$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
MeanBio$Tagsp<- factor(MeanBio$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
MeanBio$spp.code<-as.factor(with(MeanBio, paste(Tagsp,transplant,sep="_")))

# Open plots - not exclosures at Seronera
MeanBio<-MeanBio[!MeanBio$treatment=="Exclosed",]

# Plant biomass  graph
TPbiom<-ggplot(MeanBio,aes(y=Biomass.g.m2,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # alpha=treatment ## group = grouping vector for lines
TPbiom<-TPbiom+geom_errorbar(data=MeanBio,aes(ymin=Biomass.g.m2-sd, ymax=Biomass.g.m2+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPbiom<-TPbiom+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPbiom<-TPbiom+ylab(expression(paste("Biomass (kg ",m^-2,")")))+xlab("")
TPbiom<-TPbiom+facet_wrap(~region, ncol=5)
TPbiom<-TPbiom+scale_shape_manual(values=c(22,21))
#TPbiom<-TPbiom+scale_alpha_manual(values=c(.33,1))
TPbiom<-TPbiom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPbiom<-TPbiom+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
TPbiom<-TPbiom+scale_y_continuous(limits=c(0,1.1),breaks=c(0,.25,.5,.75,1.),labels=c(0,.25,.5,.75,1.),expand=c(0,0))
TPbiom<-TPbiom+ggtitle("(c) Aboveground biomass")
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
TPbiom<-TPbiom+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey30"),col="grey30", stroke=1.25)),
                      #alpha = guide_legend("Exclosures",override.aes = list(shape=c(21), size=3.75,alpha=c(0.33,1),fill="grey30",col="grey30", stroke=1.25)),
                        colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                  col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TPbiom

######################################################
#### Plant Biomass modelling ####
######################################################

TransNuts5NA<-TransNuts3[!is.na(TransNuts3$Biomass.g.m2),]
dim(TransNuts5NA) # 148 13 (77 62 without exclosures)

TPbio<-lmer(Biomass.g.m2~landuse+region+transplant+Tagsp+treatment+
              #Tagsp:landuse+Tagsp:region+
              #transplant:Tagsp+landuse:region+
              #transplant:landuse:region+
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
abline(h = 0, lty = 2, col = 1) # OK with gamma (conical gaussian)

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
library(glmmTMB)
min(TransNuts5NA$Biomass.g.m2)
TransNuts5NAz<-TransNuts5NA[!TransNuts5NA$Biomass.g.m2<.1,]
min(TransNuts5NAz$Biomass.g.m2)
TransNuts5NA$Biomass.g.m2[TransNuts5NA$Biomass.g.m2<.1]<-0.1
#levels(TransNuts5NA$region)<-c("Mesic","Wet", "Wet" )
TPbio<-glmmTMB(Biomass.g.m2~transplant+Tagsp+treatment+region+#landuse+
                 #Tagsp:landuse+#Tagsp:region+
                 #transplant:Tagsp+landuse:region+
                 #transplant:landuse:region+
                (1|site/block),
            family=list(family="Gamma",link="logit"),
              na.action=na.fail,TransNuts5NA)

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
abline(h = 0, lty = 2, col = 1) # OK with gamma (conical gaussian)

#TPbio2<-glmmTMB(Biomass.g.m2~transplant+Tagsp+treatment+landuse+
#                   (1|site/block),family=list(family="Gamma",link="logit"),
#                 na.action=na.fail,TransNuts5NA)

# Generate model p-values - change in plant cover
#TPbio2 <- update(TPbio, .~. -Tagsp:landuse)
TPbio3 <- update(TPbio, .~. -treatment)
TPbio4 <- update(TPbio, .~. -region)
TPbio5 <- update(TPbio, .~. -Tagsp)
TPbio6 <- update(TPbio, .~. -transplant)

#anova(TPbio,TPbio2)
anova(TPbio,TPbio3)
anova(TPbio,TPbio4)
anova(TPbio,TPbio5)
anova(TPbio,TPbio6)

#       Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
#TPbio  11 -235.45 -202.48 128.73  -257.45 12.589      1   0.000388 *** # Treatment
#TPbio  11 -235.45 -202.48 128.73  -257.45 6.8718      1   0.008757 ** Region
#TPbio  11 -235.45 -202.48 128.73  -257.45 42.371      4  1.397e-08 ***# Spp
#TPbio  11 -235.45 -202.48 128.73  -257.45 34.744      1   3.76e-09 *** # Transplant

# Rainfall region influence on plant biomass
aggregate(Biomass.g.m2~region, TransNuts5NA,mean)
aggregate(Biomass.g.m2~region, TransNuts5NA,sd)
# region Biomass.g.m2
#1  Mesic    0.1506507
#2    Wet    0.2987374

# Landuse influence on plant biomass
aggregate(Biomass.g.m2~landuse, TransNuts5NA,mean)
aggregate(Biomass.g.m2~landuse, TransNuts5NA,sd)
#landuse Biomass.g.m2
#1 Pasture    0.1675247
#2    Wild    0.4007787
aggregate(Biomass.g.m2~landuse+region, TransNuts5NA,mean)

# Exclosure and plant biomass averages
names(TransNuts5NA)
TransNuts5NAS<-TransNuts5NA[TransNuts5NA$site=="Seronera",]
aggregate(Biomass.g.m2~treatment, TransNuts5NAS,mean)
#treatment Biomass.g.m2
#1  Exclosed    0.6566011
#2      Open    0.3392480
(0.6566011-0.3392480)/0.3392480
0.3392480+(0.3392480/100)*0.9354605
0.3392480/100*194

#Pairwise contrasts *ghlt* and *lsmeans*
library(emmeans)

# Species differences
emm.s.recalmod <- emmeans(TPbio,~Tagsp)
pairs(emm.s.recalmod) # Nothing singificantly different
# Spp differences
#Chl pyc - The tri   -1.251 0.215 137 -5.811  <.0001 
#Chr ori - The tri   -1.162 0.219 137 -5.315  <.0001 
#Cyn dac - The tri   -0.932 0.217 137 -4.303  0.0003 
#Dig mac - The tri   -0.798 0.214 137 -3.726  0.0026 

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
TPs<-TPs+ylab("Probability (%)")+xlab("Landuse")
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
#### Plant Nitrogen concencentrations ####
################################################################################################################################################
#### GRAPH: Nitrogen concentrations ####

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
TNs<-TNs+ylab("Nitrogen concentration (%)")+xlab("")
TNs<-TNs+facet_wrap(~region, ncol=5)
TNs<-TNs+scale_shape_manual(values=c(22,21))
TNs<-TNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TNs<-TNs+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TNs<-TNs+ggtitle("(d) Leaf nitrogen concentration")
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
dim(TransNutsNA) # 93 13 (77 62 without exclosures)

Nlm<-lmer(Plant.N~landuse+region+transplant+Tagsp+treatment+
            Tagsp:region+Tagsp:landuse+
            transplant:Tagsp+
            transplant:landuse:region+
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
levels(TransNutsNA$region)<-c("Mesic","Wet" )
Nlm<-lmer(Plant.N~landuse+region+transplant+#Tagsp+#treatment+
            #Tagsp:region+Tagsp:landuse+
            #transplant:Tagsp+
            transplant:landuse:region+
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

# Interaction plot transplant x species
with(TransNutsNA, {interaction.plot(region,landuse,Plant.N,
                                    xlab = "Land-use",
                                    ylab = "Plant.N (%)",
                                    fun=mean)})

Nlm2<-lmer(Plant.N~landuse+region+transplant+
            (1|site/block),na.action=na.fail,REML=T,TransNutsNA)
  
# Generate model p-values - change in plant cover
Nlm2 <- update(Nlm, .~. -transplant:landuse:region)
Nlm3 <- update(Nlm2, .~. -transplant)
Nlm4 <- update(Nlm2, .~. -landuse)
Nlm5 <- update(Nlm2, .~. -region)

anova(Nlm,Nlm2)
anova(Nlm2,Nlm3)
anova(Nlm2,Nlm4)
anova(Nlm2,Nlm5)

#     Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq) 
#Nlm  11 31.184 59.043  -4.5921    9.184 24.361      4   6.76e-05 *** #transplant:landuse:region
#Nlm2  7 47.545 65.274 -16.773   33.545 2.1344      1      0.144 # transplant
#Nlm2  7 47.545 65.274 -16.773   33.545 0.471      1     0.4925 # landuse
#Nlm2  7 47.545 65.274 -16.773   33.545 4.4411      1    0.03508 * # region

# Emmeans contrast three-way interaction
NlmFINAL <- emmeans(Nlm, pairwise~transplant:Tagsp,type="response") #Creating emmeans across the factor levels in the interaction.
NlmFINAL$emmeans
NlmFINAL.pairs <- pairs(NlmFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
NlmFINAL.pairs$emmeans
plot(NlmFINAL, comparisons = FALSE)

NlmFINAL <- emmeans(Nlm, pairwise~region:transplant:landuse,type="response") #Creating emmeans across the factor levels in the interaction.
NlmFINAL$emmeans
NlmFINAL.pairs <- pairs(NlmFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
NlmFINAL.pairs$emmeans
plot(NlmFINAL, comparisons = FALSE)

# TagSpp contrast
library(MuMIn)
summary(glht(Nlm, mcp(Tagsp="Tukey"))) 
#Estimate Std. Error z value Pr(>|z|)  
#Cyn dac - Chl pyc == 0  0.59212    0.18526   3.196   0.0120 *
#Cyn dac - Chr ori == 0  0.52642    0.20349   2.587   0.0722 .


# Average of combined rainfall region and land-use
aggregate(Plant.N~region+landuse,TransNutsNA,mean)
aggregate(Plant.N~region+landuse,TransNutsNA,sd)

#region landuse   Plant.N
#1  Mesic Pasture 1.2045555
#2    Wet Pasture 1.1797805
#3  Mesic    Wild 1.9647881
#4    Wet    Wild 0.8987178

##############################################################
#### Biomass in relation to plant nitrogen ####
##############################################################

TransNuts5NA<-TransNuts3[!is.na(TransNuts3$Biomass.g.m2) & !is.na(TransNuts3$Plant.N),]
dim(TransNuts5NA) # 90 13

NlmBio<-lmer(Biomass.g.m2~Plant.N+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNuts5NA)

summary(NlmBio)
drop1(NlmBio, test="Chisq")

plot(predict(NlmBio))
     
#Model matrix
MyData <- expand.grid(Plant.N = seq(min(TransNuts5NA$Plant.N),max(TransNuts5NA$Plant.N),length= 50))
head(MyData)
     
#Convert the covariate values into an X matrix
     Xp <- model.matrix(~ Plant.N, data = MyData)
     
#Extract parameters and parameter covariance matrix
betas    <- fixef(NlmBio)
Covbetas <- vcov(NlmBio)

#Calculate the fitted values in the predictor scale
 MyData$eta <- Xp %*% betas
     
     #Calculate the SEs on the scale of the predictor function
     MyData$se    <- sqrt(diag(Xp %*% Covbetas %*% t(Xp)))
     MyData$SeUp  <- exp(MyData$eta + 1.96 *MyData$se) / 
       (1 + exp(MyData$eta  + 1.96 *MyData$se))
     MyData$SeLo  <- exp(MyData$eta - 1.96 *MyData$se) / 
       (1 + exp(MyData$eta  - 1.96 *MyData$se))
     
MyData
MyData<-as.data.frame(MyData)
plot(MyData$eta~MyData$Plant.N ) # Linear decline
     
# Non-linear model for plant biomass and plant N
fit <- nls(Biomass.g.m2~ SSasymp(Plant.N, yf, y0, log_alpha), data = TransNuts5NA)
fit
phm <- nls(Biomass.g.m2~ yf + (y0 - yf)*exp(-b *Plant.N), data = TransNuts5NA, start = list(yf=0.09446,y0=1.47027,b=1.71611317))
summary(phm)
#        yf         y0          b 
#0.09445743 1.47022371 1.71605234
coef(phm)
plot(phm)
predict(phm)

newph <- data.frame(Plant.N=seq(min(TransNuts5NA$Plant.N),max(TransNuts5NA$Plant.N),length= 50))
Discharge.pred <- predict(phm, newdata=newph)
Discharge.pred
#plot(Biomass.g.m2~Plant.N, data =TransNuts5NA)
#lines(newph$Plant.N,Discharge.pred,lty = 1, lwd =1.75, col = "black")
#plot(newph$Plant.N~Discharge.pred) # Follows decay function - plant N diluted at higher biomasses
TNBioNls<-cbind(newph,Discharge.pred )
colnames(TNBioNls)[2]<-"Biomass.g.m2"

# Treatment code for filling symbols
TransNuts3$spp.code<-as.factor(with(TransNuts3, paste(Tagsp,transplant,sep="_")))

# Relevel so that other
levels(TransNuts3$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")

# Plant Biomass and N conc
TransNuts3$spp.code<-as.factor(with(TransNuts3, paste(Tagsp,transplant,sep="_")))
TNBio<-ggplot(TransNuts3,aes(y=Biomass.g.m2,x=Plant.N)) # group = grouping vector for lines
TNBio<-TNBio+geom_point(aes(shape=transplant, colour=Tagsp, fill=spp.code),size=3.5, stroke=1.25)
TNBio<-TNBio+geom_line(data=TNBioNls,aes(y=Biomass.g.m2,x=Plant.N), colour="black",size=1)
TNBio<-TNBio+ylab(expression(paste("Aboveground biomass (kg ",m^-2,")")))+xlab("Leaf nitrogen concentration (%)")
#TNBio<-TNBio+facet_wrap(~region, ncol=5)
TNBio<-TNBio+scale_shape_manual(values=c(22,21))
TNBio<-TNBio+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TNBio<-TNBio+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
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
        ,legend.position = c(.8, .99)
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
TNBio<-TNBio+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey30"),col="grey30", stroke=1.25)),
                    colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                              col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
TNBio

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Transplant experiment/Biomass.PlantN.jpeg",
       width= 11, height = 13,units ="cm",
       dpi = 600, limitsize = TRUE)



# Marginal density plot of x (top panel) and y (right panel)
names(TransNuts3)
xplot <- ggdensity(TransNuts3, "Plant.N",color="landuse", fill = "landuse")
yplot <- ggdensity(TransNuts3, "Biomass.g.m2", color="landuse", fill = "landuse")+ rotate()

xplot <- ggdensity(TransNuts3, "Plant.N",color="region", fill = "region")
yplot <- ggdensity(TransNuts3, "Biomass.g.m2", color="region", fill = "region")+ rotate()


# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()

detach(package:egg)
# Arranging the plot
ggarrange(xplot, NULL, TNBio, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, .75), heights = c(.75, 2),
          common.legend = TRUE)


################################################################################################
#### CARBON TO NITROGEN RATIO ####
################################################################################################

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
TCNs<-TCNs+ylab("C:N ratio")+xlab("Landuse")
TCNs<-TCNs+facet_wrap(~region, ncol=5)
TCNs<-TCNs+scale_shape_manual(values=c(22,21))
TCNs<-TCNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TCNs<-TCNs+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TCNs<-TCNs+ggtitle("(e) C:N ratio")
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

#####################################################################
##### Plant nitrogen to carbon ratio - modelling ####
#####################################################################

TransNutsCNA<-TransNuts3[!is.na(TransNuts3$Plant.CN),]
dim(TransNutsCNA) # 90 14

CNlm<-lmer(Plant.CN~landuse+region+transplant+Tagsp+treatment+
             Tagsp:region+Tagsp:landuse+
             transplant:Tagsp+
             transplant:landuse:region+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNutsCNA)

summary(CNlm)
drop1(CNlm, test="Chisq")

# Model averaging
modsetlmer_CNlm <- dredge(CNlm,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_CNlm) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_CNlm<-model.avg(modsetlmer_CNlm)
importance(modavglmer_modsetlmer_CNlm)
PlantCNimportance<-as.data.frame(importance(modavglmer_modsetlmer_CNlm))
plot(PlantCNimportance)           

# Reduced CN ratio model
CNlm<-lmer(Plant.CN~landuse+region+transplant+Tagsp+#treatment+
             #Tagsp:region+Tagsp:landuse+
             transplant:Tagsp+
             transplant:landuse:region+
            (1|site/block),
          na.action=na.fail,
          REML=T,TransNutsCNA)

summary(CNlm)
drop1(CNlm, test="Chisq")

# Checking resids vs fit
E1 <- resid(CNlm, type = "pearson")
F1 <- fitted(CNlm)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Very good

# Interaction plot transplant x species
with(TransNutsCNA, {interaction.plot(transplant,Tagsp,Plant.CN,
                                    xlab = "Treatment",
                                    ylab = "Plant.CN",
                                    fun=mean)})

# Generate model p-values - change in plant cover
Nlm2 <- update(Nlm, .~. -transplant:landuse:region)
Nlm3 <- update(Nlm2, .~. -transplant)
Nlm4 <- update(Nlm2, .~. -landuse)
Nlm5 <- update(Nlm2, .~. -region)

anova(Nlm,Nlm2)
anova(Nlm2,Nlm3)
anova(Nlm2,Nlm4)
anova(Nlm2,Nlm5)

# Generate model p-values - change in C:N ratio 
CNlm2 <- update(CNlm, .~. -transplant:landuse:region)
CNlm3 <- update(CNlm2, .~. -transplant:Tagsp)
CNlm4 <- update(CNlm2, .~. -landuse)
CNlm5 <- update(CNlm2, .~. -region)
CNlm6 <- update(CNlm3, .~. -transplant)
CNlm7 <- update(CNlm3, .~. -Tagsp)

anova(CNlm,CNlm2)
anova(CNlm2,CNlm3)
anova(CNlm2,CNlm4)
anova(CNlm2,CNlm5)
anova(CNlm3,CNlm6)
anova(CNlm3,CNlm7)

#      Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq) 
#CNlm  16 704.95 744.94 -336.47   672.95 12.09      1  0.0005069 *** #transplant:landuse:region
#CNlm2 15 715.04 752.53 -342.52   685.04 13.326      4   0.009789 ** #transplant:Tagsp
#CNlm2 15 715.04 752.53 -342.52   685.04 0.6465      1     0.4214  #landuse
#CNlm2 15 715.04 752.53 -342.52   685.04 4.6006      1    0.03196 * #region
#CNlm3 11 720.36 747.86 -349.18   698.36 3.1857      1    0.07429 .#transplant
#CNlm3 11 720.36 747.86 -349.18   698.36 6.0929      4     0.1923 #Tagsp

# Emmeans contrast three-way interaction
CNlmFINAL <- emmeans(CNlm, pairwise~transplant:Tagsp,type="response") #Creating emmeans across the factor levels in the interaction.
CNlmFINAL$emmeans
CNlmFINAL.pairs <- pairs(CNlmFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CNlmFINAL.pairs$emmeans
plot(CNlmFINAL, comparisons = FALSE)

CNlmFINAL <- emmeans(CNlm, pairwise~transplant:landuse:region,type="response") #Creating emmeans across the factor levels in the interaction.
CNlmFINAL$emmeans
CNlmFINAL.pairs <- pairs(CNlmFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
CNlmFINAL.pairs$emmeans
plot(CNlmFINAL, comparisons = FALSE)

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
#### SERONERA EXCLOSURES ONLY ####
########################################################################

# Seronera only data
Sero<-TransNuts3[TransNuts3$site=="Seronera",]

# Relevel species
levels(Sero$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
Sero$Tagsp<- factor(Sero$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
Sero$spp.code<-as.factor(with(Sero, paste(Tagsp,transplant,sep="_")))

# Seronera means
names(Sero)
# Change in plant cover
SeroCC<-aggregate(CoverChange~transplant+Tagsp+treatment+spp.code,Sero,mean)
SeroCCsd<-aggregate(CoverChange~transplant+Tagsp+treatment+spp.code,Sero,sd)
SeroCC$sd<-SeroCCsd$CoverChange
#SeroCC$Plant.measure<-"CoverChange"
#colnames(SeroCC)[4]<-"Value"
# Final plant cover
SeroFC<-aggregate(FilTagspCover~transplant+Tagsp+treatment+spp.code,Sero,mean)
SeroFCsd<-aggregate(FilTagspCover~transplant+Tagsp+treatment+spp.code,Sero,sd)
SeroFC$sd<-SeroFCsd$FilTagspCover
#SeroFC$Plant.measure<-"Finalcover"
#colnames(SeroFC)[4]<-"Value"
# Plant biomass
SeroBio<-aggregate(Biomass.g.m2~transplant+Tagsp+treatment+spp.code,Sero,mean)
SeroBiosd<-aggregate(Biomass.g.m2~transplant+Tagsp+treatment+spp.code,Sero,sd)
SeroBio$sd<-SeroBiosd$Biomass.g.m2
#SeroBio$Plant.measure<-"PlantBiomass"
#colnames(SeroBio)[4]<-"Value"
# Plant nitrogen
SeroN<-aggregate(Plant.N~transplant+Tagsp+treatment+spp.code,Sero,mean)
SeroNsd<-aggregate(Plant.N~transplant+Tagsp+treatment+spp.code,Sero,sd)
SeroN$sd<-SeroNsd$Plant.N
#SeroN$Plant.measure<-"PlantNitrogen"
#colnames(SeroN)[4]<-"Value"
# Plant CN ratio
SeroCN<-aggregate(Plant.CN~transplant+Tagsp+treatment+spp.code,Sero,mean)
SeroCNsd<-aggregate(Plant.CN~transplant+Tagsp+treatment+spp.code,Sero,sd)
SeroCN$sd<-SeroCNsd$Plant.CN
#SeroCN$Plant.measure<-"CNratio"
#colnames(SeroCN)[4]<-"Value"

# Combine datasets
#SeroMean<-rbind(SeroCC,SeroFC,SeroBio,SeroN,SeroCN)
# Relevel 
#SeroMean$Plant.measure<-as.factor(SeroMean$Plant.measure)
#levels(SeroMean$Plant.measure)
#SeroMean$Plant.measure<- factor(SeroMean$Plant.measure, levels = c(
#  "CoverChange","Finalcover","PlantBiomass","PlantNitrogen","CNratio"))
#SeroMean$f2 <- factor(SeroMean$Plant.measure, labels = c("Plant cover change (%)", "Relative cover (%)",expression(paste("Biomass (kg ",m^-2,")")), "Nitrogen (%)", "C:N ratio"))

# Seronera exclosure graphs: Change in cover, Final cover, biomass, plant N and CN ratio
# Seronera - Change in plant cover
SerCC<-ggplot(SeroCC,aes(y=CoverChange,x=treatment, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
SerCC<-SerCC+geom_hline(yintercept=0,linetype="dashed")
SerCC<-SerCC+geom_errorbar(data=SeroCC,aes(ymin=CoverChange-sd, ymax=CoverChange+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
SerCC<-SerCC+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
SerCC<-SerCC+ylab("Change in plant cover (%)")+xlab("")
#TPbiom<-TPbiom+ylab(expression(paste("Biomass (kg ",m^-2,")")))+xlab("")
SerCC<-SerCC+scale_shape_manual(values=c(22,21))
SerCC<-SerCC+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
SerCC<-SerCC+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","white","orangered3"))
#SerCC<-SerCC+scale_y_continuous(limits=c(-.1,1.85),breaks=c(0,.5,1.,1.5),labels=c(0,.5,1.,1.5),expand=c(0,0))
SerCC<-SerCC+ggtitle("(a) Change in plant cover")
SerCC<-SerCC+theme_bw()+
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
        ,strip.text =  element_blank()
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(-0.25, "mm")
        ,legend.key.width = unit(1.2,"cm"))
SerCC<-SerCC+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey30"),col="grey30", stroke=1.25)),
                      colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
SerCC<-SerCC+guides(fill=F,linetype=F,shape =F, colour=F)
SerCC

# Final plant cover graph
SeroF<-ggplot(SeroFC,aes(y=FilTagspCover,x=treatment, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
SeroF<-SeroF+geom_errorbar(data=SeroFC,aes(ymin=FilTagspCover-sd, ymax=FilTagspCover+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
SeroF<-SeroF+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
SeroF<-SeroF+ylab("Plant cover (%)")+xlab("")
SeroF<-SeroF+scale_shape_manual(values=c(22,21))
SeroF<-SeroF+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
SeroF<-SeroF+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","white","orangered3"))
SeroF<-SeroF+scale_y_continuous(limits=c(-3,102),breaks=c(0,25,50,75,100),labels=c(0,25,50,75,100),expand=c(0,0))
SeroF<-SeroF+ggtitle("(b) Final plant cover")
SeroF<-SeroF+theme_bw()+
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
SeroF<-SeroF+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1.25)),
                        colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                  col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
SeroF<-SeroF+guides(fill=F,linetype=F,shape = F, colour=F)
SeroF

# Seronera plant biomass
Serob<-ggplot(SeroBio,aes(y=Biomass.g.m2,x=treatment, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
Serob<-Serob+geom_errorbar(data=SeroBio,aes(ymin=Biomass.g.m2-sd, ymax=Biomass.g.m2+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
Serob<-Serob+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
Serob<-Serob+ylab(expression(paste("Biomass (kg ",m^-2,")")))+xlab("")
Serob<-Serob+scale_shape_manual(values=c(22,21))
Serob<-Serob+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
Serob<-Serob+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","white","orangered3"))
Serob<-Serob+scale_y_continuous(limits=c(-.1,1.85),breaks=c(0,.5,1.,1.5),labels=c(0,.5,1.,1.5),expand=c(0,0))
Serob<-Serob+ggtitle("(c) Aboveground biomass")
Serob<-Serob+theme_bw()+
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
Serob<-Serob+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey30"),col="grey30", stroke=1.25)),
                      colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )
Serob

# Seronera Plant Nitrogen concentrations graph
SeroNs<-ggplot(SeroN,aes(y=Plant.N,x=treatment, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
SeroNs<-SeroNs+geom_errorbar(data=SeroN,aes(ymin=Plant.N-sd, ymax=Plant.N+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
SeroNs<-SeroNs+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
SeroNs<-SeroNs+ylab("Nitrogen concentration (%)")+xlab("")
SeroNs<-SeroNs+scale_shape_manual(values=c(22,21))
SeroNs<-SeroNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
SeroNs<-SeroNs+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","white","orangered3"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
SeroNs<-SeroNs+ggtitle("(d) Leaf nitrogen concentration")
SeroNs<-SeroNs+theme_bw()+
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
SeroNs<-SeroNs+guides(fill=F,linetype=F,shape =F, colour=F)
SeroNs

# Seronera Plant Carbon-to-Nitrogen ratio graph
SeroCNs<-ggplot(SeroCN,aes(y=Plant.CN,x=treatment, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
SeroCNs<-SeroCNs+geom_errorbar(data=SeroCN,aes(ymin=Plant.CN-sd, ymax=Plant.CN+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
SeroCNs<-SeroCNs+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
SeroCNs<-SeroCNs+ylab("C:N ratio")+xlab("Herbivore exclosure (Seronera only)")
SeroCNs<-SeroCNs+scale_shape_manual(values=c(22,21))
SeroCNs<-SeroCNs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
SeroCNs<-SeroCNs+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","white","orangered3"))
#TNs<-TNs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
SeroCNs<-SeroCNs+ggtitle("(e) C:N ratio")
SeroCNs<-SeroCNs+theme_bw()+
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
SeroCNs<-SeroCNs+guides(fill=F,linetype=F,shape =F, colour=F)
SeroCNs

#### Combining Seronera graphs ####

# Extra legend from legend plot
library(grid)
library(gridExtra)
library(ggpubr)
library(egg)
legend <- get_legend(Serob)
arrangeGrob(legend)

filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/", "Sero.Excl", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=18, height=37.6, res=400, unit="cm")
egg::ggarrange(SerCC,SeroF,Serob,SeroNs,SeroCNs, ncol=1) #common.legend = T,legend="right")
dev.off()


##### Seronera only linear model ####
names(Sero)
SeroCovMod<-lm(FilTagspCover~treatment+Tagsp+treatment:Tagsp,Sero)
anova(SeroCovMod)
plot(SeroCovMod)
drop1(SeroCovMod, test="Chisq") # Interaction NS

SeroBioMod<-lm(Biomass.g.m2~treatment+Tagsp+treatment:Tagsp,Sero)
anova(SeroBioMod)
plot(SeroBioMod)
drop1(SeroBioMod, test="Chisq") # Interaction NS

########################################################################
#### SOIL PROPERTIES ####
########################################################################

levels(TP2$region)<-c("Mesic","Wet","Wet")


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
TPBD<-TPBD+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25,show.legend=F)
TPBD<-TPBD+ylab(expression(paste("Soil bulk density (g ",cm^-3,")")))+xlab("Landuse")
TPBD<-TPBD+facet_wrap(~region, ncol=5)
TPBD<-TPBD+scale_shape_manual(values=c(22,21))
TPBD<-TPBD+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPBD<-TPBD+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
TPBD<-TPBD+ggtitle("(b) Bulk density")
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
        ,strip.text =  element_blank()
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

#### Bulk density modelling ####
TP2BDNA<-TP2[!is.na(TP2$BD.gcm3),]
BDlm<-lmer(BD.gcm3~landuse+region+transplant+Tagsp+treatment+
             Tagsp:region+Tagsp:landuse+
             transplant:Tagsp+landuse:region+
             transplant:landuse:region+
             (1|site/block),
           na.action=na.fail,
           REML=T,TP2BDNA)
summary(BDlm)
drop1(BDlm, test="Chisq")

# Model averaging
modsetlmer_BDlm <- dredge(BDlm,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_BDlm) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_BDlm<-model.avg(modsetlmer_BDlm)
importance(modavglmer_modsetlmer_BDlm)
PlantBDimportance<-as.data.frame(importance(modavglmer_modsetlmer_BDlm))
plot(PlantBDimportance)           

# Reduced Soil pH  model
BDlm<-lmer(BD.gcm3~region+Tagsp+transplant+#landuse+treatment+
             #Tagsp:landuse+Tagsp:region+
             transplant:Tagsp+#landuse:region+
             # transplant:landuse:region+
             (1|site/block),
           na.action=na.fail,
           REML=T,TP2BDNA)

summary(BDlm)
drop1(BDlm, test="Chisq")

# Checking resids vs fit
E1 <- resid(BDlm, type = "pearson")
F1 <- fitted(BDlm)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Good


BDlm2<-lmer(pH~region+Tagsp+transplant+(1|site/block),na.action=na.fail,REML=T,TP2pHNA)

# Generate model p-values - bulk density
BDlm2 <- update(BDlm, .~. -transplant:Tagsp)
BDlm3 <- update(BDlm2, .~. -region)
BDlm4 <- update(BDlm2, .~. -Tagsp)
BDlm5 <- update(BDlm2, .~. -transplant)

anova(BDlm,BDlm2)
anova(BDlm2,BDlm3)
anova(BDlm2,BDlm4)
anova(BDlm2,BDlm5)

# Soil pH analysis 
#     Df    AIC    BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)  
#BDlm  14 -272.79 -231.91 150.39  -300.79 15.197      4   0.004309 ** # transplant:Tagsp
#BDlm2 10 -265.59 -236.39 142.79  -285.59 7.9792      1   0.004732 ** # region
#BDlm2 10 -265.59 -236.39 142.79  -285.59 2.3933      4     0.6638 # Tagsp
#BDlm2 10 -265.59 -236.39 142.79  -285.59 0.7062      1     0.4007 # transplant


# Emmeans contrast
BDlmFinFINAL <- emmeans(BDlm, pairwise~transplant:Tagsp,type="response") #Creating emmeans across the factor levels in the interaction.
BDlmFinFINAL$emmeans
BDlmFinFINAL.pairs <- pairs(BDlmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
BDlmFinFINAL.pairs$emmeans
plot(pHlmFinFINAL, comparisons = FALSE)

#Tagsp   transplant contrast             estimate     SE    df t.ratio p.value
#Chl pyc .          Control - Transplant  0.19673 0.0536 115.6  3.668  0.0093 
#.       Control    Chl pyc - The tri     0.22404 0.0677 112.4  3.311  0.0312 


#### Soil  pH ####

# Graph soil pH
pH<-aggregate(pH~landuse+region+transplant+Tagsp,TP2, mean)
pHSe<-aggregate(pH~landuse+region+transplant+Tagsp,TP2, sd)
pH$sd<-pHSe$pH

# Relevel spp
levels(pH$Tagsp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
pH$Tagsp<- factor(pH$Tagsp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Treatment code for filling symbols
pH$spp.code<-as.factor(with(pH, paste(Tagsp,transplant,sep="_")))


levels(pH$region)<-c("Mesic region","Wet region")

# Soil pH  graph
TPpH<-ggplot(pH,aes(y=pH,x=landuse, shape=transplant, colour=Tagsp,fill=spp.code)) # group = grouping vector for lines
TPpH<-TPpH+geom_errorbar(data=pH,aes(ymin=pH-sd, ymax=pH+sd),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPpH<-TPpH+geom_point(position=position_dodge(width=.45),size=5, stroke=1.25)
TPpH<-TPpH+ylab("pH")+xlab("")#+xlab("Land-use")
TPpH<-TPpH+facet_wrap(~region, ncol=5)
TPpH<-TPpH+scale_shape_manual(values=c(22,21))
TPpH<-TPpH+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
TPpH<-TPpH+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
TPpH<-TPpH+ggtitle("(a) pH")
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
        ,axis.text.x=element_blank()
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

#### pH modelling ####
TP2pHNA<-TP2[!is.na(TP2$pH),]
pHlm<-lmer(pH~landuse+region+transplant+Tagsp+treatment+
             Tagsp:region+Tagsp:landuse+
             transplant:Tagsp+landuse:region+
             transplant:landuse:region+
            (1|site/block),
          na.action=na.fail,
          REML=T,TP2pHNA)
summary(pHlm)
drop1(pHlm, test="Chisq")

# Model averaging
modsetlmer_pHlm <- dredge(pHlm,trace=2) #,fixed=c("Season","Region","Landuse","Treatment","C.N","Temp","Sand")) 
model.sel(modsetlmer_pHlm) #Model selection table giving AIC, deltaAIC and weighting
modavglmer_modsetlmer_pHlm<-model.avg(modsetlmer_pHlm)
importance(modavglmer_modsetlmer_pHlm)
PlantpHimportance<-as.data.frame(importance(modavglmer_modsetlmer_pHlm))
plot(PlantpHimportance)           

# Reduced Soil pH  model
pHlm<-lmer(pH~landuse+region+Tagsp+#transplant+treatment+
             #Tagsp:landuse+Tagsp:region+
            #transplant:Tagsp+landuse:region+
            # transplant:landuse:region+
             (1|site/block),
           na.action=na.fail,
           REML=T,TP2pHNA)

summary(pHlm)
drop1(pHlm, test="Chisq")

# Checking resids vs fit
E1 <- resid(pHlm, type = "pearson")
F1 <- fitted(pHlm)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Good


pHlm2<-lmer(pH~landuse+region+(1|site/block),na.action=na.fail,REML=T,TP2pHNA)

# Generate model p-values - change in plant cover
pHlm2 <- update(pHlm, .~. -Tagsp)
pHlm3 <- update(pHlm2, .~. -region)
pHlm4 <- update(pHlm2, .~. -landuse)

anova(pHlm,pHlm2)
anova(pHlm2,pHlm3)
anova(pHlm2,pHlm4)

# Soil pH analysis 
#     Df    AIC    BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)  
#pHlm  10 104.36 133.49 -42.181   84.361 67.65      4  7.111e-14 *** #Tagsp
#pHlm2  6 164.01 181.49 -76.006   152.01 7.2908      1   0.006931 ** # region
#pHlm2  6 164.01 181.49 -76.006   152.01 4.0011      1    0.04547 * # landuse

# Emmeans contrast
pHlmFinFINAL <- emmeans(pHlm, pairwise~Tagsp,type="response") #Creating emmeans across the factor levels in the interaction.
pHlmFinFINAL$emmeans
pHlmFinFINAL.pairs <- pairs(pHlmFinFINAL,simple = "each", combine =TRUE) # THIS IS A GREATE OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
pHlmFinFINAL.pairs$emmeans
plot(pHlmFinFINAL, comparisons = FALSE)

# Soil pH species differences
#contrast          estimate     SE  df t.ratio p.value
#Chl pyc - Dig mac    0.365 0.0872 119  4.184  0.0006 
#Chl pyc - The tri    0.501 0.0915 116  5.476  <.0001 
#Chr ori - Cyn dac   -0.399 0.0908 113 -4.390  0.0003 
#Chr ori - The tri    0.310 0.0898 113  3.449  0.0079 
#Cyn dac - Dig mac    0.572 0.0827 116  6.916  <.0001 
#Cyn dac - The tri    0.708 0.0880 113  8.046  <.0001 

aggregate(pH~landuse, TP2pHNA,mean)
#  landuse       pH
#1 Pasture 6.204255
#2    Wild 6.346404

aggregate(pH~region, TP2pHNA,mean)
#region       pH
#1  Mesic 6.506170
#2    Wet 6.186966

# Correlations
MyEnv<-c("Target.weight.g","FilTagspCover","Plant.N" ,        
         "BD.gcm3", "pH")
pairs(TP2[,MyEnv], lower.panel= panel.cor)

plot(Plant.N~pH,col=c(Tagsp),TP2)
abline(lm(Plant.N~pH,TP2pHNA))
summary(lm(Plant.N~pH,TP2pHNA))

aggregate(pH~Tagsp,TP2pHNA,mean)
#    Tagsp       pH
#1 Chl pyc 6.476522
#2 Chr ori 6.254167
#3 Cyn dac 6.730385
#4 Dig mac 6.154167
#5 The tri 5.956667

aggregate(pH~landuse,TP2pHNA,mean)
aggregate(pH~region,TP2pHNA,mean)
#### Combining soil property graphs ####

# Extra legend from legend plot
library(grid)
library(gridExtra)
library(ggpubr)
library(egg)
legend2 <- get_legend(TPpH)
arrangeGrob(legend2)

filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/", "Transplant.Soil.properties", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=16, height=22, res=400, unit="cm")
egg::ggarrange(TPpH, TPBD,ncol=1) #common.legend = T,legend="right")
dev.off()


################################################################################################################################################
#### HOME FIELD ADVANTAGE ####
################################################################################################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/")
# Combined species, aboveground biomass and 5 cm soil
HFA<-read.csv(file="Transplant experiment/HFA.csv", sep=",",header=TRUE)

names(HFA)
str(HFA)

# Rename species and relevel spp
levels(HFA$Spp)<- c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")
HFA$Spp<- factor(HFA$Spp, levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda"))

# Rename and relevel plant.measure
levels(HFA$Plant.measure)<-c("Plant biomass", "Plant cover", "Plant nitrogen")
HFA$Plant.measure<- factor(HFA$Plant.measure, levels = c("Plant cover","Plant biomass", "Plant nitrogen"))

# Treatment code for filling symbols
HFA$spp.code<-as.factor(with(HFA, paste(Spp,HFAfactor,sep="_")))
HFA$spp.code.measure<-as.factor(with(HFA, paste(Spp,HFAfactor,Plant.measure,sep="_")))

levels(HFA$Plant.measure)<-c("(a) Plant cover", "(b) Plant biomass", "(c) Leaf nitrogen concentration")

# Home Field Advantage - 
HFABar<-ggplot(HFA,aes(y=HA,x=Spp, colour=Spp,shape=HFAfactor,fill=spp.code)) # group = grouping vector for lines
HFABar<-HFABar+geom_hline(yintercept=0,linetype="solid", colour="black")
HFABar<-HFABar+geom_bar(stat="identity",width=.01,linetype="dashed",position=position_dodge(width=.5), alpha=.5,show.legend=F)
HFABar<-HFABar+geom_point(size=4,position=position_dodge(width=.5), stroke =1)
HFABar<-HFABar+facet_wrap(~Plant.measure, scale="free", ncol=3)
HFABar<-HFABar+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
HFABar<-HFABar+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
HFABar<-HFABar+scale_shape_manual(values=c(21,21))
HFABar<-HFABar+xlab("Species")+ylab("Homefield advantage")
HFABar<-HFABar+coord_flip()
HFABar<-HFABar+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=12.5,color="black")
        ,axis.title.x=element_text(size=12.5,color="black")
        ,axis.text.x=element_text(size=12,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.25, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,legend.text = element_text(size=12,color="black")
        ,legend.title = element_text(size=12,color="black")
        ,strip.text = element_text(size=12,color="black",hjust = 0)
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.spacing.x = unit(1, "mm")
        ,legend.key.width = unit(1,"cm"))
HFABar<-HFABar+guides(fill=F,linetype=F, shape = guide_legend("HFA origin",override.aes = list(shape=c(21,21), size=3.75,fill=c("white","grey"),col="grey", stroke=.75, alpha=1)),
                  colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                            col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=.75)) )
HFABar

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/HFABar.jpeg",
       width= 32, height = 11,units ="cm",
       dpi = 600, limitsize = TRUE)

# Average HFA
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
aggregate(HA~Plant.measure+HFAfactor, HFA,mean)
aggregate(HA~Plant.measure+HFAfactor, HFA,se)
#   Plant.measure   HFAfactor          HA
#1    Plant cover    Land-use -22.9833339
#2  Plant biomass    Land-use   0.5464801
#3 Plant nitrogen    Land-use  -0.9161356
#4    Plant cover Rain region 119.6666669
#5  Plant biomass Rain region   0.8278415
#6 Plant nitrogen Rain region   0.5510434

aggregate(HA~Spp+Plant.measure, HFA,mean)

HFAcov<-HFA[HFA$Plant.measure=="Plant cover",]
HFAbio<-HFA[HFA$Plant.measure=="Plant biomass",]
HFAN<-HFA[HFA$Plant.measure=="Plant nitrogen",]

aggregate(HA~HFAfactor, HFAcov,mean)
#HFAfactor        HA
#1    Land-use -22.98333
#2 Rain region 119.66667

aggregate(HA~HFAfactor, HFAN,mean)
#HFAfactor         HA
#1    Land-use -0.9161356
#2 Rain region  0.5510434

################################################################################################################################################
#### TEMPORAL PRODUCTIVITY, CONSUMPTION AND PLANT N ####
################################################################################################################################################

# Use moveable exclosure data as CONTROL 
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Transplant experiment/")

Datastack <-read.csv("BiomassStacked2.csv")
names(Datastack)

# Removing Ex2 - separate analysis
Datastack <- droplevels(Datastack[Datastack$treatment!="EX2",]) #Removing Mesh exclosures  #300 obs
Datastack <- droplevels(Datastack[Datastack$harvest!="H0",]) #removing H0                #280 obs

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
names(Datastack)
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

# Rdate create month column. default was (="%d.%m.%Y")
Rdate<-strptime(as.character(Datastack$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )# East African time #USE
class(Rdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
#Datastack$Rdate<-Rdate# Add to the dataframe #
# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
Datastack$YrMonth<-format(as.Date(Rdate), "%Y-%m")
class(Datastack$YrMonth)

#Renaming levels in region, landuse and treatment columns
levels(Datastack$region)<-c("Dry Region","Wet Region","Wet Region")
levels(Datastack$landuse)<-c("Pasture","Wildlife protected")
levels(Datastack$treatment)<-c("Exclosed","Open")

# Target species only
DatastackT <- droplevels(Datastack[Datastack$pool=="target",])

# Removing very negative outliers
dotchart(DatastackT$prodsp.per)
plot(DatastackT$prodsp.per)
#identify(DatastackT$prodsp.per) #9 111 145 275
DatastackT[c(9,11,145,275),]
#WET_P_1_EX_H1
#WET_P_2_EX_H1
#DRY_P_1_EX_H4
#SE_2_EX_H7
plot(DatastackT$conssp.per)
#identify(DatastackT$conssp.per) #115
DatastackT[c(115),]
#SE_2_EX_H3 

#DatastackT2 <- droplevels(DatastackT[!DatastackT$treatment=="OP" & DatastackT$prodsp>-2 & DatastackT$conssp>-1,])
DatastackT2 <- droplevels(DatastackT[!DatastackT$treatment=="OP" & DatastackT$prodsp.per>-4& DatastackT$conssp.per>-.05,])


# Average plant cover
names(DatastackT2)
# target.sp.cover.setup  & target.sp.cover.harvest..
mean(c(DatastackT2$target.sp.cover.setup, DatastackT2$target.sp.cover.harvest..), na.rm=T)
min(c(DatastackT2$target.sp.cover.setup, DatastackT2$target.sp.cover.harvest..), na.rm=T)
max(c(DatastackT2$target.sp.cover.setup, DatastackT2$target.sp.cover.harvest..), na.rm=T)

sd(c(DatastackT2$target.sp.cover.setup, DatastackT2$target.sp.cover.harvest..), na.rm=T)
# Mean = 22.56338 & Sd =19.98952
# Min = 1 - 92% 

#######################################################################################
#### Plot Target NAP and CON seasonal WEIGHTED #### 
#dotchart(DatastackT2$prodsp)
#dotchart(DatastackT2$conssp)
dotchart(DatastackT2$prodsp.per)
dotchart(DatastackT2$conssp.per)

DatastackT2[DatastackT2$Tagsp=="Chloris",]

AvgprodWT<-aggregate(prodsp.per~YrMonth+landuse+region+site.id+Tagsp,DatastackT2,na.rm=T,mean)
AvgprodWTcount<-DatastackT2 %>% group_by(YrMonth,landuse,region,site.id,Tagsp) %>% summarise(number = n())
AvgprodWTsd<-aggregate(prodsp.per~YrMonth+landuse+region+site.id+Tagsp,DatastackT2,na.rm=T,sd)
AvgprodWT$sd<-AvgprodWTsd$prodsp.per
#AvgprodWT$count<-AvgprodWTcount$number
max(AvgprodWT$prodsp.per) 
# 1.851663 - # uncorrected
#7.706863 - corrected
hist(AvgprodWT$prodsp.per)

AvgconWT<-aggregate(conssp.per~YrMonth+landuse+region+site.id+Tagsp,DatastackT2,na.rm=T,mean)
AvgconWTsd<-aggregate(conssp.per~YrMonth+landuse+region+site.id+Tagsp,DatastackT2,na.rm=T,sd)
AvgconWT$sd<-AvgconWTsd$conssp.per
max(AvgconWT$conssp) 
colnames(AvgconWT)[6]<-"prodsp.per"
#1.067744 # uncorrected
# 5.465115 # corrected
hist(AvgconWT$prodsp.per)

# Rainfall
DatastackT2$rain.day <- DatastackT2$rain.sum/DatastackT2$growth.period

AvgprodWT$YrMonth<-as.Date(paste(AvgprodWT$YrMonth,"-01",sep=""))
AvgconWT$YrMonth<-as.Date(paste(AvgconWT$YrMonth,"-01",sep=""))
DatastackT2$YrMonth<-as.Date(paste(DatastackT2$YrMonth,"-01",sep=""))

AvgprodWT$spp.code<-as.factor(with(AvgprodWT, paste(site.id,Tagsp,sep="_")))
AvgconWT$spp.code<-as.factor(with(AvgconWT, paste(site.id,Tagsp,sep="_")))
DatastackT2$spp.code<-as.factor(with(DatastackT2, paste(site.id,Tagsp,sep="_")))

# Relevel so that other
levels(AvgprodWT$Tagsp)<- c("Chloris", "Chrysochloa", "Cynodon", "Digitaria", "Themeda")
levels(AvgconWT$Tagsp)<- c("Chloris", "Chrysochloa", "Cynodon", "Digitaria", "Themeda")
levels(DatastackT2$Tagsp)<- c("Chloris", "Chrysochloa", "Cynodon", "Digitaria", "Themeda")

# Panel titles
AvgprodWT$Panel.titles<-AvgprodWT$Tagsp
AvgconWT$Panel.titles<-AvgconWT$Tagsp
DatastackT2$Panel.titles<-DatastackT2$Tagsp

levels(AvgprodWT$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")
levels(AvgconWT$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")
levels(DatastackT2$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")

AvgprodWT$Panel.titles<- factor(AvgprodWT$Panel.titles, levels = c("Mesic pastures","Wet pasture", "Mesic wild", "Wet wild","Wet wild Seronera"))
AvgconWT$Panel.titles<- factor(AvgconWT$Panel.titles, levels = c("Mesic pastures", "Wet pasture","Mesic wild", "Wet wild","Wet wild Seronera"))
DatastackT2$Panel.titles<- factor(DatastackT2$Panel.titles, levels = c("Mesic pastures", "Wet pasture","Mesic wild","Wet wild","Wet wild Seronera"))

# Productivity vs consumption symbols
AvgprodWT$ProdCON<-AvgprodWT$landuse
AvgconWT$ProdCON<-AvgconWT$landuse
DatastackT2$ProdCON<-DatastackT2$landuse

levels(AvgprodWT$ProdCON)<-c("Productivity", "Consumption")
levels(AvgconWT$ProdCON)<-c("Productivity", "Consumption")
levels(DatastackT2$ProdCON)<-c("Productivity", "Consumption")

DatastackT2<-DatastackT2[!is.na(DatastackT2$ProdCON),]

# Scale factor
scaleFactor <- mean(DatastackT2$rain.sum,na.rm=T)/mean(AvgprodWT$prodsp,na.rm=T)
scaleFactor 
max(Datastack$rain.sum,na.rm=T)/55

max(AvgprodWT$prodsp.per)
min(AvgprodWT$prodsp.per)

# Productivity and consumption per target spp
NAPdom<- ggplot(AvgprodWT, aes(x=YrMonth, y=prodsp.per, colour=Tagsp, group=spp.code, shape=ProdCON))
NAPdom<-NAPdom+ geom_line(data=DatastackT2,aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)
NAPdom<-NAPdom+ geom_point(data=DatastackT2,aes(y = rain.sum/70),colour="dark blue",size=.9,alpha=.1)
NAPdom<-NAPdom+ geom_hline(yintercept = 0, size =.5, linetype="dashed", colour="black")
NAPdom<-NAPdom+geom_line(linetype="dashed",size=1.2, alpha=.5, show.legend=F)
NAPdom<-NAPdom+geom_errorbar(aes(ymin=prodsp.per-sd, ymax=prodsp.per+sd),position=position_dodge(width=.25),linetype="solid",width=.2,lwd=1.1,show.legend=F)
NAPdom<-NAPdom+geom_point(aes(shape=treatment),position=position_dodge(width=.25),shape=22, size=4, fill="white", stroke=1)
NAPdom<-NAPdom+geom_line(data=AvgconWT,linetype="dashed",size=1.2, alpha=.5, show.legend=F)
NAPdom<-NAPdom+geom_errorbar(data=AvgconWT,aes(ymin=prodsp.per-sd, ymax=prodsp.per+sd),linetype="solid",width=.2,lwd=1.1,show.legend=F)
NAPdom<-NAPdom+geom_point(data=AvgconWT,aes(colour=Tagsp,fill=Tagsp),shape=21, size=3, stroke=1)
NAPdom<-NAPdom+facet_wrap(~Panel.titles,ncol=1,scales='fixed', drop=F)
NAPdom<-NAPdom+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
NAPdom<-NAPdom+scale_y_continuous(limits=c(-2.5,8.75),sec.axis = sec_axis(~ . *70, breaks = c(0,200,400,600), labels = c(0,200,400,600), name = "Precipitation (mm)" )) #"Precipitation (mm)"
NAPdom<-NAPdom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
NAPdom<-NAPdom+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
NAPdom<-NAPdom+ xlab("Month|Year") + ylab(expression(paste("Productivity & consumption (g ",m^-2," ",day^-1,")")))
NAPdom<-NAPdom+ggtitle("(a) Productivity and consumption")
NAPdom<-NAPdom+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(hjust=0,size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#NAPdom<-NAPdom+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#NAPdom <- NAPdom+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#NAPdom<-NAPdom+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
NAPdom<-NAPdom+guides(fill=F,linetype=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey"),col="grey", stroke=1, alpha=1)),
                    colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                              col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )                                                       
NAPdom

#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/TargetSpp.Prod.Con.weighted.jpeg",
#       width=15, height=25,units ="cm",dpi = 600, limitsize = TRUE)


# Scale factor
scaleFactor <- mean(DatastackT2$zero.days,na.rm=T)/mean(DatastackT2$prodtot.per,na.rm=T)
max(DatastackT2$zero.days,na.rm=T)

# Productivity and consumption per target spp - ZERO DAYs
NAPdom<- ggplot(AvgprodWT, aes(x=YrMonth, y=prodsp, colour=Tagsp, group=spp.code, shape=ProdCON))
NAPdom<-NAPdom+ geom_line(data=DatastackT2,aes(y = zero.days/30),colour="dark blue",linetype=1,size=1, alpha=.1)
NAPdom<-NAPdom+ geom_point(data=DatastackT2,aes(y = zero.days/30),colour="dark blue",size=.9,alpha=.1)
NAPdom<-NAPdom+ geom_hline(yintercept = 0, size =.5, linetype="dashed", colour="black")
NAPdom<-NAPdom+geom_line(linetype="dashed",size=1.2, alpha=.5, show.legend=F)
NAPdom<-NAPdom+geom_errorbar(aes(ymin=prodsp-sd, ymax=prodsp+sd),position=position_dodge(width=.25),linetype="solid",width=.2,lwd=1.1,show.legend=F)
NAPdom<-NAPdom+geom_point(aes(shape=treatment),position=position_dodge(width=.25),shape=22, size=4, fill="white", stroke=1)
NAPdom<-NAPdom+geom_line(data=AvgconWT,linetype="dashed",size=1.2, alpha=.5, show.legend=F)
NAPdom<-NAPdom+geom_errorbar(data=AvgconWT,aes(ymin=prodsp-sd, ymax=prodsp+sd),linetype="solid",width=.2,lwd=1.1,show.legend=F)
NAPdom<-NAPdom+geom_point(data=AvgconWT,aes(colour=Tagsp,fill=Tagsp),shape=21, size=3, stroke=1)
NAPdom<-NAPdom+facet_wrap(~Panel.titles,ncol=1,scales='fixed', drop=F)
NAPdom<-NAPdom+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
NAPdom<-NAPdom+scale_y_continuous(limits=c(-.75,2.95),sec.axis = sec_axis(~ . *30, breaks = c(0,25,50,75), labels = c(0,25,50,75), name = "Number of no rain days" ))
NAPdom<-NAPdom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
NAPdom<-NAPdom+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
NAPdom<-NAPdom+ xlab("Month|Year") + ylab(expression(paste("Productivity & consumption (g ",m^-2," ",day^-1,")")))
NAPdom<-NAPdom+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(hjust=0,size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#NAPdom<-NAPdom+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#NAPdom <- NAPdom+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#NAPdom<-NAPdom+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
NAPdom<-NAPdom+guides(fill=F,linetype=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey"),col="grey", stroke=1, alpha=1)),
                      colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )                                                       
#NAPdom2<-NAPdom+guides(fill=F,linetype=F,colour=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(22,21), size=3.75,fill=c("white","grey"),col="grey", stroke=1, alpha=1)))  

NAPdom

#### Plot spatial and temporal Nitrogen ####
DatastackTop <- droplevels(DatastackT[DatastackT$treatment=="Open",])
TransTop <- droplevels(TransNuts3[TransNuts3$treatment=="Open",])

DatastackTopN<-droplevels(subset(DatastackTop,Plant.N<3.6 | Plant.N>0.51 | is.na(Plant.N))) 
dim(DatastackTop)
dim(DatastackTopN)

MeanTrans<-aggregate(Plant.N~Tagsp,TransTop,mean)
MoveTrans<-aggregate(Plant.N~Tagsp,DatastackTopN,mean)

MeanTransSD<-aggregate(Plant.N~Tagsp,TransTop,sd)
MoveTransSD<-aggregate(Plant.N~Tagsp,DatastackTopN,sd)

MeanTrans$Plant.N.MOVE<-MoveTrans$Plant.N
MeanTrans$Plant.N.TransSD<-MeanTransSD$Plant.N
MeanTrans$Plant.N.MoveSD<-MoveTransSD$Plant.N

ggplot(MeanTrans, aes(x=Plant.N, y=Plant.N.MOVE, colour=Tagsp))+
  geom_abline(slope=1, intercept=0, size =.5)+geom_point(size=4.5)+
  xlab("Transplant leaf nitrogen (%)")+ylab("Temporal open plot leaf nitrogen (%)")+
  scale_colour_manual("Species",values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))+
 geom_errorbar(aes(ymin = Plant.N.MOVE-Plant.N.MoveSD,ymax = Plant.N.MOVE+Plant.N.MoveSD),width=.01,size=0.5,show.legend=F) +
geom_errorbarh(aes(xmin = Plant.N-Plant.N.TransSD,xmax = Plant.N+Plant.N.TransSD),height=.01,size=0.5,show.legend=F)+
  theme_classic()

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/Spatial.temp.plant.N.jpeg",
       width=15, height=15,units ="cm",dpi = 600, limitsize = TRUE)


#### Plant nitrogen through time ####
DatastackT <- Datastack[Datastack$pool=="target",]
dotchart(DatastackT$Plant.N)
DatastackTn<-droplevels(subset(DatastackT,Plant.N<3.6 & Plant.N>0.51 | is.na(Plant.N)))

# Panel titles
DatastackTn$Panel.titles<-DatastackTn$Tagsp
levels(DatastackTn$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")
DatastackTn$Panel.titles<- factor(DatastackTn$Panel.titles, levels = c("Mesic pastures","Wet pasture", "Mesic wild", "Wet wild","Wet wild Seronera"))

AvgTN<-aggregate(Plant.N~YrMonth+landuse+region+site.id+Tagsp+treatment+Panel.titles,DatastackTn,mean)
AvgTNsd<-aggregate(Plant.N~YrMonth+landuse+region+site.id+Tagsp+treatment+Panel.titles,DatastackTn,sd)
AvgTN$sd<-AvgTNsd$Plant.N

AvgTN$YrMonth<-as.Date(paste(AvgTN$YrMonth,"-01",sep=""))
DatastackTn$YrMonth<-as.Date(paste(DatastackTn$YrMonth,"-01",sep=""))

AvgTN$spp.code<-as.factor(with(AvgTN, paste(site.id,Tagsp,sep="_")))
AvgTN$spp.code2<-as.factor(with(AvgTN, paste(Tagsp,treatment,sep="_")))
DatastackTn$spp.code<-as.factor(with(DatastackTn, paste(site.id,Tagsp,sep="_")))
DatastackTn$spp.code2<-as.factor(with(DatastackTn, paste(Tagsp,treatment,sep="_")))


# Relevel so that other
levels(AvgTN$Tagsp)<- c("Chloris", "Chrysochloa", "Cynodon", "Digitaria", "Themeda")
levels(DatastackTn$Tagsp)<- c("Chloris", "Chrysochloa", "Cynodon", "Digitaria", "Themeda")

# Nitrogen graph
Ndom<- ggplot(AvgTN, aes(x=YrMonth, y=Plant.N, colour=Tagsp, group=spp.code, fill=spp.code2))
Ndom<-Ndom+ geom_line(data=DatastackTn,aes(y = rain.sum/190),colour="dark blue",linetype=1,size=1, alpha=.1)
Ndom<-Ndom+ geom_point(data=DatastackTn,aes(y = rain.sum/190),colour="dark blue",fill="dark blue",size=.9,alpha=.1)
Ndom<-Ndom+ geom_line(linetype="dashed",size=1.2, alpha=.5, show.legend=F)
Ndom<-Ndom+ geom_errorbar(aes(ymin=Plant.N-sd, ymax=Plant.N+sd),position=position_dodge(width=.25),linetype="solid",width=.2,lwd=1.1,show.legend=F)
Ndom<-Ndom+ geom_point(aes(shape=treatment),position=position_dodge(width=.25),size=4, stroke=1)
Ndom<-Ndom+ facet_wrap(~Panel.titles,ncol=1,scales='fixed', drop=F)
Ndom<-Ndom+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
Ndom<-Ndom+scale_y_continuous(limits=c(0,3.1),sec.axis = sec_axis(~ . *190, breaks = c(0,200,400,600), labels = c(0,200,400,600), name = "Precipitation (mm)"))
Ndom<-Ndom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
Ndom<-Ndom+scale_fill_manual(values=c("white","chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white","orangered3","white"))
Ndom<-Ndom+scale_shape_manual(values = c(24,25))
Ndom<-Ndom+xlab("Month|Year") + ylab("Nitrogen concentration (%)")
Ndom<-Ndom+ggtitle("(b) Leaf nitrogen concentration")
Ndom<-Ndom+theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(hjust=0,size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#NAPdom<-NAPdom+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#NAPdom <- NAPdom+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#NAPdom<-NAPdom+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
Ndom<-Ndom+guides(fill=F,linetype=F,colour=F,shape = guide_legend("Treatment",override.aes = list(shape=c(24,25), size=3.75,fill=c("white","grey"),col="grey", stroke=1, alpha=1)))#,
                      #colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                       #                                                         col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)) )                                                       
Ndom

#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/TargetSpp.Nitrogen.jpeg",
#       width=15, height=25,units ="cm",dpi = 600, limitsize = TRUE)

# Combine productivity, consumption and plant N plots throught time

# Extra legend from legend plot
library(grid)
library(gridExtra)
library(ggpubr)
library(egg)

# Extract legend
mylegend<-get_legend(NAPdom)
mylegend2<-get_legend(Ndom)

NAPdom2<-NAPdom+ theme(legend.position="none")
Ndom2<-Ndom+ theme(legend.position="none")

filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/", "Prod.Con.N.Time", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=30, height=20, res=400, unit="cm")
p3 <- arrangeGrob(NAPdom2,Ndom2,mylegend,mylegend2, ncol=3, nrow=4,widths=c(1.5,1.5,.5), heights=c(1,1,1,1),layout_matrix = cbind(c(1,1,1,1), c(2,2,2,2),c(NA,3,4,NA))) #common.legend = T)
grid.arrange(p3)
dev.off()


#filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/", "Prod.Con.N.Time", "_",Sys.Date(), ".jpeg" )
#jpeg (filename, width=30, height=20, res=400, unit="cm")
#egg::ggarrange(NAPdom,Ndom, ncol=2)#common.legend = T) #common.legend = T,legend="right")
#dev.off()

##################################################################################################
#### Modelling target species productivity, nutrients and nitrogen #####
##################################################################################################

# Rain and no rain days
plot( rain.sum~zero.days,DatastackT)
MyVar<-c("rain.sum","zero.days")
corvif(DatastackT[,MyVar]) 
#              GVIF
#rain.sum.1 1.457471
#zero.days  1.457471 # Variance inflation issues - check with and without in models

# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
DatastackT2$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(DatastackT2$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
names(DatastackT2)
DatastackT2$site.id
DatastackT2$plot.code <- as.factor(with(DatastackT2,paste(site.id,block,sep="_")))
levels(DatastackT$plot.code) #20 levels - productivity and consumption

#### Productivity- repeat measure lme ####

# Remove missing productivity data
DatastackTprod<-droplevels(subset(DatastackT2, !treatment=="Open" & !is.na(prodsp.per)))

#Implementing the AR-1 autocorrelation
library(nlme)
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = DatastackTprod)
corMatrix(cs1AR1.) #What does this give? 

names(DatastackTprod)

# Productivity per species in relaiton to rainfall
NAP.lme <- lme(prodsp.per~zero.days,#Tagsp+Tagsp:zero.days+
               #rain.sum,#Tagsp:rain.sum,
               random=~1|plot.code, method="ML",correlation=cs1AR1,data=DatastackTprod)
summary(NAP.lme)
anova(NAP.lme) 
AIC(NAP.lme) 
drop1(NAP.lme, test="Chisq") # Only zero days 

# Checking resids vs fit
E1 <- resid(NAP.lme, type = "pearson")
F1 <- fitted(NAP.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Strong negative values - try model without outliers? 

# Generate model p-values - productivity
NAP.lme2 <- update(NAP.lme, .~. -zero.days)

anova(NAP.lme,NAP.lme2) # No rainfall days

#         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#NAP.lme2     2  4 342.7674 351.8181 -167.3837 1 vs 2 17.14804  <.0001 # No rainfall days

# Consecutive days without rain
min(DatastackTprod$consec.NoRain.days)
max(DatastackTprod$consec.NoRain.days)
NAP.lmeC <- lme(prodsp.per~consec.NoRain.days,#Tagsp+Tagsp:consec.NoRain.days+Tagsp:consec.NoRain.days
                #rain.sum,#Tagsp:rain.sum,
                random=~1|plot.code, method="ML",correlation=cs1AR1,data=DatastackTprod)
summary(NAP.lmeC)
anova(NAP.lmeC) 
AIC(NAP.lmeC) 
drop1(NAP.lmeC, test="Chisq") # Only consecutive days wihtout rain

# Generate model p-values - productivity
NAP.lmeC2 <- update(NAP.lmeC, .~. -consec.NoRain.days)

anova(NAP.lmeC,NAP.lmeC2) # No rainfall days
#         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#NAP.lmeC      1  5 334.3997 345.7131 -162.1999                        
#NAP.lmeC2     2  4 342.7674 351.8181 -167.3837 1 vs 2 10.36766  0.0013

# Within 10 days productivity = 0 
names(DatastackTprod)
GrowP<-levels(as.factor(DatastackTprod$growth.period))
GrowP<-as.numeric(as.character(GrowP))
mean(GrowP) #64 - # 16% of mean growing period

# Emmeans contrast two-way interaction
NAP.lmeFINAL <- emmeans(NAP.lme , pairwise~Tagsp:rain.sum,type="response") #Creating emmeans across the factor levels in the interaction.
NAP.lmeFINAL$emmeans
NAP.lmeFINAL.pairs <- pairs(NAP.lmeFINAL,simple = "each", combine =TRUE) # THIS IS A GREAT OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
NAP.lmeFINAL.pairs$emmeans
plot(NAP.lmeFINAL, comparisons = FALSE) # None of the comparisons significant

NAP.lmeFINAL <- emmeans(NAP.lme , pairwise~Tagsp:zero.days,type="response") #Creating emmeans across the factor levels in the interaction.
NAP.lmeFINAL$emmeans
NAP.lmeFINAL.pairs <- pairs(NAP.lmeFINAL,simple = "each", combine =TRUE) # THIS IS A GREAT OUTPUT TO USE! Compare the EMMs of predictor factors in the model with one another. The use of simple="each"  generates all simple main-effect comparisons. Useage of combine=TRUE generates all contrasts combined into one family. The dots (.) in this result correspond to which factor is being contrasted. 
NAP.lmeFINAL.pairs$emmeans
plot(NAP.lmeFINAL, comparisons = FALSE) ## None of the comparisons significant

xyplot(prodsp.per~zero.days|Tagsp,DatastackTprod)
xyplot(prodsp.per~rain.sum|Tagsp,DatastackTprod)

#### Consumption - repeat measure lme ####
# Remove missing conumption data
DatastackTcon<-droplevels(subset(DatastackT2, !is.na(conssp.per)))

#Implementing the AR-1 autocorrelation
library(nlme)
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|plot.code) # AR matrix needs to be unique
table(DatastackTcon$YrMonthNumber,DatastackTcon$plot.code)
cs1AR1. <- Initialize(cs1AR1, data = DatastackTcon)
corMatrix(cs1AR1.) #What does this give? 

# Consumption model
dotchart(DatastackTcon$conssp.per)
Con.lme <- lme(conssp.per~prodsp.per,#Tagsp+
                 #zero.days+Tagsp:zero.days+
                 #rain.sum+Tagsp:rain.sum,
               random=~1|plot.code, method="ML",correlation=cs1AR1,data=DatastackTcon)
summary(Con.lme)#productivity 
anova(Con.lme) 
AIC(Con.lme) 
drop1(Con.lme, test="Chisq") # Nothing significant....

#Value  Std.Error DF  t-value p-value
#(Intercept) 1.3393659 0.22667993 50 5.908622       0
#prodsp.per  0.3438648 0.07371809 50 4.664593       0

# Checking resids vs fit
E1 <- resid(Con.lme, type = "pearson")
F1 <- fitted(Con.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Good

# Generate model p-values - productivity
Con.lme2 <- update(Con.lme, .~. -prodsp.per)

anova(Con.lme,Con.lme2) # Productivity

#         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Con.lme2     2  4 292.5732 301.6239 -142.2866 1 vs 2 19.45022  <.0001 # Productivity

#### Plant Nitrogen - repeat measure ####

# Remove missing productivity data
DatastackTn<-droplevels(subset(DatastackT, !is.na(Plant.N)))

# Plot.code to follow through tim - add treatment for plant N
DatastackT$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(DatastackT$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
DatastackTn$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(DatastackTn$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

DatastackT$plot.codeN <- as.factor(with(DatastackT,paste(site.id,block,treatment,sep="_")))
DatastackTn$plot.codeN <- as.factor(with(DatastackTn,paste(site.id,block,treatment,sep="_")))

levels(DatastackTn$plot.codeN) #40 levels - plant N, includes treatment
#Implementing the AR-1 autocorrelation
library(nlme)
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|plot.codeN) # AR matrix needs to be unique
table(DatastackTn$YrMonthNumber,DatastackTn$plot.codeN)
cs1AR1. <- Initialize(cs1AR1, data = DatastackTn)
corMatrix(cs1AR1.) #What does this give? 

# N model - test with linear and nolinear
Pn.lme <- lme(Plant.N~Tagsp+#treatment+
                 zero.days+#zero.days:treatment+#Tagsp:zero.days+
                 rain.sum,#rain.sum:treatment+#Tagsp:rain.sum,
               random=~1|plot.codeN, method="ML",correlation=cs1AR1,data=DatastackTn)
summary(Pn.lme)#Tag sp, rain, zero days - no interactions
anova(Pn.lme) #get F statistics and P-values
AIC(Pn.lme) #High
drop1(Pn.lme, test="Chisq") # Rainfall and rainfall x species - as well as no rainfall

# Non linear rainfall variables
# Cannot have poly rain and poly zero days interactions in the same graph - collinear
Pn.lme <- lme(Plant.N~Tagsp+#treatment+
                poly(zero.days,2)+poly(rain.sum,2),
              random=~1|plot.codeN, method="ML",correlation=cs1AR1,data=DatastackTn)
summary(Pn.lme)#Tag sp, rain, zero days - no interactions
anova(Pn.lme) #get F statistics and P-values
AIC(Pn.lme) #High
drop1(Pn.lme, test="Chisq")

# Checking resids vs fit
E1 <- resid(Pn.lme, type = "pearson")
F1 <- fitted(Pn.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1) # Conical when poly - hmmm

# Species differences
emm.plantN <- emmeans(Pn.lme,~Tagsp)
pairs(emm.plantN) # Nothing singificantly different
# Spp differences
# Chloris - Themeda         0.3972 0.124 35  3.203  0.0227 
#Chrysochloa - Digitaria   0.4400 0.120 35  3.654  0.0070 
#Chrysochloa - Themeda     0.5209 0.118 35  4.419  0.0008 
#Cynodon - Themeda         0.3914 0.125 35  3.140  0.0265 

# Generate model p-values - productivity
Pn.lme2 <- update(Pn.lme, .~. -poly(rain.sum,2))
Pn.lme3 <- update(Pn.lme, .~. -poly(zero.days,2))
Pn.lme4 <- update(Pn.lme, .~. -Tagsp)

anova(Pn.lme,Pn.lme2) #rain
anova(Pn.lme,Pn.lme3) #zero
anova(Pn.lme,Pn.lme4) #Tagsp

#        Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Pn.lme2     2 10 405.6444 439.6712 -192.8222 1 vs 2 24.41075  <.0001 # Rain
#Pn.lme3     2 10 391.7897 425.8165 -185.8948 1 vs 2 10.55602  0.0051 # Zero days
#Pn.lme4     2  8 405.5941 432.8155 -194.7970 1 vs 2 28.36042  <.0001 # Tag sp

# Plant nitrogen consecutive days
names(DatastackTn)
Pn.lme2 <- lme(Plant.N~Tagsp+#treatment+
                 poly(consec.NoRain.days,2)+#consec.NoRain.days:treatment+Tagsp:consec.NoRain.days+
                rain.sum,#rain.sum:treatment+#Tagsp:rain.sum,
              random=~1|plot.codeN, method="ML",correlation=cs1AR1,data=DatastackTn)
summary(Pn.lme2)#Tag sp, rain, zero days - no interactions
anova(Pn.lme2) #get F statistics and P-values
AIC(Pn.lme2) #High
drop1(Pn.lme2, test="Chisq") # Rainfall and rainfall x species - as well as no rainfall

plot(Plant.N~consec.NoRain.days,DatastackTn)

# Generate model p-values - productivity
Pn.lme2b <- update(Pn.lme2, .~. -  poly(consec.NoRain.days,2))

anova(Pn.lme2,Pn.lme2b) #  poly(consec.NoRain.days,2)
#         Model df     AIC      BIC    logLik   Test  L.Ratio p-value
#Pn.lme2      1 11 370.831 408.2605 -174.4155                        
#Pn.lme2b     2  9 405.822 436.4461 -193.9110 1 vs 2 38.99094  <.0001


##################################################################################################
#### Prod and Plant N in relation to rainfall and no rain days #####
##################################################################################################

# Yr-Month 
DatastackT2$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(DatastackT2$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
DatastackT2$plot.code <- as.factor(with(DatastackT2,paste(site.id,block,sep="_")))
levels(DatastackT2$plot.code) #20 levels

# Remove missing productivity data
DatastackTprod<-droplevels(subset(DatastackT2, !is.na(prodsp.per)))
DatastackTprodOUT<-droplevels(subset(DatastackT2, prodsp.per>-10 & !is.na(prodsp.per)))

# Productivity and rain
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = DatastackTprod)
corMatrix(cs1AR1.) #What does this give? 
NAP.lme2 <- lme(prodsp.per~Tagsp+ rain.sum+Tagsp:rain.sum,
               random=~1|plot.code, method="ML",correlation=cs1AR1,data=DatastackTprod)

# Plot predicted vs actual data
ProdData <- expand.grid(Tagsp=levels(DatastackTprod$Tagsp),
                      rain.sum = seq(min(DatastackTprod$rain.sum), max(DatastackTprod$rain.sum), length = 25))

#Create X matrix with expand.grid
X <- model.matrix(~Tagsp+ rain.sum+Tagsp:rain.sum, data = ProdData)
head(X)

#Calculate predicted values
ProdData$Pred <- X %*% fixef(NAP.lme2 )  # = X * beta

#D. Calculate standard errors (SE) for predicted values
ProdData$SE <- sqrt(  diag(X %*% vcov(NAP.lme2) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
ProdData$SeUp <- ProdData$Pred + 1.96 *ProdData$SE
ProdData$SeLo <- ProdData$Pred - 1.96 *ProdData$SE

# Column title
colnames(ProdData)[3]<-"prodsp.per"

# Panel titles
DatastackTprodOUT$Panel.titles<-DatastackTprodOUT$Tagsp
ProdData$Panel.titles<-ProdData$Tagsp

levels(DatastackTprodOUT$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")
levels(ProdData$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")

DatastackTprodOUT$Panel.titles<- factor(DatastackTprodOUT$Panel.titles, levels = c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild"))
ProdData$Panel.titles<- factor(ProdData$Panel.titles, levels = c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild"))

# Productivity and precipitation
Prodom<- ggplot(DatastackTprodOUT, aes(x=rain.sum, y=prodsp.per, colour=Tagsp))
#Prodom<-Prodom+geom_ribbon(data=ProdData,aes(ymin=SeLo,ymax=SeUp, fill=Tagsp),alpha=0.1,colour=NA)
Prodom<-Prodom+geom_point(size=3.5, stroke=1)
#Prodom<-Prodom+geom_line(data=ProdData,size=.75, alpha=.8, show.legend=F)
Prodom<-Prodom+facet_wrap(~Panel.titles, ncol=5)
Prodom<-Prodom+scale_x_continuous(limits=c(-14,500), breaks = c(0,250,500), labels = c(0,250,500), expand=c(0,0))
Prodom<-Prodom+scale_y_continuous(limits=c(-10,10), breaks = c(0,-10,-5,0,5,10), labels = c(0,-10,-5,0,5,10), expand=c(0,0))
Prodom<-Prodom+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
Prodom<-Prodom+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
Prodom<-Prodom+xlab("Precipitation (mm)") + ylab("")#+ ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
Prodom<-Prodom+ggtitle("(a) Precipitation")
Prodom<-Prodom+theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(hjust=0,size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#NAPdom<-NAPdom+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#NAPdom <- NAPdom+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#NAPdom<-NAPdom+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
Prodom<-Prodom+guides(fill=F,linetype=F,shape = F, colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                            col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)))                                                       
Prodom

# NO RAIN DAYS
# Productivity and NO rain days
NAP.lmeZ <- lme(prodsp.per~ zero.days+Tagsp,
                random=~1|plot.code, method="ML",correlation=cs1AR1,data=DatastackTprodOUT)

# Plot predicted vs actual data
ProdDataZ<- expand.grid(Tagsp=levels(DatastackTprodOUT$Tagsp),
                        zero.days = seq(min(DatastackTprodOUT$zero.days), max(DatastackTprod$zero.days), length = 25))

#Create X matrix with expand.grid
Xz <- model.matrix(~zero.days+Tagsp, data = ProdDataZ)
head(Xz)

#Calculate predicted values
ProdDataZ$Pred <- Xz %*% fixef(NAP.lmeZ )  # = X * beta

#D. Calculate standard errors (SE) for predicted values
ProdDataZ$SE <- sqrt(  diag(Xz %*% vcov(NAP.lmeZ) %*% t(Xz)))

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
ProdDataZ$SeUp <- ProdDataZ$Pred + 1.96 *ProdDataZ$SE
ProdDataZ$SeLo <- ProdDataZ$Pred - 1.96 *ProdDataZ$SE

# Column titles
colnames(ProdDataZ)[3]<-"prodsp.per"

# Panel titles
DatastackTprodOUT$Panel.titles<-DatastackTprod$Tagsp
ProdDataZ$Panel.titles<-ProdDataZ$Tagsp

levels(DatastackTprodOUT$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")
levels(ProdDataZ$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")

DatastackTprodOUT$Panel.titles<- factor(DatastackTprodOUT$Panel.titles, levels =c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild"))
ProdDataZ$Panel.titles<- factor(ProdDataZ$Panel.titles, levels =c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild"))

# Productivity and no rainfall days
ProdomZ<- ggplot(DatastackTprodOUT, aes(x=zero.days, y=prodsp.per))
ProdomZ<-ProdomZ+geom_ribbon(data=ProdDataZ,aes(ymin=SeLo,ymax=SeUp),fill="grey",alpha=0.5,colour=NA)
ProdomZ<-ProdomZ+geom_point(aes(colour=Tagsp),size=3.5, stroke=1,show.legend=F)
ProdomZ<-ProdomZ+geom_line(data=ProdDataZ,colour="dark grey",size=.75, alpha=.8, show.legend=F)
ProdomZ<-ProdomZ+facet_wrap(~Tagsp, ncol=5)
ProdomZ<-ProdomZ+scale_x_continuous(limits=c(-5,75), breaks = c(0,25,50,75), labels = c(0,25,50,75), expand=c(0,0))
ProdomZ<-ProdomZ+scale_y_continuous(limits=c(-10,10), breaks = c(0,-10,-5,0,5,10), labels = c(0,-10,-5,0,5,10), expand=c(0,0))
ProdomZ<-ProdomZ+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
ProdomZ<-ProdomZ+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
ProdomZ<-ProdomZ+xlab("Number of days without rainfall") + ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
ProdomZ<-ProdomZ+ggtitle("(b) Days without rainfall")
ProdomZ<-ProdomZ+theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text =  element_blank()
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#NAPdom<-NAPdom+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#NAPdom <- NAPdom+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#NAPdom<-NAPdom+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
ProdomZ<-ProdomZ+guides(fill=F,linetype=F,shape = F, colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                                             col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)))                                                       
ProdomZ

# CONSECUTIVE NO RAIN DAYS
names(DatastackTprodOUT)
NAP.lmeZ2 <- lme(prodsp.per~ consec.NoRain.days+Tagsp,
                random=~1|plot.code, method="ML",correlation=cs1AR1,data=DatastackTprodOUT)

# Plot predicted vs actual data
ProdDataZ2<- expand.grid(Tagsp=levels(DatastackTprodOUT$Tagsp),
                         consec.NoRain.days = seq(min(DatastackTprodOUT$consec.NoRain.days), max(DatastackTprod$consec.NoRain.days), length = 25))

#Create X matrix with expand.grid
Xz2 <- model.matrix(~consec.NoRain.days+Tagsp, data = ProdDataZ2)
head(Xz2)

#Calculate predicted values
ProdDataZ2$Pred <- Xz %*% fixef(NAP.lmeZ2 )  # = X * beta

#D. Calculate standard errors (SE) for predicted values
ProdDataZ2$SE <- sqrt(  diag(Xz2 %*% vcov(NAP.lmeZ2) %*% t(Xz2)))

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
ProdDataZ2$SeUp <- ProdDataZ2$Pred + 1.96 *ProdDataZ2$SE
ProdDataZ2$SeLo <- ProdDataZ2$Pred - 1.96 *ProdDataZ2$SE

# Column titles
colnames(ProdDataZ2)[3]<-"prodsp.per"

# Panel titles
DatastackTprodOUT$Panel.titles<-DatastackTprod$Tagsp
ProdDataZ2$Panel.titles<-ProdDataZ$Tagsp

levels(DatastackTprodOUT$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")
levels(ProdDataZ2$Panel.titles)<-c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild")

DatastackTprodOUT$Panel.titles<- factor(DatastackTprodOUT$Panel.titles, levels =c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild"))
ProdDataZ2$Panel.titles<- factor(ProdDataZ2$Panel.titles, levels =c("Mesic pastures", "Wet pasture", "Mesic wild", "Wet wild Seronera", "Wet wild"))


# Productivity and consecutive no rain days
ProdomZ2<- ggplot(DatastackTprodOUT, aes(x=consec.NoRain.days, y=prodsp.per))
ProdomZ2<-ProdomZ2+geom_ribbon(data=ProdDataZ2,aes(ymin=SeLo,ymax=SeUp),fill="grey",alpha=0.5,colour=NA)
ProdomZ2<-ProdomZ2+geom_point(aes(colour=Tagsp),size=3.5, stroke=1,show.legend=F)
ProdomZ2<-ProdomZ2+geom_line(data=ProdDataZ2,colour="dark grey",size=.75, alpha=.8, show.legend=F)
ProdomZ2<-ProdomZ2+facet_wrap(~Tagsp, ncol=5)
ProdomZ2<-ProdomZ2+scale_x_continuous(limits=c(-5,36), breaks = c(0,10,20,30), labels = c(0,10,20,30), expand=c(0,0))
ProdomZ2<-ProdomZ2+scale_y_continuous(limits=c(-10,10), breaks = c(0,-10,-5,0,5,10), labels = c(0,-10,-5,0,5,10), expand=c(0,0))
ProdomZ2<-ProdomZ2+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
ProdomZ2<-ProdomZ2+scale_fill_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
ProdomZ2<-ProdomZ2+xlab("Maximum consecutive days without rain") + ylab("") #ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
ProdomZ2<-ProdomZ2+ggtitle("(c) Consecutive days without rainfall")
ProdomZ2<-ProdomZ2+theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text =  element_blank()
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#NAPdom<-NAPdom+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#NAPdom <- NAPdom+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#NAPdom<-NAPdom+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
ProdomZ2<-ProdomZ2+guides(fill=F,linetype=F,shape = F, colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                                                               col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1.25)))                                                       
ProdomZ2


# Extra legend from legend plot
library(grid)
library(gridExtra)
library(ggpubr)
library(egg)

filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/", "Productivity.Rain.NoRainDays", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=27, height=22, res=400, unit="cm")
egg::ggarrange(Prodom,ProdomZ,ProdomZ2, ncol=1) #common.legend = T,legend="right")
dev.off()

############################################################################
# Plant Nitrogen 
#Implementing the AR-1 autocorrelation
library(nlme)
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|plot.codeN) # AR matrix needs to be unique
table(DatastackTn$YrMonthNumber,DatastackTn$plot.codeN)
cs1AR1. <- Initialize(cs1AR1, data = DatastackTn)
corMatrix(cs1AR1.) #What does this give? 

names(DatastackTn)

# N model
Pn.lme <- lme(Plant.N~Tagsp+ poly(rain.day,2),#+treatment,#zero.days
              random=~1|plot.codeN, method="ML",correlation=cs1AR1,data=DatastackTn)
summary(Pn.lme)#Tag sp, rain, zero days - no interactions
anova(Pn.lme) #get F statistics and P-values
AIC(Pn.lme) #High
drop1(Pn.lme, test="Chisq")

# Plot predicted vs actual data
NData <- expand.grid(Tagsp=levels(DatastackTn$Tagsp),
                     #treatment=levels(DatastackTn$treatment),
                     rain.day= seq(min(DatastackTn$rain.day), max(DatastackTn$rain.day), length = 25))

#Create X matrix with expand.grid
Xn <- model.matrix(~Tagsp+poly(rain.day,2),data = NData)
head(Xn)

#Calculate predicted values
NData$Pred <- Xn %*% fixef(Pn.lme)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
NData$SE <- sqrt(  diag(Xn %*% vcov(Pn.lme) %*% t(Xn))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

colnames(NData)[3]<-"Plant.N"

ProdomN<- ggplot(DatastackTn, aes(x=rain.day, y=Plant.N, colour=Tagsp))
ProdomN<-ProdomN+geom_point(aes(shape=treatment,fill=spp.code),size=4, stroke=1)
ProdomN<-ProdomN+geom_line(data=NData,size=1)
ProdomN<-ProdomN+facet_wrap(~Tagsp)
ProdomN

ProdomNz<- ggplot(DatastackTn, aes(x=zero.days, y=Plant.N, colour=Tagsp))
ProdomNz<-ProdomNz+geom_point(aes(shape=treatment,fill=spp.code),size=4, stroke=1)
ProdomNz<-ProdomNz+facet_wrap(~Tagsp)
ProdomNz

############################################################################
#### Cumulative productivity and consumption####
require(dplyr)

# Cumulative consumption

# Negative values to zero
names(DatastackTcon0)
DatastackTcon0<-droplevels(subset(DatastackT2, !is.na(conssp.per)))
DatastackTcon0<-droplevels(DatastackTcon0[!DatastackTcon0$YrMonth=="2017-03-01",])
DatastackTcon0$conssp[DatastackTcon0$conssp<0]<-0
sum(DatastackTcon0$conssp.per)
max(DatastackTcon0$conssp.per)

DatastackTcon0cum<-DatastackTcon0 %>% group_by(Tagsp,block.id) %>% mutate(csum = cumsum(conssp.per))
plot(DatastackTcon0cum$csum, col=c(DatastackTcon0cum$Tagsp)) # Cumulative - max value = max for 15 months
DatastackTcon0Days<-DatastackTcon0 %>% group_by(Tagsp,block.id) %>% mutate(csum = cumsum(growth.period))
plot(DatastackTcon0Days$csum, col=c(DatastackTcon0cum$Tagsp))     

# Cumulative consumption means
ConCumBlock<-aggregate(csum~Tagsp+block.id, DatastackTcon0cum, max)
CumCon<-aggregate(csum~Tagsp, ConCumBlock, mean)
CumConsd<-aggregate(csum~Tagsp, ConCumBlock, sd)
CumCon$sd<-CumConsd$csum

ConCumBlockGROWTH<-aggregate(csum~Tagsp+block.id,DatastackTcon0Days, max)
ConCumGrowth<-aggregate(csum~Tagsp,ConCumBlockGROWTH, mean)

# Annual consumption + sd
CumCon$Annual<-(CumCon$csum*ConCumGrowth$csum)
CumCon$AnnualSD<-(CumCon$sd*ConCumGrowth$csum)
CumCon$growth<-"consumption"

CumConLM<-lm(csum~Tagsp,ConCumBlock)
anova(CumConLM)
drop1(CumConLM, test="Chisq")
#Df Sum of Sq    RSS    AIC Pr(>Chi)  
#Tagsp   4    92.262 263.56 53.571   0.0714 .

# Cumulative productivity
# Negative values to zero
DatastackT2<-droplevels(DatastackT2[!DatastackT2$treatment=="Open",])
DatastackTprod0<-droplevels(subset(DatastackT2, !is.na(prodsp.per)))
DatastackTprod0<-droplevels(DatastackTprod0[!DatastackTprod0$YrMonth=="2017-03-01",])
DatastackTprod0$prodsp.per[DatastackTprod0$prodsp.per<0.00001]<-0
sum(DatastackTprod0$prodsp.per) #113.4524

DatastackTprod0cum<-DatastackTprod0 %>% group_by(Tagsp,block.id) %>% mutate(csum = cumsum(prodsp.per))
plot(DatastackTprod0cum$csum, col=c(DatastackTprod0cum$Tagsp)) # Cumulative - max value = max for 15 months
DatastackTprod0Days<-DatastackTprod0 %>% group_by(Tagsp,block.id) %>% mutate(csum = cumsum(growth.period))
plot(DatastackTprod0Days$csum, col=c(DatastackTprod0cum$Tagsp))     
# Cumulative productivity means
ProdCumBlock<-aggregate(csum~Tagsp+block.id, DatastackTprod0cum, max)
CumProd<-aggregate(csum~Tagsp, ProdCumBlock, mean)
CumProdsd<-aggregate(csum~Tagsp, ProdCumBlock, sd)
CumProd$sd<-CumProdsd$csum

# Relevel so that other
#ProdCumBlock$Tagsp<- factor(ProdCumBlock$Tagsp, levels = c("Cynodon","Chloris", "Chrysocloa", "Digitaria", "Themeda"))
                             
CumProdLM<-lm(csum~Tagsp,ProdCumBlock)
anova(CumProdLM)
summary(CumProdLM) # Chloris < Chrysochloa and Themeda and Cynodon < Themeda
#plot(CumProdLM)
drop1(CumProdLM, test="Chisq")
#Df Sum of Sq    RSS    AIC Pr(>Chi)  
#Tagsp   4    148.01 316.27 57.217  0.01328 *

#Pairwise contrasts *ghlt* and *lsmeans*
library(emmeans)
emm.s.recalmod <- emmeans(CumProdLM,~Tagsp)
pairs(emm.s.recalmod) # Marginal Themeda and Chloris

ProdCumBlockGROWTH<-aggregate(csum~Tagsp+block.id,DatastackTprod0Days, max)
CumGrowth<-aggregate(csum~Tagsp,ProdCumBlockGROWTH, mean) # This is not right - missing values- use consumption values

# Annual productivity + sd
CumProd$Annual<-(CumProd$csum*ConCumGrowth$csum) # Productivity growth period does not make sense
CumProd$AnnualSD<-(CumProd$sd*ConCumGrowth$csum)
CumProd$growth<-"productivity"

mean(CumCon$Annual) # 1332.608
mean(CumProd$Annual) # 1307.347
sd(CumCon$Annual) # 1332.608
sd(CumProd$Annual) # 1034.159
2.238112/10 # 0.22 mg/cm2 - similar to mid and tall grasslands (i.e. low McNaugthon 1984)
1034.159/ 1332.608# 77%

# Annual consumption + sd
ProdCon2<-rbind(CumCon,CumProd)
ProdCon2$growth<-as.factor(ProdCon2$growth)
levels(ProdCon2$growth)<-c("Consumption", "Productivity")

ProdCon2$fill.code<-as.factor(with(ProdCon2, paste(Tagsp,growth,sep="_")))
ProdCon2$Annual

ConCon<-c(575.7941, 1334.4724,1234.7636,1328.9108,2189.0995)
ProdProd<-c(418.1245,2058.7045,1039.6008,1208.1513,1812.1558)
1-(mean(ProdProd/ConCon)) #3%

ProdCon2$growth<- factor(ProdCon2$growth, levels = c("Productivity","Consumption"))

# Annual productivity and consumption
NAPtot <- ggplot(ProdCon2, aes(x=Tagsp, y=Annual, colour=Tagsp, fill=fill.code, group=growth, shape=growth))
NAPtot <- NAPtot+geom_errorbar(aes(ymin=Annual, ymax=Annual+AnnualSD),stat="identity",position=position_dodge(width = 0.9),width=.2,lwd=1.1,show.legend=F, alpha=1) # ymin=Cum_prod-Cumprod_SE
NAPtot <- NAPtot+geom_bar(stat="identity",position=position_dodge(width = 0.9),size=1.2, show.legend=T, alpha=1)
NAPtot <- NAPtot+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"))
NAPtot <- NAPtot+scale_fill_manual(values=c("chartreuse3",NA,"hotpink1",NA,"cadetblue3",NA,"green4",NA,"orangered3",NA))
NAPtot <- NAPtot+xlab("Species") + ylab(expression(paste("Productivity & consumption (g ",m^-2," ",year^-1,")")))
NAPtot <- NAPtot+scale_y_continuous(limits=c(0,3000),breaks=c(0,1000,2000,3000),labels=c(0,1000,2000,3000),expand=c(0,0))
NAPtot <- NAPtot+theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=12,face = "italic")
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(hjust=0,size=12)
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
NAPtot <- NAPtot+guides(linetype=F,fill= F, shape=guide_legend("Biomass change",override.aes = list(shape=c(22,22), size=3.75,fill=c("white","grey"),col="grey", stroke=.75, alpha=1)),
                  colour = guide_legend("Grass species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"),
                                                                            col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3"), stroke=1)) )                                                       
NAPtot

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Transplant Serengeti/CumulativeProdCon.jpeg",
       width= 19, height = 13,units ="cm",
       dpi = 600, limitsize = TRUE)



##################################################################################################
table(Datastack$harvest,Datastack$harvest.date)
ProdAvg<-aggregate(prodtot~harvest+region+landuse+block+treatment,Datastack,mean)
colnames(ProdAvg)<-c("Season","Region","Landuse","Block","Exclosure","Productivity")
ProdAvgH<-droplevels(ProdAvg[ProdAvg$Season=="H1" | ProdAvg$Season=="H4",])
#ProdAvgH<-droplevels(ProdAvgH[ !ProdAvgH$Region=="SE",])
levels(ProdAvgH$Season)<-c("Wet","Dry")
levels(ProdAvgH$Region)<-c("Dry","SE","Wet")
levels(ProdAvgH$Landuse)<-c("Pasture","Wild")

DecompProd<-merge(MassAvg,ProdAvgH, by=c("Season","Region","Landuse","Block"))
levels(DecompProd$Treatment)<-c("No termite", "Termite")
levels(DecompProd$Exclosure)<-c("Exclosed herbivore", "Herbivory")
#DecompProd<-DecompProd[!DecompProd$Littertype=="Green",] # Green tea too rapid mass loss
ggplot(DecompProd, aes(x=Massloss.per, y=Productivity, colour=Landuse))+geom_point()+
  facet_wrap(~Littertype+Treatment)+
  theme_classic()

DecompProd10<-DecompProd[DecompProd$Massloss.per>10,]
ProdMass<-lmer(Productivity~Massloss.per+Treatment+Landuse+#Exclosure+#Landuse+
                 Massloss.per:Treatment+
                # Exclosure:Treatment+
                 #Massloss.per:Exclosure:Treatment+
                 (1|Block),
     na.action=na.fail,
     REML=T,data=DecompProd10)
summary(ProdMass)
drop1(ProdMass,test="Chisq")


# Plot predicted vs actual data
MyData <- expand.grid(Treatment=levels(DecompProd10$Treatment),
                     Landuse=levels(DecompProd10$Landuse),
                      #Exclosure = levels(DecompProd10$Exclosure),
                      Massloss.per = seq(min(DecompProd10$Massloss.per), max(DecompProd10$Massloss.per), length = 25))
#fSpecies.fHoles=interaction(SerRootmain2$fSpecies,
#                            SerRootmain2$fHoles)

#Create X matrix with expand.grid
X <- model.matrix(~Massloss.per+Treatment+ Massloss.per:Treatment+Landuse , data = MyData)
head(X)

#Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(ProdMass)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!
MyData$SE <- sqrt(  diag(X %*% vcov(ProdMass) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

colnames(MyData)[4]<-"Productivity"

ggplot(DecompProd, aes(x=Massloss.per, y=Productivity, colour=Landuse))+geom_point()+
  facet_wrap(~Treatment)+#geom_line(data=MyData,aes(x=Massloss.per, y=Productivity))+
  theme_classic()

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
DatastackT$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(DatastackT$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
DatastackT$plot.code <- as.factor(with(DatastackT,paste(region,landuse,block,treatment,sep="_")))
levels(DatastackT$plot.code) #32 levels

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

plot()


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
SpeciesN$target.sp.<- factor(SpeciesN$target.sp., levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda")

# Treatment code for filling symbols
SpeciesN$spp.code<-as.factor(with(SpeciesN, paste(target.sp.,treatment,sep="_")))
TNs<-ggplot(SpeciesN,aes(y=N.conc.adj,x=landuse, colour=target.sp.,fill=spp.code, shape=treatment)) # group = grouping vector for lines
TNs<-TNs+geom_errorbar(data=SpeciesN,aes(ymin=N.conc.adj-SE, ymax=N.conc.adj+SE),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TNs<-TNs+geom_point(position=position_dodge(width=.45),size=5, stroke=1)
TNs<-TNs+ylab("Plant nitrogen concentration (%)")+xlab("Landuse")
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