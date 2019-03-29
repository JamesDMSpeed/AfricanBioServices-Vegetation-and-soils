#############################################################################
#### R workshop - Mixed model ####
#Stuart Smith
#13/1/2019
############################################################################
# Clean your R environment
rm(list=ls())
#packages - not sure these are all necessary
libs<-c("lattice","Matrix","devtools","tidyr","tidyr","dplyr","ggplot2","grid","gridExtra","ggpubr","ggExtra","egg",
        "sp","rgdal","raster","rasterVis","maptools", "rgeos","lubridate","vegan","reshape","reshape2","cluster", "lme4","nlme")
lapply(libs, require, character.only = TRUE)
source("/Users/anotherswsmith/Documents/Teaching/R_workshop/HighstatLibV10.R")
############################################################################
#### Correlate NTNU, NMBU and SUA ####
############################################################################
# Import data
setwd("/Users/anotherswsmith/Documents/Teaching/R_workshop/")
Nuts<-read.csv(file="Nuts4.csv", sep=",",header=TRUE)
dim(Nuts) # 752 rows   43 columns
str(Nuts)
names(Nuts)
#### NITROGEN ####
# Nitrogen concentration - NTNU versus USDM
plot(N.conc.NTNU~N.conc.USDM, col=c(Nuts$target_other), Nuts)
abline(lm(N.conc.NTNU~0+N.conc.USDM, Nuts))
summary(lm(N.conc.NTNU~0+N.conc.USDM, Nuts))
# Nitrogen concentration - NTNU versus SUA
plot(N.conc.NTNU~N.conc.SUA, col=c(Nuts$target_other), Nuts) # OK for target - rubbish for other
abline(lm(N.conc.NTNU~0+N.conc.SUA, Nuts))
NAlllm<-lm(N.conc.NTNU~0+N.conc.SUA, Nuts) # r2 0.7119
summary(lm(N.conc.NTNU~0+N.conc.SUA, Nuts))
# Remove outliers - any N > 3.5 %
dotchart(Nuts$N.conc, groups=Nuts$target_other) # Some outliers - large over 10 conc
NutsN3.5<-Nuts[Nuts$N.conc.SUA<3.5,]
plot(N.conc.NTNU~N.conc.SUA, NutsN3.5) # Non-linear
abline(lm(N.conc.NTNU~0+N.conc.SUA, NutsN3.5))
summary(lm(N.conc.NTNU~0+N.conc.SUA, NutsN3.5)) # y = 0+0.79330x = r2 0.8219
NAlllm<-lm(N.conc.NTNU~0+N.conc.SUA, NutsN3.5)
new.df <- data.frame(N.conc.SUA=c(Nuts$N.conc.SUA))
Nuts$N.conc.SUA.adj<-predict(NAlllm,new.df)
#### Phosphorus  ####
# Phosphorus concentration - NTNU versus USDM
plot(P.conc.NMBU~P.conc.USDM, col=c(Nuts$target_other), Nuts)
abline(lm(P.conc.NMBU~0+P.conc.USDM, Nuts))
summary(lm(P.conc.NMBU~0+P.conc.USDM, Nuts))
#Phosphorus concentration - NMBU versus SUA
plot(P.conc.NMBU~P.conc.SUA, col=c(Nuts$target_other), Nuts) # Urgh no relationship!
abline(lm(P.conc.NMBU~0+P.conc.SUA, Nuts))
NAlllm<-lm(P.conc.NMBU~0+P.conc.SUA, Nuts) # r2 0.4685
summary(lm(P.conc.NMBU~0+P.conc.SUA, Nuts))
# Remove outliers - drop any P < 0.51 %
dotchart(Nuts$P.conc,groups=Nuts$target_other)
NutsP.5<-droplevels(subset(Nuts,P.conc.SUA<0.51 | is.na(P.conc.SUA)))
plot(P.conc.NMBU~P.conc.SUA, NutsP.5) # Non-linear
abline(lm(P.conc.NMBU~0+P.conc.SUA, NutsP.5))
summary(lm(P.conc.NMBU~0+P.conc.SUA,NutsP.5)) # r2 0.78
NAlllm<-lm(P.conc.NMBU~0+P.conc.SUA, NutsP.5)
new.dfP <- data.frame(P.conc.SUA=c(Nuts$P.conc.SUA))
Nuts$P.conc.SUA.adj<-predict(NAlllm,new.dfP)
plot(P.conc.SUA.adj~P.conc.SUA, Nuts)
#Pairs plot
# Collinearity X
names(Nuts)
MyVar<-c( "N.conc.SUA", "N.conc.USDM", "N.conc.NTNU","P.conc.SUA","P.conc.NMBU", "P.conc.USDM")
pairs(Nuts[,MyVar],lower.panel = panel.cor)
#Cmd_1 and HerbPRC strongly negatively correlated
# Export predicted values
#write.table(Nuts, "Nuts.txt", row.name=F, sep="\t")
############################################################################
#### Dominance and eveness of communities ####
############################################################################
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils")
SppCov<-read.csv(file="Moveable exclosures/Species.cover.harvest.csv", sep=",",header=TRUE)
names(SppCov)
SppCov2<-SppCov[,21:107]
SppCov[,21:107]<-replace(SppCov2, is.na(SppCov2), 0)
colSums(SppCov[,21:107])
# OVERALL COMMUNITY
# Shannon index, species richness and Eveness
SppCov$Shannon<-diversity(SppCov[,21:107], index = "shannon")
SppCov$richness<-specnumber(SppCov[,21:107], MARGIN = 1)
SppCov$evenness<-SppCov$Shannon/log(SppCov$richness)
SppCov1<-droplevels(SppCov[SppCov$treatment!="EX2" & SppCov$treatment!="H0",])
ShanX<-aggregate(Shannon~treatment+region+landuse, SppCov1, mean)
EvenX<-aggregate(evenness~treatment+region+landuse, SppCov1, mean)
barplot(ShanX$Shannon)
barplot(EvenX$evenness)
names(Nuts)
Nuts$TargetProp<-(Nuts$target.sp.cover.harvest/Nuts$total.veg.cover.harvest)*100
aggregate(TargetProp~landuse+target.sp.,Nuts,mean)
aggregate(TargetProp~landuse+target.sp.,Nuts,sd)
#landuse target.sp. TargetProp
#1       P    Chl.pyc   37.12499
#2       P    Chr.ori   56.05954
#3       W    Cyn.dac   39.51803
#4       W    Dig.mac   32.33532
#5       P      other   46.85283
#6       W      other   36.31777
#7       W    The.tri   37.27140
sum(37.12499,56.05954,39.51803, 32.33532,  37.27140)/5 #  40.46186
############################################################################
#### Analysing adjusted nutrient conc ####
############################################################################
# Import data
setwd("/Users/anotherswsmith/Documents/Teaching/R_workshop/")
Nuts<-read.csv(file="Nuts4.csv", sep=",",header=TRUE)
dim(Nuts) # 752 rows   43 columns
str(Nuts)
names(Nuts)
# Convert date to 'Year- Month'
Rdate1<-strptime(as.character(Nuts$setup.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
Rdate2<-strptime(as.character(Nuts$harvest.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
Nuts$SetupMonth<-format(as.Date(Rdate1), "%Y-%m")
Nuts$HarvestMonth<-format(as.Date(Rdate2), "%Y-%m")
#### Remove poor quality data from datset ####
# Remove metal mesh exclosure treatment (EX2) # but keep H0 (renamed here for analysis)
Nuts1<-droplevels(Nuts[Nuts$treatment!="EX2",])
dim(Nuts1) #620  39
Nuts<-Nuts1
Nuts$treatment
# Remove extremes N<3.5% and P<0.5% - this data is due to measurement error - machine drifting - I do not believe low values
dotchart(Nuts$N.conc.adj, groups=Nuts$target_other) # Some outliers - large over 8 conc
dotchart(Nuts$P.conc.adj,groups=Nuts$target_other) # Outliers above 0.51%
NutsN<-droplevels(subset(Nuts,N.conc.adj<3.6 & N.conc.adj>0.51 | is.na(N.conc))) # Subset by x & y but incl. NAs
NutsNP<-droplevels(subset(NutsN,P.conc.adj<0.51 | is.na(P.conc.adj)))
dim(Nuts) # 640  45
dim(NutsNP) # 556  45
dotchart(NutsNP$N.conc.adj,groups=NutsNP$target_other)
dotchart(NutsNP$P.conc.adj,groups=NutsNP$target_other) #Nice spread now
# Housekeeping - numbers to factors
NutsNP$plot.code<-as.factor(with(NutsNP, paste(region,landuse,block,treatment,sep="_"))) # No plot code - includes harvest...
NutsNP$fblock<-as.factor(as.numeric(NutsNP$block.id))
# Combine Target and Non-target average N concentration
# Average N.conc and P.conc based on shared factors between target and other species datasets
NutsNPtar<-droplevels(NutsNP[NutsNP$target_other=="target",])
NutsNPoth<-droplevels(NutsNP[NutsNP$target_other=="other",])
NutsNPtarOth<-merge(NutsNPtar,NutsNPoth, by=c("plot.id","block.id.harvest","site.id" ,"block.id","site.me","region","landuse","block",
                                              "treatment", "harvest","wp","lat","long","setup.date","growth.period","fblock",
                                              "rain.sum","rain.acc.sum","SetupMonth","harvest.date", "HarvestMonth")) # "harvest.date", "HarvestMonth"
# Community weighted N and P concentrations
names(NutsNPtarOth)
NutsNPtarOth$biomass.g.x+NutsNPtarOth$biomass.g.y
NCc<-as.data.frame(cbind((NutsNPtarOth$N.conc.adj.x*(NutsNPtarOth$biomass.g.x/NutsNPtarOth$biomass.total.g.x)),NutsNPtarOth$N.conc.adj.y*(NutsNPtarOth$biomass.g.y/NutsNPtarOth$biomass.total.g.y)))
PCc<-as.data.frame(cbind((NutsNPtarOth$P.conc.adj.x*(NutsNPtarOth$biomass.g.x/NutsNPtarOth$biomass.total.g.x)),NutsNPtarOth$P.conc.adj.y*(NutsNPtarOth$biomass.g.y/NutsNPtarOth$biomass.total.g.y)))
Prolag.x<-NutsNPtarOth$biomass.g.x/(NutsNPtarOth$biomass.g.lag.x+NutsNPtarOth$biomass.g.lag.y)
Prolag.y<-NutsNPtarOth$biomass.g.y/(NutsNPtarOth$biomass.g.lag.x+NutsNPtarOth$biomass.g.lag.y)
Prolag.x[!is.finite(Prolag.x)] <- 0
Prolag.y[!is.finite(Prolag.y)] <- 0
NCcLAG<-as.data.frame(cbind((NutsNPtarOth$N.conc.adj.lag.x*(Prolag.x)),NutsNPtarOth$N.conc.adj.lag.y*(Prolag.y)))
PCcLAG<-as.data.frame(cbind((NutsNPtarOth$P.conc.adj.lag.x*(Prolag.x)),NutsNPtarOth$P.conc.adj.lag.y*(Prolag.y)))
NutsNPtarOth$Com.N.conc<-rowMeans(NCc, na.rm = T) # Mean community N conc
NutsNPtarOth$Com.P.conc<-rowMeans(PCc, na.rm = T) # Mean community P conc
NutsNPtarOth$Com.N.conc.LAG<-rowMeans(NCcLAG, na.rm = T) # Mean community N conc
NutsNPtarOth$Com.P.conc.LAG<-rowMeans(PCcLAG, na.rm = T)
plot(NutsNPtarOth$Com.N.conc~NutsNPtarOth$Com.N.conc.LAG)
# Reduce dataframe name
NutsO<-NutsNPtarOth
summary(is.na(NutsO$Com.N.conc))
NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc) & !is.na(NutsO$rain.sum), ])
NutsOPA <- droplevels(NutsO[!is.na(NutsO$Com.P.conc) & !is.na(NutsO$rain.sum), ])
#### Duplicates ####
# Why ae there duplicates for H0?
#NutsONA[duplicated(NutsONA$plot.id), ]
#NutsONA<-NutsONA[!duplicated(NutsONA$plot.id), ]
# Remove duplicates
#NutsONA$plot.code<-as.factor(with(NutsONA, paste(region,landuse,block,treatment,sep="_")))
#NutsONA$harvest.code<-as.factor(with(NutsONA, paste(plot.code,harvest,sep="_")))
#NutsONA[duplicated(NutsONA$harvest.code), ]
#NutsONA<-NutsONA[!duplicated(NutsONA$harvest.code), ]
NutsONA
dim(NutsONA)#  164  77
#write.table(NutsONA, "NutsONA.txt", row.name=F, sep="\t")

############################################################################
#### Add tree density data from Vilde's data ####
############################################################################
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Ecosystem carbon/Tree.data")
PhilTrees<-read.csv(file="Tree.Carbon.Vilde.csv", sep=",",header=TRUE)
TreesN<-read.csv(file="Tree.BM.N.non.csv", sep=",",header=TRUE)
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils/Ecosystem carbon/Soil.data")
SoilN<-read.csv(file="Belowground.Carbon.csv", sep=",",header=TRUE)
names(PhilTrees)
names(TreesN)
names(SoilN)
colnames(PhilTrees)[2]<-"region"
colnames(SoilN)[2]<-"region"
colnames(TreesN)[2]<-"region"
colnames(TreesN)[3]<-"block"
colnames(SoilN)[4]<-"block"
colnames(PhilTrees)[3]<-"block"
colnames(SoilN)[9]<-"landuse"
levels(PhilTrees$landuse)<-c("P","W")
levels(TreesN$landuse)<-c("P","W")
levels(SoilN$landuse)<-c("P","W")
levels(NutsONA$landuse)
levels(PhilTrees$region)<-c("WET","INT","DRY","DRY","WET","INT","SE")
levels(TreesN$region)<-c("WET","INT","DRY","DRY","WET","INT","SE")
levels(SoilN$region)<-c("WET","INT","DRY","DRY","WET","INT","SE")
levels(NutsONA$region)
PhilTrees$block.code<-as.factor(with(PhilTrees, paste(region,landuse,block,sep="_")))
TreesN$block.code<-as.factor(with(TreesN, paste(region,landuse,block,sep="_")))
SoilN$block.code<-as.factor(with(SoilN, paste(region,landuse,block,sep="_")))
NutsONA$block.code<-as.factor(with(NutsONA, paste(region,landuse,block,sep="_")))
levels(PhilTrees$block.code) # 28
levels(TreesN$block.code) # 28
levels(NutsONA$block.code) # 20
levels(PhilTrees$block.code)
PhilTrees<-droplevels(PhilTrees[PhilTrees$block.code!="INT_P_1" & PhilTrees$block.code!="INT_P_2" &
                                  PhilTrees$block.code!="INT_P_3" & PhilTrees$block.code!="INT_P_4" &
                                  PhilTrees$block.code!="INT_W_1" & PhilTrees$block.code!="INT_W_2" &
                                  PhilTrees$block.code!="INT_W_3" & PhilTrees$block.code!="INT_W_4",])
TreesN<-droplevels(PhilTrees[TreesN$block.code!="INT_P_1" & TreesN$block.code!="INT_P_2" &
                               TreesN$block.code!="INT_P_3" & TreesN$block.code!="INT_P_4" &
                               TreesN$block.code!="INT_W_1" & TreesN$block.code!="INT_W_2" &
                               TreesN$block.code!="INT_W_3" & TreesN$block.code!="INT_W_4",])
levels(PhilTrees$block.code) # 20
levels(TreesN$block.code) # 20
table(PhilTrees$block.code,PhilTrees$block)
table(TreesN$block.code,TreesN$block)
TreeALL<-merge(TreesN,PhilTrees, by=c("block.code","region","landuse","block","Block.ID"))
TreeALL2<-merge(TreeALL,SoilN, by=c("block.code","region","landuse","block","Block.ID"))
NutsONA2<-merge(NutsONA,TreeALL2, by=c("block.code","region","landuse","block"))
dim(NutsONA2) #656 136
# Need to row sums
#names(TreeALL)
#names(NutsONA2)
#LegumeN<-NutsONA2[,c(73,75)]
#NutsONA2$BM.N.m2<-rowSums(LegumeN, na.rm=T)
#### Adding herbivory multivariate scores ####
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Exclosures Serengeti/Biomass excl")
HerBio<-read.csv(file="Sero.seasonal.bio_enviroFULL.csv", sep=",",header=TRUE)
levels(NutsONA$site.me) # "Handajega" "Makao"     "Maswa"     "Mwantimba" "Seronera"
levels(HerBio$area)<-c("Makao", "MakaoWMA","Maswa","Mwantimba","Handajega")
levels(NutsONA$landuse)
levels(HerBio$landuse)<-c("I","P","W")
HerBio$block.code<-as.factor(with(HerBio, paste(area,landuse,site,sep="_")))
NutsONA2$block.code<-as.factor(with(NutsONA2, paste(site.me,landuse,block.Stu.x,sep="_")))
HerBio<-droplevels(HerBio[HerBio$block.code!="Maswa_I_9" & HerBio$block.code!="Maswa_I_12" &
                            HerBio$block.code!="Maswa_I_10" & HerBio$block.code!="Maswa_I_11" &
                            HerBio$block.code!="MakaoWMA_W_5" & HerBio$block.code!="MakaoWMA_W_6" &
                            HerBio$block.code!="MakaoWMA_W_7" & HerBio$block.code!="MakaoWMA_W_8"
                          & HerBio$block.code!="Makao_P_4",])
NutsONA2<-droplevels(NutsONA2[NutsONA2$block.code!="Seronera_W_NA",])
#NutsONA2<-droplevels(NutsONA2[NutsONA2$block.code!="Seronera_W_NA",])
levels(HerBio$block.code)
levels(NutsONA2$block.code)
dim(NutsONA)
dim(NutsONA2) #500 136
## Loop does not work with Rdate - just works with as.date!
NutsONA2$date2 = as.Date(NutsONA2$harvest.date,"%d/%m/%Y") #as.Date(SerEbio3$date,"%d/%m/%Y")
NutsONA2$date3 = as.Date(NutsONA2$setup.date,"%d/%m/%Y") #as.Date(SerEbio3$date0,"%d/%m/%Y")
HerBio$date4 = as.Date(HerBio$date,"%d/%m/%Y") #as.Date(ex_precip$date,"%d.%m.%Y")
# Herbivore score
for(i in 1:nrow(NutsONA2)){
  NutsONA2$Herb.PRC[i] <-mean(HerBio$HerbPRC[which(HerBio$block.code== NutsONA2$block.code[i] & #block_code
                                                     HerBio$date4>= NutsONA2$date3[i] &
                                                     HerBio$date4<= NutsONA2$date2[i])])
}
# Total dung
names(HerBio)
for(i in 1:nrow(NutsONA2)){
  NutsONA2$total_dung[i] <-mean(HerBio$total_dung[which(HerBio$block.code== NutsONA2$block.code[i] & #block_code
                                                          HerBio$date4>= NutsONA2$date3[i] &
                                                          HerBio$date4<= NutsONA2$date2[i])])
}
for(i in 1:nrow(NutsONA2)){
  NutsONA2$wild_grazer[i] <-mean(HerBio$wild_grazer[which(HerBio$block.code== NutsONA2$block.code[i] & #block_code
                                                            HerBio$date4>= NutsONA2$date3[i] &
                                                            HerBio$date4<= NutsONA2$date2[i])])
}
names(HerBio)
for(i in 1:nrow(NutsONA2)){
  NutsONA2$wild_broswer[i] <-mean(HerBio$wild_broswer[which(HerBio$block.code== NutsONA2$block.code[i] & #block_code
                                                              HerBio$date4>= NutsONA2$date3[i] &
                                                              HerBio$date4<= NutsONA2$date2[i])])
}
for(i in 1:nrow(NutsONA2)){
  NutsONA2$live_grazer[i] <-mean(HerBio$live_grazer[which(HerBio$block.code== NutsONA2$block.code[i] & #block_code
                                                            HerBio$date4>= NutsONA2$date3[i] &
                                                            HerBio$date4<= NutsONA2$date2[i])])
}
for(i in 1:nrow(NutsONA2)){
  NutsONA2$live_broswer[i] <-mean(HerBio$live_broswer[which(HerBio$block.code== NutsONA2$block.code[i] & #block_code
                                                              HerBio$date4>= NutsONA2$date3[i] &
                                                              HerBio$date4<= NutsONA2$date2[i])])
}
dim(NutsONA) #  144  78
dim(NutsONA2) #  500 144 -There is a tripling issue
NutsONA2[duplicated(NutsONA2$plot.id), ]
NutsONA2d<-NutsONA2[!duplicated(NutsONA2$plot.id), ]
dim(NutsONA2d) # 125 144
names(NutsONA2d)
MyVar<-c( "N.conc.adj.x","N.conc.adj.y","Com.N.conc","Com.P.conc", "wild_grazer", "wild_broswer","live_broswer", "live_grazer")
pairs(NutsONA2d[,MyVar],lower.panel = panel.cor)
#Cmd_1 and HerbPRC strongly negatively correlated
names(NutsONA2d)
ggplot(NutsONA2d,aes(y=N.conc.adj.x,x=live_grazer,NutsONA2, shape=treatment))+geom_point()+facet_wrap(~landuse+treatment+target.sp..x)
summary(lm(N.conc.adj.x~live_grazer*target.sp..x*treatment,NutsONA2d))
DungN<-lmer(Com.N.conc~total_dung*landuse*treatment+(1|fblock),NutsONA2d)
anova(DungN)
drop1(DungN, test="Chisq")

############################################################################
#### Plant N and soil N - December only ####
## NEED TO PUT BACK SERONERA FOR THIS SECTION!!! ####
############################################################################
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))}
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
NutsONA2$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(NutsONA2$setup.date,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Using setup to include H0 - rain is preceeding time to 01-01-2017
NutsONA2Dec<-NutsONA2[NutsONA2$YrMonthNumber=="10",]
names(NutsONA2Dec)
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
PlantSoilN<-aggregate(Com.N.conc~AhorN.kg_m2,NutsONA2Dec,mean)
PlantSoilNse<-aggregate(Com.N.conc~AhorN.kg_m2,NutsONA2Dec,se)
PlantSoilN$SE<-PlantSoilNse$Com.N.conc
NutsONA2Dec$fblock<-as.factor(as.numeric(NutsONA2Dec$block.code))
summary(lm(Com.N.conc~AhorN.kg_m2, data = NutsONA2Dec))
#y=0.81135+2.55163x #  r2 0.2853
NutsONA2DecMIXED<-lmer(Com.N.conc~AhorN.kg_m2+(1|fblock), data = NutsONA2Dec, REML=T)
anova(NutsONA2DecMIXED)
drop1(NutsONA2DecMIXED, test="Chisq")
# Mixed model = significant also AhorN.kg_m2  1  -5.6342 8.6671 0.00324 **
# Plant N soil N correlation - December
PSN<-ggplot(PlantSoilN,aes(y=Com.N.conc,x=AhorN.kg_m2))
PSN<-PSN+geom_smooth(method='lm',formula=y~x, colour="orangered3", fill="orangered3",alpha=.5)
PSN<-PSN+geom_errorbar(aes(ymin=Com.N.conc-SE, ymax=Com.N.conc+SE),colour="grey50",show.legend=F)
PSN<-PSN+geom_point(colour="grey30",fill="grey30",size=2.5, stroke=1)
PSN<-PSN+ylab("Plant nitrogen concentration (%)")+xlab(expression(paste("Soil nitrogen (kg ",m^-2,")")))# Adding x and ylabs to plot
#PSN<-PSN+scale_y_continuous(limits=c(0,),expand=c(0,0))
PSN<-PSN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black"))
PSN
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/PlantSoilN.jpeg",
#       width=15, height=12,units ="cm",dpi = 600, limitsize = TRUE)
# CANNOT use Soil N throughout the dataset - only Dec 2017
NutsONA2<-NutsONA
############################################################################
#### Linear mixed model NITROGEN CONCETRATION DATA - COMMUNITY LEVEL ####
############################################################################
#### Linear mixed model on community N conc ####
NutsONA2<-NutsONA
Nitlmer<-lmer(Com.N.conc~landuse+rain.sum+treatment+
                landuse:rain.sum+landuse:treatment+
                +treatment:rain.sum+treatment:landuse:rain.sum+
                (1|fblock), data = NutsONA, REML=T)
summary(Nitlmer) # Important breaks down variation explained by random factors - looks poor for fblock!
AIC(Nitlmer) #  283.6051 - lower is better
anova(Nitlmer)
# Checking some assumptions
#Normalized residuals in lme are of the form:
# E / sqrt(variance of the residuals)
E1 <- resid(Nitlmer, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F1 <- fitted(Nitlmer)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) #  Conical - not great
abline(h = 0, lty = 2, col = 1)
# No autocorrelation
library(itsadug)
plot(acf_resid(Nitlmer), type="b",alpha=0.05)
abline(c(0,0), lty = 2, col = 1) # Strong temporal auto-correlation
# There is a temporal pattern!
# Likelihood Ratio Testing dropping terms based on AIC
drop1(Nitlmer, test="Chisq")
# Three way interaction - does not improve AIC
#landuse:rain.sum:treatment  1 257.60 0.070444  0.7907
#....continue with pruning
Nitlmer2<-lmer(Com.N.conc~poly(rain.sum,2)+landuse+#treatment+
                 # landuse:poly(rain.sum,2)+landuse:treatment+
                 #+treatment:poly(rain.sum,2)+#treatment:landuse:poly(rain.sum,2)+
                 (1|fblock),data = NutsONA, REML=T)
summary(Nitlmer2) # Important breaks down variation explained by random factors - looks poor for fblock!
AIC(Nitlmer2) # unimodal 218.5532 # Old = 284.3637
anova(Nitlmer2) #274.1744
drop1(Nitlmer2, test="Chisq") # Nothing significant
#Landuse and treatment - retain - no interaction
# NB. Good model testing would then put back factors removed from the original model
# Recheck the assumptions
#Normalized residuals in lme are of the form:
# E / sqrt(variance of the residuals)
E2 <- resid(Nitlmer2, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F2 <- fitted(Nitlmer2)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2,
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # Conical - not great
abline(h = 0, lty = 2, col = 1)
# Updating the model - generating p-values for each term
Nitlmer2d<- update(Nitlmer2, .~. -poly(rain.sum,2))
anova(Nitlmer2,Nitlmer2d)

##############################################################
#### Sketch fitted values ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/-
#A:Specify covariate values for predictions
MyData <- expand.grid(landuse=levels(NutsONA$landuse),
                      rain.sum = seq(min(NutsONA$rain.sum), max(NutsONA$rain.sum), length = 25))
#B. Create X matrix with expand.grid
X <- model.matrix(~ poly(rain.sum,2)+landuse, data = MyData)
head(X)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(Nitlmer2)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyData$SE <- sqrt(  diag(X %*% vcov(Nitlmer2) %*% t(X))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE
#E. Plot predicted values
names(MyData)
colnames(MyData)[3]<-"Com.N.conc"
#### Plot observed data versus prediction #####
# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(NutsONA2,aes(x=rain.sum, y=Com.N.conc)) # geom_errorbar
RN<-RN+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData,colour="red",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(stats="identity",colour="grey50",fill="grey50",size=2.5)
RN<-RN+facet_wrap(~landuse, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(-1,540), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0,0))
RN<-RN+scale_y_continuous(limits=c(0,2.5),expand=c(0,0))
RN<-RN+ylab("Nitrogen concentration (%)")+xlab("Rainfall (mm)") # Adding x and ylabs to plot
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1)
RN<-RN+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1)
RN

################################################################################
#### NITROGEN: Mixed model with temporal autocorrelaiton structure ####
################################################################################
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))}
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
NutsONA$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(NutsONA$harvest.date,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Using setup to include H0 - rain is preceeding time to 01-01-2017
# Plot ID without Harvest number
levels(NutsONA$plot.id)
NutsONA$plot.code<-as.factor(with(NutsONA, paste(region,landuse,block,treatment,sep="_")))
levels(NutsONA$plot.code) # 40 levels
# Remove NAs for nitrogen
#NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc), ])
table(NutsONA$plot.code,NutsONA$YrMonthNumber) # Month 8 and 15 few data points
NutsONA$fYrMonthNumber<-as.factor(NutsONA$YrMonthNumber)
levels(NutsONA$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
NutsONA$YrMonthNumber<-as.numeric(as.character(NutsONA$fYrMonthNumber))
table(NutsONA$plot.code,NutsONA$YrMonthNumber)
apply(table(NutsONA$plot.code, NutsONA$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
# Remove NAs - NAS for year month as no set up
NutsONA<- droplevels(NutsONA[!is.na(NutsONA$Com.N.conc) &! is.na(NutsONA$fYrMonthNumber),])
# Remove duplicates
#NutsONA$harvest.codeYr<-as.factor(with(NutsONA, paste(plot.code,harvest,fYrMonthNumber,sep="_")))
#NutsONA<-NutsONA[!duplicated(NutsONA$harvest.codeYr), ]
#table(NutsONA$plot.code,NutsONA$YrMonthNumber)
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/fblock/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =NutsONA)
corMatrix(cs1AR1.)
# LMM with auto temporal correlation # Community nitrogen
B1<-lme(Com.N.conc~landuse+rain.sum+treatment+
          landuse:rain.sum+landuse:treatment+
          +treatment:rain.sum+treatment:landuse:rain.sum,
        random= ~ 1|region/fblock, na.action=na.pass, method="REML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=NutsONA)
summary(B1)
anova(B1) # Highly signficant
AIC(B1) # 281.3696
# Check temporal autocorrelation
# Extract residuals
E2 <- resid(B1, type ="n")  # lme = "type = "n"
F2 <- fitted(B1)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2,
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # None of the fitted values <0
abline(h = 0, lty = 2, col = 1)  # Good - with temporal structure
# Time auto-correlated
acf(E2, na.action=na.pass,main="Auto-correlation plot for residuals")
# No auto-correlation
# Simplify model
B1a<-lme(Com.N.conc~rain.sum,
         random= ~ 1|region/fblock, na.action=na.pass, method="ML",
         correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=NutsONA)
AIC(B1a) #244.5504
B1<-lme(Com.N.conc~poly(rain.sum,2),
        random= ~ 1|region/fblock, na.action=na.pass, method="ML",
        correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=NutsONA)
AIC(B1) #240.4541
drop1(B1, test="Chisq") # Just rain - check whether unimodal
anova(B1a,B1) # Second model better - unimodal
drop1(B1, test="Chisq")
#                Df    AIC    LRT  Pr(>Chi)
#rain.sum  1 176.83 13.672 0.0002177 ***
#poly(rain.sum, 2)  2 310.86 51.114 7.956e-12 *** # much better
B1b<-update(B1, .~. -poly(rain.sum,2))
anova(B1,B1b)

##############################################################
#### Sketch fitted values ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/-
#A:Specify covariate values for predictions
MyData2 <- expand.grid(rain.sum = seq(min(NutsONA$rain.sum), max(NutsONA$rain.sum), length = 25))
#B. Create X matrix with expand.grid
X <- model.matrix(~ poly(rain.sum,2), data = MyData2)
head(X)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData2$Pred <- X %*% fixef(B1)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyData2$SE <- sqrt(  diag(X %*% vcov(B1) %*% t(X))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData2$SeUp <- MyData2$Pred + 1.96 * MyData2$SE
MyData2$SeLo <- MyData2$Pred - 1.96 * MyData2$SE
#E. Plot predicted values
names(MyData2)
colnames(MyData2)[2]<-"Com.N.conc"
#### Plot observed data versus prediction #####
NutsONAb<-NutsONA
levels(NutsONAb$landuse)<-c("Pasture","Wildlife protected")
levels(NutsONAb$treatment)<-c("Exclosed","Open")
# Treatment code for filling symbols
NutsONAb$trt.code<-as.factor(with(NutsONAb, paste(landuse,treatment,sep="_")))
# Remove estimated error below zero - ensure ribbon is drawn
MyData2[MyData2$SeLo<.01,]<-0
# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(NutsONAb,aes(x=rain.sum, y=Com.N.conc))
#RN<-RN+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)
#RN<-RN+geom_line(data=MyData,aes(ymin=SeUp, ymax=SeLo),colour="red",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_ribbon(data=MyData2,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="green4",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData2,aes(ymin=SeUp, ymax=SeLo),colour="grey50",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(stat="identity",aes(shape=treatment, fill=trt.code,colour=trt.code),stroke=1,size=3.5)
RN<-RN+facet_wrap(~landuse, scale="fixed")
RN<-RN+scale_colour_manual(values=c("tan3","tan3","turquoise3","turquoise3"))
RN<-RN+scale_fill_manual(values=c("tan3","white","turquoise3","white"))
RN<-RN+scale_shape_manual(values=c(22,21))
RN<-RN+scale_x_continuous(limits=c(-2,530), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0.1,0.1))
RN<-RN+scale_y_continuous(limits=c(-.1,2.6),expand=c(0,0))
RN<-RN+ylab("Plant nitrogen concentration (%)")+xlab("Rainfall (mm)") # Adding x and ylabs to plot
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,legend.position = "right"
    ,legend.direction="vertical"
    ,legend.title=element_text(size=12)
    ,legend.text=element_text(size=12)
    ,strip.text.x = element_text(size = 13, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = -2, xend = -2, size = 1)
#RN<-RN+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1)
RN<-RN+guides(colour=F, fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,#linetype=c("solid","twodash","dashed"), #size=4.5,
                                                                                               fill=c("grey30","white"),col="grey30", stroke=1)))
RN
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/PlantN_rain.jpeg",
#             width=20, height=12,units ="cm",dpi = 600, limitsize = TRUE)

###########################################################################
##### Temporal component - target versus non-target ####
###########################################################################
#### A temporal component ####
#### Add second y axis = rainfall #### Ggplot2 is poor at this - but there is good reason...
#### Target and non-target species - N conc through time####
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TargetNavgYm<-aggregate(N.conc.adj~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
TargetNsdYm<-aggregate(N.conc.adj~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
TargetNavgYm$SE<-TargetNsdYm$N.conc.adj
tail(TargetNavgYm)
RainavgYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
RainsdYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
RainavgYm$SE<-RainsdYm$rain.sum
# Grouping variable - this is necessary to connect the lines
TargetNavgYm$grp.code<-as.factor(with(TargetNavgYm, paste(region,landuse,target.sp.,treatment,sep="_")))
RainavgYm$grp.code<-as.factor(with(RainavgYm, paste(region,landuse,target.sp.,treatment,sep="_")))
levels(TargetNavgYm$grp.code)
TargetNavgYm$SE[is.na(TargetNavgYm$SE) ]<-0
# Issue - Harvest 5 spread over two months - replace Oct with Sept
#TargetNavgYm$HarvestMonth[TargetNavgYm$HarvestMonth=="2017-10"]<-"2017-09"
#RainavgYm$HarvestMonth[RainavgYm$HarvestMonth=="2017-10"]<-"2017-09"
# Remove Seronera
#TargetNavgYm2<-droplevels(TargetNavgYm[TargetNavgYm$region!="SE",])
#RainavgYm2<-droplevels(RainavgYm[RainavgYm$region!="SE",])
# Rename levels
levels(TargetNavgYm$region)<-c("Dry region","Intermediate region","Wet region")
levels(RainavgYm$region)<-c("Dry region","Intermediate region","Wet region")
levels(TargetNavgYm$landuse)<-c("Pasture","Wildlife protected")
levels(RainavgYm$landuse)<-c("Pasture","Wildlife protected")
levels(TargetNavgYm$treatment)<-c("Exclosed","Open")
levels(RainavgYm$treatment)<-c("Exclosed","Open")
# Target grass species
#Handajega: Themeda triandra
#Mwantimba: Chrysocloa orientalis
#Seronera: Digitaria macroblephara
#Maswa: Cynodon dactylon
#Makao: Chloris pycnothrics
levels(TargetNavgYm$target.sp.)<-c("Chloris","Chrysocloa","Cynodon","Digitaria","Other","Themeda")
levels(RainavgYm$target.sp.)<-c("Chloris","Chrysocloa","Cynodon","Digitaria","Other","Themeda")
# Scale factor
scaleFactor <- mean(TargetNavgYm2$rain.sum,na.rm=T)/mean(TargetNavgYm2$N.conc.adj,na.rm=T)
SpTitle<-"Species"
# Convert to date - Yr-Month
TargetNavgYm$HarvestMonth<-as.Date(paste(TargetNavgYm$HarvestMonth,"-01-01",sep=""))
RainavgYm$HarvestMonth<-as.Date(paste(RainavgYm$HarvestMonth,"-01-01",sep=""))
# Treatment code for filling symbols
TargetNavgYm$spp.code<-as.factor(with(TargetNavgYm, paste(target.sp.,treatment,sep="_")))
RainavgYm$spp.code<-as.factor(with(RainavgYm, paste(target.sp.,treatment,sep="_")))
levels(TargetNavgYm$target.sp.)
levels(TargetNavgYm$spp.code)
#"cadetblue3","hotpink1","dodgerblue1","gold1", "grey50", "orangered3"
#"hotpink1","dodgerblue1", "chartreuse3", "orangered3","gold1"
# Plotting line graph with secondary y-axis (stuck on!)
TNYm<-ggplot(TargetNavgYm,aes(y=N.conc.adj,x=HarvestMonth, shape=treatment, group =grp.code)) # group = grouping vector for lines
TNYm<-TNYm+geom_line(data=RainavgYm,aes(y=(rain.sum+400)/200),colour="dark blue", linetype="dotted", size=1,show.legend=F)
TNYm<-TNYm+geom_point(data=RainavgYm,aes(y=(rain.sum+400)/200),colour="dark blue", fill="white", shape=21, size=2.5,stroke=1,show.legend=F)
TNYm<-TNYm+geom_line(aes(colour=target.sp.),position=position_dodge(width=.65),stat = "identity",size=.75,show.legend = F)
TNYm<-TNYm+geom_errorbar(data=TargetNavgYm,aes(ymin=N.conc.adj-SE, ymax=N.conc.adj+SE,colour=target.sp.),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
TNYm<-TNYm+geom_point(aes(colour=target.sp.,fill=spp.code),position=position_dodge(width=.65),size=5, stroke=1)
TNYm<-TNYm+facet_wrap(~region+landuse, ncol=5)
#TNYm<-TNYm+geom_line(data=RainavgYm2,aes(y=(rain.sum+400)/200),linetype="solid",colour="dark blue", size=1,show.legend=F) # Offset the second rainfall legend so it is above the N concnetration
#TNYm<-TNYm+geom_smooth(data=RainavgYm,aes(y=(rain.sum+400)/200),method="loess",span=.9, se=F,linetype="dotted",colour="dark blue", size=1,show.legend=F)
TNYm<-TNYm+scale_y_continuous(limits=c(0,5),sec.axis = sec_axis(~ . *200, breaks = c(0,200,400,600,800), labels = c("","",0,200,400), name = "Rainfall (mm)"),expand=c(0,0))
TNYm<-TNYm+scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y", limits=c(as.Date("2017-01-01"),max=as.Date("2018-03-01")),expand=c(0.1,0.1))
#TNYm<-TNYm+scale_x_discrete(expand=c(0,0)) # Discrete axis and then reducing x axis expand
TNYm<-TNYm+ylab("Plant nitrogen concentration (%)")+xlab("Year|month")
TNYm<-TNYm+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"))
TNYm<-TNYm+scale_fill_manual(values=c("chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white", "grey50","white","orangered3","white"))
TNYm<-TNYm+scale_shape_manual(values=c(22,21))
TNYm<-TNYm+theme_bw()+
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
        ,axis.text.x=element_text(size=13,color="black",angle=35,hjust=1,
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 14,hjust=0,angle=0)
        ,panel.spacing = unit(.1, "lines")
        ,legend.text = element_text(size=12,color="black")
        ,legend.position = "right"
        ,legend.direction="vertical"
        ,legend.title=element_text(size=12)
        ,legend.key.width = unit(1.2,"cm"))
#TNYm<-TNYm +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = as.Date("2017-01-01"), xend = as.Date("2017-01-01"), size = 1)
#TNYm<-TNYm +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = as.Date("2017-01-01"), xend = as.Date("2018-05-01"), size = 1)
TNYm<-TNYm+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1)),
                  colour = guide_legend("Dominant species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"),
                                                                               col=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"), stroke=1)) )
TNYm
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/SpeciesTimeN.jpeg",
#      width=40, height=16,units ="cm",dpi = 600, limitsize = TRUE)
#### Point graph - each species N compared to other #####
SpeciesN<-aggregate(N.conc.adj~target.sp.+landuse+treatment,TargetNavgYm,mean)
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
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/SpeciesTargetN.jpeg",
#       width=16, height=12,units ="cm",dpi = 600, limitsize = TRUE)
aggregate(N.conc.adj~target.sp.+landuse+treatment,NutsNP,mean)
aggregate(N.conc.adj~target.sp.+landuse,NutsNP,sd)

###########################################################################
##### N conc and productivity and consumption ####
###########################################################################
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils")
ProdCon<- read.csv('Moveable exclosures/Biomass.csv', sep=',',dec='.')#Wetseason data
names(ProdCon)
dim(ProdCon) # 356  38
dim(NutsONA) # 164  81
# Convert date to 'Year- Month'
Rdate3<-strptime(as.character(ProdCon$setup.date),format="%m/%d/%Y",tz="Africa/Nairobi" )
Rdate4<-strptime(as.character(ProdCon$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )
ProdCon$SetupMonth<-format(as.Date(Rdate3), "%Y-%m")
ProdCon$HarvestMonth<-format(as.Date(Rdate4), "%Y-%m")
#### Remove EX2 (metal) ####
# Remove metal mesh exclosure treatment (EX2) # but keep H0 (renamed here for analysis)
ProdCon1<-droplevels(ProdCon[ProdCon$treatment!="EX2" & ProdCon$treatment!="",]) # There is a gap!
dim(ProdCon1) # 300  40
ProdCon<-ProdCon1
# Remove H0 from nutrient dat
#NutsONA1b<-droplevels(NutsONA2[NutsONA2$harvest!="H0",])
#dim(NutsONA1b) # 144  91
ProdCon2<-left_join(NutsONA,ProdCon1, by=c("block.id","plot.id","landuse","treatment",
                                           "block.id.harvest", "site.id","block.id",
                                           "region","block","wp","lat","long",
                                           "growth.period","HarvestMonth","SetupMonth"))
dim(ProdCon2)
# Consumption rate
ProdCon2$consumption.total.g.m2.day # NA - consumption on the exclosure line?
table(ProdCon2$consumption.total.g.m2.day,ProdCon2$treatment)
ExCon <- ProdCon2[ProdCon2$consumption.total.g.m2.day & ProdCon2$treatment=="EX", ]
OpCon <- ProdCon2[ProdCon2$treatment=="OP", ]
ExOpCon<-left_join(ExCon,OpCon, by=c("block.id","landuse",
                                     "block.id.harvest", "site.id","block.id",
                                     "region","block","wp","lat","long",
                                     "growth.period"))
OpCon$Com.N.conc
ExOpCon$Com.N.conc.y #Open N
# Productivity is Open t0 Exclosure t1
ExOpCon$NProd.diff<-ExOpCon$Com.N.conc.x-ExOpCon$Com.N.conc.LAG.y
ExOpCon$PProd.diff<-ExOpCon$Com.P.conc.x-ExOpCon$Com.P.conc.LAG.y
plot(ExOpCon$productivity.total.g.m2.day.x~ExOpCon$PProd.diff)
abline(lm(ExOpCon$productivity.total.g.m2.day.x~ExOpCon$PProd.diff))
summary(lm(ExOpCon$productivity.total.g.m2.day.x~ExOpCon$PProd.diff))
plot(ExOpCon$productivity.total.g.m2.day.x~ExOpCon$NProd.diff)
abline(lm(ExOpCon$productivity.total.g.m2.day.x~ExOpCon$NProd.diff))
summary(lm(ExOpCon$productivity.total.g.m2.day.x~ExOpCon$NProd.diff))
# Community consumption and exclosure community N
names(ExOpCon)
ConN<-ggplot(ExOpCon,aes(y=consumption.total.g.m2.day.x,x=Com.N.conc.y))
ConN<-ConN+geom_point(size=2)
ConN<-ConN+facet_wrap(~landuse)
ConN
# Community productivity and exclosure community N
names(ProdCon2)
ProdN<-ggplot(ProdCon2,aes(y=productivity.total.g.m2.day,x=Com.N.conc))
ProdN<-ProdN+geom_point(size=2)
ProdN<-ProdN+facet_wrap(~landuse)
ProdN
# Community productivity and LAG N - STRONG RELATIONSHIP!!!! LAG N ****
ProdN<-ggplot(ProdCon2,aes(y=productivity.total.g.m2.day,x=Com.N.conc.LAG,
                           shape=treatment,colour=rain.sum.x))
#ProdN<-ProdN+geom_point(data=ProdCon2b,aes(x=Com.N.conc),colour="orangered3",alpha=.5,size=2)
ProdN<-ProdN+geom_point(size=2, colour = "black")
ProdN<-ProdN+facet_wrap(~landuse)
ProdN
summary(lm(productivity.total.g.m2.day~Com.N.conc.LAG,ProdCon2))

PROCON<-ggplot(ProdCon,aes(y=consumption.total.g.m2.day,x=productivity.total.g.m2.day,
                           shape=treatment))+geom_point()+facet_wrap(~landuse)
PROCON

###########################################################################
#### NITROGEN: Production and consumption influenced by community N ####
###########################################################################
####Nitrogen Exclosure N conc for each time point of consumption ####
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")) }
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
ProdCon2$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(ProdCon2$harvest.date.x,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Plot ID and blockcode without Harvest number
ProdCon2$plot.id<-as.factor(ProdCon2$plot.id)
ProdCon2$fblock<-as.factor(ProdCon2$block)
ProdCon2$block.code<-as.factor(with(ProdCon2, paste(region,landuse,block,sep="_")))
ProdCon2$plot.code<-as.factor(with(ProdCon2, paste(region,landuse,block,treatment,sep="_")))
levels(ProdCon2$plot.code) # 40 levels

# Remove NAs for nitrogen
#NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc), ])
table(ProdCon2$plot.code,ProdCon2$YrMonthNumber) # Month 8 and 15 few data points
ProdCon2$fYrMonthNumber<-as.factor(ProdCon2$YrMonthNumber)
levels(ProdCon2$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
ProdCon2<-droplevels(ProdCon2[!ProdCon2$fYrMonthNumber=="15",])
ProdCon2$YrMonthNumber<-as.numeric(as.character(ProdCon2$fYrMonthNumber))
table(ProdCon2$plot.code,ProdCon2$YrMonthNumber)
apply(table(ProdCon2$plot.code, ProdCon2$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
# Conditional temporal lag with nutrient N
#library(dplyr)
#ProdCon1<-ProdCon2 %>%
#  group_by(harvest.date.x) %>%
#  mutate(Com.N.conc.lag = dplyr::lag(Com.N.conc, n = 1, default = NA))
#summary(is.na(ProdCon1$Com.N.conc.lag)) # 34
#table(ProdCon1$plot.code,ProdCon1$harvest.x) # H0 missing now - everything shifted forwarded
#### Productivity  - with exclosure and open ####
ProdCon2b<-ProdCon2[!is.na(ProdCon2$productivity.total.g.m2.day) & !is.na(ProdCon2$Com.N.conc.LAG) &
                      is.finite(ProdCon2$Com.N.conc.LAG)  & !is.na(ProdCon2$rain.sum.x),]

# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/block.code/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ProdCon2b)
corMatrix(cs1AR1.)
#### LMM productivity and treatment ####
ProdCon2b$treatment<-as.factor(ProdCon2b$treatment)
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+landuse+rain.sum.x+
             treatment+landuse:rain.sum.x+landuse:treatment+
             Com.N.conc.LAG:treatment+
             Com.N.conc.LAG:landuse+Com.N.conc.LAG:rain.sum.x+
             Com.N.conc.LAG:rain.sum.x:landuse+
             Com.N.conc.LAG:rain.sum.x:treatment+
             Com.N.conc.LAG:treatment:landuse,
           random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1)
anova(Pmod1)
AIC(Pmod1) #665.1072

# Nothing... no relationship with current N conc
#### Productivity ####
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment#landuse:rain.sum.x,#landuse:treatment+
           #  Com.N.conc.LAG:treatment+
           +Com.N.conc.LAG:landuse,##Com.N.conc.LAG:rain.sum.x
           # Com.N.conc.LAG:rain.sum.x:landuse,
           # Com.N.conc.LAG:rain.sum.x:treatment,#+
           # Com.N.conc.LAG:treatment:landuse,
           random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1)
anova(Pmod1)
AIC(Pmod1) #599.1194
drop1(Pmod1,test="Chisq") # Com.N.conc.LAG:rain.sum.x:landuse    1 542.67 8.5513 0.003453 **
plot(productivity.total.g.m2.day~Com.N.conc.LAG,col=c(ProdCon2b$landuse),pch=c(ProdCon2b$treatment),ProdCon2b)
abline(lm(productivity.total.g.m2.day~Com.N.conc.LAG,ProdCon2b))
#Com.N.conc.LAG:rain.sum.x:landuse    1 542.67 8.5513 0.003453 **

##############################################################
#### Sketch fitted values ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/-
ProdCon2b$treatment<-as.factor(ProdCon2b$treatment)
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment+Com.N.conc.LAG:landuse,
           random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)

#A:Specify covariate values for predictions
MyData3 <- expand.grid(landuse=levels(ProdCon2b$landuse),
                       treatment=levels(ProdCon2b$treatment),
                       Com.N.conc.LAG = seq(min(ProdCon2b$Com.N.conc.LAG, na.rm=T), max(ProdCon2b$Com.N.conc.LAG,na.rm=T), length = 25),
                       rain.sum.x = seq(min(ProdCon2b$rain.sum.x, na.rm=T), max(ProdCon2b$rain.sum.x, na.rm=T), length = 25))
#B. Create X matrix with expand.grid
X3 <- model.matrix(~Com.N.conc.LAG+rain.sum.x+ landuse+
                     treatment+Com.N.conc.LAG:landuse, data = MyData3)
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyData3$SE <- sqrt(  diag(X3 %*% vcov(Pmod1) %*% t(X3))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData3$SeUp <- MyData3$Pred + 1.96 * MyData3$SE
MyData3$SeLo <- MyData3$Pred - 1.96 * MyData3$SE
#E. Plot predicted values
names(MyData3)
colnames(MyData3)[5]<-"productivity.total.g.m2.day"
MyData3b<-aggregate(productivity.total.g.m2.day~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeUp<-aggregate(SeUp~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeLo<-aggregate(SeLo~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
colnames(MyData3b)[4]<-"productivity.total.g.m2.day"
MyData3b$SeUp<-MyData3bSeUp$V1
MyData3b$SeLo<-MyData3bSeLo$V1
# Rename levels
levels(MyData3b$treatment)<-c("Exclosed","Open")
levels(ProdCon2b$treatment)<-c("Exclosed","Open")
levels(ProdCon2$treatment)<-c("Exclosed","Open")
levels(MyData3b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2$landuse)<-c("Pasture","Wildlife protected")
# Errors above and below y axis threshold
MyData3b$SeLo[MyData3b$SeLo<0.01]<-0
MyData3b$SeUp[MyData3b$SeUp>8.5]<-8.5
# Treatment code
ProdCon2b$trt.code<-as.factor(with(ProdCon2b, paste(landuse,treatment,sep="_")))
ProdCon2$trt.code<-as.factor(with(ProdCon2, paste(landuse,treatment,sep="_")))
MyData3b$trt.code<-as.factor(with(MyData3b, paste(landuse,treatment,sep="_")))

#### Plot observed data versus prediction #####
ProdCon2$treatment<-as.factor(ProdCon2$treatment)

# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(ProdCon2,aes(x=Com.N.conc.LAG, y=productivity.total.g.m2.day, shape=treatment)) # geom_errorbar
RN<-RN+geom_ribbon(data=MyData3b,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="grey50",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData3b,colour="grey50",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(aes(colour=landuse,fill=trt.code),size=3.5, stroke=1)
RN<-RN+facet_wrap(~landuse+treatment, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(0,3.25), breaks = c(0,1,2,3), labels = c(0,1,2,3), expand=c(0.02,0.02))
RN<-RN+scale_y_continuous(limits=c(-2.7,6.4),expand=c(0.1,0.1))
RN<-RN+ylab(expression(paste("Biomass production (g ",m^-2,"",day^-1,")")))+xlab(expression('Lagged'[t-1]*' plant nitrogen concentraion (%)')) # Adding x and ylabs to plot
RN<-RN+scale_shape_manual(values=c(22,21))
RN<-RN+scale_colour_manual(values=c("tan3","turquoise3"))
RN<-RN+scale_fill_manual(values=c("tan3","white","turquoise3","white"))
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=12,color="black")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0.4, xend = 0.4, size = 1)
#RN<-RN+annotate(geom = 'segment', y = -1.95, yend = -1.95, color = 'black', x = -Inf, xend = Inf, size = 1)
RN<-RN+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white"),col="grey30", stroke=1)),
              colour= guide_legend("Land-use",override.aes = list(shape=c(21), size=3.75,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=1)))
RN

#### Productivity ####
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment+landuse:rain.sum.x,#landuse:treatment+
           #  Com.N.conc.LAG:treatment+
           #Com.N.conc.LAG:rain.sum.x+Com.N.conc.LAG:landuse,
           # Com.N.conc.LAG:rain.sum.x:landuse,
           # Com.N.conc.LAG:rain.sum.x:treatment,#+
           # Com.N.conc.LAG:treatment:landuse,
           random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
#### Productivity ####
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment+landuse:rain.sum.x,#landuse:treatment+
           #  Com.N.conc.LAG:treatment+
           #Com.N.conc.LAG:rain.sum.x+Com.N.conc.LAG:landuse,
           # Com.N.conc.LAG:rain.sum.x:landuse,
           # Com.N.conc.LAG:rain.sum.x:treatment,#+
           # Com.N.conc.LAG:treatment:landuse,
           random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1)
anova(Pmod1)
AIC(Pmod1) #599.1194
drop1(Pmod1,test="Chisq") # Com.N.conc.LAG:rain.sum.x:landuse    1 542.67 8.5513 0.003453 **

#### Productivity ####
ProdCon2b$treatment<-as.factor(ProdCon2b$treatment)
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment+landuse:rain.sum.x+#landuse:treatment+
             #  Com.N.conc.LAG:treatment+
             #Com.N.conc.LAG:rain.sum.x+Com.N.conc.LAG:landuse,
             Com.N.conc.LAG:rain.sum.x:landuse,
           # Com.N.conc.LAG:rain.sum.x:treatment,#+
           # Com.N.conc.LAG:treatment:landuse,
           random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1)
anova(Pmod1)
AIC(Pmod1) #599.1194
drop1(Pmod1,test="Chisq") # Com.N.conc.LAG:rain.sum.x:landuse    1 542.67 8.5513 0.003453 **
#Com.N.conc.LAG:rain.sum.x:landuse  2 751.81 13.656 0.0010830 **

# Model significance
Pmod1b<-update(Pmod1,.~.-Com.N.conc.LAG:rain.sum.x:landuse)
Pmod1c<-update(Pmod1b,.~.-rain.sum.x:landuse)
Pmod1d<-update(Pmod1c,.~.-treatment)

anova(Pmod1,Pmod1b) # Pmod1b     2 10 751.8137 784.7467 -365.9068 1 vs 2 13.65603  0.0011
anova(Pmod1b,Pmod1c) # NS
anova(Pmod1c,Pmod1d) # Treatment 

# Predictions
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment+landuse:rain.sum.x+Com.N.conc.LAG:landuse:rain.sum.x,
           random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)

#A:Specify covariate values for predictions
MyData3 <- expand.grid(landuse=levels(ProdCon2b$landuse),
                       treatment=levels(ProdCon2b$treatment),
                       Com.N.conc.LAG = seq(min(ProdCon2b$Com.N.conc.LAG, na.rm=T), max(ProdCon2b$Com.N.conc.LAG,na.rm=T), length = 25),
                       rain.sum.x = seq(min(ProdCon2b$rain.sum.x, na.rm=T), max(ProdCon2b$rain.sum.x, na.rm=T), length = 25))
#B. Create X matrix with expand.grid
X3 <- model.matrix(~Com.N.conc.LAG+rain.sum.x+landuse+treatment+landuse:rain.sum.x, data = MyData3)
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyData3$SE <- sqrt(  diag(X3 %*% vcov(Pmod1) %*% t(X3))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData3$SeUp <- MyData3$Pred + 1.96 * MyData3$SE
MyData3$SeLo <- MyData3$Pred - 1.96 * MyData3$SE
#E. Plot predicted values
names(MyData3)
colnames(MyData3)[5]<-"productivity.total.g.m2.day"
MyData3b<-aggregate(productivity.total.g.m2.day~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeUp<-aggregate(SeUp~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeLo<-aggregate(SeLo~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
colnames(MyData3b)[4]<-"productivity.total.g.m2.day"
MyData3b$SeUp<-MyData3bSeUp$V1
MyData3b$SeLo<-MyData3bSeLo$V1
# Rename levels
levels(MyData3b$treatment)<-c("Exclosed","Open")
levels(ProdCon2b$treatment)<-c("Exclosed","Open")
levels(ProdCon2$treatment)<-c("Exclosed","Open")
levels(MyData3b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2$landuse)<-c("Pasture","Wildlife protected")
# Errors above and below y axis threshold
MyData3b$SeLo[MyData3b$SeLo<0.01]<-0
MyData3b$SeUp[MyData3b$SeUp>8.5]<-8.5
# Treatment code
ProdCon2b$trt.code<-as.factor(with(ProdCon2b, paste(landuse,treatment,sep="_")))
ProdCon2$trt.code<-as.factor(with(ProdCon2, paste(landuse,treatment,sep="_")))
MyData3b$trt.code<-as.factor(with(MyData3b, paste(landuse,treatment,sep="_")))
#### Plot observed data versus prediction #####
ProdCon2$treatment<-as.factor(ProdCon2$treatment)
# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(ProdCon2,aes(x=Com.N.conc.LAG, y=productivity.total.g.m2.day, shape=treatment)) # geom_errorbar
RN<-RN+geom_ribbon(data=MyData3b,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="grey50",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData3b,colour="grey50",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(aes(colour=landuse,fill=trt.code),size=3.5, stroke=1)
RN<-RN+facet_wrap(~landuse+treatment, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(0,3.25), breaks = c(0,1,2,3), labels = c(0,1,2,3), expand=c(0.02,0.02))
RN<-RN+scale_y_continuous(limits=c(-2.7,6.4),expand=c(0.1,0.1))
RN<-RN+ylab(expression(paste("Biomass production (g ",m^-2,"",day^-1,")")))+xlab(expression('Lagged'[t-1]*' plant nitrogen concentraion (%)')) # Adding x and ylabs to plot
RN<-RN+scale_shape_manual(values=c(22,21))
RN<-RN+scale_colour_manual(values=c("tan3","turquoise3"))
RN<-RN+scale_fill_manual(values=c("tan3","white","turquoise3","white"))
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=12,color="black")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0.4, xend = 0.4, size = 1)
#RN<-RN+annotate(geom = 'segment', y = -1.95, yend = -1.95, color = 'black', x = -Inf, xend = Inf, size = 1)
RN<-RN+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white"),col="grey30", stroke=1)),
              colour= guide_legend("Land-use",override.aes = list(shape=c(21), size=3.75,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=1)))
RN


ProdCon2b$treatment<-as.factor(ProdCon2b$treatment)
Pmod1<-lme(productivity.total.g.m2.day~Com.N.conc.LAG+rain.sum.x+ landuse+
             treatment+landuse:rain.sum.x+Com.N.conc.LAG:landuse:rain.sum.x,
           random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
           correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)

#A:Specify covariate values for predictions
MyData3 <- expand.grid(landuse=levels(ProdCon2b$landuse),
                       treatment=levels(ProdCon2b$treatment),
                       Com.N.conc.LAG = seq(min(ProdCon2b$Com.N.conc.LAG, na.rm=T), max(ProdCon2b$Com.N.conc.LAG,na.rm=T), length = 25),
                       rain.sum.x = seq(min(ProdCon2b$rain.sum.x, na.rm=T), max(ProdCon2b$rain.sum.x, na.rm=T), length = 25))
#B. Create X matrix with expand.grid
X3 <- model.matrix(~Com.N.conc.LAG+rain.sum.x+landuse+treatment+landuse:rain.sum.x, data = MyData3)
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyData3$SE <- sqrt(  diag(X3 %*% vcov(Pmod1) %*% t(X3))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData3$SeUp <- MyData3$Pred + 1.96 * MyData3$SE
MyData3$SeLo <- MyData3$Pred - 1.96 * MyData3$SE
#E. Plot predicted values
names(MyData3)
colnames(MyData3)[5]<-"productivity.total.g.m2.day"
MyData3b<-aggregate(productivity.total.g.m2.day~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeUp<-aggregate(SeUp~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeLo<-aggregate(SeLo~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
colnames(MyData3b)[4]<-"productivity.total.g.m2.day"
MyData3b$SeUp<-MyData3bSeUp$V1
MyData3b$SeLo<-MyData3bSeLo$V1
# Rename levels
levels(MyData3b$treatment)<-c("Exclosed","Open")
levels(ProdCon2b$treatment)<-c("Exclosed","Open")
levels(ProdCon2$treatment)<-c("Exclosed","Open")
levels(MyData3b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2$landuse)<-c("Pasture","Wildlife protected")
# Errors above and below y axis threshold
MyData3b$SeLo[MyData3b$SeLo<0.01]<-0
MyData3b$SeUp[MyData3b$SeUp>8.5]<-8.5
# Treatment code
ProdCon2b$trt.code<-as.factor(with(ProdCon2b, paste(landuse,treatment,sep="_")))
ProdCon2$trt.code<-as.factor(with(ProdCon2, paste(landuse,treatment,sep="_")))
MyData3b$trt.code<-as.factor(with(MyData3b, paste(landuse,treatment,sep="_")))
#### Plot observed data versus prediction #####
ProdCon2$treatment<-as.factor(ProdCon2$treatment)
# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(ProdCon2,aes(x=Com.N.conc.LAG, y=productivity.total.g.m2.day, shape=treatment)) # geom_errorbar
RN<-RN+geom_ribbon(data=MyData3b,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="grey50",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData3b,colour="grey50",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(aes(colour=landuse,fill=trt.code),size=3.5, stroke=1)
RN<-RN+facet_wrap(~landuse+treatment, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(0,3.25), breaks = c(0,1,2,3), labels = c(0,1,2,3), expand=c(0.02,0.02))
RN<-RN+scale_y_continuous(limits=c(-2.7,6.4),expand=c(0.1,0.1))
RN<-RN+ylab(expression(paste("Biomass production (g ",m^-2,"",day^-1,")")))+xlab(expression('Lagged'[t-1]*' plant nitrogen concentraion (%)')) # Adding x and ylabs to plot
RN<-RN+scale_shape_manual(values=c(22,21))
RN<-RN+scale_colour_manual(values=c("tan3","turquoise3"))
RN<-RN+scale_fill_manual(values=c("tan3","white","turquoise3","white"))
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=12,color="black")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0.4, xend = 0.4, size = 1)
#RN<-RN+annotate(geom = 'segment', y = -1.95, yend = -1.95, color = 'black', x = -Inf, xend = Inf, size = 1)
RN<-RN+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white"),col="grey30", stroke=1)),
              colour= guide_legend("Land-use",override.aes = list(shape=c(21), size=3.75,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=1)))
RN
#A:Specify covariate values for predictions
MyData3 <- expand.grid(landuse=levels(ProdCon2b$landuse),
                       treatment=levels(ProdCon2b$treatment),
                       Com.N.conc.LAG = seq(min(ProdCon2b$Com.N.conc.LAG, na.rm=T), max(ProdCon2b$Com.N.conc.LAG,na.rm=T), length = 25),
                       rain.sum.x = seq(min(ProdCon2b$rain.sum.x, na.rm=T), max(ProdCon2b$rain.sum.x, na.rm=T), length = 25))
#A:Specify covariate values for predictions
MyData3 <- expand.grid(landuse=levels(ProdCon2b$landuse),
                       treatment=levels(ProdCon2b$treatment),
                       Com.N.conc.LAG = seq(min(ProdCon2b$Com.N.conc.LAG, na.rm=T), max(ProdCon2b$Com.N.conc.LAG,na.rm=T), length = 25),
                       rain.sum.x = seq(min(ProdCon2b$rain.sum.x, na.rm=T), max(ProdCon2b$rain.sum.x, na.rm=T), length = 25))
#B. Create X matrix with expand.grid
X3 <- model.matrix(~Com.N.conc.LAG+rain.sum.x+landuse+treatment+landuse:rain.sum.x, data = MyData3)
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
MyData3$SE <- sqrt(  diag(X3 %*% vcov(Pmod1) %*% t(X3))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData3$SeUp <- MyData3$Pred + 1.96 * MyData3$SE
MyData3$SeLo <- MyData3$Pred - 1.96 * MyData3$SE
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
#B. Create X matrix with expand.grid
X3 <- model.matrix(~Com.N.conc.LAG+rain.sum.x+ landuse+
                     treatment+landuse:rain.sum.x+Com.N.conc.LAG:landuse:rain.sum.x, data = MyData3)
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
MyData3$SE <- sqrt(  diag(X3 %*% vcov(Pmod1) %*% t(X3))  )
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(Pmod1)  # = X * beta
MyData3$SE <- sqrt(  diag(X3 %*% vcov(Pmod1) %*% t(X3))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData3$SeUp <- MyData3$Pred + 1.96 * MyData3$SE
MyData3$SeLo <- MyData3$Pred - 1.96 * MyData3$SE
#E. Plot predicted values
names(MyData3)
colnames(MyData3)[5]<-"productivity.total.g.m2.day"
MyData3b<-aggregate(productivity.total.g.m2.day~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeUp<-aggregate(SeUp~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
MyData3bSeLo<-aggregate(SeLo~Com.N.conc.LAG+landuse+treatment,MyData3,mean)
colnames(MyData3b)[4]<-"productivity.total.g.m2.day"
MyData3b$SeUp<-MyData3bSeUp$V1
MyData3b$SeLo<-MyData3bSeLo$V1
# Rename levels
levels(MyData3b$treatment)<-c("Exclosed","Open")
levels(ProdCon2b$treatment)<-c("Exclosed","Open")
levels(ProdCon2$treatment)<-c("Exclosed","Open")
levels(MyData3b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2b$landuse)<-c("Pasture","Wildlife protected")
levels(ProdCon2$landuse)<-c("Pasture","Wildlife protected")
# Errors above and below y axis threshold
MyData3b$SeLo[MyData3b$SeLo<0.01]<-0
MyData3b$SeUp[MyData3b$SeUp>8.5]<-8.5
# Treatment code
ProdCon2b$trt.code<-as.factor(with(ProdCon2b, paste(landuse,treatment,sep="_")))
ProdCon2$trt.code<-as.factor(with(ProdCon2, paste(landuse,treatment,sep="_")))
MyData3b$trt.code<-as.factor(with(MyData3b, paste(landuse,treatment,sep="_")))
#### Plot observed data versus prediction #####
ProdCon2$treatment<-as.factor(ProdCon2$treatment)

# Scatter plot with Community N concentrations and rainfall
RN<-ggplot(ProdCon2,aes(x=Com.N.conc.LAG, y=productivity.total.g.m2.day, shape=treatment)) # geom_errorbar
RN<-RN+geom_ribbon(data=MyData3b,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="grey50",alpha=.65,lwd=NA,show.legend=F)
RN<-RN+geom_line(data=MyData3b,colour="grey50",alpha=.9,lwd=2,show.legend=F)
RN<-RN+geom_point(aes(colour=landuse,fill=trt.code),size=3.5, stroke=1)
RN<-RN+facet_wrap(~landuse+treatment, scale="fixed")
RN<-RN+scale_x_continuous(limits=c(0,3.25), breaks = c(0,1,2,3), labels = c(0,1,2,3), expand=c(0.02,0.02))
RN<-RN+scale_y_continuous(limits=c(-2.7,6.4),expand=c(0.1,0.1))
RN<-RN+ylab(expression(paste("Biomass production (g ",m^-2,"",day^-1,")")))+xlab(expression('Lagged'[t-1]*' plant nitrogen concentraion (%)')) # Adding x and ylabs to plot
RN<-RN+scale_shape_manual(values=c(22,21))
RN<-RN+scale_colour_manual(values=c("tan3","turquoise3"))
RN<-RN+scale_fill_manual(values=c("tan3","white","turquoise3","white"))
RN<-RN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=12,color="black")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#RN<-RN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0.4, xend = 0.4, size = 1)
#RN<-RN+annotate(geom = 'segment', y = -1.95, yend = -1.95, color = 'black', x = -Inf, xend = Inf, size = 1)
RN<-RN+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("white"),col="grey30", stroke=1)),
              colour= guide_legend("Land-use",override.aes = list(shape=c(21), size=3.75,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=1)))
RN

#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/LaggedNprod.jpeg",
#       width=25, height=18,units ="cm",dpi = 600, limitsize = TRUE)

# SCRIPT SHOULD WORK TO HERE UNINTERRUPED!

#######################################################################################
#### Productivity TARGET AND OTHER SPECIES - LAG NITROGEN CONCENTRATION####
#####################################################################################

setwd("/Users/anotherswsmith/Documents/Teaching/R_workshop/")
BioStack<- read.csv('Biomass.Stacked.csv', sep=',',dec='.')#Wetseason data
names(BioStack)
dim(BioStack) # 712  35
setwd("/Users/anotherswsmith/Documents/Teaching/R_workshop/")
Nuts<-read.csv(file="Nuts4.csv", sep=",",header=TRUE)
# Convert date to 'Year- Month'
Rdate1<-strptime(as.character(Nuts$setup.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
Rdate2<-strptime(as.character(Nuts$harvest.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
Nuts$SetupMonth<-format(as.Date(Rdate1), "%Y-%m")
Nuts$HarvestMonth<-format(as.Date(Rdate2), "%Y-%m")
Rdate5<-strptime(as.character(BioStack$setup.date),format="%m/%d/%Y",tz="Africa/Nairobi" )
Rdate6<-strptime(as.character(BioStack$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )
BioStack$SetupMonth<-format(as.Date(Rdate5), "%Y-%m")
BioStack$HarvestMonth<-format(as.Date(Rdate6), "%Y-%m")
#### Remove poor quality data from datset ####
# Remove metal mesh exclosure treatment (EX2) # but keep H0 (renamed here for analysis)
Nuts1<-droplevels(Nuts[Nuts$treatment!="EX2",])
BioStack1<-droplevels(BioStack[BioStack$treatment!="EX2" & !BioStack$treatment=="H0",])
dim(Nuts1) #640  44
dim(BioStack1) #600  37
Nuts<-Nuts1
BioStack<-BioStack1
# Remove extremes N<3.5% and P<0.5% - this data is due to measurement error - machine drifting - I do not believe low values
dotchart(Nuts$N.conc.adj, groups=Nuts$target_other) # Some outliers - large over 8 conc
dotchart(Nuts$P.conc.adj,groups=Nuts$target_other) # Outliers above 0.51%
NutsN<-droplevels(subset(Nuts,N.conc.adj.lag<3.6 & N.conc.adj.lag>0.5 | is.na(N.conc.adj))) # Subset by x & y but incl. NAs
NutsNP<-droplevels(subset(NutsN,P.conc.adj.lag<0.51 | is.na(P.conc.adj.lag)))
dim(Nuts) #  640  44
dim(NutsNP) #  448  45
dotchart(NutsNP$N.conc.adj.lag,groups=NutsNP$target_other)
dotchart(NutsNP$P.conc.adj.lag,groups=NutsNP$target_other) #Nice spread now
# Housekeeping - numbers to factors
NutsNP$plot.code<-as.factor(with(NutsNP, paste(region,landuse,block,treatment,sep="_"))) # No plot code - includes harvest...
NutsNP$fblock<-as.factor(as.numeric(NutsNP$block.id))
names(BioStack)
levels(BioStack$target.sp.)
levels(NutsNP$target.sp.)
Tother<-merge(BioStack,NutsNP, by=c("block.id","plot.id","landuse","treatment","target.sp.",
                                    "block.id.harvest", "site.id","block.id",
                                    "region","block","wp","lat","long",
                                    "growth.period","SetupMonth","HarvestMonth"))
dim(BioStack)
dim(NutsNP)
dim(Tother)
#write.table(Tother, "Tother.txt", row.name=F, sep="\t")
# Productivity versus consumption across target and other species
BioStackt<-droplevels(BioStack[BioStack$target.sp.!="other",])
BioStackO<-droplevels(BioStack[BioStack$target.sp.=="other",])

TotherS<-ggplot(BioStackt,aes(x=productivity.g.m2.day,y=consumption.g.m2.day,shape=target.sp.,colour=rain.sum))+geom_point()
TotherS<-TotherS+geom_point()
TotherS<-TotherS+facet_wrap(~landuse)
TotherS

TotherSo<-ggplot(BioStackO,aes(x=productivity.g.m2.day,y=consumption.g.m2.day,shape=target.sp.,colour=rain.sum))+geom_point()
TotherSo<-TotherSo+geom_point()
TotherSo<-TotherSo+facet_wrap(~landuse)
TotherSo

# Running numeric value for months

# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")) }
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
Tother$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(Tother$harvest.date.y,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Plot ID and blockcode without Harvest number
Tother$plot.id<-as.factor(Tother$plot.id)
Tother$fblock<-as.factor(Tother$block)
Tother$block.code<-as.factor(with(Tother, paste(region,landuse,block,sep="_")))
Tother$plot.code<-as.factor(with(Tother, paste(region,landuse,block,treatment,target.sp.,sep="_")))
levels(Tother$plot.code) # 80 levels
# Remove NAs for nitrogen
#NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc), ])
table(Tother$plot.code,Tother$YrMonthNumber) # Month 8 and 15 few data points
Tother$fYrMonthNumber<-as.factor(Tother$YrMonthNumber)
levels(Tother$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
Tother$YrMonthNumber<-as.numeric(as.character(Tother$fYrMonthNumber))
table(Tother$plot.code,Tother$YrMonthNumber)
apply(table(Tother$plot.code, Tother$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
#### Productivity  - with exclosure and open ####
names(Tother)
Tothert<-Tother[!is.na(Tother$N.conc.adj.lag),]
Tothert$N.conc.adj.lag
table(Tothert$plot.code,Tothert$YrMonthNumber)

# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/block.code/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =Tothert)
corMatrix(cs1AR1.)

#Remove NAs
Tothert1 <- droplevels(Tothert[!is.na(Tothert$productivity.g.m2.day) & !is.na(Tothert$rain.sum.x) &
                                 !is.na(Tothert$N.conc.adj),])

# Model with N.conc.adj
Pmod1tN<-lme(productivity.g.m2.day~N.conc.adj+rain.sum.x+target.sp.+
              treatment+target.sp.:rain.sum.x+#target.sp.:treatment+
              #N.conc.adj:treatment+
              N.conc.adj.lag:rain.sum.x,
            # N.conc.adj.lag:rain.sum.x:target.sp.,
            #N.conc.adj.lag:rain.sum.x:treatment+
            # N.conc.adj.lag:treatment:target.sp.,
            random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=Tothert1)
summary(Pmod1tN)
anova(Pmod1tN)

#Pairwise contrasts *ghlt* and *lsmeans*
library(multcomp)
library(emmeans)

# Species
summary(glht(Pmod1tN, mcp(target.sp.="Tukey"))) # Nothing
emm.s.recalmod <- emmeans(Pmod1tN,~target.sp.)
pairs(emm.s.recalmod) # Nothing singificantly different

# Remove NAs
Tothert2 <- droplevels(Tothert[!is.na(Tothert$productivity.g.m2.day) & !is.na(Tothert$rain.sum.x) &
                                 !is.na(Tothert$N.conc.adj.lag),])

# Model with N.conc.adj.lag
Pmod1t<-lme(productivity.g.m2.day~N.conc.adj.lag+rain.sum.x+
              treatment+target.sp.:rain.sum.x+#target.sp.:treatment+
              #N.conc.adj.lag:treatment+
              N.conc.adj.lag:rain.sum.x,
            # N.conc.adj.lag:rain.sum.x:target.sp.,
            #N.conc.adj.lag:rain.sum.x:treatment+
            # N.conc.adj.lag:treatment:target.sp.,
            random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=Tothert2)
summary(Pmod1t)
anova(Pmod1t)
AIC(Pmod1t) # 491.4142
drop1(Pmod1t,test="Chisq") # All significant...

names(Tothert)

plot(productivity.total.g.m2.day~Com.N.conc.LAG,col=c(ProdCon2b$landuse),pch=c(ProdCon2b$treatment),ProdCon2b)
abline(lm(productivity.total.g.m2.day~Com.N.conc.LAG,ProdCon2b))

ggplot(Tothert,aes(y=productivity.g.m2.day,x=N.conc.adj, colour=target.sp.,size=rain.sum.x/10))+#, size=rain.sum.x/10
  geom_point()+facet_wrap(~landuse+target.sp., scale="free") #+
#geom_text(aes(label=HarvestMonth),hjust=0, vjust=0)

ggplot(Tothert,aes(y=productivity.g.m2.day,x=P.conc.adj, colour=target.sp.))+
  geom_point()+facet_wrap(~landuse+target.sp., scale="free")

ggplot(Tothert,aes(y=consumption.g.m2.day,x=P.conc.adj, colour=target.sp.))+
  geom_point()+facet_wrap(~landuse+target.sp., scale="free")

abline(lm(productivity.g.m2.day~N.conc.adj.lag,Tothert))
summary(lm(productivity.g.m2.day~N.conc.adj.lag:target.sp.:landuse,Tothert))

Pmod1t<-lme(productivity.g.m2.day~consumption.g.m2.day+rain.sum.x+
              target.sp.:rain.sum.x+#target.sp.:treatment+
              #N.conc.adj.lag:treatment+
            # landuse:consumption.g.m2.day+
               target.sp.:consumption.g.m2.day,
            # N.conc.adj.lag:rain.sum.x:target.sp.,
            #N.conc.adj.lag:rain.sum.x:treatment+
            # N.conc.adj.lag:treatment:target.sp.,
            random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=Tothert2)
summary(Pmod1t)
anova(Pmod1t)
AIC(Pmod1t) # 491.4142
drop1(Pmod1t,test="Chisq") # All significant...


#### R-INLA PRODUCTIVITY ####
library(INLA)
library(nlme)
library(mgcv)
library(brinla)
# Global biomass model with
class(ProdCon2$YrMonthNumber)
Tothert$fYrMonthNumber<-as.factor(Tothert$YrMonthNumber)
Tothert$block.code
Tothert$plot.codeID<-as.numeric(Tothert$plot.code)
fProdT<- productivity.total.g.m2.day ~  f(fYrMonthNumber, model="ar1", replicate=plot.codeID)+
  N.conc.adj.lag+rain.sum.x+treatment+
  target.sp.:rain.sum.x+target.sp.:treatment+
  +target.sp.:landuse+
  N.conc.adj.lag:treatment+
  N.conc.adj.lag:rain.sum.x+
  N.conc.adj.lag:landuse+
  #N.conc.adj.lag:rain.sum.x:target.sp.+
  #N.conc.adj.lag:rain.sum.x:treatment+
  #N.conc.adj.lag:treatment:target.sp.+
  f(region, model = "iid")+f(block.code, model = "iid")
# Productivity model
ProModT <- inla(fProdT, family = "gaussian",
                control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE),
                data = Tothert)
bri.fixed.plot(ProModT) # Nothing is important - lots of marginality
# Landuse W x rain x LAG N - most likely important
# Lag x rain
# Round summary
BetasProd <-  ProModT$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")]
print(round(BetasProd , digits = 3)) # Issues betas are all really small
# plot betas - compare model with and without rainfall
BetasProd $mod<-"Productivity model"
BetasProd $names <- rownames(BetasProd)
colnames(BetasProd)[3]<-'SeLo'
colnames(BetasProd )[4]<-'SeUp'
pp<-ggplot(BetasProd , aes(x=names, y=mean,ymin=SeLo, ymax=SeUp, fill=mod,colour=mod))
pp<- pp+geom_hline(yintercept =0, colour="grey")
pp<- pp+ geom_errorbar(width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
pp<- pp+ geom_point(size = 2, stroke=.5,position=position_dodge(width=.65))
pp<-pp+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
pp
###################################################################################
#### Consumption and lagged nitrogen ####
# Remove NAs for consumption
ProdCon2a <- droplevels(ProdCon2[!is.na(ProdCon2$consumption.total.g.m2.day) & 
                                   !is.na(ProdCon2$Com.N.conc),])
ProdCon2b <- droplevels(ProdCon2[!is.na(ProdCon2$consumption.total.g.m2.day) & !is.na(ProdCon2$rain.sum.x) &
                                   !is.na(ProdCon2$Com.N.conc),])
ProdCon2c <- droplevels(ProdCon2[!is.na(ProdCon2$consumption.total.g.m2.day) & !is.na(ProdCon2$rain.sum.x) &
                                   !is.na(ProdCon2$Com.N.conc.LAG),])
# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/fblock/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ProdCon2c)
corMatrix(cs1AR1.)
# Non Lagged nitrogen and consumption
B2<-lme(consumption.total.g.m2.day~Com.N.conc+landuse,#rain.sum.x,
        # Com.N.conc:landuse+Com.N.conc:rain.sum.x,
        # Com.N.conc:rain.sum.x:landuse,
        random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2a)
summary(B2)
anova(B2)
AIC(B2)
drop1(B2,test="Chisq") # NOTHING...

# Lagged nitrogen and consumption
B3<-lme(consumption.total.g.m2.day~Com.N.conc.LAG+landuse+rain.sum.x+
          Com.N.conc.LAG:landuse+Com.N.conc.LAG:rain.sum.x+
          Com.N.conc.LAG:rain.sum.x:landuse,
        random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2c)
summary(B3)
anova(B3)
AIC(B3) # 298.6639
# Only Lag N
ProdCon2a <- droplevels(ProdCon2[!is.na(ProdCon2$consumption.total.g.m2.day) & !is.na(ProdCon2$Com.N.conc),])
B3<-lme(consumption.total.g.m2.day~Com.N.conc.LAG,#+landuse+rain.sum.x,
        #Com.N.conc.LAG:landuse+Com.N.conc.LAG:rain.sum.x,
        #Com.N.conc.LAG:rain.sum.x:landuse,
        random= ~ 1|region/block.code, na.action=na.pass, method="ML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2a)
summary(B3)
anova(B3) # All 3 x significant
AIC(B3) # 254.9133
drop1(B3,test="Chisq")
plot(consumption.total.g.m2.day~Com.N.conc.LAG,col=c(ProdCon2c$landuse),ProdCon2c)
abline(lm(consumption.total.g.m2.day~Com.N.conc.LAG,ProdCon2c))
#### INLA consumption ###
fCon<-consumption.total.g.m2.day ~  f(fYrMonthNumber, model="ar1", replicate=plot.codeID)+
  Com.N.conc.LAG+landuse+rain.sum.x+
  Com.N.conc:landuse+Com.N.conc:rain.sum.x+
  Com.N.conc:rain.sum.x:landuse+
  Com.N.conc.LAG:landuse+Com.N.conc.LAG:rain.sum.x+
  Com.N.conc.LAG:rain.sum.x:landuse+
  f(region, model = "iid")+f(block.code, model = "iid")
# Consumption model
ConMod <- inla(fCon, family = "gaussian",
               control.compute = list(waic=TRUE, cpo=TRUE, dic=TRUE),
               data = ProdCon2c)
bri.fixed.plot(ConMod) # Nothing is important

##############################################################
#### Sketch fitted values ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/-

B3<-lme(consumption.total.g.m2.day~Com.N.conc.LAG,
        random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2a)
anova(B3)
#A:Specify covariate values for predictions
MyData3 <- expand.grid(
  Com.N.conc.LAG = seq(min(ProdCon2c$Com.N.conc.LAG, na.rm=T), max(ProdCon2c$Com.N.conc.LAG,na.rm=T), length = 25))
#B. Create X matrix with expand.grid
X3 <- model.matrix(~Com.N.conc.LAG, data = MyData3)
head(X3)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData3$Pred <- X3 %*% fixef(B3)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyData3$SE <- sqrt(  diag(X3 %*% vcov(B3) %*% t(X3))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData3$SeUp <- MyData3$Pred + 1.96 * MyData3$SE
MyData3$SeLo <- MyData3$Pred - 1.96 * MyData3$SE
#E. Plot predicted values
names(MyData3)
colnames(MyData3)[2]<-"productivity.total.g.m2.day"
# Treatment code
#ProdCon2c$trt.code<-as.factor(with(ProdCon2c, paste(landuse,treatment,sep="_")))
# Rename levels
levels(ProdCon2c$treatment)<-c("Exclosed","Open")
levels(ProdCon2c$landuse)<-c("Pasture","Wildlife protected")
#### Plot observed data versus prediction #####
# Scatter plot with Community N concentrations and rainfall
CN<-ggplot(ProdCon2,aes(x=Com.N.conc.LAG, y=productivity.total.g.m2.day)) # geom_errorbar
#CN<-CN+geom_ribbon(data=MyData3,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="grey50",alpha=.65,lwd=NA,show.legend=F)
#CN<-CN+geom_line(data=MyData3,colour="grey50",alpha=.9,lwd=2,show.legend=F)
CN<-CN+geom_point(aes(fill=landuse,colour=landuse),size=3.5, stroke=1)
CN<-CN+facet_wrap(~landuse, scale="fixed")
CN<-CN+scale_x_continuous(limits=c(0,3.25), breaks = c(0,1,2,3), labels = c(0,1,2,3), expand=c(0.05,0.05))
CN<-CN+scale_y_continuous(limits=c(-1.95,6.5),expand=c(0,0))
CN<-CN+ylab(expression(paste("Herbivore consumption (g ",m^-2,"",day^-1,")")))+xlab(expression('Lagged'[t-1]*' plant nitrogen concentraion (%)')) # Adding x and ylabs to plot
CN<-CN+scale_shape_manual(values=c(21))
CN<-CN+scale_colour_manual("Land-use",values=c("tan3","turquoise3"))
CN<-CN+scale_fill_manual("Land-use",values=c("tan3","turquoise3"))
CN<-CN+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#CN<-CN+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0.4, xend = 0.4, size = 1)
#CN<-CN+annotate(geom = 'segment', y = -1.95, yend = -1.95, color = 'black', x = -Inf, xend = Inf, size = 1)
CN

ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/LaggedNcons.jpeg",
       width=15, height=10,units ="cm",dpi = 600, limitsize = TRUE)


##### Open Nitrogen consumption and productivity ####
ExOpCon$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(ExOpCon$harvest.date.x.x,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Plot ID without Harvest number
ExOpCon$plot.id.x<-as.factor(ExOpCon$plot.id.x)
ExOpCon$fblock<-as.factor(ExOpCon$block)
ExOpCon$plot.code<-as.factor(with(ExOpCon, paste(region,landuse,block,sep="_")))
levels(ExOpCon$plot.code) # 20 levels
# Remove NAs for nitrogen
#NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc), ])
table(ExOpCon$plot.code,ExOpCon$YrMonthNumber) # Month 8 and 15 few data points
ExOpCon$fYrMonthNumber<-as.factor(ExOpCon$YrMonthNumber)
levels(ExOpCon$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
ExOpCon$YrMonthNumber<-as.numeric(as.character(ExOpCon$fYrMonthNumber))
table(ExOpCon$plot.code,ExOpCon$YrMonthNumber)
apply(table(ExOpCon$plot.code, ExOpCon$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
# Create a temporal lag with nutrient N
#ExOpCon1 <- ExOpCon%>%
#  group_by(harvest.date.x.x) %>%
#  mutate(Com.N.conc.lag = dplyr::lag(-Com.N.conc.y, n = 1, default = NA))
#summary(is.na(ExOpCon2$Com.N.conc.lag)) # 28
# Remove NAs for consumption
ExOpCon2 <- droplevels(ExOpCon[!is.na(ExOpCon$consumption.total.g.m2.day.x) & !is.na(ExOpCon$Com.N.conc.LAG.y) , ])
# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/fblock/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ExOpCon2)
corMatrix(cs1AR1.)
# LMM difference in Nitrogen versus consumption - each time point
B3Op<-lme(consumption.total.g.m2.day.x~Com.N.conc.LAG.y+
            landuse+rain.sum.x.x+ #Com.N.conc.y
            # Com.N.conc.y:landuse+Com.N.conc.y:rain.sum.x.x+
            #Com.N.conc.y:rain.sum.x.x:landuse+
            Com.N.conc.LAG.y:landuse+Com.N.conc.LAG.y:rain.sum.x.x+
            Com.N.conc.LAG.y:rain.sum.x.x:landuse,
          random= ~ 1|region/fblock, na.action=na.pass, method="REML", # REML=T
          correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ExOpCon2)
summary(B3Op)
anova(B3Op) # Nothing... no relationship to consumption
AIC(B3Op) # 185.0564
# Simplify open N and consumption model
B3Op<-lme(consumption.total.g.m2.day.x~Com.N.conc.LAG.y+ #Com.N.conc.y
            landuse+rain.sum.x.x,
          #Com.N.conc.y:landuse+Com.N.conc.y:rain.sum.x.x+
          #Com.N.conc.y:rain.sum.x.x:landuse,
          #Com.N.conc.LAG.y:landuse+Com.N.conc.LAG.y:rain.sum.x.x,
          #Com.N.conc.LAG.y:rain.sum.x.x:landuse,
          random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
          correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ExOpCon2)
summary(B3Op)
anova(B3Op) # LAG N AND RAIN
AIC(B3Op) # 235.1362
drop1(B3Op, test="Chisq")
# Nothing significant for community N or LAG N
# Productivity - Exclsoures nitrogen concentration
B3OP<-lme(productivity.total.g.m2.day.x~Com.N.conc.LAG.x+ #Com.N.conc.y
            landuse+rain.sum.x.x,
          #Com.N.conc.y:landuse+Com.N.conc.y:rain.sum.x.x+
          #Com.N.conc.y:rain.sum.x.x:landuse,
          #Com.N.conc.LAG.x:landuse+Com.N.conc.LAG.x:rain.sum.x.x,
          #Com.N.conc.LAG.x:rain.sum.x.x:landuse,
          random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
          correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ExOpCon2)
summary(B3OP)
anova(B3OP) # landuse
AIC(B3OP) # 151.4113
drop1(B3OP, test="Chisq")
# All the rest significant
plot(productivity.total.g.m2.day.x~Com.N.conc.LAG.x,col=c(ExOpCon2$landuse),ExOpCon2)
abline(lm(productivity.total.g.m2.day.x~Com.N.conc.LAG.x,ExOpCon2))
# Productivity - Open nitrogen concentration
B3OP<-lme(productivity.total.g.m2.day.x~Com.N.conc.LAG.y+ #Com.N.conc.y
            landuse+rain.sum.x.x,
          #Com.N.conc.y:landuse+Com.N.conc.y:rain.sum.x.x+
          #Com.N.conc.y:rain.sum.x.x:landuse,
          #Com.N.conc.LAG.y:landuse+Com.N.conc.LAG.y:rain.sum.x.x,
          #Com.N.conc.LAG.y:rain.sum.x.x:landuse,
          random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
          correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ExOpCon2)
summary(B3OP)
anova(B3OP) # landuse
AIC(B3OP) # 178.0
drop1(B3OP, test="Chisq")
# Nothing significant...
#### N Difference between Excl t1 - Open t0 ####
ExOpCon$NProd.diff<-ExOpCon$Com.N.conc.x-ExOpCon$Com.N.conc.LAG.y
# Remove NAs for consumption
ExOpCon2 <- droplevels(ExOpCon[!is.na(ExOpCon$consumption.total.g.m2.day.x) & !is.na(ExOpCon$NProd.diff) , ])
# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/fblock/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ExOpCon2)
corMatrix(cs1AR1.)
# Productivity - N Difference between Excl t1 - Open t0
B3OP<-lme(productivity.total.g.m2.day.x~NProd.diff,
          # landuse+rain.sum.x.x,
          #NProd.diff:landuse+NProd.diff:rain.sum.x.x,
          # NProd.diff:rain.sum.x.x:landuse,
          random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
          correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ExOpCon2)
summary(B3OP)
anova(B3OP) # landuse
AIC(B3OP) # 151.4113
drop1(B3OP, test="Chisq")
# All the rest significant
plot(productivity.total.g.m2.day.x~NProd.diff,col=c(ExOpCon2$landuse),ExOpCon2)
abline(lm(productivity.total.g.m2.day.x~NProd.diff,ExOpCon2))
#### Demonstrating the lag effect...?
names(ExOpCon)
ExOpCon$NProd.diffTARGET<-ExOpCon$N.conc.adj.x.x-ExOpCon$N.conc.adj.lag.x.y
ExOpCon$target.sp..x.x
plot(productivity.target.g.m2.day.x~NProd.diffTARGET,col=c(ExOpCon$target.sp..x.x),ExOpCon) # Target species
abline(lm(productivity.target.g.m2.day.x~NProd.diffTARGET,ExOpCon))
SpLagN<-lm(productivity.target.g.m2.day.x~NProd.diffTARGET+target.sp..x.x+
             NProd.diffTARGET:target.sp..x.x,ExOpCon)
summary(SpLagN)
anova(SpLagN)
#### DIFFERENCE IN NITROGEN OPEN VERSUS EXCLOSED ####
# Difference in N between exclosed and open
ProdCon2EX<-ProdCon2[ProdCon2$treatment=="EX",]
ProdCon2OP<-ProdCon2[ProdCon2$treatment=="OP",]
ProdCon2ex<-merge(ProdCon2EX,ProdCon2OP,by=c("block.id","landuse","block.id.harvest", "site.id","block.id",
                                             "region","block","wp","lat","long"))
#Difference in community nitrogen between exclosed and open (Exclosed-Open)
ProdCon2ex$Com.N.conc.diff<-ProdCon2ex$Com.N.conc.x-ProdCon2ex$Com.N.conc.y
# Create a temporal lag with nutrient N
ProdCon2ex$Com.N.conc.diff.lag<-lag(ProdCon2ex$Com.N.conc.diff, 1)
plot(Com.N.conc.diff~Com.N.conc.diff.lag,ProdCon2ex)
names(ProdCon2ex)
ConN2<-ggplot(ProdCon2ex,aes(y=consumption.total.g.m2.day.x,x=Com.N.conc.diff))
ConN2<-ConN2+geom_point(size=2)
ConN2<-ConN2+facet_wrap(~landuse)
ConN2
names(ProdCon2ex)
ConN3<-ggplot(ProdCon2ex,aes(y=consumption.total.g.m2.day.x,x=Com.N.conc.diff.lag))
ConN3<-ConN3+geom_point(size=2)
ConN3<-ConN3+facet_wrap(~landuse)
ConN3
# Difference OP and EX needs to be the change from harvest t0 to t1?
ProdN2<-ggplot(ProdCon2ex,aes(y=productivity.total.g.m2.day.x,x=Com.N.conc.diff))
ProdN2<-ProdN2+geom_point(size=2)
ProdN2<-ProdN2+facet_wrap(~landuse)
ProdN2
# Difference OP and EX needs to be the change from harvest t0 to t1 - lag by 1 harvest
ProdN3<-ggplot(ProdCon2ex,aes(y=productivity.total.g.m2.day.x,x=Com.N.conc.diff.lag))
ProdN3<-ProdN3+geom_point(size=2)
ProdN3<-ProdN3+facet_wrap(~landuse)
ProdN3
# Consumption and Nitrogen differences  (Ex versus open) incl. temporal lag
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
ProdCon2ex$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(ProdCon2ex$harvest.date.x.x,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Plot ID without Harvest number
ProdCon2ex$plot.id.x<-as.factor(ProdCon2ex$plot.id.x)
ProdCon2ex$fblock<-as.factor(ProdCon2ex$block)
ProdCon2ex$plot.code<-as.factor(with(ProdCon2ex, paste(region,landuse,block,sep="_")))
levels(ProdCon2ex$plot.code) # 40 levels
# Remove NAs for nitrogen
table(ProdCon2ex$plot.code,ProdCon2ex$YrMonthNumber) # Month 8 and 15 few data points
ProdCon2ex$fYrMonthNumber<-as.factor(ProdCon2ex$YrMonthNumber)
levels(ProdCon2ex$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
ProdCon2ex$YrMonthNumber<-as.numeric(as.character(ProdCon2ex$fYrMonthNumber))
table(ProdCon2ex$plot.code,ProdCon2ex$YrMonthNumber)
apply(table(ProdCon2ex$plot.code, ProdCon2ex$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/fblock/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ProdCon2ex)
corMatrix(cs1AR1.)
# Consumption and community difference N (Ex - open) and temporal lag
# LMM difference in Nitrogen differences between exclosed - open
B3<-lme(consumption.total.g.m2.day.x~Com.N.conc.diff+landuse+rain.sum.x.x+Com.N.conc.diff.lag+
          Com.N.conc.diff:landuse+Com.N.conc.diff:rain.sum.x.x+
          Com.N.conc.diff:rain.sum.x.x:landuse,
        random= ~ 1|region/fblock, na.action=na.pass, method="REML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ProdCon2ex)
summary(B3)
anova(B3) #
AIC(B3) # 235.1362
B3<-lme(consumption.total.g.m2.day.x~Com.N.conc.diff+landuse+rain.sum.x.x,
        #Com.N.conc.diff:landuse+Com.N.conc.diff:rain.sum.x.x,
        # Com.N.conc.diff:rain.sum.x.x:landuse,
        random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
        correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ProdCon2ex)
summary(B3)
anova(B3)
AIC(B3) # 235.1362
drop1(B3,test="Chisq") # N conc diff lag = significant to consumption
plot(consumption.total.g.m2.day.x~Com.N.conc.diff.lag, ProdCon2ex)
abline(lm(consumption.total.g.m2.day.x~Com.N.conc.diff.lag, ProdCon2ex))
abline(v = 0, lwd = 2, col = 2)
summary(lm(consumption.total.g.m2.day.x~Com.N.conc.diff.lag:landuse:rain.sum, ProdCon2ex))
# Productivity and community difference N (Ex - open) and temporal lag
# LMM difference in Nitrogen differences between exclosed - open
B3p<-lme(productivity.total.g.m2.day.x~Com.N.conc.diff+landuse+rain.sum.x.x+
           Com.N.conc.diff:landuse+Com.N.conc.diff:rain.sum.x.x+
           Com.N.conc.diff:rain.sum.x.x:landuse,
         random= ~ 1|region/fblock, na.action=na.pass, method="REML", # REML=T
         correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ProdCon2ex)
summary(B3p)
anova(B3p) #
AIC(B3p) # 189.6383
B3p<-lme(productivity.total.g.m2.day.x~Com.N.conc.diff,#Com.N.conc.diff+rain.sum.x.x+landuse
         # Com.N.conc.diff.lag:landuse+Com.N.conc.diff.lag:rain.sum.x.x+
         #Com.N.conc.diff.lag:rain.sum.x.x:landuse+
         #Com.N.conc.diff:rain.sum.x.x, #Com.N.conc.diff:landuse
         #Com.N.conc.diff:rain.sum.x.x:landuse,
         random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
         correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=ProdCon2ex)
summary(B3p)
anova(B3p)
AIC(B3p) # 235.1362
drop1(B3p,test="Chisq") # N conc diff lag = significant to consumption
plot(productivity.total.g.m2.day.x~Com.N.conc.diff.lag, ProdCon2ex)
abline(lm(productivity.total.g.m2.day.x~Com.N.conc.diff.lag, ProdCon2ex))
abline(v = 0, lwd = 2, col = 2)
# LAG - productivity lower when exclosure N is higher than Open the month before

###########################################################################
#### Productivity, Consumption, Community N conc Target and non-target species - N conc through time####
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
names(ProdCon2)
TargetNavgYm<-aggregate(Com.N.conc~+region+landuse+treatment+HarvestMonth.x,ProdCon2,na.rm=T,mean)
TargetNsdYm<-aggregate(Com.N.conc~+region+landuse+treatment+HarvestMonth.x,ProdCon2,se)
TargetNavgYm$SE<-TargetNsdYm$Com.N.conc
TargetProdavgYm<-aggregate(productivity.total.g.m2.day~region+landuse+treatment+HarvestMonth.x,ProdCon2,na.rm=T,mean)
TargetProdsdYm<-aggregate(productivity.total.g.m2.day~region+landuse+treatment+HarvestMonth.x,ProdCon2,se)
TargetProdavgYm$SE<-TargetProdsdYm$productivity.total.g.m2.day
#RainavgYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
#RainsdYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
#RainavgYm$SE<-RainsdYm$rain.sum
# Grouping variable - this is necessary to connect the lines
TargetNavgYm$grp.code<-as.factor(with(TargetNavgYm, paste(region,landuse,treatment,sep="_")))
TargetProdavgYm$grp.code<-as.factor(with(TargetProdavgYm, paste(region,landuse,treatment,sep="_")))
levels(TargetNavgYm$grp.code)
# Issue - Harvest 5 spread over two months - replace Oct with Sept
TargetNavgYm$HarvestMonth.x[TargetNavgYm$HarvestMonth.x=="2017-10"]<-"2017-09"
TargetProdavgYm$HarvestMonth.x[TargetProdavgYm$HarvestMonth.x=="2017-10"]<-"2017-09"
# Convert to date - Yr-Month
TargetNavgYm$HarvestMonth<-as.Date(paste(TargetNavgYm$HarvestMonth,"-01-01",sep=""))
RainavgYm$HarvestMonth<-as.Date(paste(RainavgYm$HarvestMonth,"-01-01",sep=""))
# Remove Seronera
TargetNavgYm2<-droplevels(TargetNavgYm[TargetNavgYm$region!="SE",])
TargetProdavgYm2<-droplevels(TargetProdavgYm[TargetProdavgYm$region!="SE",])
# Rename levels
levels(TargetNavgYm2$region)<-c("Dry region","Wet region")
levels(TargetProdavgYm2$region)<-c("Dry region","Wet region")
levels(TargetNavgYm2$landuse)<-c("Pasture","Wildlife protected")
levels(TargetProdavgYm2$landuse)<-c("Pasture","Wildlife protected")
# Scale factor
scaleFactor <- mean(TargetNavgYm2$Com.N.conc)/mean(TargetProdavgYm2$productivity.total.g.m2.day,na.rm=T)
# Plotting line graph with secondary y-axis (stuck on!)
TNYm<-ggplot(TargetNavgYm2,aes(y=Com.N.conc,x=HarvestMonth.x,shape=treatment,fill=treatment, group =grp.code)) # group = grouping vector for lines
TNYm<-TNYm+geom_line(data=TargetNavgYm2,size=.75,show.legend = T)
TNYm<-TNYm+geom_errorbar(data=TargetNavgYm2,aes(ymin=Com.N.conc-SE, ymax=Com.N.conc+SE),stats="identity",width=.2,lwd=1.1,show.legend=F)
TNYm<-TNYm+geom_point(data=TargetNavgYm2,stats="identity",size=4.5,stroke=1.5)
TNYm<-TNYm+facet_wrap(~region+landuse)
#TNYm<-TNYm+geom_errorbar(data=TargetProdavgYm2,aes(ymin=(((productivity.total.g.m2.day-SE)+4)/2), ymax=(((productivity.total.g.m2.day-SE)+4)/2)),width=.2,lwd=1.1,show.legend=F)
TNYm<-TNYm+geom_line(data=TargetProdavgYm2,aes(y=(productivity.total.g.m2.day+4)/2),linetype="solid",colour="green4", size=1,show.legend=F) # Offset the second rainfall legend so it is above the N concnetration
TNYm<-TNYm+scale_y_continuous(limits=c(0,5),sec.axis = sec_axis(~ . *2, breaks = c(0,2,4,6,8,10), labels = c("",0,2,4,6,8), name = expression(paste("Biomass (g ",m^-2,"",day^-1,")"))),expand=c(0,0))
TNYm<-TNYm+geom_point(data=TargetProdavgYm2,aes(y=(productivity.total.g.m2.day+4)/2),colour="green4",fill=c("green4"),size=3.5,show.legend=F)
TNYm<-TNYm+scale_x_discrete(expand=c(0,0)) # Discrete axis and then reducing x axis expand
TNYm<-TNYm+scale_fill_manual(values=c("OP"="white","EX"="black"))
TNYm<-TNYm+ylab("Nitrogen concentration (%)")+xlab("Year|month")
TNYm<-TNYm + scale_shape_manual(values=c(21,22))
TNYm
TNYm<-TNYm+theme_bw()+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,strip.background = element_blank()
        ,strip.text.x =element_text(size=12)
        ,axis.line = element_line(color = 'black', size =.5)
        ,axis.text.x=element_text(angle=35, hjust=1))
TNYm<-TNYm +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = "2017-02", xend = "2017-02", size = 1)
TNYm<-TNYm +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1)
TNYm

############################################################################
#### Linear mixed model PHOSPHORUS CONCETRATION DATA - COMMUNITY LEVEL ####
############################################################################
#### Linear mixed model on community N conc ####
Plmer<-lmer(Com.P.conc~landuse+rain.sum+treatment+
              landuse:rain.sum+landuse:treatment+
              +treatment:rain.sum+treatment:landuse:rain.sum+
              (1|fblock), data = NutsOPA, REML=T)
summary(Plmer)
AIC(Plmer) # -291.275
anova(Plmer)
# Checking some assumptions
#Normalized residuals in lme are of the form:
# E / sqrt(variance of the residuals)
E1p <- resid(Plmer, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F1p <- fitted(Plmer)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1p,
     y = E1p,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) #  Good spread
abline(h = 0, lty = 2, col = 1)
# No autocorrelation
library(itsadug)
plot(acf_resid(Plmer), type="b",alpha=0.05)
abline(c(0,0), lty = 2, col = 1) # Strong temporal auto-correlation
# There is a temporal pattern!
# Likelihood Ratio Testing dropping terms based on AIC
drop1(Plmer, test="Chisq")
# Three way interaction - does not improve AIC
#landuse:rain.sum:treatment  1 257.60 0.070444  0.7907
#....continue with pruning
Plmer2<-lmer(Com.P.conc~rain.sum+landuse+#treatment+
               #landuse:poly(rain.sum,2)+landuse:treatment+
               #treatment:poly(rain.sum,2)+
               #treatment:landuse:poly(rain.sum,2)+
               (1|fblock),data = NutsOPA, REML=T)
summary(Plmer2)
AIC(Plmer2) # -339.0575 # unimodal better
anova(Plmer2)
drop1(Plmer2, test="Chisq") # Nothing significant
#Landuse and treatment - retain - no interaction
# NB. Good model testing would then put back factors removed from the original model
# Recheck the assumptions
#Normalized residuals in lme are of the form:
# E / sqrt(variance of the residuals)
E2p <- resid(Plmer2, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F2p <- fitted(Plmer2)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2p,
     y = E2p,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # Good spread
abline(h = 0, lty = 2, col = 1)
# Updating the model - generating p-values for each term
Plmer2d<- update(Plmer2, .~. -rain.sum)
anova(Plmer2,Plmer2d)
#          Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#Plmer2   6 -390.03 -371.00 201.01  -402.03 39.834      2  2.239e-09 *** # rain.sum
##############################################################
#### Sketch fitted values ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/-
#A:Specify covariate values for predictions
MyDataP <- expand.grid(landuse=levels(NutsOPA$landuse),
                       rain.sum = seq(min(NutsOPA$rain.sum), max(NutsOPA$rain.sum), length = 25))
#B. Create X matrix with expand.grid
XP <- model.matrix(~ rain.sum+landuse, data = MyDataP)
head(XP)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyDataP$Pred <- XP %*% fixef(Plmer2)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyDataP$SE <- sqrt(  diag(XP %*% vcov(Plmer2) %*% t(XP))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyDataP$SeUp <- MyDataP$Pred + 1.96 * MyDataP$SE
MyDataP$SeLo <- MyDataP$Pred - 1.96 * MyDataP$SE
#E. Plot predicted values
names(MyDataP)
colnames(MyDataP)[3]<-"Com.P.conc"
#### Plot observed data versus prediction #####
# Scatter plot with Community P concentrations and rainfall
RP<-ggplot(NutsOPA,aes(x=rain.sum, y=Com.P.conc)) # geom_errorbar
RP<-RP+geom_ribbon(data=MyDataP,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)
RP<-RP+geom_line(data=MyDataP,colour="red",alpha=.9,lwd=2,show.legend=F)
RP<-RP+geom_point(stats="identity",colour="grey50",fill="grey50",size=2.5)
RP<-RP+facet_wrap(~landuse, scale="fixed")
RP<-RP+scale_x_continuous(limits=c(-1,540), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0,0))
RP<-RP+scale_y_continuous(limits=c(0,.5),expand=c(0,0))
RP<-RP+ylab("Phosphorus concentration (%)")+xlab("Rainfall (mm)") # Adding x and ylabs to plot
RP<-RP+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
RP<-RP+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1)
RP<-RP+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1)
RP

###########################################################################
#### PHOSPHORUS: Mixed model with temporal autocorrelaiton structure ####
###########################################################################
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))}
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
NutsOPA$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(NutsOPA$setup.date,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Using setup to include H0 - rain is preceeding time to 01-01-2017
# Plot ID without Harvest number
NutsOPA$plot.code<-as.factor(with(NutsOPA, paste(region,landuse,block,treatment,sep="_")))
levels(NutsOPA$plot.code) # 40 levels
# Remove NAs for nitroge
table(NutsOPA$plot.code,NutsOPA$YrMonthNumber) # Month 8 and 15 few data points
NutsOPA$fYrMonthNumber<-as.factor(NutsOPA$YrMonthNumber)
levels(NutsOPA$fYrMonthNumber)<-c("0","1", "3","5","7","8","10","13","15")
NutsOPA$YrMonthNumber<-as.numeric(as.character(NutsOPA$fYrMonthNumber))
table(NutsOPA$plot.code,NutsOPA$YrMonthNumber)
apply(table(NutsOPA$plot.code, NutsOPA$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
# Remove NAs - NAS for year month as no set up
NutsOPA<- droplevels(NutsOPA[!is.na(NutsOPA$Com.N.conc) &! is.na(NutsOPA$fYrMonthNumber),])
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/fblock/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =NutsOPA)
corMatrix(cs1AR1.)
# LMM with auto temporal correlation # Community nitrogen
B1p<-lme(Com.P.conc~landuse+rain.sum+treatment+
           landuse:rain.sum+landuse:treatment+
           +treatment:rain.sum+treatment:landuse:rain.sum,
         random= ~ 1|region/fblock, na.action=na.pass, method="REML", # REML=T
         correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=NutsOPA)
summary(B1p)
anova(B1p) # Highly signficant
AIC(B1p) #-389.402
# Check temporal autocorrelation
# Extract residuals
E2p <- resid(B1p, type ="n")  # lme = "type = "n"
F2p <- fitted(B1p)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2p,
     y = E2p,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # None of the fitted values <0
abline(h = 0, lty = 2, col = 1)  # Good - with temporal structure
# Time auto-correlated
acf(E2p, na.action=na.pass,main="Auto-correlation plot for residuals")
# No auto-correlation
# Simplify model
B1pA<-lme(Com.P.conc~rain.sum,#+treatment+landuse,
          # landuse:rain.sum+landuse:treatment,
          # treatment:rain.sum+treatment:landuse:rain.sum,
          random= ~ 1|region/fblock, na.action=na.pass, method="ML",
          correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=NutsOPA)
AIC(B1pA) #-494.6461
summary(B1pA)
anova(B1pA) # Just rain
drop1(B1pA, test="Chisq")
#rain.sum  1 -334.15 32.237 1.365e-08 ***
B1pAb<-update(B1pA, .~. -rain.sum)
anova(B1pA,B1pAb)
#B1pB<-lme(Com.P.conc~landuse+poly(rain.sum,2)+treatment+
#                landuse:poly(rain.sum,2)+landuse:treatment+
#                +treatment:poly(rain.sum,2)+treatment:landuse:poly(rain.sum,2),
#              random= ~ 1|region/fblock, na.action=na.pass, method="ML", # REML=T
#              correlation=corAR1(0.2, form=~YrMonthNumber|region/fblock/plot.code),data=NutsOPA)
#drop1(B1pB, test="Chisq") # Just rain - check whether unimodal
#AIC(B1pB) # -302.4835 - unimodel is worse
#anova(B1pA,B1pB) # Second model worse - keep it linear for P

##############################################################
#### Sketch fitted values ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/-
#A:Specify covariate values for predictions
MyDataP2 <- expand.grid(#landuse=levels(NutsOPA$landuse),
  #treatment=levels(NutsOPA$treatment),
  rain.sum = seq(min(NutsOPA$rain.sum), max(NutsOPA$rain.sum), length = 25))
#B. Create X matrix with expand.grid
XP2 <- model.matrix(~ rain.sum, data = MyDataP2)
head(XP2)
#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyDataP2$Pred <- XP2 %*% fixef(B1pA)  # = X * beta
#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)
#   Take this for granted!
MyDataP2$SE <- sqrt(  diag(XP2 %*% vcov(B1pA) %*% t(XP2))  )
#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyDataP2$SeUp <- MyDataP2$Pred + 1.96 * MyDataP2$SE
MyDataP2$SeLo <- MyDataP2$Pred - 1.96 * MyDataP2$SE
#E. Plot predicted values
names(MyDataP2)
colnames(MyDataP2)[2]<-"Com.P.conc"
#### Plot observed data versus prediction #####
NutsOPAb<-NutsOPA
levels(NutsOPAb$landuse)<-c("Pasture","Wildlife protected")
levels(NutsOPAb$treatment)<-c("Exclosed","Open")
# Treatment code for filling symbols
NutsOPAb$trt.code<-as.factor(with(NutsOPAb, paste(landuse,treatment,sep="_")))
# Scatter plot with Community P concentrations and rainfall
RP<-ggplot(NutsOPAb,aes(x=rain.sum, y=Com.P.conc)) # geom_errorbar
RP<-RP+geom_ribbon(data=MyDataP2,aes(ymin=SeUp, ymax=SeLo),fill="grey50",colour="grey50",alpha=.65,lwd=NA,show.legend=F)
RP<-RP+geom_line(data=MyDataP2,colour="grey50",alpha=.9,lwd=2,show.legend=F)
RP<-RP+geom_point(stats="identity",aes(fill=trt.code,colour=landuse, shape=treatment),size=4.5, stroke=1)
RP<-RP+facet_wrap(~landuse, scale="fixed")
RP<-RP+scale_x_continuous(limits=c(-1,540), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0.1,0.1))
RP<-RP+scale_y_continuous(limits=c(0,.52),expand=c(0,0))
RP<-RP+ylab("Phosphorus concentration (%)")+xlab("Rainfall (mm)")
RP<-RP+scale_colour_manual(values=c("tan3","turquoise3"))
RP<-RP+scale_fill_manual(values=c("tan3","white","turquoise3","white"))
RP<-RP+scale_shape_manual(values=c(22,21))
RP<-RP+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=12,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(size=12,margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
#RP<-RP+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1)
#RP<-RP+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1)
RP<-RP+guides(colour=F, fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,#linetype=c("solid","twodash","dashed"), #size=4.5,
                                                                                               fill=c("grey30","white"),col="grey30", stroke=1)))
RP
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/PlantP_rain.jpeg",
#       width=20, height=12,units ="cm",dpi = 600, limitsize = TRUE)


###########################################################################
##### Temporal component - target versus non-target ####
###########################################################################
#### A temporal component ####
#### Add second y axis = rainfall #### Ggplot2 is poor at this - but there is good reason...
#### Target and non-target species - N conc through time####
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TargetPavgYm<-aggregate(P.conc.adj~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
TargetPsdYm<-aggregate(P.conc.adj~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
TargetPavgYm$SE<-TargetPsdYm$P.conc.adj
RainavgYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
RainsdYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
RainavgYm$SE<-RainsdYm$rain.sum
# Grouping variable - this is necessary to connect the lines
TargetPavgYm$grp.code<-as.factor(with(TargetPavgYm, paste(region,landuse,target.sp.,treatment,sep="_")))
RainavgYm$grp.code<-as.factor(with(RainavgYm, paste(region,landuse,target.sp.,treatment,sep="_")))
levels(TargetPavgYm$grp.code)
# Issue - Harvest 5 spread over two months - replace Oct with Sept
#TargetNavgYm$HarvestMonth[TargetNavgYm$HarvestMonth=="2017-10"]<-"2017-09"
#RainavgYm$HarvestMonth[RainavgYm$HarvestMonth=="2017-10"]<-"2017-09"
# Remove Seronera
#TargetNavgYm2<-droplevels(TargetNavgYm[TargetNavgYm$region!="SE",])
#RainavgYm2<-droplevels(RainavgYm[RainavgYm$region!="SE",])
# Rename levels
levels(TargetPavgYm$region)<-c("Dry region","Intermediate region","Wet region")
levels(RainavgYm$region)<-c("Dry region","Intermediate region","Wet region")
levels(TargetPavgYm$landuse)<-c("Pasture","Wildlife protected")
levels(RainavgYm$landuse)<-c("Pasture","Wildlife protected")
levels(TargetPavgYm$treatment)<-c("Exclosed","Open")
levels(RainavgYm$treatment)<-c("Exclosed","Open")
# Target grass species
#Handajega: Themeda triandra
#Mwantimba: Chrysocloa orientalis
#Seronera: Digitaria macroblephara
#Maswa: Cynodon dactylon
#Makao: Chloris pycnothrics
levels(TargetPavgYm$target.sp.)<-c("Chloris","Chrysocloa","Cynodon","Digitaria","Other","Themeda")
levels(RainavgYm$target.sp.)<-c("Chloris","Chrysocloa","Cynodon","Digitaria","Other","Themeda")
# Scale factor
scaleFactor <- mean(TargetPavgYm$P.conc.adj,na.rm=T)/mean(RainavgYm$rain.sum,na.rm=T)
#756
# Treatment code for filling symbols
TargetPavgYm$spp.code<-as.factor(with(TargetPavgYm, paste(target.sp.,treatment,sep="_")))
RainavgYm$spp.code<-as.factor(with(RainavgYm, paste(target.sp.,treatment,sep="_")))
# Convert to date - Yr-Month
TargetPavgYm$HarvestMonth<-as.Date(paste(TargetPavgYm$HarvestMonth,"-01-01",sep=""))
RainavgYm$HarvestMonth<-as.Date(paste(RainavgYm$HarvestMonth,"-01-01",sep=""))
SpTitle<-"Species"
# Plotting line graph with secondary y-axis (stuck on!)
TPYm<-ggplot(TargetPavgYm,aes(y=P.conc.adj,x=HarvestMonth, shape=treatment, group =grp.code)) # group = grouping vector for lines
TPYm<-TPYm+geom_line(data=RainavgYm,aes(y=(rain.sum+100)*0.0014),colour="dark blue", linetype="dotted", size=1,show.legend=F)
TPYm<-TPYm+geom_point(data=RainavgYm,aes(y=(rain.sum+100)*0.0014),colour="dark blue", fill="white", shape=21, size=2.5,stroke=1,show.legend=F)
TPYm<-TPYm+geom_line(aes(colour=target.sp.),position=position_dodge(width=.65),stat = "identity",size=.75,show.legend = F)
TPYm<-TPYm+geom_errorbar(data=TargetPavgYm,aes(ymin=P.conc.adj-SE, ymax=P.conc.adj+SE,colour=target.sp.),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
TPYm<-TPYm+geom_point(aes(colour=target.sp.,fill=spp.code),position=position_dodge(width=.65),size=5, stroke=1)
TPYm<-TPYm+facet_wrap(~region+landuse, ncol=5)
#TPYm<-TPYm+geom_line(data=RainavgYm2,aes(y=(rain.sum+400)/200),linetype="solid",colour="dark blue", size=1,show.legend=F) # Offset the second rainfall legend so it is above the N concnetration
#TPYm<-TPYm+geom_smooth(data=RainavgYm,aes(y=(rain.sum+100)*0.0014),method="loess",span=.9, se=F,linetype="dotted",colour="dark blue", size=1,show.legend=F)
TPYm<-TPYm+scale_y_continuous(limits=c(0,.85),sec.axis = sec_axis(~ . /0.0014, breaks = c(0,100,200,400,600), labels = c("","",0,100,500), name = "Rainfall (mm)"),expand=c(0,0))
TPYm<-TPYm+scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y", limits=c(as.Date("2017-01-01"),max=as.Date("2018-03-01")),expand=c(0.1,0.1))
TPYm<-TPYm+ylab("Plant phosphorus concentration (%)")+xlab("Year|month")
TPYm<-TPYm+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"))
TPYm<-TPYm+scale_fill_manual(values=c("chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white", "grey50","white","orangered3","white"))
TPYm<-TPYm+scale_shape_manual(values=c(22,21))
TPYm<-TPYm+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",angle=35,hjust=1,
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 14,hjust=0,angle=0)
        ,panel.spacing = unit(.1, "lines")
        ,legend.text = element_text(size=12,color="black")
        ,legend.title = element_text(size=12,color="black")
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm"))
#TPYm<-TPYm+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = "2017-01", xend = "2017-01", size = 1)
#TPYm<-TPYm+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1)
TPYm<-TPYm+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1)),
                  colour = guide_legend("Dominant species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"),
                                                                               col=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"), stroke=1)) )
TPYm
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/SpeciesTimeP.jpeg",
#            width=40, height=16,units ="cm",dpi = 600, limitsize = TRUE)
#### Point graph - each species P compared to other #####
SpeciesP<-aggregate(P.conc.adj~target.sp.+landuse+treatment,TargetPavgYm,mean)
SpeciesPSE<-aggregate(P.conc.adj~target.sp.+landuse+treatment,TargetPavgYm,sd)
SpeciesP$SE<-SpeciesPSE$P.conc.adj

# Relevel so that other
SpeciesP$target.sp.<- factor(SpeciesP$target.sp., levels = c("Chloris", "Chrysocloa", "Cynodon", "Digitaria", "Themeda","Other"))

# Treatment code for filling symbols
SpeciesP$spp.code<-as.factor(with(SpeciesP, paste(target.sp.,treatment,sep="_")))
TPs<-ggplot(SpeciesP,aes(y=P.conc.adj,x=landuse, colour=target.sp.,fill=spp.code, shape=treatment)) # group = grouping vector for lines
TPs<-TPs+geom_errorbar(data=SpeciesP,aes(ymin=P.conc.adj-SE, ymax=P.conc.adj+SE),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPs<-TPs+geom_point(position=position_dodge(width=.45),size=5, stroke=1)
TPs<-TPs+ylab("Plant phosphorus concentration (%)")+xlab("Land-use")
TPs<-TPs+scale_shape_manual(values=c(22,21))
TPs<-TPs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3","grey50"))
TPs<-TPs+scale_fill_manual(values=c("chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white", "grey50","white","orangered3","white"))
#TPs<-TPs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TPs<-TPs+theme_bw()+
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
        ,legend.title = element_text(size=12,color="black")
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm"))
TPs<-TPs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1)),
                colour = guide_legend("Dominant species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3","grey50"),
                                                                             col=c("chartreuse3","hotpink1","cadetblue3","green4", "orangered3","grey50"), stroke=1)) )
TPs
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/SpeciesTargetP.jpeg",
#       width=16, height=12,units ="cm",dpi = 600, limitsize = TRUE)
###########################################################################
##### Phosphorus: Mixed linear model with temporal structure ####
###########################################################################
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils")
ProdCon<- read.csv('Moveable exclosures/Biomass.csv', sep=',',dec='.')#Wetseason data
names(ProdCon)
dim(ProdCon) # 356  38
dim(NutsONA) # 572 121
# Convert date to 'Year- Month'
Rdate3<-strptime(as.character(ProdCon$setup.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
Rdate4<-strptime(as.character(ProdCon$harvest.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
ProdCon$SetupMonth<-format(as.Date(Rdate3), "%Y-%m")
ProdCon$HarvestMonth<-format(as.Date(Rdate4), "%Y-%m")
#### Remove EX2 (metal) ####
# Remove metal mesh exclosure treatment (EX2) # but keep H0 (renamed here for analysis)
ProdCon1<-droplevels(ProdCon[ProdCon$treatment!="EX2" & ProdCon$treatment!="",]) # There is a gap!
dim(ProdCon1) # 300  40
ProdCon<-ProdCon1
# Remove H0 from nutrient dat
#NutsONA1b<-droplevels(NutsONA2[NutsONA2$harvest!="H0",])
#dim(NutsONA1b) # 144  91
ProdCon2<-merge(NutsONA,ProdCon1, by=c("block.id","plot.id","landuse","treatment",
                                       "block.id.harvest", "site.id","block.id",
                                       "region","block","wp","lat","long",
                                       "growth.period"))
# Consumption rate
ProdCon2$consumption.total.g.m2.day # NA - consumption on the exclosure line?
table(ProdCon2$consumption.total.g.m2.day,ProdCon2$treatment)
ExCon <- ProdCon2[ProdCon2$consumption.total.g.m2.day & ProdCon2$treatment=="EX", ]
OpCon <- ProdCon2[ProdCon2$treatment=="OP", ]
ExOpCon<-left_join(ExCon,OpCon, by=c("block.id","landuse",
                                     "block.id.harvest", "site.id","block.id",
                                     "region","block","wp","lat","long",
                                     "growth.period"))
OpCon$Com.N.conc
ExOpCon$Com.N.conc.y #Open N
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
ProdCon2$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(ProdCon2$harvest.date.x,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Plot ID without Harvest number
# Plot ID and blockcode without Harvest number
ProdCon2$plot.id<-as.factor(ProdCon2$plot.id)
ProdCon2$fblock<-as.factor(ProdCon2$block)
ProdCon2$block.code<-as.factor(with(ProdCon2, paste(region,landuse,block,sep="_")))
ProdCon2$plot.code<-as.factor(with(ProdCon2, paste(region,landuse,block,treatment,sep="_")))
levels(ProdCon2$plot.code) # 40 levels
# Remove NAs for nitrogen
#NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc), ])
table(ProdCon2$plot.code,ProdCon2$YrMonthNumber) # Month 8 and 15 few data points
ProdCon2$fYrMonthNumber<-as.factor(ProdCon2$YrMonthNumber)
levels(ProdCon2$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
ProdCon2$YrMonthNumber<-as.numeric(as.character(ProdCon2$fYrMonthNumber))
table(ProdCon2$plot.code,ProdCon2$YrMonthNumber)
apply(table(ProdCon2$plot.code, ProdCon2$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicates per group
# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/block.code/plot.code) # NOTE this needs to be a straight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ProdCon2)
corMatrix(cs1AR1.)
ProdCon2b<-droplevels(ProdCon2[!is.na(ProdCon2$productivity.total.g.m2.day) & is.na(ProdCon2$Com.P.conc),])
#### LMM productivity and treatment ####
Pmod1p<-lme(productivity.total.g.m2.day~Com.P.conc+landuse+
              treatment+landuse:treatment+
              Com.P.conc:treatment+
              Com.P.conc:landuse+
              Com.P.conc:treatment:landuse,
            random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1p)
anova(Pmod1p)
AIC(Pmod1p) #565.8928
ProdCon2$productivity.total.g.m2.day
ProdCon2$Com.P.conc
ProdCon2b<-droplevels(ProdCon2[!is.na(ProdCon2$productivity.total.g.m2.day) & !is.na(ProdCon2$Com.P.conc),])
ProdCon2b$Com.P.conc
#### LMM productivity and treatment ####
Pmod1p<-lme(productivity.total.g.m2.day~Com.P.conc+landuse+
              treatment+landuse:treatment+
              Com.P.conc:treatment+
              Com.P.conc:landuse+
              Com.P.conc:treatment:landuse,
            random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1p)
anova(Pmod1p)
AIC(Pmod1p) #565.8928
names(ProdCon2b)
plot(ProdCon2b$productivity.total.g.m2.day~ProdCon2b$Com.P.conc)
xyplot(productivity.total.g.m2.day~Com.P.conc|treatment,ProdCon2b)
xyplot(productivity.total.g.m2.day~Com.P.conc|treatment,ProdCon2)
xyplot(productivity.total.g.m2.day~Com.P.conc.lag|treatment,ProdCon2)
xyplot(productivity.total.g.m2.day~Com.P.conc.LAG|treatment,ProdCon2)

#############################################################
##### Temporal component - target versus non-target ####
###########################################################################
#### A temporal component ####
#### Add second y axis = rainfall #### Ggplot2 is poor at this - but there is good reason...
#### Target and non-target species - N conc through time####
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TargetPavgYm<-aggregate(P.conc.adj~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
TargetPsdYm<-aggregate(P.conc.adj~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
TargetPavgYm$SE<-TargetPsdYm$P.conc.adj
RainavgYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,na.rm=T,mean)
RainsdYm<-aggregate(rain.sum~target.sp.+region+landuse+treatment+target_other+HarvestMonth,NutsNP,se)
RainavgYm$SE<-RainsdYm$rain.sum
# Grouping variable - this is necessary to connect the lines
TargetPavgYm$grp.code<-as.factor(with(TargetPavgYm, paste(region,landuse,target.sp.,treatment,sep="_")))
RainavgYm$grp.code<-as.factor(with(RainavgYm, paste(region,landuse,target.sp.,treatment,sep="_")))
levels(TargetPavgYm$grp.code)
# Issue - Harvest 5 spread over two months - replace Oct with Sept
#TargetNavgYm$HarvestMonth[TargetNavgYm$HarvestMonth=="2017-10"]<-"2017-09"
#RainavgYm$HarvestMonth[RainavgYm$HarvestMonth=="2017-10"]<-"2017-09"
# Remove Seronera
#TargetNavgYm2<-droplevels(TargetNavgYm[TargetNavgYm$region!="SE",])
#RainavgYm2<-droplevels(RainavgYm[RainavgYm$region!="SE",])
# Rename levels
levels(TargetPavgYm$region)<-c("Dry region","Intermediate region","Wet region")
levels(RainavgYm$region)<-c("Dry region","Intermediate region","Wet region")
levels(TargetPavgYm$landuse)<-c("Pasture","Wildlife protected")
levels(RainavgYm$landuse)<-c("Pasture","Wildlife protected")
levels(TargetPavgYm$treatment)<-c("Exclosed","Open")
levels(RainavgYm$treatment)<-c("Exclosed","Open")
# Target grass species
#Handajega: Themeda triandra
#Mwantimba: Chrysocloa orientalis
#Seronera: Digitaria macroblephara
#Maswa: Cynodon dactylon
#Makao: Chloris pycnothrics
levels(TargetPavgYm$target.sp.)<-c("Chloris","Chrysocloa","Cynodon","Digitaria","Other","Themeda")
levels(RainavgYm$target.sp.)<-c("Chloris","Chrysocloa","Cynodon","Digitaria","Other","Themeda")
# Scale factor
scaleFactor <- mean(TargetPavgYm$P.conc.adj,na.rm=T)/mean(RainavgYm$rain.sum,na.rm=T)
#756
# Treatment code for filling symbols
TargetPavgYm$spp.code<-as.factor(with(TargetPavgYm, paste(target.sp.,treatment,sep="_")))
RainavgYm$spp.code<-as.factor(with(RainavgYm, paste(target.sp.,treatment,sep="_")))
# Convert to date - Yr-Month
TargetPavgYm$HarvestMonth<-as.Date(paste(TargetPavgYm$HarvestMonth,"-01-01",sep=""))
RainavgYm$HarvestMonth<-as.Date(paste(RainavgYm$HarvestMonth,"-01-01",sep=""))
SpTitle<-"Species"
# Plotting line graph with secondary y-axis (stuck on!)
TPYm<-ggplot(TargetPavgYm,aes(y=P.conc.adj,x=HarvestMonth, shape=treatment, group =grp.code)) # group
TPYm<-TPYm+geom_line(aes(colour=target.sp.),position=position_dodge(width=.65),stat = "identity",size=.75,show.legend = F)
TPYm<-TPYm+geom_errorbar(data=TargetPavgYm,aes(ymin=P.conc.adj-SE, ymax=P.conc.adj+SE,colour=target.sp.),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
TPYm<-TPYm+geom_point(aes(colour=target.sp.,fill=spp.code),position=position_dodge(width=.65),size=5,stroke=1)
TPYm<-TPYm+facet_wrap(~region+landuse, ncol=5)
#TPYm<-TPYm+geom_line(data=RainavgYm2,aes(y=(rain.sum+400)/200),linetype="solid",colour="dark blue",size=1,show.legend=F) # Offset the second rainfall legend so it is above the N concnetration
TPYm<-TPYm+geom_smooth(data=RainavgYm,aes(y=(rain.sum+100)*0.0014),method="loess",span=.9, se=F,linetype="dotted",colour="dark blue", size=1,show.legend=F)
TPYm<-TPYm+scale_y_continuous(limits=c(0,.85),sec.axis = sec_axis(~ . /0.0014, breaks = c(0,100,200,400,600), labels = c("","",0,100,500), name = "Rainfall (mm)"),expand=c(0,0))
TPYm<-TPYm+scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y", limits=c(as.Date("2017-01-1"),max=as.Date("2018-03-01")),expand=c(0.1,0.1))
TPYm<-TPYm+ylab("Plant phosphorus concentration (%)")+xlab("Year|month")
TPYm<-TPYm+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"))
TPYm<-TPYm+scale_fill_manual(values=c("chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white", "grey50","white","orangered3","white"))
TPYm<-TPYm+scale_shape_manual(values=c(22,21))
TPYm<-TPYm+theme_bw()+
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=13,color="black")
        ,axis.title.x=element_text(size=13,color="black")
        ,axis.text.x=element_text(size=13,color="black",angle=35,hjust=1,
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(1,1.5,5,1.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 14,hjust=0,angle=0)
        ,panel.spacing = unit(.1, "lines")
        ,legend.text = element_text(size=12,color="black")
        ,legend.title = element_text(size=12,color="black")
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm"))
#TPYm<-TPYm+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = "2017-01", xend ="2017-01", size = 1)
#TPYm<-TPYm+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size=1)
TPYm<-TPYm+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1)),
                  colour = guide_legend("Dominant species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"),
                                                                               col=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"), stroke=1)) )
TPYm
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/SpeciesTimeP.jpeg",
#            width=40, height=16,units ="cm",dpi = 600, limitsize = TRUE)
#### Point graph - each species P compared to other #####
SpeciesP<-aggregate(P.conc.adj~target.sp.+landuse+treatment,TargetPavgYm,mean)
SpeciesPSE<-aggregate(P.conc.adj~target.sp.+landuse+treatment,TargetPavgYm,sd)
SpeciesP$SE<-SpeciesPSE$P.conc.adj
# Treatment code for filling symbols
SpeciesP$spp.code<-as.factor(with(SpeciesP, paste(target.sp.,treatment,sep="_")))
TPs<-ggplot(SpeciesP,aes(y=P.conc.adj,x=landuse, colour=target.sp.,fill=spp.code, shape=treatment))#group = grouping vector for lines
TPs<-TPs+geom_errorbar(data=SpeciesP,aes(ymin=P.conc.adj-SE, ymax=P.conc.adj+SE),stat = "identity",width=.2,lwd=1.1,position=position_dodge(width=.45),show.legend=F)
TPs<-TPs+geom_point(position=position_dodge(width=.45),size=5, stroke=1)
TPs<-TPs+ylab("Plant phosphorus concentration (%)")+xlab("Land-use")
TPs<-TPs+scale_shape_manual(values=c(22,21))
TPs<-TPs+scale_colour_manual(values=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"))
TPs<-TPs+scale_fill_manual(values=c("chartreuse3","white","hotpink1","white","cadetblue3","white","green4","white", "grey50","white","orangered3","white"))
#TPs<-TPs+scale_x_continuous(limits=c(0.5,2.5),breaks=c(1,2),labels=levels(SpeciesN$landuse),expand=c(0,0))
TPs<-TPs+theme_bw()+
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
        ,legend.title = element_text(size=12,color="black")
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm"))
TPs<-TPs+guides(fill=F,linetype=F,shape = guide_legend("Treatment",override.aes = list(shape=c(22,21), size=3.75,fill=c("grey30","white"),col="grey30", stroke=1)),
                colour = guide_legend("Dominant species",override.aes = list(shape=c(21), size=3.75,fill=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"),
                                                                             col=c("chartreuse3","hotpink1","cadetblue3","green4", "grey50", "orangered3"), stroke=1)) )
TPs
#ggsave("/Users/anotherswsmith/Documents/Teaching/R_workshop/SpeciesTargetP.jpeg",
#       width=16, height=12,units ="cm",dpi = 600, limitsize = TRUE)

###########################################################################
##### Phosphorus: Mixed linear model with temporal structure ####
###########################################################################
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/VegSoil_AfricanBioServices/AfricanBioServices-Vegetation-and-soils")
ProdCon<- read.csv('Moveable exclosures/Biomass.csv', sep=',',dec='.')#Wetseason data
names(ProdCon)
dim(ProdCon) # 356  38
dim(NutsONA) # 572 121
# Convert date to 'Year- Month'
Rdate3<-strptime(as.character(ProdCon$setup.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
Rdate4<-strptime(as.character(ProdCon$harvest.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
ProdCon$SetupMonth<-format(as.Date(Rdate3), "%Y-%m")
ProdCon$HarvestMonth<-format(as.Date(Rdate4), "%Y-%m")
#### Remove EX2 (metal) ####
# Remove metal mesh exclosure treatment (EX2) # but keep H0 (renamed here for analysis)
ProdCon1<-droplevels(ProdCon[ProdCon$treatment!="EX2" & ProdCon$treatment!="",]) # There is a gap!
dim(ProdCon1) # 300  40
ProdCon<-ProdCon1
# Remove H0 from nutrient dat
#NutsONA1b<-droplevels(NutsONA2[NutsONA2$harvest!="H0",])
#dim(NutsONA1b) # 144  91
ProdCon2<-merge(NutsONA,ProdCon1, by=c("block.id","plot.id","landuse","treatment",
                                       "block.id.harvest", "site.id","block.id",
                                       "region","block","wp","lat","long",
                                       "growth.period"))
# Consumption rate
ProdCon2$consumption.total.g.m2.day # NA - consumption on the exclosure line?
table(ProdCon2$consumption.total.g.m2.day,ProdCon2$treatment)
ExCon <- ProdCon2[ProdCon2$consumption.total.g.m2.day & ProdCon2$treatment=="EX", ]
OpCon <- ProdCon2[ProdCon2$treatment=="OP", ]
ExOpCon<-left_join(ExCon,OpCon, by=c("block.id","landuse",
                                     "block.id.harvest", "site.id","block.id",
                                     "region","block","wp","lat","long",
                                     "growth.period"))
OpCon$Com.N.conc
ExOpCon$Com.N.conc.y #Open N
# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))}
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
ProdCon2$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(ProdCon2$harvest.date.x,format="%d/%m/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1
# Plot ID without Harvest number
# Plot ID and blockcode without Harvest number
ProdCon2$plot.id<-as.factor(ProdCon2$plot.id)
ProdCon2$fblock<-as.factor(ProdCon2$block)
ProdCon2$block.code<-as.factor(with(ProdCon2, paste(region,landuse,block,sep="_")))
ProdCon2$plot.code<-as.factor(with(ProdCon2, paste(region,landuse,block,treatment,sep="_")))
levels(ProdCon2$plot.code) # 40 levels
# Remove NAs for nitrogen
#NutsONA <- droplevels(NutsO[!is.na(NutsO$Com.N.conc), ])
table(ProdCon2$plot.code,ProdCon2$YrMonthNumber) # Month 8 and 15 few data points
ProdCon2$fYrMonthNumber<-as.factor(ProdCon2$YrMonthNumber)
levels(ProdCon2$fYrMonthNumber)<-c("1", "3","5","7","7","10","13","15")
ProdCon2$YrMonthNumber<-as.numeric(as.character(ProdCon2$fYrMonthNumber))
table(ProdCon2$plot.code,ProdCon2$YrMonthNumber)
apply(table(ProdCon2$plot.code, ProdCon2$fYrMonthNumber) > 1, 1, sum) > 0 # All false...No duplicatesper group
# Autocorrelation structure
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|region/block.code/plot.code) # NOTE this needs to be astraight line
# AR matrix needs to be unique value per group
cs1AR1. <- Initialize(cs1AR1, data =ProdCon2)
corMatrix(cs1AR1.)
ProdCon2b<-droplevels(ProdCon2[!is.na(ProdCon2$productivity.total.g.m2.day) & is.na(ProdCon2$Com.P.conc),])
#### LMM productivity and treatment ####
Pmod1p<-lme(productivity.total.g.m2.day~Com.P.conc+landuse+
              treatment+landuse:treatment+
              Com.P.conc:treatment+
              Com.P.conc:landuse+
              Com.P.conc:treatment:landuse,
            random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1p)
anova(Pmod1p)
AIC(Pmod1p) #565.8928
ProdCon2$productivity.total.g.m2.day
ProdCon2$Com.P.conc
ProdCon2b<-droplevels(ProdCon2[!is.na(ProdCon2$productivity.total.g.m2.day) & !is.na(ProdCon2$Com.P.conc),])
ProdCon2b$Com.P.conc
#### LMM productivity and treatment ####
Pmod1p<-lme(productivity.total.g.m2.day~Com.P.conc+landuse+
              treatment+landuse:treatment+
              Com.P.conc:treatment+
              Com.P.conc:landuse+
              Com.P.conc:treatment:landuse,
            random= ~ 1|region/block.code, na.action=na.pass, method="REML", # REML=T
            correlation=corAR1(0.2, form=~YrMonthNumber|region/block.code/plot.code),data=ProdCon2b)
summary(Pmod1p)
anova(Pmod1p)
AIC(Pmod1p) #565.8928
names(ProdCon2b)
plot(ProdCon2b$productivity.total.g.m2.day~ProdCon2b$Com.P.conc)
xyplot(productivity.total.g.m2.day~Com.P.conc|treatment,ProdCon2b)
xyplot(productivity.total.g.m2.day~Com.P.conc|treatment,ProdCon2)
xyplot(productivity.total.g.m2.day~Com.P.conc.lag|treatment,ProdCon2)
xyplot(productivity.total.g.m2.day~Com.P.conc.LAG|treatment,ProdCon2)


############################################################################
#### END ####
############################################################################


