#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/DW.Block.csv",head=T)

# Remove NAs 
Herbaceous <- na.omit(Herbaceous)
Herbaceous <- droplevels(Herbaceous)
names(Herbaceous)

### Exploring the data at Region level ####
Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)
Soil <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.csv",header=T)

Woody$Region <- as.factor(c("Makao","Maswa", "Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
names(Woody)
Woody <- Woody[,c(1,2,14,11:13,3:10)]

#Relevel 
Woody$Region<- factor(Woody$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil$Region<- factor(Soil$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Merge the three datasets in two steps 
Ecosystem.C1 <- merge(Woody[,c(2:6,7:14)],Herbaceous[2:4],all.x = TRUE,by="Region")
Ecosystem.C2 <- merge(Ecosystem.C1,Deadwood[2:6],all.x = TRUE,by="Region")
Ecosystem.C <- merge(Ecosystem.C2,Soil[2:16],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1:6,14,18,27,29,21:26,31,33,8:13)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,7,15,19,28,30)]

# Make the data into a long format instead of a wide
library(tidyr)
library(plyr)

data_long.C <- gather(EcosystemCarbon, Carbon.pool,C.amount, TreeC_m2:CMinHor,factor_key=TRUE)
data_long.CSE <- gather(EcosystemCarbonSE, Carbon.poolSE,C.amountSE, SE.TreeC_m2:SE.CMinHor,factor_key=TRUE)

Tot.EcosystemCarbon <- cbind(data_long.C,data_long.CSE[3])

# Rename the Carbon pool names 
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="TreeC_m2"] <- "WoodyC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="HerbC_m2"] <- "HerbC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="DWC.g_m2"] <- "DWC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="CAHor"] <- "SoilCAHor"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="CMinHor"] <- "SoilCMinHor"

Tot.EcosystemCarbon <- arrange(Tot.EcosystemCarbon,Region)
colnames(Tot.EcosystemCarbon)[colnames(Tot.EcosystemCarbon) == "Landuse.x"] <- "Landuse"
Tot.EcosystemCarbon$Climate <- as.factor(c("Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Int-Dry","Int-Dry","Int-Dry","Int-Dry","Int-Dry","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet"))

write.csv(Tot.EcosystemCarbon,file="Ecosystem carbon/Tot.Ecosystem.Carbon.csv")
### Ploting Ecosystem Carbon  #### 

Total.ecosystem.carbon <- read.csv("Ecosystem carbon/Tot.Ecosystem.Carbon.csv", head=T)

levels(Total.ecosystem.carbon$Carbon.pool)
Total.ecosystem.carbon$Carbon.pool<- factor(Total.ecosystem.carbon$Carbon.pool, levels = c("WoodyC","HerbC", "DWC","SoilCAHor","SoilCMinHor"))
Total.ecosystem.carbon$Region<- factor(Total.ecosystem.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

library(ggplot2)
library(dplyr)

# Plots per region of ECOSYSTEM CARBON

# Legend titeles
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

#### TOT ECOSYSTEM C ####
# Point

# To get different shape of point: scale_shape_manual(values=c(1,15))
EcosystemC.plot1 <- ggplot(data = Total.ecosystem.carbon, aes(x = Region,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.plot1 + xlab("Region") +  ylab(expression(paste("Ecosystem Carbon (g", m^-2,")")))  + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=.4,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4"))

#ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))

# Bar - dodge 
EcosystemC.bar1 <- ggplot(data = Total.ecosystem.carbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill=Carbon.pool))

EcosystemC.bar1 + 
  geom_errorbar(width=0.6,lwd=0.5,position=position_dodge(width=0.9),show.legend=F) +
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + xlab("Region") + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")"))) + 
  scale_fill_manual(breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

# Bar - stacked 
EcosystemC.bar2 <- ggplot(data = Total.ecosystem.carbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE, ymax=C.amount+C.amountSE, fill=factor(Carbon.pool)))

EcosystemC.bar2 + 
  geom_bar(stat="identity", position="stack",width = 0.7,na.rm=T) +
  theme_bw() + Lines_gone  + 
  xlab("Region") + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")"))) + 
  scale_fill_manual(breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

#+ geom_errorbar(position="dodge",width=.2,show.legend=F, na.rm=T)

# MAP and ECOSYSTEM CARBON
# Seems to be a positive relationship between MAP and C Except for Maswa which is quite hight in tree C even though it is considered a dry region. 
EcosystemC.MAP <- ggplot(data = Total.ecosystem.carbon, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.MAP + xlab(expression(paste("MAP (mm", yr^-1,")")))+ ylab(expression(paste("Ecosystem Carbon (g", m^-2,")"))) + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=30,lwd=1.1,show.legend=F, na.rm=T) + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Other version 

EcosystemC.plot2 <- ggplot(data = Total.ecosystem.carbon, aes(x = Climate,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.plot2 + 
  geom_point(size=4,fill="white",stroke=1.2,position=position_dodge(width=.5),show.legend=T) + 
  geom_errorbar(width=1,lwd=1,position=position_dodge(width=.5),show.legend=F) +
  scale_shape_manual(legend_titleLAND,values=c(16,0))  + 
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone + 
  xlab("Climate") + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")")))

# Soil texture and ECOSYSTEM CARBON

# 1. Clay 
# Clay seems to be affecting ecosystem C positive, except for with Handajega (low clay, but loads of aboveground C) 

EcosystemC.Soil.clay<- ggplot(data = Total.ecosystem.carbon, aes(x = Clay,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.Soil.clay  + xlab("Clay (%)") + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")"))) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

# 2. Sand
# Negative relationship 
EcosystemC.Soil.sand<- ggplot(data = Total.ecosystem.carbon, aes(x = Sand,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.Soil.sand  + xlab("Sand (%)") + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")"))) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

#### Trees and ECOSYSTEM CARBON - maybe not so relevant #### 
# Tre basal area and ECOSYSTEM CARBON

EcosystemC.basal.area <- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBasalA_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.basal.area  + xlab(expression(paste("Tree basal area (per ", m^-2,")"))) + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")")))  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.01,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Number of trees and ECOSYSTEM CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
EcosystemC.No.trees<- ggplot(data = Total.ecosystem.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.No.trees  + xlab("Number of trees (m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC"),values=c("dimgray","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone

# Tree Biomass and ECOSYSTEM CARBON

EcosystemC.treeBM<- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBM_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.treeBM  + xlab("Tree Biomass (g/m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)   + 
  geom_errorbar(stat = "identity",width=35,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC"),values=c("dimgray","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone

#### SOIL CARBON #### 

# Tre basal area and SOIL CARBON

# Creating a table with soil carbon 
Soil.carbon <- Total.ecosystem.carbon %>%
  filter(Carbon.pool == "SoilCAHor" | Carbon.pool== "SoilCMinHor")
Soil.carbon <- arrange(Soil.carbon,Region)

SoilC.basal.area <- ggplot(data = Soil.carbon, aes(x = TreeBasalA_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

SoilC.basal.area  + xlab(expression(paste("Tree basal area (per ", m^-2,")"))) + ylab(expression(paste("Soil Carbon (g", m^-2,")")))  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.01,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Number of trees and SOIL CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
SoilC.No.trees<- ggplot(data = Soil.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

SoilC.No.trees  + xlab("Number of trees (m2)") + ylab("Soil Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Tree Biomass and SOIL CARBON

SoilC.treeBM<- ggplot(data = Soil.carbon, aes(x = TreeBM_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

SoilC.treeBM  + xlab("Tree Biomass (g/m2)") + ylab("Soil Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)   + 
  geom_errorbar(stat = "identity",width=35,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) +
  theme_bw() + Lines_gone

### Aggregate TOTAL CARBON per region ####

EcosystemC <- aggregate(C.amount~Region, data=Total.ecosystem.carbon,sum)
EcosystemCSE <- aggregate(C.amountSE~Region, data=Total.ecosystem.carbon,sum)
No.trees_m2 <- aggregate(No.trees_m2~Region, data=Total.ecosystem.carbon,median)
TreeBM_m2 <- aggregate(TreeBM_m2~Region, data=Total.ecosystem.carbon,median)
Ecosystem.Carbon <- cbind(EcosystemC,EcosystemCSE[2],No.trees_m2[2],TreeBM_m2[2])
colnames(Ecosystem.Carbon) <- c("Region","C.amount","SE.C.Amount","No.trees_m2","TreeBM_m2") # Maswa extreamly high in C and woody cover
Ecosystem.Carbon$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

# Seperate per land-use 
Wild.EcosystemC <- Ecosystem.Carbon[Ecosystem.Carbon$Landuse!="Pasture",]# Only wild regions
Wild.EcosystemC <- droplevels(Wild.AbovegroundC)
Pasture.EcosystemC <- Ecosystem.Carbon[Ecosystem.Carbon$Landuse!="Wild",]# Only pasture regions
Pasture.EcosystemC <- droplevels(Pasture.AbovegroundC)

# Number of trees and ECOSYSTEM CARBON
# Point
Tree.no.plot <- ggplot(data=Ecosystem.Carbon, aes(x=No.trees_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

Tree.no.plot + xlab("Number of Trees") + ylab("Ecosystem Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat="identity",width=.001,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

# Biomass of trees and ECOSYSTEM CARBON

# Point
BM.plot <- ggplot(data=Ecosystem.Carbon, aes(x=TreeBM_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

BM.plot + xlab("Tree Biomass (g/m2)") + ylab("Ecosystem Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(width=35,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

#???
# Wild plot
Woody.cover.plot.wild <- ggplot(data=Wild.AbovegroundC, aes(x=Woody.Cover,y=C.amount, colour="Wild"))

Woody.cover.plot.wild + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Wild"),values=c("forestgreen"))

# Pasture plot
Woody.cover.plot.pasture <- ggplot(data=Pasture.AbovegroundC, aes(x=Woody.Cover,y=C.amount,colour="Pasture"))

Woody.cover.plot.pasture + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Pasture"),values=c("chocolate2"))

### Correlation between variables #### 

# Want to do the analysis at block size 
Soil.properties <- read.csv("Ecosystem carbon/Soil.data/Soil.properties.csv", head=T) # 28 obs
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T) #27 obs, missing one in Seronera 

# Using DataCombine to insert a new row in Tree data 
library(DataCombine)

New.row <- c("20","Seronera",4,"Wild",2500,855.6199,NA,NA,NA,NA,NA,NA)
Tree.carbon <- InsertRow(Tree.carbon,New.row,20)

names(Soil.properties)
names(Tree.carbon)

Correlation <- cbind(Soil.properties,Tree.carbon[9:12])
Correlation <- na.omit(Correlation)
Correlation <- droplevels(Correlation)
str(Correlation)

Correlation$Total.basal.area_m2<- as.numeric(Correlation$Total.basal.area_m2)
Correlation$No.trees_m2 <- as.numeric(Correlation$No.trees_m2 )
# RUN THIS CODE FIRST (FROM STU)

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

# Then select the variables to use in the pair function with panel.cor

names(Correlation)
MyVar<-c("MAP","Clay","Last.fire", "Fire.freq","No.trees_m2","Total.basal.area_m2")

# Want to get these two in one matrix. 
pairs(Correlation[,MyVar],lower.panel = panel.cor)

# If I want these values in a table:
Red.Ecosystem.C <- Total.ecosystem.carbon[,c(4:7,13,17)] # first select the collums I want to use 
library("Hmisc")
Mycor <- rcorr(as.matrix(Red.Ecosystem.C), type="pearson") # Use the pearson correlation (r-value)
MycorR <- as.data.frame(round(Mycor$r, digits=3))
MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

# ANOVA on LANDUSE to look for correlation, is the variance bigger between than within? 
par(mfrow=c(1,1))
plot(TreeBasalA_m2~Landuse.x, data= Total.ecosystem.carbon)
plot(No.trees_m2~Landuse.x, data= Total.ecosystem.carbon)

# Tree basal area and landuse 1. per Region 
Tree.basal.area <- lm(TreeBasalA_m2~factor(Landuse), data= Total.ecosystem.carbon)
anova(Tree.basal.area) # Look at the F- value, quite high, and the P value for the F-test is significant. 
summary(Tree.basal.area) # Not look at the P value here - because this is for the t-test!! Look at the Adjusted R-squared value (0.3597), and the difference between the estimates: Wild= 0.0087, Pasture = 0.0087+0.068 = 0.076, quite a difference -> use this if plotting the values.. 
par(mfrow=c(2,2))
plot(Tree.basal.area)
TukeyHSD(aov(Tree.basal.area)) # Post Hoc test to look at the difference between the variables, not important when I only have to variables, I get the same information from summary.. 

tapply(Total.ecosystem.carbon$TreeBasalA_m2, Total.ecosystem.carbon$Landuse, mean)

# Testing if correlated per block.. 
Tree.BA <- lm(Total.basal.area_m2~factor(landuse), data=Tree.carbon)
summary(Tree.BA) # Still a correlation, but less significant, lower r-value.. 

Tree.no <- lm(No.trees_m2~factor(landuse),data=Tree.carbon)
summary(Tree.no) # Quite the same as at regional level actually! 

# Number of trees and landuse 
Number.of.trees <- lm(No.trees_m2~factor(Landuse), data= Total.ecosystem.carbon)
anova(Number.of.trees)
summary(Number.of.trees)
par(mfrow=c(2,2))
plot(Number.of.trees)
TukeyHSD(aov(Number.of.trees))

# Landuse and correlation with other variables 
MAP <- lm(MAP.mm_yr~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(MAP)
Clay <- lm(Clay~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(Clay)
Last.fire <- lm(Last_fire.yr~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(Last.fire)
Fire.freq <- lm(Fire_frequency.2000_2017~factor(Landuse.x), data= Total.ecosystem.carbon)
summary(Fire.freq)


         