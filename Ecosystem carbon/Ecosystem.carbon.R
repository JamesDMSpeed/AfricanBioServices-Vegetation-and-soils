#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/DW.Block.csv",head=T)

# Remove NAs 
Herbaceous <- na.omit(Herbaceous)
Herbaceous <- droplevels(Herbaceous)
names(Herbaceous)

#### Exploring all data at region level ####

Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)

# Renaming SNP Handajega to Handajega 
Woody$Region <- as.factor(c("Makao","Maswa", "Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
names(Woody)
Woody <- Woody[,c(1,2,12,9:11,3:8)]

#Relevel 
Woody$Region<- factor(Woody$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Merge the three datasets in two steps 
Ecosystem.C1 <- merge(Woody[,c(2:4,7:12)],Herbaceous[2:4],all.x = TRUE,by="Region")
Ecosystem.C <- merge(Ecosystem.C1,Deadwood[2:6],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1:3,6,8,4,10,14)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,5,11,15)]

# Make the data into a long format instead of a wide
library(tidyr)
library(plyr)

data_long.C <- gather(EcosystemCarbon, Carbon.pool,C.amount, TreeC_m2:DWC.g_m2,factor_key=TRUE)
data_long.CSE <- gather(EcosystemCarbonSE, Carbon.poolSE,C.amountSE, SE.TreeC_m2:SE_DWC_m2,factor_key=TRUE)

Tot.EcosystemCarbon <- cbind(data_long.C,data_long.CSE[3])

# Rename the Carbon pool names 
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="TreeC_m2"] <- "TreeC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="HerbC_m2"] <- "HerbC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="DWC.g_m2"] <- "DWC"

Tot.EcosystemCarbon <- arrange(Tot.EcosystemCarbon,Region)

#### Ploting Ecosystem Carbon  #### 

library(ggplot2)

# Plots per region of ECOSYSTEM CARBON
#-------------------------------------
# Point
EcosystemC.plot <- ggplot(data = Tot.EcosystemCarbon, aes(x = Region,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

EcosystemC.plot + xlab("Region") + ylab("Aboveground Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Bar
EcosystemC.bar <- ggplot(data = Tot.EcosystemCarbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill=Carbon.pool))

EcosystemC.bar + geom_bar(stat="identity", position="stack",na.rm=T) + theme_bw() + Lines_gone  + xlab("Region") + ylab("Aboveground Carbon (g/m2)")  + geom_errorbar(width=.2,show.legend=F, na.rm=T) + scale_fill_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# MAP and ECOSYSTEM CARBON
#------------------------------------- 
EcosystemC.MAP.bar <- ggplot(data = Tot.EcosystemCarbon, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill = Carbon.pool))

EcosystemC.MAP.bar + geom_bar(stat="identity", position="stack", na.rm=T) + theme_bw() + Lines_gone  + xlab("MAP (mm/yr)") + ylab("Aboveground Carbon (g/m2)")  + geom_errorbar(stat = "identity",show.legend=F, na.rm=T) + scale_fill_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Aggregate per region
#------------------------
AbovegroundC <- aggregate(C.amount~Region, data=Tot.EcosystemCarbon,sum)
AbovegroundCSE <- aggregate(C.amountSE~Region, data=Tot.EcosystemCarbon,sum)
No.trees_m2 <- aggregate(No.trees_m2~Region, data=Tot.EcosystemCarbon,median)
TreeBM_m2 <- aggregate(TreeBM_m2~Region, data=Tot.EcosystemCarbon,median)
Aboveground.Carbon <- cbind(AbovegroundC,AbovegroundCSE[2],No.trees_m2[2],TreeBM_m2[2])
colnames(Aboveground.Carbon) <- c("Region","C.amount","SE.C.Amount","No.trees_m2","TreeBM_m2") # Maswa extreamly high in C and woody cover
Aboveground.Carbon$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

# Seperate per land-use 
Wild.AbovegroundC <- Aboveground.Carbon[Aboveground.Carbon$Landuse!="Pasture",]# Only wild regions
Wild.AbovegroundC <- droplevels(Wild.AbovegroundC)
Pasture.AbovegroundC <- Aboveground.Carbon[Aboveground.Carbon$Landuse!="Wild",]# Only wild regions
Pasture.AbovegroundC <- droplevels(Pasture.AbovegroundC)

# Number of trees and ECOSYSTEM CARBON
#-------------------------------------
# Point
Tree.no.plot <- ggplot(data=Aboveground.Carbon, aes(x=No.trees_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

Tree.no.plot + xlab("# Trees") + ylab("Aboveground Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat="identity",width=.01,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

# Biomass of trees and ECOSYSTEM CARBON
#-------------------------------------

# Point
BM.plot <- ggplot(data=Aboveground.Carbon, aes(x=TreeBM_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

BM.plot + xlab("Tree Biomass (g/m2)") + ylab("Aboveground Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(width=0.2,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

#???
# Wild plot
Woody.cover.plot.wild <- ggplot(data=Wild.AbovegroundC, aes(x=Woody.Cover,y=C.amount, colour="Wild"))

Woody.cover.plot.wild + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Wild"),values=c("forestgreen"))

# Pasture plot
Woody.cover.plot.pasture <- ggplot(data=Pasture.AbovegroundC, aes(x=Woody.Cover,y=C.amount,colour="Pasture"))

Woody.cover.plot.pasture + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Pasture"),values=c("chocolate2"))



