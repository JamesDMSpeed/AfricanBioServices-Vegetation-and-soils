#### Exploring all data at region level ####

Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)

# Renaming SNP Handajega to Handajega 

Woody <- Woody[,c(3:10,15)]

Woody$Region <- as.factor(c("Makao","Maswa", "Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Woody$Woody.cover <- Woody$Median.TreeBM.m2/Woody$No_trees.m2

#Relevel 
Woody$Region<- factor(Woody$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Merge the three datasets in two steps 
Ecosystem.C1 <- merge(Woody[,c(1:2,8:10)],Herbaceous[2:4],all.x = TRUE,by="Region")
Ecosystem.C <- merge(Ecosystem.C1,Deadwood[2:6],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1,5,4,2,6,10)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,3,7,11)]

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

# Point plot per region
EcosystemC.plot <- ggplot(data = Tot.EcosystemCarbon, aes(x = Region,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

EcosystemC.plot + xlab("Region") + ylab("Aboveground Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Stacked plot per region 
EcosystemC.bar <- ggplot(data = Tot.EcosystemCarbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill=Carbon.pool))

EcosystemC.bar + geom_bar(stat="identity", position="stack",na.rm=T) + theme_bw() + Lines_gone  + xlab("Region") + ylab("Aboveground Carbon (g/m2)")  + geom_errorbar(width=.2,show.legend=F, na.rm=T) + scale_fill_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Bar plot of MAP-carbon 
EcosystemC.MAP.bar <- ggplot(data = Tot.EcosystemCarbon, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill = Carbon.pool))

EcosystemC.MAP.bar + geom_bar(stat="identity", position="stack", na.rm=T) + theme_bw() + Lines_gone  + xlab("MAP (mm/yr)") + ylab("Aboveground Carbon (g/m2)")  + geom_errorbar(stat = "identity",show.legend=F, na.rm=T) + scale_fill_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Number of trees 
Tree.no.plot <- ggplot(data=Woody, aes(x=Region,y=No_trees.m2, ymin=No_trees.m2-SE.No_trees.m2,ymax=No_trees.m2+SE.No_trees.m2, group=Landuse, colour=Landuse))

Tree.no.plot + xlab("Region") + ylab("No of trees")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Pasture", "Wild"),values=c("chocolate2","forestgreen"))

# Woody cover 
Woody.cover.plot <- ggplot(data=Woody, aes(x=Region,y=Woody.cover,group=Landuse, colour=Landuse))

Woody.cover.plot + xlab("Region") + ylab("Median Tree BM/No_trees.m2") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone

