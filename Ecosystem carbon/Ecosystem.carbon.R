#### Exploring all data at region level ####

Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)

# Renaming SNP Handajega to Handajega 

Woody <- Woody[,c(3:6,12)]

Woody$Region <- as.factor(c("Makao","Maswa", "Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))


#Relevel 
Woody$Region<- factor(Woody$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Merge the three datasets in two steps 
Ecosystem.C1 <- merge(Woody,Herbaceous[2:4],all.x = TRUE,by="Region")
Ecosystem.C <- merge(Ecosystem.C1,Deadwood[2:6],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1,6,2,7,11)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,6,3,8,12)]

# Make the data into a long format instead of a wide
library(tidyr)
library(plyr)

data_long.C <- gather(EcosystemCarbon, Carbon.pool,C.amount, TreeC_m2:DWC.g_m2,factor_key=TRUE)
data_long.CSE <- gather(EcosystemCarbonSE, Carbon.poolSE,C.amountSE, SE.TreeC_m2:SE_DWC_m2,factor_key=TRUE)

Tot.EcosystemCarbon <- cbind(data_long.C,data_long.CSE[4])

# Rename the Carbon pool names 
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="TreeC_m2"] <- "TreeC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="HerbC_m2"] <- "HerbC"
levels(Tot.EcosystemCarbon$Carbon.pool)[levels(Tot.EcosystemCarbon$Carbon.pool)=="DWC.g_m2"] <- "DWC"

Tot.EcosystemCarbon <- arrange(Tot.EcosystemCarbon,Region)

#### Ploting Ecosystem Carbon #### 

library(ggplot2)

# Point plot 
EcosystemC.plot <- ggplot(data = Tot.EcosystemCarbon, aes(x = Region,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

EcosystemC.plot + xlab("Region") + ylab("Carbon")  + geom_point(size = 2, shape=20,stroke=2)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) + scale_color_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Stacked plot 
EcosystemC.bar <- ggplot(data = Tot.EcosystemCarbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill=Carbon.pool))

EcosystemC.bar + geom_bar(stat="identity", position="stack") + theme_bw() + Lines_gone  + xlab("Region") + ylab("Aboveground Carbon (g/m2)")  + geom_errorbar(stat = "identity",width=.2,show.legend=F) + scale_fill_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))


