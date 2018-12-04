#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/DW.Block.csv",head=T)

# Remove NAs 
Herbaceous <- na.omit(Herbaceous)
Herbaceous <- droplevels(Herbaceous)
names(Herbaceous)

####SOIL####
#Soilfull <- read.csv(file="Ecosystem carbon/Soil.data/Soil.texture.csv",header=T)
#Soilfull <- Soilfull[,c(2:5,7:11)]
#SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
#Clay.se <- aggregate(Clay.per~Region+Land_Use,data=Soilfull,SE)
#Clay <- aggregate(Clay.per~Region+Land_Use,data=Soilfull,mean)
#Silt.se <- aggregate(Silt.per~Region+Land_Use,data=Soilfull,SE)
#Silt <- aggregate(Silt.per~Region+Land_Use,data=Soilfull,mean)
#Sand.se <- aggregate(Sand.per~Region+Land_Use,data=Soilfull,SE)
#Sand <- aggregate(Sand.per~Region+Land_Use,data=Soilfull,mean)
#Soil <- cbind(Clay,Clay.se[3],Silt[3],Silt.se[3],Sand[3],Sand.se[3])
#colnames(Soil) <- c("Region","Landuse","Clay","SE.Clay","Silt","SE.Silt","Sand","SE.Sand")
#write.csv(Soil,file="Ecosystem carbon/Soil.data/Soil.texture.csv")
# Renaming SNP Handajega to Handajega 

### Exploring the data at Region level ####
Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)
Soil <- read.csv(file="Ecosystem carbon/Soil.data/Soil.texture.csv",header=T)

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
Ecosystem.C <- merge(Ecosystem.C2,Soil[2:9],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1:5,6,14,18,21:26,8:13)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,7,15,19)]

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

write.csv(Tot.EcosystemCarbon,file="Ecosystem carbon/Tot.Ecosystem.Carbon.csv")
### Ploting Ecosystem Carbon  #### 

Total.ecosystem.carbon <- read.csv("Ecosystem carbon/Tot.Ecosystem.Carbon.csv", head=T)

levels(Total.ecosystem.carbon$Carbon.pool)
Total.ecosystem.carbon$Carbon.pool<- factor(Total.ecosystem.carbon$Carbon.pool, levels = c("TreeC","HerbC", "DWC"))

library(ggplot2)

# Plots per region of ECOSYSTEM CARBON

# Legend titeles
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

# TOT ECOSYSTEM C - REGION
# Point

# To get different shape of point: scale_shape_manual(values=c(1,15))
EcosystemC.plot1 <- ggplot(data = Total.ecosystem.carbon, aes(x = Region,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.plot1 + xlab("Region") +  ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))  + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=.4,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

#ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))

# Other version 

EcosystemC.plot2 <- ggplot(data = Total.ecosystem.carbon, aes(x = Region,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.plot2 + 
  geom_point(size=4,fill="white",stroke=1.2,position=position_dodge(width=.5),show.legend=T) + 
  geom_errorbar(width=1,lwd=1,position=position_dodge(width=.5),show.legend=F) +
  scale_shape_manual(legend_titleLAND,values=c(4,1))  + 
  scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone + 
  xlab("Region") + ylab("Aboveground Carbon (g/m2)")

# Bar - dodge 
EcosystemC.bar1 <- ggplot(data = Total.ecosystem.carbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, fill=Carbon.pool))

EcosystemC.bar1 + 
  geom_errorbar(width=0.6,lwd=0.5,position=position_dodge(width=0.9),show.legend=F) +
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + xlab("Region") + ylab("Aboveground Carbon (g/m2)") + 
  scale_fill_manual(breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Bar - stacked 
EcosystemC.bar2 <- ggplot(data = Total.ecosystem.carbon, aes(x=Region,y=C.amount,ymin=C.amount-C.amountSE, ymax=C.amount+C.amountSE, fill=factor(Carbon.pool)))

EcosystemC.bar2 + 
  geom_bar(stat="identity", position="stack",width = 0.7,na.rm=T) +
  theme_bw() + Lines_gone  + 
  xlab("Region") + ylab("Aboveground Carbon (g/m2)") + 
  scale_fill_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

#+ geom_errorbar(position="dodge",width=.2,show.legend=F, na.rm=T)

# MAP and ECOSYSTEM CARBON
# Seems to be a positive relationship between MAP and C Except for Maswa which is quite hight in tree C even though it is considered a dry region. 
EcosystemC.MAP <- ggplot(data = Total.ecosystem.carbon, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.MAP + xlab(expression(paste("MAP (mm", yr^-1,")")))+ ylab(expression(paste("Aboveground Carbon (g", m^-2,")"))) + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=30,lwd=1.1,show.legend=F, na.rm=T) + 
  theme_bw() + Lines_gone  + scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Tre basal area and ECOSYSTEM CARBON
# Tree basal area per m2 seems to affect C positive - will be exciting here to see the relationship with soil C. 

EcosystemC.basal.area <- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBasalA_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.basal.area  + xlab(expression(paste("Tree basal area (per ", m^-2,")"))) + ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))  + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=0.01,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Number of trees and ECOSYSTEM CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
EcosystemC.No.trees<- ggplot(data = Total.ecosystem.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.No.trees  + xlab("Number of trees (m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Tree Biomass and ECOSYSTEM CARBON
# Difficult relationship - not really a pattern here. 

EcosystemC.treeBM<- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBM_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.treeBM  + xlab("Tree Biomass (g/m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=35,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

# Soil texture and ECOSYSTEM CARBON
# Clay seems to be affecting ecosystem C positive, except for with Handajega (low clay, but loads of aboveground C) 

EcosystemC.Soil.texture<- ggplot(data = Total.ecosystem.carbon, aes(x = Clay,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.Soil.texture  + xlab("Clay (%)") + ylab(expression(paste("Aboveground Carbon (g", m^-2,")"))) + 
  geom_point(size = 4, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("TreeC", "HerbC","DWC"),values=c("darkolivegreen4","forestgreen","chocolate2"))

### Aggregate TOTAL CARBON per region ####

AbovegroundC <- aggregate(C.amount~Region, data=Total.ecosystem.carbon,sum)
AbovegroundCSE <- aggregate(C.amountSE~Region, data=Total.ecosystem.carbon,sum)
No.trees_m2 <- aggregate(No.trees_m2~Region, data=Total.ecosystem.carbon,median)
TreeBM_m2 <- aggregate(TreeBM_m2~Region, data=Total.ecosystem.carbon,median)
Aboveground.Carbon <- cbind(AbovegroundC,AbovegroundCSE[2],No.trees_m2[2],TreeBM_m2[2])
colnames(Aboveground.Carbon) <- c("Region","C.amount","SE.C.Amount","No.trees_m2","TreeBM_m2") # Maswa extreamly high in C and woody cover
Aboveground.Carbon$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

# Seperate per land-use 
Wild.AbovegroundC <- Aboveground.Carbon[Aboveground.Carbon$Landuse!="Pasture",]# Only wild regions
Wild.AbovegroundC <- droplevels(Wild.AbovegroundC)
Pasture.AbovegroundC <- Aboveground.Carbon[Aboveground.Carbon$Landuse!="Wild",]# Only pasture regions
Pasture.AbovegroundC <- droplevels(Pasture.AbovegroundC)

# Number of trees and ECOSYSTEM CARBON
# Point
Tree.no.plot <- ggplot(data=Aboveground.Carbon, aes(x=No.trees_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

Tree.no.plot + xlab("# Trees") + ylab("Aboveground Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(stat="identity",width=.001,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

# Biomass of trees and ECOSYSTEM CARBON

# Point
BM.plot <- ggplot(data=Aboveground.Carbon, aes(x=TreeBM_m2,y = C.amount, ymin=C.amount-SE.C.Amount,ymax=C.amount+SE.C.Amount, group = Landuse, colour= Landuse))

BM.plot + xlab("Tree Biomass (g/m2)") + ylab("Aboveground Carbon (g/m2)")  + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + geom_errorbar(width=35,show.legend=F, na.rm=T) + scale_color_manual(breaks = c("Wild", "Pasture"),values=c("forestgreen","chocolate2"))

#???
# Wild plot
Woody.cover.plot.wild <- ggplot(data=Wild.AbovegroundC, aes(x=Woody.Cover,y=C.amount, colour="Wild"))

Woody.cover.plot.wild + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Wild"),values=c("forestgreen"))

# Pasture plot
Woody.cover.plot.pasture <- ggplot(data=Pasture.AbovegroundC, aes(x=Woody.Cover,y=C.amount,colour="Pasture"))

Woody.cover.plot.pasture + xlab("Woody cover") + ylab("Carbon amount") + geom_point(size = 2, shape=20,stroke=2, na.rm=T)  + theme_bw() + Lines_gone + scale_color_manual(breaks = c("Pasture"),values=c("chocolate2"))




### Correlation between variables #### 

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

names(Total.ecosystem.carbon)
MyVar<-c("MAP.mm_yr","Clay","TreeBasalA_m2","No.trees_m2","Last_fire.yr")

pairs(Total.ecosystem.carbon[,MyVar],lower.panel = panel.cor) 
# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP. 
