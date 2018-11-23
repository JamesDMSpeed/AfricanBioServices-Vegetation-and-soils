#### 1. Sorting tree data to what I need to use ####

rm(list=ls())

# packages 
#library(lattice)
#library(MASS)
#library(dplyr)
#library(plyr)
#library(lubridate)
#library(data.table)
#library(xlsx)
#library(ggplot2)

# Data
Philtrees<-read.csv(file="Ecosystem carbon/Tree.data/Tree.data.Seregenti.PhilipoBio.csv", sep=",",header=TRUE)
names(Philtrees)
levels(Philtrees$area)
class(Philtrees$area)

# Removing branch, and removing areas I did not visit 
# - find a way to do this in one process??
Philtrees2<-Philtrees[Philtrees$tree.part!="branch",]
Philtrees3 <- Philtrees2[Philtrees2$area!="MakaoWMA",]
Philtrees3 <- Philtrees3[Philtrees3$area!="Ololosokwan",]
Philtrees4 <- Philtrees3[Philtrees3$area!="SNP kleins gate",]

Philtrees4<-droplevels(Philtrees4)

levels(Philtrees4$area)

# Reducing the data - removing columns I dont need
names(Philtrees4)
Vildetrees <- Philtrees4[,c(1:7,9,26,28,31:33)]

# Rearanging my data in the order I visited the sites: 
Vildetrees$area<- factor(Vildetrees$area, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Vildetrees$area) # Releveled

# making unique block id (factor) by using the paste function - creating block.id column with area and block together seperated by "_" 
Vildetrees$block.id <- as.factor(with(Vildetrees,paste(area,block,sep="_")))
# Then transforming each unique combination into a number
Vildetrees$block.id <- as.factor(as.numeric(Vildetrees$block.id))
summary(levels(Vildetrees$block.id)) # 27 unique blocks (nit 28 - missing one in Seronera)

# Aggregate carbon per block
names(Vildetrees)

Carbon.per.block<-aggregate(Carbon.kg.per.tree~area+block.id, Vildetrees,sum)
Median.Biomass.per.block <- aggregate(Biomass.kg.per.tree~area+block.id,Vildetrees,median)
Trees.per.block <- aggregate(number~area+block.id, Vildetrees,length)
Block.size<-aggregate(area.m2~area+block.id,Vildetrees,mean)
Block <- aggregate(block~area+block.id,Vildetrees,mean)
MAP.mm_2015_2017 <- aggregate(annual.precip.mm2015_2017~area+block.id,Vildetrees,mean)
Fire.freq <- aggregate(Fire.freq~area+block.id,Vildetrees,mean)
Year.of.last.fire <- aggregate(Year.of.last.fire~area+block.id,Vildetrees,mean)

# creating a dataset at block size # WHY STILL WRONG ORDER OF REGIONS?? 
Tree.carbon <- cbind(Block.size,Block[3],Carbon.per.block[3],Median.Biomass.per.block[3],Trees.per.block[3],MAP.mm_2015_2017[3],Fire.freq[3],Year.of.last.fire[3])
colnames(Tree.carbon) <- c("Region","Block.id","Block_area.m2","Philipo.Block","TreeC.kg_block","Median.Tree.biomass.block","No_trees", "MAP.mm_yr","Fire_frequency.2000_2017", "Last_fire.yr")

# Adding a collumn of carbon in g, and per m2, and no of trees per m2 
Tree.carbon$TreeC.g_block <- Tree.carbon$TreeC.kg_block*1000
Tree.carbon$TreeC_m2 <- Tree.carbon$TreeC.g_block/Tree.carbon$Block_area.m2
Tree.carbon$Median.TreeBM.g <- Tree.carbon$Median.Tree.biomass.block*1000
Tree.carbon$Median.TreeBM_m2 <- Tree.carbon$TreeBM.g/Tree.carbon$Block_area.m2
Tree.carbon$No_trees.m2 <- Tree.carbon$No_trees/Tree.carbon$Block_area.m2
names(Tree.carbon)
levels(Tree.carbon$Region)

# adding a collumn of landuse
landuse <- c("Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild")
Tree.carbon$landuse <- landuse

# Make a collumn for my Block.id
Vilde.block <- c(1,2,3,4,3,1,2,4,3,4,1,2,1,4,3,2,1,2,3,4,1,2,3,3,4,1,2)
Tree.carbon$Vilde.block <- Vilde.block
names(Tree.carbon)
Tree.carbon.Vilde <- Tree.carbon[,c(1,2,16,3,15,8:10,12:14)]

# Order the dataset so my block id is increasing
Tree.carbon.Vilde <- Tree.carbon.Vilde[
  order(Tree.carbon.Vilde[,1], Tree.carbon.Vilde[,3] ),
  ]

write.csv(Tree.carbon.Vilde,file="Ecosystem carbon/Tree.data/Tree.Carbon.Vilde.csv") # for further use 

#### 2. Make a table at "region level"####

rm(list=ls())

Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
levels(Tree.carbon$Region)# Remember wrong order of regions.. 
names(Tree.carbon)

#Relevel
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Tree.carbon$Region) # Releveled

# Making a table for Tree Carbon per region 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TreeC.Region.m2<-aggregate(TreeC_m2~Region, Tree.carbon,mean)
TreeC.Region.m2.SE<-aggregate(TreeC_m2~Region, Tree.carbon,SE)
No.trees.m2 <- aggregate(No_trees.m2~Region, Tree.carbon,mean)
No.trees.m2.SE <- aggregate(No_trees.m2~Region, Tree.carbon,SE)
TreeBM.m2 <- aggregate(TreeBM_m2~Region,Tree.carbon,mean)
TreeBM.m2.SE <- aggregate(TreeBM_m2~Region,Tree.carbon,SE)
Median.TreeBM.m2 <- aggregate(TreeBM_m2~Region,Tree.carbon,median)
MAP <- aggregate(MAP.mm_yr~Region,Tree.carbon,mean)
MAP.sd <- aggregate(MAP.mm_yr~Region,Tree.carbon,sd)
Fire.frequency <- aggregate(Fire_frequency.2000_2017~Region,Tree.carbon,mean)
Fire.frequency.sd <- aggregate(Fire_frequency.2000_2017~Region,Tree.carbon,sd)
Year.of.last.fire <- aggregate(Last_fire.yr~Region,Tree.carbon,mean)

TreeC.Region <- cbind(TreeC.Region.m2,TreeC.Region.m2.SE[2],No.trees.m2[2],No.trees.m2.SE[2],TreeBM.m2[2],TreeBM.m2.SE[2],Median.TreeBM.m2[2],MAP[2],MAP.sd[2],Fire.frequency[2],Fire.frequency.sd[2],Year.of.last.fire[2])

colnames(TreeC.Region) <- c("Region","TreeC_m2","SE.TreeC_m2","No_trees.m2","SE.No_trees.m2","TreeBM.m2","TreeBM.m2.SE","Median.TreeBM.m2","MAP.mm_yr","MAP.sd","Fire_frequency.2000_2017","Fire_frequency.sd","Last_fire.yr")

# Add landuse
TreeC.Region$Region
TreeC.Region$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

write.csv(TreeC.Region, file="Ecosystem carbon/Tree.data/TreeC.Region.csv")
#### 3. Exploring the data #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
TreeC.Region <- read.csv(file="Ecosystem Carbon/Tree.data/TreeC.Region.csv", head=T)

# Relevel before making plots
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

TreeC.Region$Region<- factor(TreeC.Region$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

par(mfrow=c(1,2))

# C and no. trees per Region
boxplot(TreeC.m2 ~ Region, 
        xlab = "Region",
        ylab = "Carbon.m2",
        data = Tree.carbon)

boxplot(No_trees ~ Region, 
        xlab = "Region",
        ylab = "#Trees",
        data = Tree.carbon)

# C and no. trees per landuse
plot(No_trees~landuse,
     xlab = "Land Use",
     ylab = "#Trees",
     data=Tree.carbon)

plot(TreeC.m2~landuse,
     xlab = "Land Use",
     ylab = "Carbon per m2",
     data=Tree.carbon)

# Carbon per tree in pasture vs wild  
par(mfrow=c(1,1))
plot (TreeC.m2/No_trees~landuse,
      xlab = "Land Use",
      ylab = "Carbon per tree",
      data=Tree.carbon)

# Plotting trees per Region 

library(ggplot2)

TreeC.Region.plot <- ggplot(data = TreeC.Region, aes(x = Region,y = TreeC_m2, ymin=TreeC_m2-SE.TreeC_m2,ymax=TreeC_m2+SE.TreeC_m2, group = Landuse, colour= Landuse))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

TreeC.Region.plot + xlab("Region") + ylab("Woody plant carbon")  + geom_point(size = 3, shape=20,stroke=2)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) +  scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen"))

# No of trees vs BD 
total.soil.data<- read.csv("Ecosystem Carbon/Soil.data/Total.soil.data.csv", head = TRUE)
names(total.soil.data)

BD.total <- total.soil.data[,c(1:7,14,23)]
tail(BD.total)
BD.total <- na.omit(BD.total)
# Remove O-hor as I only have this for very few plots. 
BD.total <- BD.total[BD.total$Horizon!="O-hor",]
BD.total <- droplevels(BD.total)

BD.total$Region <- factor(BD.total$Region,levels = c("Makao","Maswa","Mwantimba","Handajega", "Seronera","Park Nyigoti","Ikorongo"))

# SE function to use in R + aggrigate 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BD.SE <- aggregate(BD_fine_earth_air_dry~Region,data=BD.total,SE)
BD <- aggregate(BD_fine_earth_air_dry~Region,data=BD.total,mean)
No.trees <- aggregate(No_trees~Region, Tree.carbon,sum)
No.trees.se <- aggregate(No_trees~Region, Tree.carbon,SE)
MAP <- aggregate(MAP.mm_yr~Region, Tree.carbon,mean)


Trees.BD <- cbind(No.trees,No.trees.se[2],BD[2],BD.SE[2],MAP[2])
names(Trees.BD)
colnames(Trees.BD) <- c("Region","No.trees","No.trees.se","BD","BD.se","MAP")
Trees.BD$Land.Use <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

plot(BD~No.trees, data=Trees.BD)
plot(No.trees~BD, data=Trees.BD)
plot(No.trees~MAP, data=Trees.BD)
plot(BD~MAP, data=Trees.BD)

summary(lm(BD~No.trees, data=Trees.BD))
summary(lm(No.trees~MAP, data=Trees.BD))
summary(lm(BD~MAP, data=Trees.BD))
summary(lm(No.trees~MAP+BD, data=Trees.BD))

library(ggplot2)

# Plotting trees against BD 

Plot.trees.BD <- ggplot(data = Trees.BD, aes(x = No.trees,y = BD, ymin=BD-BD.se,ymax=BD+BD.se, colour= Region))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

Plot.trees.BD + geom_point(aes(shape= factor(Land.Use)),stroke=2,size=3)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) 

# Plotting trees against MAP 

Plot.trees.MAP <- ggplot(data = Trees.BD, aes(x = MAP,y = No.trees, ymin=No.trees-No.trees.se,ymax=No.trees+No.trees.se, colour= Region))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

Plot.trees.MAP + geom_point(aes(shape= factor(Land.Use)),stroke=2,size=3)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) 


#### Adding dead wood data ####
Dead.wood <- read.csv("Ecosystem carbon/Tree.data/03Dead_wood.csv",head=T)
names(Dead.wood)
Dead.wood.red <- Dead.wood[,c(1:5,10:16)]
Dead.wood.red<-na.omit(Dead.wood.red)
Dead.wood.red <- droplevels(Dead.wood.red)
levels(Dead.wood.red$Region)
class(Dead.wood.red$Region)

# Aggregate dead wood per circle - and then per Block and Region
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Dead.wood.circle <- aggregate(Carbon.g~Circle+Block+Region,data=Dead.wood.red,sum)
Dead.wood.block <- aggregate(Carbon.g~Block+Region,data=Dead.wood.circle,sum)
Dead.wood.block.SE <- aggregate(Carbon.g~Block+Region,data=Dead.wood.circle,SE)

Dead.wood.Region <- aggregate(Carbon.g~Region,data=Dead.wood.circle,sum)
Dead.wood.Region.SE <- aggregate(Carbon.g~Region,data=Dead.wood.circle,SE)

DW.Region <- cbind(Dead.wood.Region, Dead.wood.Region.SE[2])
colnames(DW.Region) <- c("Region","DWC.g","SE_DWC")

DW.Block <- cbind(Dead.wood.block, Dead.wood.block.SE[3])
DW.Block <- DW.block[,c(2,1,3,4)]
colnames(DW.block) <- c("Region","Block","DWC.g_block","SE_DWC")

# Adding a collumn of C per m2 and SE per m2
DW.Region$DWC.g_m2 <- DW.Region$DWC.g/2500 # dividing by 2500 as this is the block size 
DW.Region$SE_DWC_m2 <- DW.Region$SE_DWC/2500

DW.block$DWC.g_m2 <- DW.block$DWC.g_block/2500 
DW.block$SE_DWC_m2 <- DW.block$SE_DWC/2500

# Adding a collumn of land-use
Landuse <- c("Wild","Wild","Pasture","Wild","Pasture")
DW.Region$Landuse <- Landuse

DW.Region$Region<- factor(DW.Region$Region, levels = c("Makao","Maswa","Handajega","Park Nyigoti","Ikorongo"))

write.csv(DW.Region,file="Ecosystem carbon/Tree.data/DW.Region.csv")

# Exploring the data DW per region
library(ggplot2)
plot(DWC.g_m2~Region,DW.block)
names(DW.Region)

DWC.plot <- ggplot(data = DW.Region, aes(x = Region,y = DWC.g_m2, ymin=DWC.g_m2-SE_DWC_m2,ymax=DWC.g_m2+SE_DWC_m2, group = Landuse, colour= Landuse))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

DWC.plot + xlab("Region") + ylab("Dead wood carbon")  + geom_point(size = 3, shape=20,stroke=2)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) +  scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen"))

# Changing colors to ggplot: 
# 1. Manually: + scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen")
# 2. by a predefined palette: + scale_color_brewer(palette="Dark2")




