#### 1. Sorting tree data to what I need to use ####

rm(list=ls()) 

# packages 
#library(lattice)
#library(MASS)
library(dplyr)
library(plyr)
#library(lubridate)
#library(data.table)
#library(xlsx)
library(ggplot2)
library(tidyr)

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
Philtrees3 <- Philtrees3[Philtrees3$area!="SNP kleins gate",]
Philtrees4 <- Philtrees3[Philtrees3$Biomass.kg.per.tree!=0,]
Philtrees4<-droplevels(Philtrees4)

levels(Philtrees4$area)
levels(Philtrees4$date)

TargetMAY <- c("06.05.2017","07.05.2017","11.05.2017","13.05.2017", "14.5.2017", "17.05.2017", "9.5.2017")  
TargetDEC<- c("10.12.2017", "12.12.2017", "13.12.2017",  "14.12.2017","16.12.2017", "17.12.2017", "18.12.2017") 

PhiltreesMAY <- filter(Philtrees4, date == TargetMAY)
PhiltreesDEC <- filter(Philtrees4, date == TargetDEC)

# Reducing the data - removing columns I dont need
names(PhiltreesDEC)
levels(PhiltreesDEC$area)

table(PhiltreesDEC$area)
table(PhiltreesMAY$area)
      
Vildetrees <- PhiltreesDEC[,c(1:4,6,7,9,25,26,28,31:33)]

# Rearanging my data in the order I visited the sites: 
Vildetrees$area<- factor(Vildetrees$area, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Vildetrees$area) # Releveled

# Adding carbon and biomass in g, and median biomass
Vildetrees$Carbon.g.tree<- Vildetrees$Carbon.kg.per.tree*1000
Vildetrees$Biomass.g.tree <- Vildetrees$Biomass.kg.per.tree*1000

write.csv(Vildetrees,file="Ecosystem carbon/Tree.data/Vildetrees.csv")

#### AGGREGATE PER BLOCK #### 
# Aggregate carbon per block

Vildetrees <- read.csv(file="Ecosystem carbon/Tree.data/Vildetrees.csv", head=T)
Vildetrees$area<- factor(Vildetrees$area, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

names(Vildetrees)

Tree.carbon <- cbind((aggregate(area.m2~block+area,Vildetrees,mean)),
                     (aggregate(Carbon.g.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(Carbon.kg.per.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(number~block+area, Vildetrees,length))[3],
                      (aggregate(Biomass.g.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(Biomass.kg.per.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(total.basal.area.m2~block+area, Vildetrees,sum))[3],
                      (aggregate(annual.precip.mm2015_2017~block+area,Vildetrees,mean))[3],
                      (aggregate(Fire.freq~block+area,Vildetrees,mean))[3],
                      (aggregate(Year.of.last.fire~block+area,Vildetrees,mean))[3])

# creating a dataset at block size # WHY STILL WRONG ORDER OF REGIONS?? 
colnames(Tree.carbon) <- c("Philipo.Block","Region","Block.area_m2","TreeC.g_block","TreeC.kg_block","No.trees","TreeBM.g_block","TreeBM.kg_block","Total.basal.area_m2", "MAP.mm_yr","Fire_frequency.2000_2017", "Last.fire_yr")

# Adding a collumn of carbon per m2, and no of trees per m2 
Tree.carbon$TreeC.kg_m2 <- Tree.carbon$TreeC.kg_block/Tree.carbon$Block.area_m2
Tree.carbon$TreeBM.kg_m2 <- Tree.carbon$TreeBM.kg_block/Tree.carbon$Block.area_m2
# Tree.carbon$Median.TreeBM_m2 <- Tree.carbon$Median.Tree.BM_g/Tree.carbon$Block_area.m2
Tree.carbon$No.trees_m2 <- Tree.carbon$No.trees/Tree.carbon$Block.area_m2
#Tree.carbon$Woody.cover <- Tree.carbon$No_trees * Tree.carbon$Median.Tree.BM_g
#Tree.carbon$Woody.cover_m2 <- Tree.carbon$Woody.cover/Tree.carbon$Block_area.m2
names(Tree.carbon)
levels(Tree.carbon$Region)

# Missing one row for Seronera - want to add this as a NA row. 
library(DataCombine)

New.row <- c(4,"Seronera",2500,NA,NA,NA,NA,NA,NA,855.6199048,NA,NA,NA,NA,NA)
Tree.carbon <- InsertRow(Tree.carbon,New.row,20)

# adding a collumn of landuse
Tree.carbon$landuse <- as.factor(c("Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild"))

# Make a collumn for my Block.id
Tree.carbon$Vilde.block <- as.numeric(c(3,1,2,4,3,4,1,2,1,4,3,2,3,4,1,2,1,2,3,4,1,2,3,4,1,2,3,4))
names(Tree.carbon)

Tree.carbon.Vilde <- Tree.carbon[,c(2,16,17,3,10:12,4:9,13:15)]

# Order the dataset so my block id is increasing
Tree.carbon.Vilde <- Tree.carbon.Vilde[
  order(Tree.carbon.Vilde[,1], Tree.carbon.Vilde[,3] ),
  ]

# Make unique block ID
# 1. version: 
# making unique block id (factor) by using the paste function - creating block.id column with area and block together seperated by "_" 
#Vildetrees$block.id <- as.factor(with(Vildetrees,paste(area,block,sep="_")))
# Then transforming each unique combination into a number
#Vildetrees$block.id <- as.factor(as.numeric(Vildetrees$block.id))
#summary(levels(Vildetrees$block.id))

#2. version - easy here. 
Tree.carbon.Vilde$Block.ID <- as.numeric(c(1:28))
names(Tree.carbon)

write.csv(Tree.carbon.Vilde,file="Ecosystem carbon/Tree.data/Tree.Carbon.Vilde.csv") # for further use 

#### 2. Make a table at "region level"####

rm(list=ls())

Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
levels(Tree.carbon$Region)# Remember wrong order of regions.. 

#Relevel
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Tree.carbon$Region) # Releveled
names(Tree.carbon)

# Making a table for Tree Carbon per region 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

TreeC.Region <- cbind((aggregate(TreeC.kg_m2~Region, Tree.carbon,mean)),
                      (aggregate(TreeC.kg_m2~Region, Tree.carbon,SE))[2],
                      (aggregate(No.trees_m2~Region, Tree.carbon,mean))[2],
                      (aggregate(No.trees_m2~Region, Tree.carbon,SE))[2],
                      (aggregate(TreeBM.kg_m2~Region,Tree.carbon,mean))[2],
                      (aggregate(TreeBM.kg_m2~Region,Tree.carbon,SE))[2],
                      (aggregate(Total.basal.area_m2~Region,Tree.carbon,mean))[2],
                      (aggregate(Total.basal.area_m2~Region,Tree.carbon,SE))[2],
                      (aggregate(MAP.mm_yr~Region,Tree.carbon,mean))[2],
                      (aggregate(MAP.mm_yr~Region,Tree.carbon,SE))[2],
                      (aggregate(Fire_frequency.2000_2017~Region,Tree.carbon,mean))[2],
                      (aggregate(Fire_frequency.2000_2017~Region,Tree.carbon,SE))[2],
                      (aggregate(Last.fire_yr~Region,Tree.carbon,mean))[2],
                      (aggregate(Last.fire_yr~Region,Tree.carbon,SE))[2])
                      

colnames(TreeC.Region) <- c("Region","TreeC.kg_m2","SE.TreeC.kg_m2","No_trees.m2","SE.No_trees.m2","TreeBM.kg_m2","SE.TreeBM.kg_m2","TreeBasalA.m2","SE.TreeBasalA.m2","MAP.mm_yr","SE.MAP.mm_yr","Fire_frequency.2000_2017","SE.Fire_frequency.2000_2017","Last_fire.yr","SE.Last_fire.yr")

# Add landuse
TreeC.Region$Region
TreeC.Region$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild", "Pasture","Wild"))

write.csv(TreeC.Region, file="Ecosystem carbon/Tree.data/TreeC.Region.csv")

#### 3. Exploring the data #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
TreeC.Region <- read.csv(file="Ecosystem Carbon/Tree.data/TreeC.Region.csv", head=T)
Vildetrees <- read.csv(file="Ecosystem Carbon/Tree.data/Vildetrees.csv",head=T)
# Relevel before making plots
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

TreeC.Region$Region<- factor(TreeC.Region$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

# Make a histogram of the distribution of trees 
p <- ggplot(Vildetrees,aes(x=Biomass.g.tree))
p + geom_histogram()+facet_grid(~area)+theme_bw()

hist(Vildetrees$Biomass.g.tree[Vildetrees$area=="SNP handejega"])
hist(Vildetrees$Biomass.g.tree)

table(Vildetrees$block.id, Vildetrees$area) # Ikorongo,Makao, Maswa, Mwantimba, Park Nyigoti, Seronera, Handajega 

# Simple plots of trees 
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

plot(Biomass.g.tree~block.id,
     xlab = "Block",
     ylab = "Tree Biomass (g)",
     col=c(landuse),
     data=Vildetrees)

par(mfrow=c(1,2))
dotchart(Vildetrees$Biomass.g.tree,groups=Vildetrees$area,main = "area") # Maswa and Handajega have big trees
dotchart(Vildetrees$Biomass.g.tree,groups=Vildetrees$landuse,main = "landuse") # wild have bigger trees than pasture. 
par(mfrow=c(1,1))

# Tree carbon vs LANDUSE
plot(No.trees_m2~landuse,
     xlab = "Land Use",
     ylab = "Number of Trees",
     data=Tree.carbon)

plot(TreeC_m2~landuse,
     xlab = "Land Use",
     ylab = "",
     data=Tree.carbon)

title(ylab= expression("Tree Carbon" ~ (g ~ m^{-2})), line=2)
#cex.lab=1.2, family="Calibri Light"

# Carbon per tree in pasture vs wild  

plot (TreeC_m2/No.trees_m2~landuse,
      xlab = "Land Use",
      ylab = "Carbon per tree",  
      data=Tree.carbon)

identify(Tree.carbon$TreeC_m2/Tree.carbon$No.trees_m2~Tree.carbon$landuse) # The outliar in Pasture is Park Nyigoti block 1. 

# Making a plot of total number of trees over median size of trees per Region 
size.number <- cbind((aggregate(total.basal.area.m2~area+block+landuse + annual.precip.mm2015_2017,data=Vildetrees, median)),(aggregate(X~area+block+landuse,data=Vildetrees,length))[4])

colnames(size.number) <- c("Region","block","landuse","MAP","median.basal.area","no.trees")
size.number <- arrange(size.number,Region)

size.number$Region<- factor(size.number$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

plot(no.trees/median.basal.area~landuse, data=size.number)
plot(no.trees/median.basal.area~MAP, data=size.number)

# Tree basal area vs landuse 
plot (Total.basal.area_m2~landuse,
      xlab = "Land Use",
      ylab = "Tree basal area per m2",  
      data=Tree.carbon)

# Look at the size distribution of trees in Serengeti 
#----------------------------------------------------

# Properties of the data
#mean(Vildetrees$Biomass.g.tree) #51656.06
#median(Vildetrees$Biomass.g.tree) #1479.607

#mean(Vildetrees$total.basal.area.m2) # 0.007951697
#median(Vildetrees$total.basal.area.m2) # 0.00014385

# Sort by median tree BM  
large.trees.median <- Vildetrees %>%
  filter(Biomass.g.tree >= 1479.607) %>%
  select(Biomass.g.tree,area,landuse)

table(large.trees.median$area)

small.trees.median <- Vildetrees %>%
  filter(Biomass.g.tree < 1479.607) %>%
  select(Biomass.g.tree,area,landuse)

table(small.trees.median$area)

# Make a new long data set based on median tree BM size 
large.trees <- aggregate(Biomass.g.tree~area,data=large.trees.median,length)
colnames(large.trees) <- c("Region","Large")
small.trees <- aggregate(Biomass.g.tree~area,data=small.trees.median,length)
colnames(small.trees) <- c("Region","Small")
Tree.size <- cbind(large.trees,small.trees[2])
Tree.BM.dist <- gather(Tree.size,Size, Count, Large:Small,factor_key=TRUE)
Tree.BM.dist <- arrange(Tree.BM.dist,Region)
Tree.BM.dist$Region<- factor(Tree.BM.dist$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))
Tree.BM.dist$Landuse <- as.factor(c("Wild","Wild","Pasture","Pasture","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild"))
#### Sort by mean tree BM ####
large.trees.mean <- Vildetrees %>%
  filter(Biomass.g.tree >= 51656.06) %>%
  select(Biomass.g.tree,area,landuse)

large.trees2 <- as.data.frame(table(large.trees.mean$area))

small.trees.mean <- Vildetrees %>%
  filter(Biomass.g.tree < 51656.06) %>%
  select(Biomass.g.tree,area,landuse)

small.trees2 <- as.data.frame(table(small.trees.mean$area))

# Make a new long data set based on mean tree BM size 
Tree.size2 <- cbind(large.trees2,small.trees2[2]) 
colnames(Tree.size2) <- c("Region","Large","Small")
Tree.BM.dist2 <- gather(Tree.size2,Size, Freq, Large:Small,factor_key=TRUE)
Tree.BM.dist2 <- arrange(Tree.BM.dist2,Region)
Tree.BM.dist2$Region<- factor(Tree.BM.dist$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))
Tree.BM.dist2$Landuse <- as.factor(c("Wild","Wild","Pasture","Pasture","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild"))

# PLOTTING
#-------------------------------------------
# Plotting the size distribution based on median tree biomass. 

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

legend_titleLAND <- "Land-use"
legent_titleSIZE <- "Size"

# DENSITY PLOT 
size.plot.density <- ggplot(Tree.BM.dist, aes(x=Count, fill= Size))
size.plot.density + geom_density() # Smooth histogram, shows the distribution of the small and large trees. So some regions had a lot of small trees, some had really few, but most regions had something in the midle. While small trees where more spread out. 

# FACET WRAP - Want to look at all regions at the same time MEDIAN
size.plot.wrap <- ggplot(Tree.BM.dist, aes(x= Size, y= Count, colour=Landuse, shape=Size)) 

size.plot.wrap + 
  geom_point(size = 4,stroke=2, na.rm=T, show.legend = T) +
  facet_wrap(~Region) + 
  scale_shape_manual(legent_titleSIZE, values=c(4,1)) + 
  scale_color_manual(legend_titleLAND,breaks = c("Wild", "Pasture"),values=c("goldenrod3", "forestgreen")) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  Lines_gone

# FACET WRAP - Want to look at all regions at the same time MEAN
size.plot.wrap2 <- ggplot(Tree.BM.dist2, aes(x= Size, y= Freq, colour=Landuse, shape=Size)) 

size.plot.wrap2 + 
  geom_point(size = 4,stroke=2, na.rm=T, show.legend = T) +
  facet_wrap(~Region) + 
  scale_shape_manual(legent_titleSIZE, values=c(4,1)) + 
  scale_color_manual(legend_titleLAND,breaks = c("Wild", "Pasture"),values=c("goldenrod3", "forestgreen")) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  Lines_gone


# Barplot of size 
size.plot.bar <- ggplot(Tree.BM.dist, aes(x=Region,y=Count, fill= Size))
  
size.plot.bar+ 
  geom_bar(stat="identity", position="stack",na.rm=T) + 
  theme_bw() + Lines_gone + xlab("Region") + ylab("Number of Trees") + ggtitle("Median tree biomass")  + 
  scale_fill_manual(breaks = c("Large", "Small"),values=c("goldenrod3", "forestgreen")) 

# Plotting the size distribution based on mean tree biomass. 
#size.plot2 <- ggplot(Tree.size.dist2, aes(x=Region,y=Count, fill= Size))
#size.plot2 + geom_bar(stat="identity", position="stack",na.rm=T) + xlab("Region") + ylab("Number of Trees")+ ggtitle("Mean tree biomass") + theme_bw()  + scale_fill_manual(breaks = c("Large", "Small"),values=c("goldenrod3", "forestgreen"))

#### Plotting Tree data at Regional Level ####

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

TreeC.Region.plot <- ggplot(data = TreeC.Region, aes(x = Region,y = TreeC_m2, ymin=TreeC_m2-SE.TreeC_m2,ymax=TreeC_m2+SE.TreeC_m2, group = Landuse, colour= Landuse))

TreeC.Region.plot + xlab("Region") + ylab("Woody plant carbon")  + 
  geom_point(size = 3, shape=20,stroke=2)  +
  geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) + 
  theme_bw() + Lines_gone +  scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen"))

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

SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BD.SE <- aggregate(BD_fine_earth_air_dry~Region,data=BD.total,SE)
BD <- aggregate(BD_fine_earth_air_dry~Region,data=BD.total,mean)

# Include BD in Tree dataset 
Trees.BD <- cbind(TreeC.Region,BD[2],BD.SE[2])
names(Trees.BD)
colnames(Trees.BD)[15] <-"BD"
colnames(Trees.BD)[16] <-"SE.BD"

plot(BD~No.trees, data=Trees.BD)
plot(No.trees~BD, data=Trees.BD)
plot(No.trees~MAP, data=Trees.BD)
plot(BD~MAP, data=Trees.BD)

summary(lm(BD~No.trees, data=Trees.BD))
summary(lm(No.trees~MAP, data=Trees.BD))
summary(lm(BD~MAP, data=Trees.BD))
summary(lm(No.trees~MAP+BD, data=Trees.BD))

# Plotting trees against BD 

Plot.trees.BD <- ggplot(data = Trees.BD, aes(x = BD,y = TreeBasalA_m2, ymin=TreeBasalA_m2-SE.TreeBasalA_m2,ymax=TreeBasalA_m2+SE.TreeBasalA_m2, colour= Region))

Plot.trees.BD + 
  geom_point(aes(shape= factor(Landuse)),stroke=2,size=3)  + 
  geom_errorbar(stat = "identity",width=.02,lwd=1.1,show.legend=F) + 
  theme_bw() + Lines_gone 

# Plotting trees against MAP 

# Number of trees 
Plot.notrees.MAP <- ggplot(data = Trees.BD, aes(x = MAP.mm_yr,y = No.trees_m2, ymin=No.trees_m2-SE.No.trees_m2,ymax=No.trees_m2+ SE.No.trees_m2, colour= Landuse, shape=Landuse))

Plot.notrees.MAP + 
  geom_point(size=4,fill="white",stroke=1.2,position=position_dodge(width=.5),show.legend=T) + 
  geom_errorbar(stat = "identity",width=20,lwd=1.1,show.legend=F) + 
  scale_shape_manual(legend_titleLAND,values=c(4,1))  + 
  theme_bw() + Lines_gone + 
  xlab(expression(paste("MAP (mm", yr^-1,")"))) + ylab("Number of trees per m2")

# Tree biomass
Plot.TreeBM.MAP <- ggplot(data = Trees.BD, aes(x = MAP.mm_yr,y =TreeBM_m2, ymin=TreeBM_m2-SE.TreeBM_m2,ymax=TreeBM_m2+ SE.TreeBM_m2, colour= Landuse, shape=Landuse))

Plot.TreeBM.MAP + 
  geom_point(size=4,fill="white",stroke=1.2,position=position_dodge(width=.5),show.legend=T) + 
  geom_errorbar(stat = "identity",width=20,lwd=1.1,show.legend=F) + 
  scale_shape_manual(legend_titleLAND,values=c(4,1))  + 
  theme_bw() + Lines_gone + 
  xlab(expression(paste("MAP (mm", yr^-1,")"))) + ylab(expression(paste("Tree biomass (g", m^-2,")")))

#### Adding dead wood data ####
Dead.wood <- read.csv("Ecosystem carbon/Tree.data/Dead_wood.csv",head=T)
names(Dead.wood)
Dead.wood.red <- Dead.wood[,c(2,4:6,9)]
#Dead.wood.red<-na.omit(Dead.wood.red)
Dead.wood.red <- droplevels(Dead.wood.red)
levels(Dead.wood.red$Region)

Dead.wood.red$Region<- factor(Dead.wood.red$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera", "Park Nyigoti","Ikorongo"))

# Aggregate dead wood per circle - and then per Block and Region
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
names(Dead.wood.red)

Block.ID <- aggregate(Block.ID~Region+Block,data=Dead.wood.red,mean)
DW.Carbon <- aggregate(Carbon.kg_m2~Region+Block+Block.ID,data=Dead.wood.red,mean)
SE.DW.Carbon <- aggregate(Carbon.kg_m2~Region+Block+Block.ID,data=Dead.wood.red,SE)

SE.C.DW <- full_join(Block.ID,SE.DW.Carbon)
C.DW <- full_join(Block.ID,DW.Carbon)

Dead.wood.C.block <- cbind(C.DW,SE.C.DW[4])
colnames(Dead.wood.C.block) <- c("Region","Block","Block.ID","DWC.kg_m2","SE.DWC.kg_m2")

Dead.wood.C.block <- Dead.wood.C.block[
  order(Dead.wood.C.block[,1], Dead.wood.C.block[,2] ),
  ]

#DW.Region <- cbind((aggregate(DWC.kg_m2~Region,data=DW.block,mean)), 
#                    (aggregate(DWC.kg_m2~Region,data=DW.block,SE))[2])
#colnames(DW.Region) <- c("Region","DWC.kg_m2","SE.DWC.kg_m2")

# Adding a collumn of land-use
Dead.wood.C.block$Landuse <- as.factor(c("Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild"))

write.csv(DW.block,file="Ecosystem carbon/Tree.data/DW.Block.csv")
#write.csv(DW.Region,file="Ecosystem carbon/Tree.data/DW.Region.csv")

# Exploring the data DW per Block
library(ggplot2)
plot(DWC.kg_m2~Region,DW.block)

DWC.plot <- ggplot(data = DW.block, aes(x = Region,y = DWC.kg_m2, ymin=DWC.kg_m2-SE.DWC.kg_m2,ymax=DWC.kg_m2+SE.DWC.kg_m2, group = Landuse, colour= Landuse))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

DWC.plot + xlab("Region") + ylab("Dead wood carbon")  + geom_point(size = 3, shape=20,stroke=2)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) +  scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen"))

# Changing colors to ggplot: 
# 1. Manually: + scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen")
# 2. by a predefined palette: + scale_color_brewer(palette="Dark2")




