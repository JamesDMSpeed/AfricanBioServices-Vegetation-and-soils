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
library(DataCombine)

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

# Trying to find a cut of value for basal area at 2 m height (maybe I dont have to do this)
#names(Philtrees4)
#str(Philtrees4)
#Philtrees4$height.cm[Philtrees4$height.cm == 0] <- NA

#Height.BA <- na.omit(Philtrees4 %>%
#  select(height.cm,total.basal.area.m2))

#plot(total.basal.area.m2~height.cm, data=Height.BA)

TargetMAY <- c("06.05.2017","07.05.2017","11.05.2017","13.05.2017", "14.5.2017", "17.05.2017", "9.5.2017")  
TargetDEC<- c("10.12.2017", "12.12.2017", "13.12.2017",  "14.12.2017","16.12.2017", "17.12.2017", "18.12.2017") 

PhiltreesMAY <- filter(Philtrees4, date == TargetMAY)
PhiltreesDEC <- filter(Philtrees4, date == TargetDEC)

# Reducing the data - removing columns I dont need
names(PhiltreesDEC)
levels(PhiltreesDEC$area)

table(PhiltreesDEC$area)
table(PhiltreesMAY$area)
      
Vildetrees <- PhiltreesDEC[,c(1:4,6,7,9,12,25,26,28,30:33)]
str(Vildetrees)
# Changing SNP handajega to Handajega 
Vildetrees$area <- as.character(Vildetrees$area)
Vildetrees$area[Vildetrees$area == "SNP handejega"] <- "Handajega"

# Rearanging my data in the order I visited the sites: 
Vildetrees$area<- factor(Vildetrees$area, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Vildetrees$area) # Releveled

Vildetrees <- Vildetrees[
  order(Vildetrees[,4], Vildetrees[,6] ),
  ]

# Adding carbon and biomass in g
Vildetrees$Carbon.g.tree<- Vildetrees$Carbon.kg.per.tree*1000
Vildetrees$Biomass.g.tree <- Vildetrees$Biomass.kg.per.tree*1000

Vildetrees$Block.ID<-as.factor(with(Vildetrees, paste(area, block, sep="_")))

Vildetrees$Block.ID<- factor(Vildetrees$Block.ID, levels = c("Makao_1","Makao_2","Makao_3","Makao_4","Maswa_1","Maswa_2","Maswa_3","Maswa_4","Mwantimba_1","Mwantimba_2","Mwantimba_3","Mwantimba_4","Handajega_1","Handajega_2","Handajega_3","Handajega_4","Seronera_1","Seronera_2","Seronera_3","Park Nyigoti_1","Park Nyigoti_2","Park Nyigoti_3","Park Nyigoti_4","Ikorongo_1","Ikorongo_2","Ikorongo_3","Ikorongo_4"))

Vildetrees$Block.ID<-as.factor(as.numeric(Vildetrees$Block.ID))
summary(levels(Vildetrees$Block.ID))

write.csv(Vildetrees,file="Ecosystem carbon/Tree.data/Vildetrees.csv")

#### AGGREGATE PER BLOCK #### 
# Aggregate carbon per block

Vildetrees <- read.csv(file="Ecosystem carbon/Tree.data/Vildetrees.csv", head=T)
names(Vildetrees)

Vildetrees$area<- factor(Vildetrees$area, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera", "Park Nyigoti","Ikorongo"))
#Vildetrees <- Vildetrees[(-78),] remove outlier?? 

# adding N and non.N
Non.N <- Vildetrees %>%
  filter(N.non=="non")
N <- Vildetrees %>%
  filter(N.non=="N")
Non.Nit <- aggregate(N.non~block+area,Non.N,length)
Nit <- aggregate(N.non~block+area,N,length)
Tree.carbon <- cbind((aggregate(area.m2~block+area,Vildetrees,mean)),
                     (aggregate(Carbon.g.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(Carbon.kg.per.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(number~block+area, Vildetrees,length))[3],
                      (aggregate(Biomass.g.tree~block+area, Vildetrees,sum))[3],
                      (aggregate(Biomass.kg.per.tree~block+area, Vildetrees,sum))[3],
                     (aggregate(Biomass.kg.per.tree~block+area, Vildetrees,median))[3],
                      (aggregate(total.basal.area.m2~block+area, Vildetrees,sum))[3],
                      (aggregate(annual.precip.mm2015_2017~block+area,Vildetrees,mean))[3],
                      (aggregate(Fire.freq~block+area,Vildetrees,mean))[3],
                      (aggregate(Year.of.last.fire~block+area,Vildetrees,mean))[3])
colnames(Tree.carbon)[9] <- "Median.Biomass.kg.per.tree"
Tree.carbon <- left_join(Tree.carbon,Nit, by=c("block","area"),drop=F)
Tree.carbon <- left_join(Tree.carbon,Non.Nit, by=c("block","area"),drop=F)

# creating a dataset at block size # WHY STILL WRONG ORDER OF REGIONS?? 
colnames(Tree.carbon) <- c("Philipo.Block","Region","Block.area_m2","TreeC.g_block","TreeC.kg_block","No.trees","TreeBM.g_block","TreeBM.kg_block","Median.TreeBM.kg_block","Total.basal.area_m2", "MAP.mm_yr","Fire_frequency.2000_2017", "Last.fire_yr","N","Non.N")

# Adding a collumn of carbon per m2, and no of trees per m2 
Tree.carbon$TreeC.kg_m2 <- Tree.carbon$TreeC.kg_block/Tree.carbon$Block.area_m2
Tree.carbon$TreeBM.kg_m2 <- Tree.carbon$TreeBM.kg_block/Tree.carbon$Block.area_m2
# Tree.carbon$Median.TreeBM_m2 <- Tree.carbon$Median.Tree.BM_g/Tree.carbon$Block_area.m2
Tree.carbon$No.trees_m2 <- Tree.carbon$No.trees/Tree.carbon$Block.area_m2
#Tree.carbon$Woody.cover <- Tree.carbon$No_trees * Tree.carbon$Median.Tree.BM_g
#Tree.carbon$Woody.cover_m2 <- Tree.carbon$Woody.cover/Tree.carbon$Block_area.m2
Tree.carbon$Shrubbiness <- as.numeric(Tree.carbon$No.trees)/as.numeric(Tree.carbon$Median.TreeBM.kg_block)
str(Tree.carbon)
levels(Tree.carbon$Region)

# Missing one row for Seronera - want to add this as a NA row. 
New.row <- c(4,"Seronera",2500,NA,NA,NA,NA,NA,NA,NA,855.6199,NA,NA,NA,NA,NA,NA,NA,NA)
Tree.carbon <- InsertRow(Tree.carbon,New.row,20)

# adding a collumn of landuse
Tree.carbon$landuse <- as.factor(c("Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild"))

# Make a collumn for my Block.id
Tree.carbon$Vilde.block <- as.numeric(c(3,1,2,4,3,4,1,2,1,4,3,2,3,4,1,2,1,2,3,4,1,2,3,4,1,2,3,4))
names(Tree.carbon)

Tree.carbon.Vilde <- Tree.carbon[,c(2,21,20,3,11:13,4:10,14:19)]
names(Tree.carbon.Vilde)
# Order the dataset so my block id is increasing
Tree.carbon.Vilde <- Tree.carbon.Vilde[
  order(Tree.carbon.Vilde[,1], Tree.carbon.Vilde[,2] ),
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
names(Tree.carbon.Vilde)

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

#### 3. Exploring Tree data #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Vildetrees <- read.csv(file="Ecosystem Carbon/Tree.data/Vildetrees.csv",head=T)
# Relevel before making plots
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera", "Park Nyigoti","Ikorongo"))

Vildetrees$area<- factor(Vildetrees$area, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera", "Park Nyigoti","Ikorongo"))

# Tree carbon vs LANDUSE
plot(No.trees_m2~landuse,
     xlab = "Land Use",
     ylab = "Number of Trees",
     data=Tree.carbon)

plot(TreeC.kg_m2~landuse,
     xlab = "Land Use",
     ylab = "",
     data=Tree.carbon)

plot(N~landuse, data=Tree.carbon)
plot(Non.N~landuse, data=Tree.carbon)

# Exploring the distribution of trees
hist(Tree.carbon$TreeBM.kg_m2)

dotchart(Vildetrees$Biomass.kg.per.tree,groups=Vildetrees$area,main = "area") # Maswa and Handajega have big trees
dotchart(Vildetrees$Biomass.kg.per.tree,groups=Vildetrees$landuse,main = "landuse") # wild have bigger trees than pasture. 


# Looking for outliars 
max(Vildetrees$Biomass.kg.per.tree, na.rm=T) # One gigant tree in Handajega... 2813.959 kg 
max(Vildetrees$total.basal.area.m2, na.rm=T)
plot(Vildetrees$Biomass.kg.per.tree~Vildetrees$Block.ID)
identify(Vildetrees$Biomass.kg.per.tree~Vildetrees$Block.ID) # row number 78 

dim(Vildetrees) #166 trees
names(Vildetrees)

# Dividing into small and large trees 

AllTrees<- Vildetrees %>%
  select(Biomass.kg.per.tree,area,block,Block.ID,N.non) %>%
  group_by(area,block,Block.ID,N.non) %>%
  tally()

AllTrees <- as.data.frame(AllTrees)
ID <- AllTrees[,c(1:4)]

# Select by biomass < 2 kg, aggregate N and non N tree biomass per block 
SmallTreesBM <- Vildetrees %>%
  filter(Biomass.kg.per.tree<=2) %>%
  select(Biomass.kg.per.tree,area,block,Block.ID,area.m2,N.non) %>%
  group_by(area,block,Block.ID,N.non) 

SmallTreesBM <- as.data.frame(SmallTreesBM)

LargeTreesBM <- Vildetrees %>%
  filter(Biomass.kg.per.tree>2) %>%
  select(Biomass.kg.per.tree,area,block,Block.ID,area.m2,N.non) %>%
  group_by(area,block,Block.ID,N.non) 

LargeTreesBM <- as.data.frame(LargeTreesBM)

LargeTrees.N <- LargeTreesBM %>%
  filter(N.non=="N")
LargeTrees.N <- aggregate(Biomass.kg.per.tree~area+block+Block.ID+area.m2,sum,data=LargeTrees.N)

LargeTrees.non <- LargeTreesBM %>%
  filter(N.non=="non")
LargeTrees.non <- aggregate(Biomass.kg.per.tree~area+block+Block.ID+area.m2,sum,data=LargeTrees.non)

SmallTrees.N <- SmallTreesBM %>%
  filter(N.non=="N")
SmallTrees.N <- aggregate(Biomass.kg.per.tree~area+block+Block.ID+area.m2,sum,data=SmallTrees.N)

SmallTrees.non <- SmallTreesBM %>%
  filter(N.non=="non")
SmallTrees.non <- aggregate(Biomass.kg.per.tree~area+block+Block.ID+area.m2,sum,data=SmallTrees.non)

ID.block <- Tree.carbon[,c(2,3,4,20)]
colnames(ID.block) <- c("Region","Block","landuse","Block.ID")

#Joining tables: 
LargeTrees.N.BM <- left_join(ID.block,LargeTrees.N,by="Block.ID",drop=F)
colnames(LargeTrees.N.BM)[8] <- c("BM.Large.N")
LargeTrees.N.BM$BM.Large.N.m2 <- LargeTrees.N.BM$BM.Large.N/LargeTrees.N.BM$area.m2

LargeTrees.non.BM<- left_join(ID.block,LargeTrees.non,by="Block.ID",drop=F)
colnames(LargeTrees.non.BM)[8] <- c("BM.Large.non")
LargeTrees.non.BM$BM.Large.non.m2 <- LargeTrees.non.BM$BM.Large.non/LargeTrees.non.BM$area.m2

SmallTrees.N.BM<- left_join(ID.block,SmallTrees.N,by="Block.ID",drop=F)
colnames(SmallTrees.N.BM)[8] <- c("BM.Small.N")
SmallTrees.N.BM$BM.Small.N.m2 <- SmallTrees.N.BM$BM.Small.N/SmallTrees.N.BM$area.m2

SmallTrees.non.BM<- left_join(ID.block,SmallTrees.non,by="Block.ID",drop=F)
colnames(SmallTrees.non.BM)[8] <- c("BM.Small.non")
SmallTrees.non.BM$BM.Small.non.m2 <- SmallTrees.non.BM$BM.Small.non/SmallTrees.non.BM$area.m2

Tree.BM.N.non <- cbind(LargeTrees.N.BM[c(1:4,9)],LargeTrees.non.BM[9],SmallTrees.N.BM[9],SmallTrees.non.BM[9])

write.csv(Tree.BM.N.non,file="Ecosystem carbon/Tree.data/Tree.BM.N.non.csv")

names(Tree.BM.N.non)
plot((Tree.BM.N.non$BM.Large.N.m2+Tree.BM.N.non$BM.Small.N.m2)~Tree.BM.N.non$landuse)
plot(Tree.BM.N.non$BM.Large.N.m2~Tree.BM.N.non$landuse)
plot(Tree.BM.N.non$BM.Small.N.m2~Tree.BM.N.non$landuse)
# Df with number of small and number of large 
SmallTreesNo <- Vildetrees %>%
  filter(Biomass.kg.per.tree<=2) %>%
  select(Biomass.kg.per.tree,area,block,Block.ID,N.non) %>%
  group_by(area,block,Block.ID,N.non) %>%
  tally()

SmallTreesNo <- as.data.frame(SmallTreesNo)

LargeTreesNo <- Vildetrees %>%
  filter(Biomass.kg.per.tree>2) %>%
  select(Biomass.kg.per.tree,area,block,Block.ID,N.non) %>%
  group_by(area,block,Block.ID,N.non) %>%
  tally()

LargeTreesNo <- as.data.frame(LargeTreesNo)

SmallTreesNo <- full_join(ID,SmallTreesNo)
LargeTreesNo<- full_join(ID,LargeTreesNo)

Tree.size.no <- cbind(SmallTreesNo,LargeTreesNo[,c(5)])
names(Tree.size.no)
colnames(Tree.size.no) <- c("area", "block", "Block.ID", "N.non","small","large")  


# Make a new long data set based on mean tree BM size 

Tree.size.no.long <- gather(Tree.size.no,Size,Count, small:large,factor_key=TRUE)

Tree.size.no.long <- Tree.size.no.long[
  order(Tree.size.no.long[,1], Tree.size.no.long[,2] ),
  ]

New.row1 <- c("Seronera",4,NA,"N","small",NA)
New.row2 <- c("Seronera",4,NA,"non","small",NA)
New.row3 <- c("Seronera",4,NA,"N","large",NA)
New.row4 <- c("Seronera",4,NA,"non","large",NA)

Tree.size.no.long <- InsertRow(Tree.size.no.long,New.row1,36)
Tree.size.no.long <- InsertRow(Tree.size.no.long,New.row2,37)
Tree.size.no.long <- InsertRow(Tree.size.no.long,New.row3,87)
Tree.size.no.long <- InsertRow(Tree.size.no.long,New.row4,88)

write.csv(Tree.size.no.long,file="Ecosystem carbon/Tree.data/Tree.size.csv")

# Density distribution tree size from Philipo

#########################################################################
# Tree biomass historgraph graph
#########################################################################
names(Vildetrees)

# Group means
grp.mean<-aggregate(log(Biomass.kg.per.tree+1)~landuse+area,data=Vildetrees,mean)
colnames(grp.mean)[3]<-"log.Biomass.kg.per.tree"
Vildetrees$log.Biomass.kg.per.tree<-log(Vildetrees$Biomass.kg.per.tree+1)
max(Vildetrees$Biomass.kg.per.tree)
log(2813.959+1) # ~ 8 
log(2+1) # 1.098612 = cut of for small trees

# Main graph tree biomass density 

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

Tree.biomass <-ggplot()
Tree.biomass + geom_density(data=Vildetrees, aes(log.Biomass.kg.per.tree, fill = landuse,colour =landuse),alpha=0.4) +
  facet_wrap(~area)+
  scale_fill_manual("Land-use",values=c("darkorange","chartreuse4"))+
  scale_colour_manual("Land-use",values=c("darkorange","chartreuse4"))+
  scale_x_continuous(expand=c(0,0), limits = c(0, 8))+
  scale_y_continuous(labels = c(0,5,10,15,20,25), breaks = c(0,.5,1.0,1.5,2.0,2.5), limits = c(0, 2.5), expand=c(0,0))+
  geom_vline(data=grp.mean, aes(xintercept=log.Biomass.kg.per.tree,colour = landuse,linetype = landuse),size=.75)+
  scale_linetype_manual("Land-use",values = c(wild = "solid", pasture = "dashed"))+ 
  xlab("Log tree biomass (kg)") +  ylab("Density (%)")+ 
  theme_bw() + 
  Lines_gone

ggsave("Ecosystem carbon/Figures/Log.treeBM.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#### 4. Plotting Tree data ####

# Density plot (%)
# Per Region BM - density plot 
BM.tree.plot <- ggplot(data= Vildetrees)

BM.tree.plot + geom_density(aes(x=Biomass.kg.per.tree, fill = landuse,colour =landuse),alpha=0.4) +
  facet_wrap(~area) +
  scale_fill_manual("Land-use",values=c("darkorange","chartreuse4"))+
  scale_colour_manual("Land-use",values=c("darkorange","chartreuse4"))+
  scale_x_continuous(expand=c(0,0), limits = c(0, 4))+
  scale_y_continuous(labels = c(0,5,10,15,20,25,30), breaks = c(0,.5,1.0,1.5,2.0,2.5,3.0), limits = c(0, 2.5), expand=c(0,0)) + 
  xlab("Tree biomass (kg)") +  ylab("Density (%)") + 
  theme_bw() +
  Lines_gone 

ggsave("Ecosystem carbon/Figures/TreeBM.dist.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Per Region Basal area - frequency plot 
Basal.area.tree.plot <- ggplot(data= Vildetrees)

Basal.area.tree.plot + geom_freqpoly(aes(x=total.basal.area.m2,colour=landuse),bins=30,size=1) +
  facet_wrap(~area) +
  scale_fill_manual("Land-use",values=c("darkorange","chartreuse4"))+
  scale_colour_manual("Land-use",values=c("darkorange", "chartreuse4")) + 
  xlab(expression(paste("Tree basal area (", m^-2,")"))) +  ylab("Frequency") + 
  theme_bw() +
  Lines_gone 

ggsave("Ecosystem carbon/Figures/TreeBasalArea.Freq.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# PLOTTING
# Plotting the size distribution with small and large trees 

legent_titleSIZE <- "Tree size"

# DENSITY PLOT 
size.plot.density <- ggplot(Tree.size.long, aes(x=Count, fill= Size))
size.plot.density + geom_density() 

#  Want to look at all regions at the same time
Tree.size.region <- Tree.size.long %>%
  select(area,Size,Count) %>%
  group_by(area,Size) %>%
  tally(Count)

Size.plot <- ggplot(data=Tree.size.region, aes(x=area, y=n, colour= Size))

Size.plot +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)+
  scale_colour_manual(legent_titleSIZE,breaks= c("large","small"),values=c("darkorange","chartreuse4"))+
  xlab("Region") +  ylab("Count") + 
  theme_bw() +
  Lines_gone

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






