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
Vildetrees <- Philtrees4[,c(1:7,9,28,31:33)]

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
Trees.per.block <- aggregate(number~area+block.id, Vildetrees,length)
Block.size<-aggregate(area.m2~area+block.id,Vildetrees,mean)
Block <- aggregate(block~area+block.id,Vildetrees,mean)
MAP.mm_2015_2017 <- aggregate(annual.precip.mm2015_2017~area+block.id,Vildetrees,mean)
Fire.freq <- aggregate(Fire.freq~area+block.id,Vildetrees,mean)
Year.of.last.fire <- aggregate(Year.of.last.fire~area+block.id,Vildetrees,mean)

# creating a dataset at block size # WHY STILL WRONG ORDER OF REGIONS?? 
Tree.carbon <- cbind(Block.size,Block[3],Carbon.per.block[3],Trees.per.block[3],MAP.mm_2015_2017[3],Fire.freq[3],Year.of.last.fire[3])
colnames(Tree.carbon) <- c("Region","Block.id","Block_area.m2","Philipo.Block","Tree_C.kg_block","No_trees", "MAP.mm_yr","Fire_frequency.2000_2017", "Last_fire.yr")

# Adding a collumn of carbon per m2 
Tree.carbon$carbon.m2 <- Tree.carbon$Tree_C.kg_block/Tree.carbon$Block_area.m2
names(Tree.carbon)
levels(Tree.carbon$Region)

# adding a collumn of landuse
landuse <- c("Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild")
Tree.carbon$landuse <- landuse

# Make a collumn for my Block.id
Vilde.block <- c(1,2,3,4,3,1,2,4,3,4,1,2,1,4,3,2,1,2,3,4,1,2,3,3,4,1,2)
Tree.carbon$Vilde.block <- Vilde.block
Tree.carbon.Vilde <- Tree.carbon[,c(1,2,3,12,4,11,5,10,6,7,8,9)]

write.csv(Tree.carbon.Vilde,file="Tree.Carbon.Vilde.csv") # for further use 

#### 2. Make a table at "region level" for my method ####

rm(list=ls())

Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
levels(Tree.carbon$Region)# Remember wrong order of regions.. 
names(Tree.carbon)

#Relevel
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Tree.carbon$Region) # Releveled

# Making a table for Carbon per region 
Carbon.per.block<-aggregate(Tree_C.kg_block~Region, Tree.carbon,sum)
No.trees <- aggregate(No_trees~Region, Tree.carbon,sum)
Region.area_m2<-aggregate(Block_area.m2~Region,Tree.carbon,sum)
MAP <- aggregate(MAP.mm_yr~Region,Tree.carbon,mean)
MAP.sd <- aggregate(MAP.mm_yr~Region,Tree.carbon,sd)
Fire.frequency <- aggregate(Fire_frequency.2000_2017~Region,Tree.carbon,mean)
Fire.frequency.sd <- aggregate(Fire_frequency.2000_2017~Region,Tree.carbon,sd)
Year.of.last.fire <- aggregate(Last_fire.yr~Region,Tree.carbon,mean)

Tree.carbon.Region <- cbind(Region.area_m2,MAP[2],MAP.sd[2],Fire.frequency[2],Fire.frequency.sd[2],Year.of.last.fire[2],Carbon.per.block[2],No.trees[2])

colnames(Tree.carbon.Region) <- c("Region","Area.m2","MAP.mm_yr","MAP.sd","Fire_frequency.2000_2017","Fire_frequency.sd","Last_fire.yr","Tree_C.kg", "No_trees")

Tree.carbon.Region$Carbon.m2 <- Tree.carbon.Region$Tree_C.kg/Tree.carbon.Region$Area.m2
Tree.carbon.Region <- Tree.carbon.Region[,c(1,2,3,4,5,6,7,8,10,9)]

# write.csv(Tree.carbon.Region, file="Tree.carbon.Region.csv")

#### 3. Exploring the data #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)

par(mfrow=c(1,2))

# C and no. trees per Region
boxplot(carbon.m2 ~ Region, 
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

plot(carbon.m2~landuse,
     xlab = "Land Use",
     ylab = "Carbon per m2",
     data=Tree.carbon)

# Carbon per tree in pasture vs wild  
par(mfrow=c(1,1))
plot (Tree_C.kg_block/No_trees~landuse,
       xlab = "Land Use",
       ylab = "Carbon per tree",
       data=Tree.carbon)

# No of trees vs BD 
total.soil.data<- read.csv("Ecosystem Carbon/Total.soil.data.csv", head = TRUE)
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





