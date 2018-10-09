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

# creating a table 
Tree.carbon <- cbind(Block.size,Block[3],Carbon.per.block[3],Trees.per.block[3])
colnames(Tree.carbon) <- c("Region","Block.id","Block.area.m2","Block","Tree.carbon.block.kg","No.trees.block")

# Adding a collumn of carbon per m2 
Tree.carbon$carbon.per.m2 <- Tree.carbon$Tree.carbon.block.kg/Tree.carbon$Block.area.m2

# write.csv(Tree.carbon,file="Tree.Carbon.csv")

#### 2. Exploring the data #### 
par(mfrow=c(1,2))

boxplot(carbon.per.m2 ~ Region, 
        xlab = "Region",
        ylab = "Carbon_m2",
        data = Tree.carbon)
  
boxplot(No.trees.block ~ Region, 
        xlab = "Region",
        ylab = "#Trees",
        data = Tree.carbon)






