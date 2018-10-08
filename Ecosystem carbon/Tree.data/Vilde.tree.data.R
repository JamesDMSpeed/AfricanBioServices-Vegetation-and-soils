# Sorting tree data to what I need to use 

rm(list=ls())

# packages 
#library(lattice)
#library(MASS)
library(dplyr)
#library(plyr)
#library(lubridate)
#library(data.table)
#library(xlsx)
#library(ggplot2)

# Data
Philtrees<-read.csv(file="Ecosystem carbon/Tree.data/Tree.data.Seregenti.PhilipoBio.csv", sep=",",header=TRUE)
names(Philtrees)
levels(Philtrees$area)

# Removing branch, and removing areas I did not visit 
# - find a way to do this in one process??
Philtrees2<-Philtrees[Philtrees$tree.part!="branch",]
Philtrees3 <- Philtrees2[Philtrees2$area!="MakaoWMA",]
Philtrees3 <- Philtrees3[Philtrees3$area!="Ololosokwan",]
Philtrees4 <- Philtrees3[Philtrees3$area!="SNP kleins gate",]

Philtrees4<-droplevels(Philtrees4)

levels(Philtrees4$area)

# Reducing the data - removing columns I dont need
Philtrees.reduced <- Philtrees4[,c(1:7,28,31:33)]

# Rearanging my data in the order I visited the sites: 
Philtrees.reduced$area<- factor(Philtrees.reduced$area, levels = c("Makao","Maswa","Mwantimba","SNP handejega","Seronera", "Park Nyigoti","Ikorongo"))

levels(Philtrees.reduced$area) # Releveled

# Aggregate carbon per block
names(Philtrees.reduced)

Carbon.per.block<-aggregate(Carbon.kg.per.tree~area+block, Philtrees.reduced,sum)
Carbon.per.block.sd <- aggregate(Carbon.kg.per.tree~area+block, Philtrees.reduced,sd)
trees.per.block <- aggregate(number~area+block, Philtrees.reduced,length)

# creating a table 
Tree.carbon <- cbind(Carbon.per.block,Carbon.per.block.sd[3],trees.per.block[3])

# write.csv(Tree.carbon,file="Tree.Carbon.csv")


  







