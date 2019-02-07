#### Exploring data on block level #### 
Tree.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.Carbon.Vilde.csv", head=T)
Herbaceous.carbon <- read.csv(file="Ecosystem Carbon/Herbaceous.data/12Herbaceous.csv", head=T)
Deadwood.carbon <- read.csv(file="Ecosystem Carbon/Tree.data/DW.Block.csv",head=T)
Soil.C <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.Carbon.Block.csv", head=T)
Soil.texture <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.texture.Min_Hor.csv",head=T)
Tree.size <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.size.csv",head=T)
Belowground <- read.csv(file="Ecosystem Carbon/Soil.data/Belowground.Carbon.csv",head=T)
Tree.BM.N.non <- read.csv(file="Ecosystem Carbon/Tree.data/Tree.BM.N.non.csv",head=T)

library(tidyr)
library(plyr)
library(dplyr)

# Fixing the data for further processing 
# Soil texture 
levels(Soil.texture$Region)
Soil.texture$Region<- factor(Soil.texture$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Soil.texture <- Soil.texture[,c(1:8)]
Soil.texture <- droplevels(Soil.texture)

Soil.texture$Class <- c("SaClLo","SaLo","SaClLo","SaClLo","Cl","Cl","Cl","ClLo","Cl","SaCl","ClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaLo","SaLo","SaClLo","SaClLo","SaClLo","ClLo","Cl","ClLo","Cl","Cl","ClLo","ClLo","Cl") 

# Tree data
Tree.carbon$Region <- as.character(Tree.carbon$Region)

Tree.size.small <- Tree.size %>%
  filter(Size== "small")

Tree.size.large <- Tree.size %>%
  filter(Size=="large")

Tree.size.small.N <- Tree.size.small %>%
  filter(N.non== "N")

Tree.size.small.non <- Tree.size.small %>%
  filter(N.non== "non")

Tree.size.large.N <- Tree.size.large %>%
  filter(N.non=="N")

Tree.size.large.non <- Tree.size.large %>%
  filter(N.non=="non")

ID <- Soil.texture[,c(1,3)]
colnames(ID) <- c("area","block")
Tree.size.small.N <- full_join(ID,Tree.size.small.N)
Tree.size.small.non <- full_join(ID,Tree.size.small.non)
Tree.size.large.N <- full_join(ID,Tree.size.large.N)
Tree.size.large.non <- full_join(ID,Tree.size.large.non)

Tree.size.S <- merge(Tree.size.small.N[,c(1,2,7)],Tree.size.small.non[,c(1,2,7)],all.x = TRUE,by=c("area","block"))
colnames(Tree.size.S) <- c("Region","Block","Small.N","Small.N.non")
Tree.size.L <- merge(Tree.size.large.N[,c(1,2,7)],Tree.size.large.non[,c(1,2,7)],all.x = TRUE,by=c("area","block"))
colnames(Tree.size.L) <- c("Region","Block","Large.N","Large.N.non")
Tree.size <- cbind(Tree.size.S,Tree.size.L[c(3,4)])
Tree.size$Region<- factor(Tree.size$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Tree.size <- Tree.size[
  order(Tree.size[,1],Tree.size[,2]),
  ]

Tree.size$Block.ID <- as.numeric(1:28)
Trees.N.non <- cbind(Tree.size,Tree.BM.N.non[c(6:9)])

# Soil data
SoilAHor.carbon <- Soil.C %>%
  filter(Horizon== "A-hor")

SoilMinHor.carbon <- Soil.C %>%
  filter(Horizon=="Min-hor")

colnames(Herbaceous.carbon)[colnames(Herbaceous.carbon) == "C.kg_m2"] <- "HerbC.kg_m2"
colnames(SoilAHor.carbon)[colnames(SoilAHor.carbon) == "C.kg_m2"] <- "SoilAC.kg_m2"
colnames(SoilAHor.carbon)[colnames(SoilAHor.carbon) == "SE.C.kg_m2"] <- "SE.SoilAC.kg_m2"
colnames(SoilMinHor.carbon)[colnames(SoilMinHor.carbon) == "C.kg_m2"] <- "SoilMC.kg_m2"
colnames(SoilMinHor.carbon)[colnames(SoilMinHor.carbon) == "SE.C.kg_m2"] <- "SE.SoilMC.kg_m2"

names(SoilAHor.carbon)
Soil.carbon <- merge(SoilAHor.carbon[,c(2,3,5,13,6,11:12,7,8)],SoilMinHor.carbon[,c(5,7,8)],all.x = TRUE,by="Block.ID")

Nitrogen <- aggregate(mean.N.kg_m2~Region + Block, mean,data=Belowground)
Soil.carbon <- merge(Soil.carbon,Nitrogen[,c(1:3)],all.x = TRUE,by=c("Region","Block"))

Soil.carbon <- Soil.carbon[
  order(Soil.carbon[,3]),
  ]

#Relevel 
Tree.carbon$Region<- factor(Tree.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous.carbon$Region<- factor(Herbaceous.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Deadwood.carbon$Region<- factor(Deadwood.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil.carbon$Region<- factor(Soil.carbon$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Trees.N.non$Region<- factor(Tree.size$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

# Merge the datasets 
names(Soil.carbon)
names(Tree.carbon)
names(Soil.texture)
names(Deadwood.carbon)
names(Trees.N.non)
Ecosystem.Carbon1a <- merge(Tree.carbon[,c(20,2,3,4,6,8,7,14,17:19)],Herbaceous.carbon[,c(3,11)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon1b <- merge(Ecosystem.Carbon1a, Soil.texture[,c(5:9)],all.x=TRUE, by="Block.ID")
Ecosystem.Carbon1c <- merge(Ecosystem.Carbon1b,Trees.N.non[,c(1,7,3:6,8:11)],all.x=TRUE, by="Block.ID")
Ecosystem.Carbon1c <- merge(Ecosystem.Carbon1c,Soil.carbon[,c(3,12)],all.x=TRUE, by="Block.ID")

Ecosystem.Carbon2a <- merge(Soil.carbon,Deadwood.carbon[,c(4:6)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2b <- merge(Ecosystem.Carbon2a,Soil.texture[,c(5:9)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2c <- merge(Ecosystem.Carbon2b,Ecosystem.Carbon1c[,c(1,8,10,11)],all.x = TRUE,by="Block.ID")
Ecosystem.Carbon2d <- merge(Ecosystem.Carbon2c,Trees.N.non[,c(1,7,3:6,8:11)],all.x=TRUE, by="Block.ID")

names(Ecosystem.Carbon1c)
names(Ecosystem.Carbon2d)
Ecosystem.CHerbTree <- Ecosystem.Carbon1c[,c(1:7,13:16,26,8,10,11,18:25,9,12)]
Ecosystem.CSoilDW <- Ecosystem.Carbon2d[,c(1:7,15:18,12,19:21,23:30,8,10,13)]
SE.Ecosystem.CSoilDW <- Ecosystem.Carbon2d[,c(1:3,9,11,14)]

#### Creating a DF for the poster ####

# Belowground C 

Belowground.C <- Ecosystem.CSoilDW
Belowground.C$MAP <- round(Belowground.C$MAP, digits=2)

Belowground.C$MAP[Belowground.C$MAP=="717.36"] <- "736.10"
Belowground.C$MAP[Belowground.C$MAP=="754.84"] <- "736.10"
Belowground.C$MAP[Belowground.C$MAP=="736.10"] <- "704.07"
Belowground.C$MAP[Belowground.C$MAP=="672.04"] <- "704.07"
Belowground.C$MAP[Belowground.C$MAP=="1295.06"] <- "1287.16"
Belowground.C$MAP[Belowground.C$MAP=="1279.26"] <- "1287.16"

Deadwood.C <- Belowground.C[,c(1:5,19)]

Belowground.C$BelowgroundC <- Belowground.C$SoilAC.kg_m2 + Belowground.C$SoilMC.kg_m2
# Checking for significans of Clay, MAP and Lanuse 
summary(lm(BelowgroundC~Clay.pip.per, data=Belowground.C))
summary(lm(BelowgroundC~MAP:Clay.pip.per, data=Belowground.C))
summary(lm(BelowgroundC~Landuse:Clay.pip.per, data=Belowground.C))

SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BelowgroundC <- cbind(aggregate(BelowgroundC~Region+MAP+Landuse,mean,data=Belowground.C),
                      aggregate(BelowgroundC~Region+MAP+Landuse,SE,data=Belowground.C)[4])
colnames(BelowgroundC) <- c("Region","MAP.mm_yr","landuse","BelowgroundC","SE.BelowgroundC")

# Abovground C 
Aboveground.C <- Ecosystem.CHerbTree
names(Aboveground.C)
Aboveground.C$MAP.mm_yr <- round(Aboveground.C$MAP.mm_yr, digits=2)

Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="717.36"] <- "736.10"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="754.84"] <- "736.10"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="736.10"] <- "704.07"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="672.04"] <- "704.07"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="1295.06"] <- "1287.16"
Aboveground.C$MAP.mm_yr[Aboveground.C$MAP.mm_yr=="1279.26"] <- "1287.16"

# Creating a new collumn with all of aboveground C 
Aboveground.C$AbovegroundC <- rowSums(Aboveground.C[,c("HerbC.kg_m2", "TreeC.kg_m2")], na.rm=TRUE)
Aboveground.C$AbovegroundC <- Aboveground.C$AbovegroundC + Deadwood.C$DWC.kg_m2

AbovegroundC <- cbind(aggregate(AbovegroundC~Region+MAP.mm_yr+landuse,mean,data=Aboveground.C),
                          aggregate(AbovegroundC~Region+MAP.mm_yr+landuse,SE,data=Aboveground.C)[4])
colnames(AbovegroundC) <- c("Region","MAP.mm_yr","landuse","AbovegroundC","SE.AbovegroundC")

Above_below.C <- cbind(AbovegroundC,BelowgroundC[,c(4,5)])

AboveBelow <- Above_below.C[,c(1:4,6)]
SE.AboveBelow <- Above_below.C[,c(1:3,5,7)]

AboveBelow <- gather(AboveBelow, Carbon.pool,C.amount, AbovegroundC,BelowgroundC,factor_key=TRUE)
SE.AboveBelow <- gather(SE.AboveBelow, Carbon.pool,SE.C.amount, SE.AbovegroundC,SE.BelowgroundC,factor_key=TRUE)
AboveBelow <- cbind(AboveBelow,SE.AboveBelow[5])

write.csv(AboveBelow,file="Ecosystem carbon/Above_Below.C.csv")

#### Make the data into a long format instead of a wide ####
data_long.CTreeHerb <- gather(Ecosystem.CHerbTree, Carbon.pool,C.amount, TreeC.kg_m2:HerbC.kg_m2,factor_key=TRUE)

data_long.CSoilDW <- gather(Ecosystem.CSoilDW, Carbon.pool,C.amount, SoilAC.kg_m2:DWC.kg_m2,factor_key=TRUE)

SE.data_long.CSoilDW <- gather(SE.Ecosystem.CSoilDW, Carbon.pool,C.amount, SE.SoilAC.kg_m2:SE.DWC.kg_m2,factor_key=TRUE)

EcosystemC.SoilDW<- cbind(data_long.CSoilDW,SE.data_long.CSoilDW[5])
names(EcosystemC.SoilDW)
names(data_long.CTreeHerb)
colnames(EcosystemC.SoilDW) <- c("Block.ID","Region.x","Vilde.block","landuse","MAP.mm_yr","Last.fire_yr","Fire_frequency.2000_2017","Clay.pip.per","Silt.pip.per","Sand.pip.per","Class","mean.N.kg_m2","Total.basal.area_m2","TreeBM.kg_m2", "No.trees_m2","Small.N","Small.N.non","Large.N","Large.N.non" , "BM.Large.N.m2", "BM.Large.non.m2","BM.Small.N.m2","BM.Small.non.m2","Carbon.pool","C.amount","SE.C.amount")

EcosystemC.SoilDW <- EcosystemC.SoilDW[
  order(EcosystemC.SoilDW[,1], EcosystemC.SoilDW[,2] ),
  ]

data_long.CTreeHerb <- data_long.CTreeHerb[
  order(data_long.CTreeHerb[,1], data_long.CTreeHerb[,2] ),
  ]

# Creating one long dataset with all data
data_long.CTreeHerb$SE.C.amount <- c(NA*56)
names(data_long.CTreeHerb)
names(EcosystemC.SoilDW)
Ecosystem.Carbon <- bind_rows(data_long.CTreeHerb, EcosystemC.SoilDW, id=NULL)

Ecosystem.Carbon <- Ecosystem.Carbon[
  order(Ecosystem.Carbon[,1], Ecosystem.Carbon[,2] ),
  ]

write.csv(Ecosystem.Carbon,file="Ecosystem carbon/Ecosystem.Carbon.csv")
write.csv(EcosystemC.SoilDW,file="Ecosystem carbon/Soil.DW.Carbon.Block.csv")
write.csv(data_long.CTreeHerb,file="Ecosystem carbon/Tree.Herb.Carbon.Block.csv")

### Exploring the data at Region level ####
Herbaceous <- read.csv(file="Ecosystem carbon/HerbC.Region.csv", header=T)
Woody <- read.csv(file="Ecosystem carbon/Tree.data/TreeC.Region.csv", header=T)
Deadwood <- read.csv(file="Ecosystem carbon/Tree.data/DW.Region.csv",header=T)
Soil <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.Region.csv",header=T)

Woody$Region <- as.factor(c("Makao","Maswa", "Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
names(Woody)
Woody <- Woody[,c(1,2,14,11:13,3:10)]

#Relevel 
Woody$Region<- factor(Woody$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil$Region<- factor(Soil$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

# Merge the three datasets in two steps 
Ecosystem.C1 <- merge(Woody[,c(2:6,7:14)],Herbaceous[,c(2:4)],all.x = TRUE,by="Region")
Ecosystem.C2 <- merge(Ecosystem.C1,Deadwood[2:4],all.x = TRUE,by="Region")
Ecosystem.C <- merge(Ecosystem.C2,Soil[2:16],all.x = TRUE,by="Region")

EcosystemCarbon <- Ecosystem.C[,c(1:5,18,8,10,12,19,21,23,6,14,16,25,27)]
EcosystemCarbonSE <- Ecosystem.C[,c(1,18,7,15,17,26,28)]

# Make the data into a long format instead of a wide
library(tidyr)
library(plyr)

data_long.C <- gather(EcosystemCarbon, Carbon.pool,C.amount, TreeC.kg_m2:CMinHor,factor_key=TRUE)
data_long.CSE <- gather(EcosystemCarbonSE, Carbon.poolSE,C.amountSE, SE.TreeC.kg_m2:SE.CMinHor,factor_key=TRUE)

Tot.EcosystemCarbon <- cbind(data_long.C,data_long.CSE[3])

Tot.EcosystemCarbon <- arrange(Tot.EcosystemCarbon,Region)
colnames(Tot.EcosystemCarbon)[colnames(Tot.EcosystemCarbon) == "Landuse.x"] <- "Landuse"
Tot.EcosystemCarbon$Climate <- as.factor(c("Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Dry","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Wet","Int-Dry","Int-Dry","Int-Dry","Int-Dry","Int-Dry","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet","Int-Wet"))

write.csv(Tot.EcosystemCarbon,file="Ecosystem carbon/Tot.Ecosystem.Carbon.Region.csv")

### Ploting Ecosystem Carbon  #### 

# Fixing my belowground variable 
# Soil.min <- Block.Eco.C %>%
#   filter(Carbon.pool=="Soil Min-horizon")
# Soil.A <- Block.Eco.C %>%
#   filter(Carbon.pool=="Soil A-horizon")
# Belowground.block <- cbind(Soil.min,Soil.A[26])
# colnames(Belowground.block)[29] <- "C.Ahor"
# Belowground.block$tot.C.kg_m2 <- Belowground.block$C.amount + Belowground.block$C.Ahor

# Belowground.full <- left_join(Belowground,Soil.min,by="Block.ID",drop=F)
# names(Belowground.full)
# Belowground.full <- Belowground.full[,c(2:10,28,16,14,15,20:24,32:47)]
# Belowground.full$N.trees <- rowSums(Belowground.full[,c("Small.N", "Large.N")], na.rm=TRUE)
# Belowground.full$BM.N.trees.m2 <- rowSums(Belowground.full[,c("BM.Small.N.m2", "BM.Large.N.m2")], na.rm=TRUE)
# Belowground.full$non.N.trees <- rowSums(Belowground.full[,c("Small.N.non", "Large.N.non")], na.rm=TRUE)
# Belowground.full$BM.non.N.trees.m2 <- rowSums(Belowground.full[,c("BM.Small.non.m2", "BM.Large.non.m2")], na.rm=TRUE)

# colnames(Belowground.full)[10] <- "MAP.mm_yr"
# colnames(Belowground.full)[6] <- "landuse"

# Belowground.full$fMAP <- as.factor(Belowground.full$MAP.mm_yr)
# Belowground.block$fMAP <- as.factor(Belowground.block$MAP.mm_yr)
# write.csv(Belowground.full,file="Ecosystem Carbon/Soil.data/Belowground.Carbon.csv")

library(ggplot2)
library(dplyr)

Region.Eco.C <- read.csv("Ecosystem carbon/Tot.Ecosystem.Carbon.Region.csv", head=T)
Block.Eco.C <- read.csv("Ecosystem carbon/Ecosystem.Carbon.csv", head=T)
Belowground.full <- read.csv("Ecosystem carbon/Soil.data/Belowground.Carbon.csv", head=T)
Above_Below.C <- read.csv("Ecosystem carbon/Above_Below.C.csv",head=T)

levels(Block.Eco.C$Carbon.pool)
levels(Block.Eco.C$Class)

# Rename the Carbon pool names 
Block.Eco.C$Carbon.pool<- factor(Block.Eco.C$Carbon.pool, levels = c("TreeC.kg_m2","HerbC.kg_m2", "DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))
Block.Eco.C$Region<- factor(Block.Eco.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")

#### Plotting for poster ####
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

# Plotting C and N 
Belowground.full$fMAP<-as.factor(Belowground.full$fMAP)
levels(Belowground.full$fMAP)
TitleRain<-"Rainfall (mm/year)"

Belowground.full$fMAP<- factor(Belowground.full$fMAP, levels = c("1295.06", "1279.26",  "1134.81", "855.62",  "754.84",  "717.36" ,"672.04"))
levels(Belowground.full$fMAP) <- c("1295", "1279",  "1135", "856",  "755",  "717" ,"672")
levels(Belowground.full$landuse) <- c("Pasture","Wild")

CN.plot <- ggplot(data = Belowground.full, aes(x = tot.N.kg_m2,y = tot.C.kg_m2,group = fMAP, colour= fMAP,shape= landuse))

CN.plot + xlab(expression(paste("Soil Nitrogen (kg", m^-2,")"))) +  ylab(expression(paste("Soil Carbon (kg", m^-2,")"))) +
  geom_point(size = 3,stroke=2, na.rm=T)+
  scale_color_manual(TitleRain,breaks = c("1295", "1279",  "1135", "856",  "755",  "717" ,"672"),values=c("#6460D9","#8495E9","#74DEDA", "#9EF5C1", "#F9EE5E","#F9EE5E","#E9F882")) + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  theme_bw() +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="white")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank() 
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.line.y = element_line(color="black", size = .5) 
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(5,5,5,5), "mm")
        ,axis.text.x = element_text(size=15,color="black")
        ,axis.text.y = element_text(size=15,color="black")
        ,axis.title.y=element_text(size=20,color="black")
        ,axis.title.x=element_text(size=20,color="black")
        ,legend.text=element_text(size=15)
        ,legend.title=element_text(size=15)
        ,legend.position = c(0.15,0.6)
        ,strip.text.x = element_text(size = 0.5, hjust=0.1,colour = "black")
        ,strip.background = element_rect(fill="transparent",colour=NA))

ggsave("Ecosystem carbon/Figures/CN.MAP.png",
       width= 20, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

par(mfrow=c(1,1))
plot(tot.C.kg_m2~N.trees,data=Belowground.full)
plot(tot.C.kg_m2~Livestock.bm,data=Belowground.full)
plot(tot.C.kg_m2~log(BM.N.trees.m2),data=Belowground.full)
plot(tot.N.kg_m2~log(BM.N.trees.m2),data=Belowground.full)
plot(tot.C.kg_m2~Sand.pip.per,data=Belowground.full)
plot(tot.N.kg_m2~log(BM.Large.N.m2),data=Belowground.full)
plot(tot.C.kg_m2~log(BM.Large.N.m2),data=Belowground.full)
names(Belowground.full)
#names(Belowground.block)

summary(lm(tot.C.kg_m2~Sand.pip.per,data=Belowground.full))

#### TOT ECOSYSTEM C ####

library(ggplot2)
library(dplyr)

# Legend titeles
legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

# Point

# To get different shape of point: scale_shape_manual(values=c(1,15))
EcosystemC.plot1 <- ggplot(data = Block.Eco.C, aes(x = Region,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, group = Carbon.pool, colour= Carbon.pool))

EcosystemC.plot1 + xlab("Region") +  ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")")))  + 
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_point(size = 3, shape=20,stroke=2, na.rm=T)  + 
  geom_errorbar(stat = "identity",width=.4,lwd=1.1,show.legend=F, na.rm=T) +
  theme_bw() + Lines_gone +  
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4"))

ggsave("Ecosystem carbon/Figures/EcoC.plot.png",
      width= 25, height = 15,units ="cm",bg ="transparent",
      dpi = 600, limitsize = TRUE)

#ylab(expression(paste("Aboveground Carbon (g", m^-2,")")))

# Bar - dodge 
EcosystemC.bar1 <- ggplot(data = Block.Eco.C, aes(x=Region,y=C.amount,ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, fill=Carbon.pool))

EcosystemC.bar1 + 
  geom_errorbar(width=0.6,lwd=0.5,position=position_dodge(width=0.9),show.legend=F) +
  geom_bar(stat="identity", position="dodge",na.rm=T) + 
  theme_bw() + Lines_gone  + xlab("Region") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  scale_fill_manual(breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

# Bar - stacked per Region 
EcosystemC.bar2 <- ggplot(data = Block.Eco.C, aes(x=Region,y=C.amount,ymin=C.amount-SE.C.amount, ymax=C.amount+SE.C.amount, fill=factor(Carbon.pool)))

EcosystemC.bar2 + 
  geom_bar(stat="identity", position="stack",width = 0.7,na.rm=T) +
  theme_bw() + Lines_gone  + 
  xlab("Region") + ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  scale_fill_manual(breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

ggsave("Ecosystem carbon/Figures/EcoC.Stacked.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Bar - stacked per Landuse 
EcosystemC.bar3 <- ggplot(data = Block.Eco.C, aes(x=landuse,y=C.amount,ymin=C.amount-SE.C.amount, ymax=C.amount+SE.C.amount, fill=factor(Carbon.pool)))

EcosystemC.bar3 + 
  geom_bar(stat="identity", position="stack",width = 0.5,na.rm=T) + 
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=12,color="black")
        ,axis.title.x=element_text(size=12,color="black")
        ,axis.text.x=element_text(size=11,color="black",
                                  margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm") # - because we want them inwards. 
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y.right =element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size=12,margin = margin(.5,.5,.5,.5, "mm"),hjust = .02)
        ,strip.text.y = element_blank()
        ,panel.spacing = unit(.1, "lines")
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=12)
        ,legend.position = "right"
        ,legend.justification = "top"
        ,legend.direction="vertical"
        ,legend.key.width = unit(1.2,"cm")) +
  scale_y_continuous(labels = function(y) y / 10) +
  xlab("Land-Use") + ylab(expression(paste("Carbon (kg ", m^-2,")"))) + 
  scale_fill_manual(legend_titleCarbon,breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) 

ggsave("Ecosystem carbon/Figures/EcoC.Landuse.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#+ geom_errorbar(position="dodge",width=.2,show.legend=F, na.rm=T)

# MAP and ECOSYSTEM CARBON
# Seems to be a positive relationship between MAP and C Except for Maswa which is quite hight in tree C even though it is considered a dry region. 
EcosystemC.MAP <- ggplot(data = Block.Eco.C, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.MAP + xlab(expression(paste("MAP (mm", yr^-1,")")))+ ylab(expression(paste("Ecosystem Carbon (kg", m^-2,")"))) + 
  facet_wrap(~Carbon.pool,scales = "free") +
  geom_errorbar(stat = "identity",width=25,lwd=1.1,show.legend=F, na.rm=T) + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/EcoC.MAP.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Soil texture and ECOSYSTEM CARBON

# 2. Sand
# Negative relationship 
EcosystemC.Soil.sand<- ggplot(data = Block.Eco.C, aes(x = Sand.pip.per,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Soil.sand  + xlab("Sand (%)") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free") + 
  geom_errorbar(stat = "identity",width=3,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) +
  theme_bw() + Lines_gone 

# Texture class 
EcosystemC.class<- ggplot(data = Block.Eco.C, aes(x = Class,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool))

EcosystemC.class  + xlab("Texture Class") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free") + 
  geom_boxplot(size=0.5)  + 
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) +
  theme_bw() + Lines_gone 

# Experimenting with a line of MAP in the plot.. 
# Rain <- aggregate(MAP.mm_yr~Region+Block.ID+Sand.pip.per,mean,data=Block.Eco.C)
# plot(MAP.mm_yr~Sand.pip.per,data=Rain)
#   scale_x_continuous(expand=c(0,0), limits = c(0, 70)) + 
#     scale_y_continuous(limits = c(0, 8), expand=c(0,0)) + 
#     scale_y_continuous(sec.axis = sec_axis(~ . *150, name = "Precipitation (mm)")) + 
#     geom_line(data= Rain,aes(y=MAP.mm_yr/150, colour= "MAP.mm_yr"))
#   nap2<-nap2+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
# nap2<-nap2+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)

ggsave("Ecosystem carbon/Figures/EcoC.Sand.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

summary(lm(C.amount~Sand.pip.per+ Sand.pip.per:MAP.mm_yr,data=Herbaceous))

# Fire and Ecosystem Carbon 
# year of last fire 
EcosystemC.Fire<- ggplot(data = Block.Eco.C, aes(x = Last.fire_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Fire  + xlab("Year of last fire") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=2,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Year.of.last.fire.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

summary(lm(C.amount~Clay.pip.per,data=Soil.min))

# Fire freq
EcosystemC.Fire<- ggplot(data = Block.Eco.C, aes(x = Fire_frequency.2000_2017,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

EcosystemC.Fire  + xlab("Fire frequency") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Carbon.pool,scales = "free")+
  geom_errorbar(stat = "identity",width=1.5,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone 

ggsave("Ecosystem carbon/Figures/EcoC.Fire.freq.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#### Trees and ECOSYSTEM CARBON - maybe not so relevant #### 
# Tre basal area and ECOSYSTEM CARBON

EcosystemC.basal.area <- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBasalA_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse))

EcosystemC.basal.area  + xlab(expression(paste("Tree basal area (per ", m^-2,")"))) + ylab(expression(paste("Ecosystem Carbon (g", m^-2,")")))  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.01,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC","SoilCAHor","SoilCMinHor"),values=c("darkolivegreen","forestgreen","darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Number of trees and ECOSYSTEM CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
EcosystemC.No.trees<- ggplot(data = Total.ecosystem.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.No.trees  + xlab("Number of trees (m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC"),values=c("dimgray","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone

# Tree Biomass and ECOSYSTEM CARBON

EcosystemC.treeBM<- ggplot(data = Total.ecosystem.carbon, aes(x = TreeBM_m2,y = C.amount, ymin=C.amount-C.amountSE,ymax=C.amount+C.amountSE, colour= Carbon.pool, shape= Landuse.x))

EcosystemC.treeBM  + xlab("Tree Biomass (g/m2)") + ylab("Aboveground Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)   + 
  geom_errorbar(stat = "identity",width=35,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("WoodyC", "HerbC","DWC"),values=c("dimgray","forestgreen","chocolate2")) + 
  theme_bw() + Lines_gone

#### SOIL CARBON #### 

# Tre basal area and SOIL CARBON

# Creating a table with soil carbon 
Soil.carbon <- Block.Eco.C %>%
  filter(Carbon.pool == "Soil A-horizon" | Carbon.pool== "Soil Min-horizon")
Soil.carbon <- arrange(Soil.carbon,Region)
Soil.carbon <- droplevels(Soil.carbon)

Soil.min<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil Min-horizon")
Soil.A.hor<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil A-horizon")

summary(lm(C.amount~landuse:Sand.pip.per,data=Soil.A.hor))
plot(C.amount~Class, data=Soil.A.hor)
summary(lm(C.amount~Class, data=Soil.A.hor))

Tree.size.data <- gather(Soil.carbon, Tree.size,Count, No.small.trees:No.large.trees,factor_key=TRUE)

Tree.size.data <- Tree.size.data[
  order(Tree.size.data[,2], Tree.size.data[,3] ),
  ]

levels(Tree.size.data$Tree.size) <- c("Small trees","Large trees")
levels(Tree.size.data$Carbon.pool) <- c("Soil A-horizon","Soil Min-horizon")
levels(Soil.carbon$Carbon.pool) <- c("Soil A-horizon","Soil Min-horizon")

# Total tree biomass
SoilC.tree.biomass <- ggplot(data = Soil.carbon, aes(x = TreeBM.kg_m2,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

SoilC.tree.biomass  + xlab(expression(paste("Tree Biomass (kg", m^-2,")"))) + ylab(expression(paste("Carbon pool (kg", m^-2,")")))  + 
  facet_wrap(~Carbon.pool,ncol=1,scales = "free") +
  geom_errorbar(stat = "identity",width=0.05,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Soil A-horizon","Soil Min-horizon"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/SoilC.TreeBM.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Size of trees 
Tree.size.block<- Tree.size.data %>%
  filter(Carbon.pool== "Soil A-horizon")

Small.trees<- Tree.size.block %>%
  filter(Tree.size== "Small trees")
Large.trees<- Tree.size.block %>%
  filter(Tree.size== "Large trees")

large <- aggregate(Count~Region,sum,data=Large.trees)
small <- aggregate(Count~Region,sum,data=Small.trees)
plot(Count~Region,data=small)

# Small trees vs large trees 
SoiiC.Tree.Size <- ggplot(data = Tree.size.data, aes(x=Count,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, group= Tree.size, shape= landuse))

SoilC.Tree.Size +
  xlab("# Trees") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) + 
  facet_wrap(~Tree.size + Carbon.pool, ncol=2,scales='fixed') + 
  geom_errorbar(stat = "identity",width=1,lwd=1.1,show.legend=F, na.rm=T) +
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Soil A-horizon","Soil Min-horizon"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

ggsave("Ecosystem carbon/Figures/SoilC.TreeSize.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# Number of trees and SOIL CARBON
# Difficult to incorporate this relationship - Really scewed. Mwantimba has highest number of trees... 
SoilC.No.trees<- ggplot(data = Soil.carbon, aes(x = No.trees_m2,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount, colour= Carbon.pool, shape= landuse))

SoilC.No.trees  + xlab("Number of trees (m2)") + ylab("Soil Carbon (g/m2)")  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("SoilCAHor","SoilCMinHor"),values=c("salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Soil C and Texture class 
A.hor.class<- ggplot(data = Soil.A.hor, aes(x = MAP.mm_yr,y = C.amount, ymin=C.amount-SE.C.amount,ymax=C.amount+SE.C.amount,colour=Carbon.pool))

A.hor.class  + xlab("Texture Class") + ylab(expression(paste("Carbon pool (kg", m^-2,")"))) +
  facet_wrap(~Class,scales = "free") + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T)  + 
  geom_errorbar(stat = "identity",width=0.001,lwd=1.1,show.legend=F, na.rm=T) +
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("Soil A-horizon"),values=c("salmon4")) +
  theme_bw() + Lines_gone 


### Correlation of variables (numeric) #### 

# Want to do the analysis at block size 
Ecosystem.Carbon <- read.csv(file="Ecosystem Carbon/Ecosystem.Carbon.csv", head=T)
names(Ecosystem.Carbon)

Variables <- Ecosystem.Carbon %>%
  filter(Carbon.pool=="HerbC.kg_m2")

str(Ecosystem.Carbon)

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
names(Belowground.full)
names(Variables)
MyVar1<-c("MAP.mm_yr","Clay.pip.per","Sand.pip.per","Last.fire_yr", "Fire_frequency.2000_2017","Small.N","Small.N.non","Large.N","Large.N.non","TreeBM.kg_m2","mean.N.kg_m2")

MyVar2<-c("BM.Large.N.m2","BM.Small.N.m2","BM.N.trees.m2","mean.N.kg_m2.x")

# Want to get these two in one matrix. 
pairs(Belowground.full[,MyVar2],lower.panel = panel.cor)

# If I want these values in a table:
Variables2 <- Variables[,c(6:9,11,13,15,16)] # first select the collums I want to use 
library("Hmisc")
Mycor <- rcorr(as.matrix(Variables2), type="pearson") # Use the pearson correlation (r-value)
MycorR <- as.data.frame(round(Mycor$r, digits=3))
MycorP <- as.data.frame(round(Mycor$P, digits=3))

# The values here is pearson correlation coeficients - in other words, the r value (between -1 and 1 where 0 is no correlation). 
# Tree basal area is 100 % correlated with Tree biomass, no need to use both, however, not so correlated with number of trees. 
# Number of trees have a strong negative relationship with year of last fire. and quite a strong positive relationship with MAP.

#### LANDUSE and correlation ####
plot(TreeBM.kg_m2~landuse, data= Block.Eco.C) # COVARYING 
plot(No.trees_m2~landuse, data= Block.Eco.C) # not covarying
plot(MAP.mm_yr~landuse, data= Block.Eco.C) # not covarying
plot(Sand.pip.per~landuse, data= Block.Eco.C) # not covarying
plot(Last.fire_yr~landuse, data= Block.Eco.C) # not covarying
plot(Fire_frequency.2000_2017~landuse, data= Block.Eco.C) # not covarying 
plot(No.small.trees~landuse, data= Block.Eco.C) # not covarying 
plot(No.large.trees~landuse, data= Block.Eco.C)# not covarying 
plot(mean.N.kg_m2.x~landuse,data=Belowground.full) # not covarying
# Trees, N,non.N
par(mfrow=c(2,2))
plot(Small.N~landuse,data=Block.Eco.C) # not covarying
plot(Small.N.non~landuse,data=Block.Eco.C)# not covarying
plot(Large.N~landuse,data=Block.Eco.C)# not covarying
plot(Large.N.non~landuse,data=Block.Eco.C)# not covarying
par(mfrow=c(2,2))
plot(BM.Small.N.m2~landuse,data=Block.Eco.C)# not covarying
plot(BM.Small.non.m2~landuse,data=Block.Eco.C) # Covarying, larger in P 
plot(BM.Large.N.m2~landuse,data=Block.Eco.C) # Covarying, larger in W
plot(BM.Large.non.m2~landuse,data=Block.Eco.C) # Covarying, larger in W
par(mfrow=c(1,3))
plot(No.trees_m2~landuse,data=Belowground.full)
plot(N.trees~landuse,data=Belowground.full)
plot(non.N.trees~landuse,data=Belowground.full)

plot(TreeBM.kg_m2~landuse,data=Belowground.full)
plot(BM.N.trees.m2~landuse,data=Belowground.full)
plot(BM.non.N.trees.m2~landuse,data=Belowground.full)

par(mfrow=c(1,2))
plot(No.large.trees~Region, data= Block.Eco.C)
plot(C.amount ~ Region, data=Soil.A.hor)
N.trees <- aggregate(N.trees ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(N.trees ~landuse, sum,data=N.trees)
# landuse N.trees
# 1 Pasture      41
# 2    Wild      59
non.N.trees <- aggregate(non.N.trees ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(non.N.trees ~landuse, sum,data=non.N.trees)
# landuse non.N.trees
# 1 Pasture          36
# 2    Wild          30
No.trees_m2 <- aggregate(No.trees_m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(No.trees_m2 ~landuse, mean,data=No.trees_m2)
# landuse No.trees_m2
# 1 Pasture 0.007291667
# 2    Wild 0.002373333
BM.N.trees.m2 <- aggregate(BM.N.trees.m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(BM.N.trees.m2 ~landuse, mean,data=BM.N.trees.m2)
# landuse BM.N.trees.m2
# 1 Pasture    0.01182283
# 2    Wild    0.16568178
BM.non.N.trees.m2 <- aggregate(BM.non.N.trees.m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(BM.non.N.trees.m2 ~landuse, mean,data=BM.non.N.trees.m2)
# landuse BM.non.N.trees.m2
# 1 Pasture       0.006182991
# 2    Wild       0.040260107
TreeBM.kg_m2 <- aggregate(TreeBM.kg_m2 ~Block.ID+landuse, mean,data=Belowground.full)
aggregate(TreeBM.kg_m2 ~landuse, mean,data=TreeBM.kg_m2)
# landuse TreeBM.kg_m2
# 1 Pasture   0.02363048
# 2    Wild   0.21517161
### Data modelling/ analysis ####

library(nlme)
library(lme4)
library(glmmADMB) 
library(piecewiseSEM) # SEM
library(MuMIn) # to make "model.sel()" of different models 
library(emmeans) # estimated marginal means --> look at this for three ways interactions.. 

# About mixed effect models (nlme package)
# REML = restricted maximum likelihood estimation 
# Fixed effects influence the mean of y, while Random effects influence the variance of y 
# Use REML= F when looking at the fixed effects 
# Use REML = T when looking at the random effects, and the parameter estimates
# I have region and block.ID as random effects 
# Choose the model with the smallest AIC value 

str(Block.Eco.C)
Soil.min<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil Min-horizon")
Soil.Ahor<- Block.Eco.C %>%
  filter(Carbon.pool== "Soil A-horizon")
DW<- Block.Eco.C %>%
  filter(Carbon.pool== "Dead wood")
Woody<- Block.Eco.C %>%
  filter(Carbon.pool== "Woody")
Herbaceous<- Block.Eco.C %>%
  filter(Carbon.pool== "Herbaceous")

# Run a full model of all my fixed factors interactions with the carbon pools ####
# Need Belowground full created earlier. 
Belowground.full <- read.csv(file="Ecosystem Carbon/Soil.data/Belowground.Carbon.csv",head=T)
names(Belowground.full)
BelowgroundNA <- droplevels(na.omit(Belowground.full))

Belowground.tot<-lmer(tot.C.kg_m2~ MAP.mm_yr.x + landuse + Fire_frequency.2000_2017.x + tot.N.kg_m2 + TreeBM.kg_m2 + BM.N.trees.m2 +
                        landuse:MAP.mm_yr.x + landuse:Fire_frequency.2000_2017.x  + tot.N.kg_m2:landuse + landuse:Sand.pip.per + landuse:BM.N.trees.m2 + 
                        MAP.mm_yr.x:Sand.pip.per +  TreeBM.kg_m2:Sand.pip.per + landuse:Sand.pip.per + tot.N.kg_m2:Sand.pip.per + Fire_frequency.2000_2017.x:Sand.pip.per + BM.N.trees.m2:Sand.pip.per + 
                         TreeBM.kg_m2:MAP.mm_yr.x +  tot.N.kg_m2:MAP.mm_yr.x + Fire_frequency.2000_2017.x:MAP.mm_yr.x + BM.N.trees.m2:MAP.mm_yr.x + 
                        TreeBM.kg_m2:tot.N.kg_m2 +  Fire_frequency.2000_2017.x:tot.N.kg_m2 + TreeBM.kg_m2:Fire_frequency.2000_2017.x +BM.N.trees.m2: tot.N.kg_m2 + 
                        (1|Region.x.x/Block), data = Belowground.full, REML=T)

summary(Belowground.tot)
drop1(Belowground.tot,test="Chisq")
anova(Belowground.tot)
AIC(Belowground.tot) #260.2629

# Reduce the model to what was significant in drop1 
Belowground.tot.red<-lmer(tot.C.kg_m2~ MAP.mm_yr.x + landuse + Fire_frequency.2000_2017.x + tot.N.kg_m2 + TreeBM.kg_m2 + BM.N.trees.m2 +
                        #landuse:MAP.mm_yr.x + tot.N.kg_m2:landuse
                          + landuse:Fire_frequency.2000_2017.x + landuse:Sand.pip.per + landuse:BM.N.trees.m2 + 
                        MAP.mm_yr.x:Sand.pip.per +  TreeBM.kg_m2:Sand.pip.per + landuse:Sand.pip.per + Fire_frequency.2000_2017.x:Sand.pip.per + BM.N.trees.m2:Sand.pip.per + #+ tot.N.kg_m2:Sand.pip.per + 
                        TreeBM.kg_m2:MAP.mm_yr.x +  tot.N.kg_m2:MAP.mm_yr.x + BM.N.trees.m2:MAP.mm_yr.x +  #Fire_frequency.2000_2017.x:MAP.mm_yr.x + 
                        #TreeBM.kg_m2:tot.N.kg_m2 +BM.N.trees.m2: tot.N.kg_m2 + 
                          +  Fire_frequency.2000_2017.x:tot.N.kg_m2 + TreeBM.kg_m2:Fire_frequency.2000_2017.x + (1|Region.x.x/Block), data = Belowground.full, REML=T)

summary(Belowground.tot.red)
drop1(Belowground.tot.red,test="Chisq") #everything significant..? 
anova(Belowground.tot.red) # Nitrogen really high - something going on here.. 
AIC(Belowground.tot.red) # 231.7689

Belowground.full$fMAP <- as.factor(Belowground.full$MAP.mm_yr.x)
library(lattice)
xyplot(tot.C.kg_m2~log(BM.N.trees.m2)|landuse,data=Belowground.full)
xyplot(tot.C.kg_m2~log(TreeBM.kg_m2 )|fMAP,data=Belowground.full)
xyplot(tot.C.kg_m2~log(BM.N.trees.m2 )|fMAP,data=Belowground.full)
xyplot(tot.N.kg_m2~log(BM.N.trees.m2 )|fMAP,data=Belowground.full)

Test1<-lmer(tot.C.kg_m2~ BM.N.trees.m2+ MAP.mm_yr +BM.N.trees.m2:MAP.mm_yr + (1|Region.x.x/Block), data = Belowground.full, REML=T)
drop1(Test1,test="Chisq") 
Test1<-lmer(tot.C.kg_m2~ BM.N.trees.m2+ MAP.mm_yr +BM.N.trees.m2:MAP.mm_yr + (1|Region.x.x/Block), data = Belowground.full, REML=T)
drop1(Test1,test="Chisq") 
Test1<-lmer(tot.N.kg_m2~ BM.N.trees.m2+ MAP.mm_yr +BM.N.trees.m2:MAP.mm_yr + (1|Region.x.x/Block), data = Belowground.full, REML=T)
drop1(Test1,test="Chisq") 
anova(Test1)

# Remove landuse in the total model 

Belowground1<-lmer(tot.C.kg_m2~ MAP.mm_yr.x  + Sand.pip.per + tot.N.kg_m2 + Fire_frequency.2000_2017.x + TreeBM.kg_m2 + BM.N.trees.m2 + 
                     MAP.mm_yr.x:Sand.pip.per + BM.N.trees.m2:Sand.pip.per + TreeBM.kg_m2:Sand.pip.per + tot.N.kg_m2:Sand.pip.per + Fire_frequency.2000_2017.x:Sand.pip.per +
                     BM.N.trees.m2:MAP.mm_yr.x +  TreeBM.kg_m2:MAP.mm_yr.x + tot.N.kg_m2:MAP.mm_yr.x + Fire_frequency.2000_2017.x:MAP.mm_yr.x + 
                     BM.N.trees.m2:tot.N.kg_m2 + TreeBM.kg_m2:tot.N.kg_m2 + Fire_frequency.2000_2017.x:tot.N.kg_m2 + 
                     (1|Region.x.x/Block), data = Belowground.full, REML=T)

summary(Belowground1)
drop1(Belowground1,test="Chi") 
anova(Belowground1) 
AIC(Belowground1) #238.23

# Reducing the model
Belowground1.red<-lmer(tot.C.kg_m2~ MAP.mm_yr.x  + Sand.pip.per + tot.N.kg_m2 + TreeBM.kg_m2 + #+ Fire_frequency.2000_2017.x + BM.N.trees.m2 + 
                     #MAP.mm_yr.x:Sand.pip.per + tot.N.kg_m2:Sand.pip.per + Fire_frequency.2000_2017.x:Sand.pip.per + BM.N.trees.m2:Sand.pip.per 
                       + TreeBM.kg_m2:Sand.pip.per + #BM.N.trees.m2:MAP.mm_yr.x +  TreeBM.kg_m2:MAP.mm_yr.x + 
                       tot.N.kg_m2:MAP.mm_yr.x + #Fire_frequency.2000_2017.x:MAP.mm_yr.x + 
                     #BM.N.trees.m2:tot.N.kg_m2 + TreeBM.kg_m2:tot.N.kg_m2 + Fire_frequency.2000_2017.x:tot.N.kg_m2 + 
                     (1|Region.x.x/Block), data = Belowground.full, REML=T)

summary(Belowground1.red)
drop1(Belowground1.red,test="Chi") 
anova(Belowground1.red) 
AIC(Belowground1.red) #tot.238.23 #new 191.89 #Reducing further, new 188.0718 # Reducing further, new: 164.9673
# ending up with the same as below, MAP.mm_yr.x:tot.N.kg_m2 is the significant. 

# model without sand and landuse
Belowground2<-lmer(tot.C.kg_m2~ MAP.mm_yr.x + tot.N.kg_m2 + Fire_frequency.2000_2017.x + TreeBM.kg_m2 + No.small.trees + No.large.trees +
                     No.large.trees:MAP.mm_yr.x +  TreeBM.kg_m2:MAP.mm_yr.x + No.small.trees:MAP.mm_yr.x + tot.N.kg_m2:MAP.mm_yr.x + Fire_frequency.2000_2017.x:MAP.mm_yr.x + 
                     No.large.trees:tot.N.kg_m2 + TreeBM.kg_m2:tot.N.kg_m2 + No.small.trees:tot.N.kg_m2 + Fire_frequency.2000_2017.x:tot.N.kg_m2 + 
                     (1|Region.x/Block), data = Belowground.full, REML=T)

summary(Belowground2)
drop1(Belowground2,test="Chi") # significant:MAP.mm_yr.x:tot.N.kg_m2, MAP.mm_yr.x:No.small.trees, tot.N.kg_m2:No.small.trees  
anova(Belowground2)
AIC(Belowground2) #194.7897

# distribution of residuals 
E2 <- resid(Belowground2, type ="pearson") 
F2 <- fitted(Belowground2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2) # None of the fitted values <0
abline(h = 0, lty = 2, col = 1)  # Good

# reduce the model

Belowground2red1<-lmer(tot.C.kg_m2~ MAP.mm_yr.x + tot.N.kg_m2 + #+ Fire_frequency.2000_2017.x + TreeBM.kg_m2 + No.large.trees + 
                         No.small.trees + 
                         #No.large.trees:MAP.mm_yr.x +  TreeBM.kg_m2:MAP.mm_yr.x + Fire_frequency.2000_2017.x:MAP.mm_yr.x 
                         + No.small.trees:MAP.mm_yr.x + tot.N.kg_m2:MAP.mm_yr.x + 
                         #No.large.trees:tot.N.kg_m2 + TreeBM.kg_m2:tot.N.kg_m2 + Fire_frequency.2000_2017.x:tot.N.kg_m2 + 
                         No.small.trees:tot.N.kg_m2 + 
                         (1|Region.x/Block), data = Belowground.full, REML=T)

summary(Belowground2red1)
AIC(Belowground2red1) #194.7897,new: 165.625
anova(Belowground2red1)
drop1(Belowground2red1,test="Chi") # drop No.small.trees:tot.N.kg_m2 

Belowground2red2<-lmer(tot.C.kg_m2~ MAP.mm_yr.x + tot.N.kg_m2 + #+ Fire_frequency.2000_2017.x + TreeBM.kg_m2 + No.large.trees + 
                         No.small.trees + 
                         #No.large.trees:MAP.mm_yr.x +  TreeBM.kg_m2:MAP.mm_yr.x + Fire_frequency.2000_2017.x:MAP.mm_yr.x 
                         + No.small.trees:MAP.mm_yr.x + tot.N.kg_m2:MAP.mm_yr.x + 
                         #No.large.trees:tot.N.kg_m2 + TreeBM.kg_m2:tot.N.kg_m2 + Fire_frequency.2000_2017.x:tot.N.kg_m2 + No.small.trees:tot.N.kg_m2 + 
                         (1|Region.x/Block), data = Belowground.full, REML=T)

summary(Belowground2red2)
AIC(Belowground2red2) #194.7897,new: 165.625, new:161.8329
anova(Belowground2red2)
drop1(Belowground2red2,test="Chi") # drop No.small.trees:tot.N.kg_m2 and MAP.mm_yr.x:No.small.trees 

Belowground2red3<-lmer(tot.C.kg_m2~ MAP.mm_yr.x + tot.N.kg_m2 + tot.N.kg_m2:MAP.mm_yr.x +
                         (1|Region.x.x/Block), data = Belowground.full, REML=T)
summary(Belowground2red3)
AIC(Belowground2red3) #194.7897,new: 165.625, new:161.8329, new:147.3839
anova(Belowground2red3)

# Updating the model - generating p-values for each term
Belowground2red3a<- update(Belowground2red3, .~. -tot.N.kg_m2:MAP.mm_yr.x)
Belowground2red3b<- update(Belowground2red3a, .~. -tot.N.kg_m2)
Belowground2red3c<- update(Belowground2red3a, .~. -MAP.mm_yr.x)

anova(Belowground2red3,Belowground2red3a)
anova(Belowground2red3a,Belowground2red3b)
anova(Belowground2red3a,Belowground2red3c)
#                  Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#Belowground2red3   7 122.34 141.37 -54.169   108.34 26.892      1  2.152e-07 *** # the interaction is significant 
#Belowground2red3a  6 147.23 163.54  -67.615   135.23 158.64      1  < 2.2e-16 *** # Nitrogen is significant
#Belowground2red3a  6 147.23 163.54 -67.615   135.23 5.1323      1    0.02348 * # MAP is significant (but nitrogen more significant)

#interaction plot
with(Belowground.full, {interaction.plot(tot.N.kg_m2,MAP.mm_yr.x,tot.C.kg_m2,
                                         xlab = "Nitrogen",
                                         ylab = " Carbon",
                                         fun=mean)})

Belowground.full$fMAP <- as.factor(Belowground.full$MAP.mm_yr)
library(lattice)
xyplot(tot.C.kg_m2~tot.N.kg_m2|fMAP,data=Belowground.full)

# Testing my first hypothesis 1 where I think MAP has an effect on C, however, this relationship can be mediated by soil texture. 

# Soil mineral horizon 
Soil.min.H11<-lmer(C.amount~ MAP.mm_yr + (1|Region), data = Soil.min, REML=F)
Soil.min.H12<-lmer(C.amount~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.min, REML=F)
Soil.min.H13<-lmer(C.amount~ MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.min, REML=F)
Soil.min.H14<-lmer(C.amount~ Sand.pip.per + (1|Region), data = Soil.min, REML=F)

model.sel(Soil.min.H11,Soil.min.H12,Soil.min.H13,Soil.min.H14)
# Look at delta AIC - the first is 0 (the best model), and the change to the next indicates if I should continue using it or not. If it is between 1 and 2, continue with second model as well. Here delta mod H14 = 0.59 (bring along). 

Soil.min.H12<-lmer(C.amount~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.min, REML=T)
Soil.min.H14<-lmer(C.amount~ Sand.pip.per + (1|Region), data = Soil.min, REML=T)

summary(Soil.min.H12)
summary(Soil.min.H14)

# Assumption 1: Within group errors are independent, and identically normally distributed, mean= 0. And they are independent of the random effect. 

hist(resid(Soil.min.H12)) # A bit sqewed towards right, more on the positive side - could consider log.transforming the data 
hist(resid(Soil.min.H14)) # "more normally distributed" 
plot(Soil.min.H12,factor(Region)~resid(.),abline=0) # Makao is strange Block 4 has a very low carbon amount (0.8 kg). 
plot(Soil.min.H14,factor(Region)~resid(.),abline=0)

# Another way to show the same. 
plot(Soil.min.H12,resid(.)~fitted(.)|factor(Region)) # Distribution of residuals within each group. 

# Soil A-horizon: The best model is taking MAP and the relationship between MAP:Sand into account####
Soil.A.H11<-lmer(C.amount~ MAP.mm_yr + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H12<-lmer(C.amount~ MAP.mm_yr + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H13<-lmer(C.amount~ MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H14<-lmer(C.amount~ Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)
Soil.A.H15<-lmer(C.amount~ Sand.pip.per + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=F)

model.sel(Soil.A.H11,Soil.A.H12,Soil.A.H13,Soil.A.H14,Soil.A.H15)
# AIC: H14 < H15 < H11 < H12 < H13 
drop1(Soil.A.H15, test="Chi")

Soil.A.H15<-lmer(C.amount~ Sand.pip.per + MAP.mm_yr:Sand.pip.per + (1|Region), data = Soil.Ahor, REML=T)

summary(Soil.A.H15)
hist(resid(Soil.A.H15)) # normally distriuted - a bit sqewed towards right 
anova(Soil.A.H15, Soil.A.H14) # Due to high F value: Sand is very strong, MAP:Sand is 1.6 
AIC(Soil.A.H15) # AIC: 30.42954

# Testing second hypothesis H2 where I ask about the effect of land use and woody plant cover. 

# Fist just look at the relationship between landuse and my belowground C variable 

plot(tot.C.kg_m2~Region,data = Belowground.full)
Landuse.C<-lmer(tot.C.kg_m2~ landuse + fMAP + landuse:fMAP + (1|Region/Block), data = Belowground.full, REML=T)
summary(Landuse.C)
drop1(Landuse.C)
anova(Landuse.C)

Soil.min$BM.N.trees.m2 <- Soil.min$BM.Large.N.m2+Soil.min$BM.Small.N.m2
plot(C.amount~log(BM.N.trees.m2),data=Soil.min)

Soil.min.H21<-lmer(C.amount~ landuse + (1|Region), data = Soil.min, REML=F)
Soil.min.H22<-lmer(C.amount~ No.small.trees + (1|Region), data = Soil.min, REML=F)
Soil.min.H23<-lmer(C.amount~ landuse + No.small.trees + (1|Region), data = Soil.min, REML=F)
Soil.min.H24<-lmer(C.amount~ landuse + No.small.trees + landuse:No.small.trees + (1|Region), data = Soil.min, REML=F)
Soil.min.H25<-lmer(C.amount~ landuse:No.small.trees + (1|Region), data = Soil.min, REML=F)

model.sel(Soil.min.H21,Soil.min.H22,Soil.min.H23,Soil.min.H24,Soil.min.H25) # AIC: H22<H25<H23<H24<H21
anova(Soil.min.H22) # No.small.trees F: 0.13
anova(Soil.min.H25) # landuse:No.small.trees F: 0.91

Soil.min.H25<-lmer(C.amount~ landuse:No.small.trees + (1|Region), data = Soil.min, REML=T)
Soil.min.H22<-lmer(C.amount~ No.small.trees + (1|Region), data = Soil.min, REML=T)

summary(Soil.min.H25)
summary(Soil.min.H22)
hist(resid(Soil.min.H25))
hist(resid(Soil.min.H22))
AIC(Soil.min.H25) # 73.558
AIC(Soil.min.H22) # 69.6068

# per pool. 
names(Soil.Ahor)
Soil.A_horizon<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.Ahor, REML=T)

drop1(Soil.A_horizon,test="Chi")

Soil.Min_horizon<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Soil.min, REML=T)

drop1(Soil.Min_horizon,test="Chi")

Woody.mod<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Woody, REML=T)

drop1(Woody.mod,test="Chi")

Herbacaous.mod<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = Herbaceous, REML=T)

drop1(Herbacaous.mod,test="Chi")

DW.mod<-lmer(C.amount~ landuse*TreeBM.kg_m2 + landuse*No.large.trees + landuse*Last.fire_yr + landuse*Sand.pip.per + MAP.mm_yr*Sand.pip.per + (1|Region), data = DW, REML=T)

drop1(DW.mod,test="Chi")

Soil.A_horizon1 <- update(Soil.A_horizon, .~. -landuse:TreeBM.kg_m2)
anova(Soil.A_horizon,Soil.A_horizon1) # using simpler model because the p-value is larger than 0.05?? (no difference between the two models)

Soil.A_horizon2 <- update(Soil.A_horizon1, .~. -landuse:No.large.trees)
anova(Soil.A_horizon1,Soil.A_horizon2) #Remove "landuse:No.large.trees"
