####Making tables####
# Packages 
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

# Uploading the soil file
total.soil.data<- read.csv("Ecosystem Carbon/Soil.data/Total.Soil.Data.csv", head = TRUE)
names(total.soil.data)
Metabolic.rate <- read.csv("Ecosystem Carbon/CattleMetabolic.csv", head = TRUE)
Dung.counts1 <- read.csv("Permanent exclosures/Herbivore dung/Sero.prod.dungFULL.csv", head=TRUE)
Soil.texture <- read.csv(file="Ecosystem Carbon/Soil.data/Soil.texture.Tot_Hor.csv",head=T)

total.soil.data$Region<- factor(total.soil.data$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
names(total.soil.data)
# Look at data per Region 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
P <- cbind(aggregate(P.g_kg~Region, mean, data=total.soil.data),
           aggregate(P.g_kg~Region, SE, data=total.soil.data)[2])
colnames(P)[3] <- "SE.P"
BD <- cbind(aggregate(BD_fine_earth_air_dry~Region, mean, data=total.soil.data),
           aggregate(BD_fine_earth_air_dry~Region, SE, data=total.soil.data)[2])
colnames(BD)[3] <- "SE.BD"
InorgC <- cbind(aggregate(fraction_inorg_C~Region, mean, data=total.soil.data),
            aggregate(fraction_inorg_C~Region, SE, data=total.soil.data)[2])
colnames(InorgC)[3] <- "SE.InorgC"
OrgC <- cbind(aggregate(fraction_org_C~Region, mean, data=total.soil.data),
             aggregate(fraction_org_C~Region, SE, data=total.soil.data)[2])
colnames(OrgC)[3] <- "SE.OrgC"
pH <- cbind(aggregate(pH~Region, mean, data=total.soil.data),
              aggregate(pH~Region, SE, data=total.soil.data)[2])
colnames(pH)[3] <- "SE.pH"
Al <- cbind(aggregate(Al.g_kg~Region, mean, data=total.soil.data),
            aggregate(Al.g_kg~Region, SE, data=total.soil.data)[2])
colnames(Al)[3] <- "SE.Al"
Fe <- cbind(aggregate(Fe.g_kg~Region, mean, data=total.soil.data),
            aggregate(Fe.g_kg~Region, SE, data=total.soil.data)[2])
colnames(Fe)[3] <- "SE.Fe"
mean(Fe$Fe.g_kg)
CEC <- cbind(aggregate(CEC.cmol_kg~Region, mean, data=total.soil.data),
            aggregate(CEC.cmol_kg~Region, SE, data=total.soil.data)[2])
colnames(CEC)[3] <- "SE.CEC"

# Per land-use 
Soil.landuse <- cbind(aggregate(P.g_kg~Land_Use, mean, data=total.soil.data),
                      aggregate(P.g_kg~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(BD_fine_earth_air_dry~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(BD_fine_earth_air_dry~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(fraction_inorg_C~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(fraction_inorg_C~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(fraction_org_C~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(fraction_org_C~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(pH~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(pH~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(Al.g_kg~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(Al.g_kg~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(Fe.g_kg~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(Fe.g_kg~Land_Use, SE, data=total.soil.data)[2],
                      aggregate(CEC.cmol_kg~Land_Use, mean, data=total.soil.data)[2],
                      aggregate(CEC.cmol_kg~Land_Use, SE, data=total.soil.data)[2])
colnames(Soil.landuse) <- c("Landuse","P","SE.P","BD","SE.BD","InC","SE.InC","OrgC","SE.OrgC","pH","SE.pH","Al","SE.Al","Fe","SE.Fe","CEC","SE.CEC")
# Look at soil properties with landuse
landuse <- c("Pasture","Wild","Pasture","Wild","Wild","Pasture","Wild")
# CEC
CEC$landuse <- landuse
colnames(CEC)[3] <- "SE.CEC"
boxplot(CEC.cmol_kg~landuse, data=CEC)
# pH
pH$landuse <- landuse
colnames(pH)[3] <- "SE.pH"
boxplot(pH~landuse, data=pH)


Metabolic.rate$Region<- factor(Metabolic.rate$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

levels(Soil.texture$Region)
Soil.texture$Region<- factor(Soil.texture$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Soil.texture <-Soil.texture[,c(1:3,5:8)]
Soil.texture <- na.omit(Soil.texture)
Soil.texture <- droplevels(Soil.texture)

Soil.texture$Class <- c("SaClLo","SaLo","SaClLo","SaClLo","Cl","Cl","Cl","ClLo","SaCl","SaCl","ClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaClLo","SaLo","SaLo","SaLo","ClLo","Cl","ClLo","Cl", "Cl","ClLo","ClLo","Cl") 
names(Soil.texture)
Soil.texture.R <- cbind(aggregate(Sand.pip.per~Region, mean, data=Soil.texture),
                        aggregate(Sand.pip.per~Region, SE, data=Soil.texture)[2],
                        aggregate(Silt.pip.per~Region, mean, data=Soil.texture)[2],
                        aggregate(Silt.pip.per~Region, SE, data=Soil.texture)[2],
                        aggregate(Clay.pip.per~Region, mean, data=Soil.texture)[2],
                        aggregate(Clay.pip.per~Region, SE, data=Soil.texture)[2])
colnames(Soil.texture.R) <- c("Region","Sand","SE.Sand","Silt","SE.Silt","Clay","SE.Clay")

Soil.texture.landuse <- cbind(aggregate(Sand.pip.per~Landuse, mean, data=Soil.texture),
                              aggregate(Sand.pip.per~Landuse, SE, data=Soil.texture)[2])
# Looking at dung counts 
levels(Dung.counts1$area)
levels(Dung.counts1$landuse)
Dung.counts2<-Dung.counts1[Dung.counts1$area!="MakaoWMA",]
Dung.counts3<-Dung.counts2[Dung.counts2$landuse!="illegal",]
Dung.counts4<-Dung.counts3[Dung.counts3$landuse!="pasture common",]
Dung.counts <- droplevels(Dung.counts4)
colnames(Dung.counts)
Dung.block <- cbind(aggregate(wild_broswer~area+landuse+block,Dung.counts,mean),
                    aggregate(wild_grazer~area+landuse+block,Dung.counts,mean)[4],
                    aggregate(live_broswer~area+landuse+block,Dung.counts,mean)[4],
                    aggregate(live_grazer~area+landuse+block,Dung.counts,mean)[4])
Dung.block$livestock <- Dung.block$live_broswer + Dung.block$live_grazer
Dung.block$wild <- Dung.block$wild_broswer + Dung.block$wild_grazer
Dung.block$total.dung <- Dung.block$livestock + Dung.block$wild
Dung.block$area <- as.character(Dung.block$area)
Dung.block$area[Dung.block$area == "SNP"] <- "Handajega"
Dung.block$vilde.block <- c(3,1,2,4,3,4,1,2,1,4,3,2,3,4,1,2)
Dung.block <- Dung.block[
  order(Dung.block[,1], Dung.block[,11] ),
  ]
Dung.block <- Dung.block[,c(1,2,11,8:10)]
Dung.block$area <-  factor(Dung.block$area, levels = c("Makao","Maswa","Mwantimba","Handajega"))
colnames(Dung.block) <- c("Region","landuse","Block", "livestock","wild","total.dung" )
ID1 <- total.soil.data[,c(2,4)]
Dung <- full_join(ID1,Dung.block)

# Metabolic rate -  not a good variable. 
names(Metabolic.rate)
names(total.soil.data)
ID <- total.soil.data[,c(2,4,5,6,7)]
Metabolic.rate2 <- full_join(ID,Metabolic.rate)
names(Metabolic.rate2)

# Add metabolic rate and dung counts 
total.soil.data1 <- cbind(total.soil.data,Dung[c(4:6)])
total.soil.data2 <- cbind(total.soil.data1,Metabolic.rate2[c(30:32)])

# # Comparing data from NTNU with data from NMBU
# compare1 <- read.csv("Ecosystem Carbon/Soil.data/TBS.NMBU.csv", head=T)
# summary(lm(C.per.TBS~C.per.NMBU, data=compare1))
# cor.test(compare$C.per.TBS,compare$C.per.NMBU, method=c("pearson"))
# 
# # Comparing my data with Stuart`s data 
# compare2 <- read.csv("Ecosystem Carbon/Soil.data/C.Stu.Vilde.csv", head=T)
# str(compare2)
# compare2$Soil.C.Vilde <- as.numeric(compare2$Soil.C.Vilde)
# summary(lm(Soil.C.Vilde~Soil.C.Stu, data=compare2))
# cor.test(compare2$Soil.C.Vilde,compare2$Soil.C.Stu, method=c("pearson"))

# Making tables for C and N - Belowground 
names(total.soil.data2)
Soilred <- total.soil.data2[,c(2:7,52,53,19:22,35:37,39,42,49:51,54)]
Soilred$Region<- factor(Soilred$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

AHorizon <- Soilred[Soilred$Horizon=="A-hor",]
MinHorizon <- Soilred[Soilred$Horizon=="Min-hor",]
Belowground <- cbind(AHorizon,MinHorizon[,c(13:17)])
colnames(Belowground)[c(9,16,17,21:26)] <- c("landuse","AhorC.kg_m2","AhorN.kg_m2","Livestock.bm","Clay.min","Silt.min","Sand.min","MinC.kg_m2","MinN.kg_m2")
Belowground$tot.C.kg_m2 <- Belowground$AhorC.kg_m2+Belowground$MinC.kg_m2
Belowground$tot.N.kg_m2 <- Belowground$AhorN.kg_m2+Belowground$MinN.kg_m2
Belowground$mean.N.kg_m2 <- (Belowground$AhorN.kg_m2+Belowground$MinN.kg_m2)/2
Belowground2 <- full_join(Belowground,Soil.texture)
names(Belowground2)
names(Belowground)
Belowground <- Belowground2[,c(1:5,7:21,25:29,31:34)]

Block.Eco.C <- read.csv("Ecosystem carbon/Ecosystem.Carbon.csv", head=T)

levels(Block.Eco.C$Carbon.pool)
levels(Block.Eco.C$Class)

# Rename the Carbon pool names 
Block.Eco.C$Carbon.pool<- factor(Block.Eco.C$Carbon.pool, levels = c("TreeC.kg_m2","HerbC.kg_m2", "DWC.kg_m2","SoilAC.kg_m2","SoilMC.kg_m2"))
Block.Eco.C$Region<- factor(Block.Eco.C$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

levels(Block.Eco.C$Carbon.pool) <- c("Woody","Herbaceous","Dead wood","Soil A-horizon","Soil Min-horizon")

# Fixing my belowground variable remember to updata this if I change the belowground data!!! 
Soil.min <- Block.Eco.C %>%
  filter(Carbon.pool=="Soil Min-horizon")
Soil.A <- Block.Eco.C %>%
  filter(Carbon.pool=="Soil A-horizon")
Belowground.block <- cbind(Soil.min,Soil.A[26])
colnames(Belowground.block)[29] <- "C.Ahor"
Belowground.block$tot.C.kg_m2 <- Belowground.block$C.amount + Belowground.block$C.Ahor

Belowground.full2 <- left_join(Belowground,Soil.min,by="Block.ID",drop=F)
names(Belowground.full2)
Belowground.full <- Belowground.full2[,c(1:11,15:29,41:52)]
Belowground.full$N.trees <- rowSums(Belowground.full[,c("Small.N", "Large.N")], na.rm=TRUE)
Belowground.full$BM.N.trees.m2 <- rowSums(Belowground.full[,c("BM.Small.N.m2", "BM.Large.N.m2")], na.rm=TRUE)
Belowground.full$non.N.trees <- rowSums(Belowground.full[,c("Small.N.non", "Large.N.non")], na.rm=TRUE)
Belowground.full$BM.non.N.trees.m2 <- rowSums(Belowground.full[,c("BM.Small.non.m2", "BM.Large.non.m2")], na.rm=TRUE)

colnames(Belowground.full)[c(1,8,11,23:25)] <- c("Region","landuse","MAP.mm_yr","Clay","Silt","Sand")

SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# On Block size
Soil.Block <- cbind((aggregate(Block.ID~Region+Block+Horizon,data=Soilred,mean)),
                    (aggregate(MAP.mm_yr~Region+Block+Horizon,data=Soilred,mean))[4],
                    (aggregate(C.kg_m2~Region+Block+Horizon,data=Soilred,mean))[4],
                     (aggregate(C.kg_m2~Region+Block+Horizon,data=Soilred,SE))[4],
                     #(aggregate(C.kg_m2~Region+Block,data=MinHorizon,mean))[3],
                     #(aggregate(C.kg_m2~Region+Block,data=MinHorizon,SE))[3],
                     (aggregate(N.kg_m2~Region+Block+Horizon,data=Soilred,mean))[4],
                     (aggregate(N.kg_m2~Region+Block+Horizon,data=Soilred,SE))[4])
                    
                     #(aggregate(N.kg_m2~Region+Block,data=MinHorizon,mean))[3],
                     #(aggregate(N.kg_m2~Region+Block,data=MinHorizon,SE))[3])

colnames(Soil.Block) <- c("Region","Block","Horizon","Block.ID","MAP","C.kg_m2","SE.C.kg_m2","N.kg_m2","SE.N.kg_m2")
#Clay <- aggregate(Clay.per~Region+Block+Horizon,data=Soilred,unique)
#Silt <- aggregate(Silt.per~Region+Block+Horizon,data=Soilred,unique)
#Sand <- aggregate(Sand.per~Region+Block+Horizon,data=Soilred,unique)
Last.fire <- aggregate(Last_fire.yr~Region+Block+Horizon,data=Soilred,unique)
Fire.freq <- aggregate(Fire_frequency.2000_2017~Region+Block+Horizon,data=Soilred,unique)
ID <- Soil.Block[c(1,2,3)]
#Clay.full <- full_join(ID,Clay)
#Silt.full <- full_join(ID,Silt)
#Sand.full <- full_join(ID,Sand)
Last.fire <- full_join(ID,Last.fire)
Fire.freq <- full_join(ID,Fire.freq)

SoilBlock <- cbind(Soil.Block,Last.fire[4],Fire.freq[4])

SoilBlock <- SoilBlock[
  order(SoilBlock[,1], SoilBlock[,2] ),
  ]

SoilBlock$Landuse <- as.factor(c("Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Pasture","Wild","Wild","Wild","Wild","Wild","Wild","Wild","Wild"))

# CSV files for furhter use 
write.csv(SoilBlock,file="Ecosystem carbon/Soil.data/Soil.Carbon.Block.csv")
write.csv(Belowground.full,file="Ecosystem carbon/Soil.data/Belowground.Carbon.csv")

### Soil properties ####
# Want to have a table with SOIL TEXTURE (clay, silt and sand) and chemical traits for the analysis I did - now have data from Anders and Stu also - not included here..  
# First, reorganizing and removing collumns 
names(total.soil.data)
soil.properties.full <- total.soil.data[,c(1:7,19,28,31:34,41:48)]
names(soil.properties.full)

soil.properties.full$Region<- factor(soil.properties.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

soil.properties.full$Land_Use<- factor(soil.properties.full$Land_Use, levels = c("Pasture","Wild"))

soil.properties.full2 <- na.omit(soil.properties.full)


#Making a table, by using the aggregata function - taking the mean values of texture by region 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

Soil.properties <- cbind((aggregate(Clay.per~Block+Region,soil.properties.full,mean)),
                         (aggregate(Clay.per~Block+Region,soil.properties.full,SE))[3], 
                         (aggregate(Sand.per~Block+Region,soil.properties.full,mean))[3],
                         (aggregate(Sand.per~Block+Region,soil.properties.full,SE))[3],
                         (aggregate(Silt.per~Block+Region,soil.properties.full,mean))[3],
                         (aggregate(Silt.per~Block+Region,soil.properties.full,SE))[3],
                         (aggregate(CEC.cmol_kg~Block+Region,soil.properties.full,mean))[3],
                         (aggregate(CEC.cmol_kg~Block+Region,soil.properties.full,SE))[3],
                         (aggregate(Al.g_kg~Block+Region,soil.properties.full,mean))[3],
                         (aggregate(Al.g_kg~Block+Region,soil.properties.full,SE))[3],
                         (aggregate(Fe.g_kg~Block+Region,soil.properties.full,mean))[3],
                         (aggregate(Fe.g_kg~Block+Region,soil.properties.full,SE))[3],
                         (aggregate(P.g_kg~Block+Region,soil.properties.full,mean))[3],
                         (aggregate(P.g_kg~Block+Region,soil.properties.full,SE))[3])

colnames(Soil.properties)<-c("Block","Region","Clay","Clay.SE","Sand","Sand.SE","Silt","Silt.SE","CEC","CEC.SE","Al","Al.SE","Fe","Fe.SE","P","P.SE")

#pH<-aggregate(pH~Block+Region,soil.properties,mean)
#SE.pH<-aggregate(pH~Block+Region,soil.properties,SE)

write.csv(Soil.properties,file="Ecosystem carbon/Soil.data/Soil.Properties.csv")

#### Exploring soil C and N ####

plot(Tot.C.per~Region,data=total.soil.data)
plot(C.kg_m2~Region,data=total.soil.data)

total.soil.data$Region<- factor(total.soil.data$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)

Soil.carbon.block <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.Block.csv",head=T)
#soil.carbon.region <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.Region.csv",head=T)
Soil.full <- read.csv(file="Ecosystem carbon/Soil.data/Total.Soil.Data.csv",head=T)

Soil.carbon.block$Region<- factor(Soil.carbon.block$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
Soil.full$Region<- factor(Soil.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

#soil.carbon.region$Region<- factor(soil.carbon.region$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))
par(mfrow=c(1,2)) 
plot(C.kg_m2~Land_Use, data= Soil.full)
plot(N.kg_m2~Land_Use, data= Soil.full)

plot(C.kg_m2~Region,data=Soil.carbon.block)
plot(N.kg_m2~Region,data=Soil.carbon.block)

summary(lm(C.kg_m2/N.kg_m2~Landuse,data=Soil.carbon.block))

# Dotchart C and N (g/m2)
par(mfrow=c(2,2))
dotchart(Soil.full$C.kg_m2,groups = Soil.full$Region, main= "C Region")
dotchart(Soil.full$C.kg_m2, groups=Soil.full$Land_Use,main = "C landuse")
str(Soil.full)
dotchart(Soil.full$N.kg_m2, groups=Soil.full$Region,main = "N Region")
dotchart(Soil.full$N.kg_m2,groups=Soil.full$Land_Use,main = "N landuse")
dotchart(Soil.full$C.kg_m2/Soil.full$N.kg_m2,groups=Soil.full$Land_Use,main = "landuse")

# Dotchart per horizon based on total C and N (%)
dotchart(Soil.full$Tot.C.per, groups=Soil.full$Horizon, main= "Carbon pool")
dotchart(Soil.full$Tot.N.per, groups=Soil.full$Horizon, main= "Nitrogen pool")

par(mfrow=c(1,1))

hist(SoilC$C.g_m2[SoilC$Region=="Handejega"])
hist(Soil.carbon.block$C.kg_m2)

# Graphs 

Soil.carbon.block$Landuse<- factor(Soil.carbon.block$Landuse, levels = c("Pasture","Wild"))
Soil.full$Land_Use<- factor(Soil.full$Land_Use, levels = c("Pasture","Wild"))
Soil.full <- droplevels(Soil.full)

legend_titleLAND <- "Land-use"
legend_titleCarbon <- "Carbon Pool"

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

SoilCSand<- ggplot(data = Soil.full, aes(x = Sand.per,y = C.kg_m2,colour= Horizon, shape= Land_Use))

SoilCSand + xlab("Sand (%)") + ylab(expression(paste("Soil Carbon (g", m^-2,")")))  + 
  geom_point(fill="white",size=4,stroke=1.2,show.legend=T) 
  scale_shape_manual(legend_titleLAND,values=c(16,0)) +
  scale_color_manual(legend_titleCarbon, breaks = c("A-Hor","Min-Hor","O-Hor"),values=c("darkgoldenrod","salmon4","burlywood4")) + 
  theme_bw() + Lines_gone

# Clay, correlation with MAP, fire, land-use?

# Making a simple plot of clay as a function of MAP 
par(mfrow=c(2,2))
plot(Clay.per~MAP.mm_yr, data=Soil.full,col=Soil.full$Region)
plot(Clay.per~factor(Land_Use), data=Soil.full)
plot(Clay.per~Last_fire.yr, data=Soil.full)
plot(Clay.per~Fire_frequency.2000_2017, data=Soil.full)

summary(lm(Clay.per~MAP.mm_yr, data=Soil.full))

# Boxplot illustrating difference in clay per site with colors indicating amount of MAP
par(mfrow=c(1,1))
plot(Clay.per~factor(Region), 
     xlab= "Region",
     ylab= "Clay (%)",
     data=Soil.full,
     border=c("darkgoldenrod2","darkgoldenrod2","blueviolet","blueviolet","palegreen3","darkcyan","darkcyan"))

#### Bulk Density exploration ####
# Making a table with BD and clay to see if they are correlated.
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Clay <- aggregate(Clay.per~Horizon+Region, data=MAP.clay,mean)
Clay.se <- aggregate(Clay.per~Horizon+Region, data=MAP.clay,SE)

Clay.BD <- cbind(BD.horizon,Clay[3],Clay.se[3])
colnames(Clay.BD) <- c("Horizon","Region","BD","BD.se","Clay","Clay.se")

# Checking for correlation between Clay and BD to see if I can use BD instead of clay 

par(mfrow=c(2,2))
Model.clay.BD <- lm(BD~Clay,data=Clay.BD)
summary(Model.clay.BD) # significant 
plot(Model.clay.BD)

par(mfrow=c(1,1))
plot(Clay~BD, data=Clay.BD)

# Creating a dataframe for clay with the fit of the model, and the SE of the fit. 
xseq <- seq(from=min(Clay.BD$Clay),to=max(Clay.BD$Clay), 1)
CorrLine<-data.frame(Clay= xseq, predict(Model.clay.BD, data.frame(Clay=xseq), se.fit= TRUE))

# Plot prediction line using ggplot and geom_ribbon 

Clay.BD.plot <- ggplot(data = Clay.BD, aes(x = Clay,y = BD, group_by(Horizon)))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

Clay.BD.plot +
  geom_point(aes(shape=factor(Horizon),stroke=3,color=Region)) +
  geom_line(data=CorrLine, aes(Clay, fit))+
  geom_ribbon(data=CorrLine, aes(ymin=fit-se.fit, ymax=fit+se.fit, x = Clay), alpha=0.4,inherit.aes = FALSE)+
  theme_bw() +
  Lines_gone

total.soil.data<- read.csv("Ecosystem Carbon/Total.soil.data.csv", head = TRUE)
names(total.soil.data)

BD.total <- total.soil.data[,c(1:7,14,23)]
tail(BD.total)
BD.total <- na.omit(BD.total)
# Remove O-hor as I only have this for very few plots. 
BD.total <- BD.total[BD.total$Horizon!="O-hor",]
BD.total <- droplevels(BD.total)

# Look at the difference between A-horizon, Min and O-Horizon 
plot(BD_fine_earth_air_dry~factor(Horizon), data=BD.total)

# Look at BD per land-use
plot(BD_fine_earth_air_dry~factor(Land_Use), data=BD.total)

# Look at BD per Region 
levels(BD.total$Region)
BD.total$Region <- factor(BD.total$Region,levels = c("Makao","Maswa","Mwantimba","Handajega", "Seronera","Park Nyigoti","Ikorongo"))

# Look at BD per region for the different horizons 
# SE function to use in R 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
BD.SE <- aggregate(BD_fine_earth_air_dry~Horizon+Region,data=BD.total,SE)
BD <- aggregate(BD_fine_earth_air_dry~Horizon+Region,data=BD.total,mean)

BD.horizon <- cbind(BD,BD.SE[3])
colnames(BD.horizon)[4] <- "BD_SE" 
colnames(BD.horizon)[3] <- "BD"

# Per Region
BD.plot.region <- ggplot(data=BD.total, aes(x = Region,y = BD_fine_earth_air_dry))
BD.plot.region + geom_boxplot()

# and Horizon
BD.plot.horizon <- ggplot(data = BD.horizon, aes(x = Region,y = BD, ymin=BD-BD_SE,ymax=BD+BD_SE, group = Horizon, colour= Horizon))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

BD.plot.horizon + geom_point(size = 3, shape=20,stroke=2)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) 


# Uploading the table of Bulk density per block and Bulk density "control"
Bulk.density <- read.csv("Ecosystem Carbon/02BulkSoil.csv", head=T)
names(Bulk.density)

tail(Bulk.density) # A lot of NA rows
Bulk.density2 <- Bulk.density[,c(1:7)]  
Bulk.density2 <- na.omit(Bulk.density2)# Remove NA rows

# making vectors
BD.average <- Bulk.density2$BD.average.block_g.cm3
BD.controll <- Bulk.density2$BD.controll_g.cm3

# making a lm 
BD.model <- lm(BD.average~BD.controll,Bulk.density2)
summary(BD.model)
par(mfrow=(c(2,2)))
plot(BD.model) # outlayers: 6 (Mwantimba B2),9 (Handajega B2),13 (Park Nyigoti B2)

# Prediction line
av<-seq(0,1.5, length.out=15)
Bulkpred.lm<-predict(BD.model, list(BD.controll=av),
                     se.fit=T) # Se fit gives 95% confience interval estimates (standard error)
length(Bulkpred.lm)

# Plot + prediction line # This is nicer/easier in ggplot with geom_ribbon...
par(mfrow=(c(1,1)))
plot(BD.average~BD.controll, data =Bulk.density)
abline(c(0,1), col="black", lwd=3)
lines(av,Bulkpred.lm$fit+Bulkpred.lm$se.fit,lty = 2, lwd =1.75, col = "red") # upper se-line 
lines(av,Bulkpred.lm$fit-Bulkpred.lm$se.fit,lty = 2, lwd =1.75, col = "red") # lower se-line
lines(av,Bulkpred.lm$fit,lty = 1, lwd =1.75, col = "red") # line of best fit

# This is interactive to identify missing outliers - outside error
identify(Bulk.density$BD.average~Bulk.density$BD.controll)
# 9 and 13 are large outliers
Bulk.density[9,] # Handajega   S4, B2 
Bulk.density[13,] # Park Nyigoti   S6, B2 
# I have tried to look at both of these outliers to check if I have made some mistakes - but the values seems to be correct Handajega B2 have heavy samples in general, while Park Nyigoti B2 have light samples in general. 

# Aggrigate BD per region 
BD.average_region <- aggregate(BD.average.block_g.cm3~Region,Bulk.density2,mean)
BD.control_region <- aggregate(BD.controll_g.cm3~Region,Bulk.density2,mean)

# Making a dataset of BD per region 
BD.region <- cbind(BD.average_region,BD.control_region[2])
colnames(BD.region) <- c("Region","BD.average","BD.control")

# Looking for correlation
BD.model2 <- lm(BD.average~BD.control, data=BD.region)
summary(BD.model2)
par(mfrow=(c(2,2)))
plot(BD.model2)




