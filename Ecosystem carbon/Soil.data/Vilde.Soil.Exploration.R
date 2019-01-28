####Making tables####

# Uploading the soil file
total.soil.data<- read.csv("Ecosystem Carbon/Soil.data/Total.Soil.Data.csv", head = TRUE)
names(total.soil.data)

# Comparing data from NTNU with data from NMBU
compare1 <- read.csv("Ecosystem Carbon/Soil.data/TBS.NMBU.csv", head=T)
summary(lm(C.per.TBS~C.per.NMBU, data=compare1))
cor.test(compare$C.per.TBS,compare$C.per.NMBU, method=c("pearson"))

# Comparing my data with Stuart`s data 
compare2 <- read.csv("Ecosystem Carbon/Soil.data/C.Stu.Vilde.csv", head=T)
str(compare2)

compare2$Soil.C.Vilde <- as.numeric(compare2$Soil.C.Vilde)

summary(lm(Soil.C.Vilde~Soil.C.Stu, data=compare2))
cor.test(compare2$Soil.C.Vilde,compare2$Soil.C.Stu, method=c("pearson"))

# Want to have a table with SOIL TEXTURE (clay, silt and sand) and chemical traits for the analysis I did - now have data from Anders and Stu also - not included here..  
# First, reorganizing and removing collumns 
soil.properties.full <- total.soil.data[,c(1:7,16,26:34,41:45)]
names(soil.properties.full)
tail(soil.properties.full)

soil.properties.full$Region<- factor(soil.properties.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

soil.properties.full$Land_Use<- factor(soil.properties.full$Land_Use, levels = c("Pasture","Wild"))

soil.properties.full <- na.omit(soil.properties.full)


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

# Making tables for C and N 
Soilfull <- read.csv(file="Ecosystem carbon/Soil.data/Total.Soil.Data.csv",header=T)
names(Soilfull)
Soilred <- Soilfull[,c(2:7,16:19,32:34,36,39)]
Soilred$Region<- factor(Soilred$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

#AHorizon <- Soilred[Soilred$Horizon=="A-hor",]
#MinHorizon <- Soilred[Soilred$Horizon=="Min-hor",]
#OrgHorizon <- Soilred[Soilred$Horizon=="O-hor",]
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
library(dplyr)

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

write.csv(SoilBlock,file="Ecosystem carbon/Soil.data/Soil.Carbon.Block.csv")


### On region size #### 
Soil.Region <- cbind((aggregate(Clay.per~Region+Land_Use,data=Soilred,mean)),
              (aggregate(Clay.per~Region+Land_Use,data=Soilred,SE))[3],
              (aggregate(Silt.per~Region+Land_Use,data=Soilred,mean))[3],
              (aggregate(Silt.per~Region+Land_Use,data=Soilred,SE))[3],
              (aggregate(Sand.per~Region+Land_Use,data=Soilred,mean))[3],
              (aggregate(Sand.per~Region+Land_Use,data=Soilred,SE))[3],
              #(aggregate(C.g_m2~Region+Land_Use,data=OrgHorizon,mean))[3],
              #(aggregate(C.g_m2~Region+Land_Use,data=OrgHorizon,SE))[3],
              (aggregate(C.kg_m2~Region+Land_Use,data=AHorizon,mean))[3],
              (aggregate(C.kg_m2~Region+Land_Use,data=AHorizon,SE))[3],
              (aggregate(C.kg_m2~Region+Land_Use,data=MinHorizon,mean))[3],
              (aggregate(C.kg_m2~Region+Land_Use,data=MinHorizon,SE))[3],
              (aggregate(N.kg_m2~Region+Land_Use,data=AHorizon,mean))[3],
              (aggregate(N.kg_m2~Region+Land_Use,data=AHorizon,SE))[3],
              (aggregate(N.kg_m2~Region+Land_Use,data=MinHorizon,mean))[3],
              (aggregate(N.kg_m2~Region+Land_Use,data=MinHorizon,SE))[3])

colnames(Soil.Region) <- c("Region","Landuse","Clay","SE.Clay","Silt","SE.Silt","Sand","SE.Sand","CAHor","SE.CAHor","CMinHor","SE.CMinHor","NAHor","SE.NAHor","NMinHor","SE.NMinHor")
write.csv(Soil.Region,file="Ecosystem carbon/Soil.data/Soil.Carbon.Region.csv")

#### Exploring soil C and N ####

library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)

Soil.carbon.block <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.Block.csv",head=T)
#soil.carbon.region <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.Region.csv",head=T)
Soil.full <- read.csv(file="Ecosystem carbon/Soil.data/Total.Soil.Data.csv",head=T)

Soil.carbon.block$Region<- factor(Soil.carbon.block$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

#soil.carbon.region$Region<- factor(soil.carbon.region$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

Soil.carbon.block$Landuse<- factor(Soil.carbon.block$Landuse, levels = c("Pasture","Wild"))
Soil.full$Land_Use<- factor(Soil.full$Land_Use, levels = c("Pasture","Wild"))
Soil.full <- droplevels(Soil.full)

par(mfrow=c(1,2)) 
plot(C.kg_m2~Land_Use, data= Soil.full)
plot(N.kg_m2~Land_Use, data= Soil.full)

plot(C.kg_m2~Region,data=Soil.carbon.block)
plot(N.kg_m2~Region,data=Soil.carbon.block)

summary(lm(C.kg_m2/N.kg_m2~Landuse,data=Soil.carbon.block))

# Dotchart C and N (g/m2)
dotchart(Soil.full$C.kg_m2, groups=Soil.full$Region,main = "Region")
dotchart(Soil.full$C.kg_m2, groups=Soil.full$Land_Use,main = "landuse")

dotchart(Soil.full$N.kg_m2, groups=Soil.full$Region,main = "Region")
dotchart(Soil.full$N.kg_m2,groups=Soil.full$Land_Use,main = "landuse")
dotchart(Soil.full$C.kg_m2/Soil.full$N.kg_m2,groups=Soil.full$Land_Use,main = "landuse")

# Dotchart per horizon based on total C and N (%)
dotchart(Soil.full$Tot.C.per, groups=Soil.full$Horizon, main= "Carbon pool")
dotchart(Soil.full$Tot.N.per, groups=Soil.full$Horizon, main= "Nitrogen pool")

par(mfrow=c(1,1))

hist(SoilC$C.g_m2[SoilC$Region=="Handejega"])
hist(Soil.carbon.block$C.kg_m2)

# Graphs 

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
  
#### Making models####

# Making basic lm to check for correlations
SandMAP <- lm(C.kg_m2~Sand.per*MAP.mm_yr, data=Soil.full)
summary(SandMAP)

ClayMAP <- lm(C.kg_m2~Clay.per*MAP.mm_yr, data=Soil.full)
summary(ClayMAP)

SandLanduse <- lm(C.kg_m2~Sand.per*factor(Land_Use), data=Soil.full)
summary(SandLanduse)

ClayLanduse <- lm(C.kg_m2~Clay.per*factor(Land_Use), data=Soil.full)
summary(ClayLanduse)

#### Bulk Density exploration ####

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
library(ggplot2)
library(dplyr)

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




