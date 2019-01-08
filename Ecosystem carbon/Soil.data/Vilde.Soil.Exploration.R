####Making tables####

# Uploading the soil file
total.soil.data<- read.csv("Ecosystem Carbon/Soil.data/Total.Soil.Data.csv", head = TRUE)
names(total.soil.data)

compare <- read.csv("Ecosystem Carbon/Soil.data/TBS.NMBU.csv", head=T)

summary(lm(C.per.TBS~C.per.NMBU, data=compare))
cor.test(compare$C.per.TBS,compare$C.per.NMBU, method=c("pearson"))

# Want to have a table with SOIL TEXTURE (clay, silt and sand) and chemical traits
# First, reorganizing and removing collumns 
soil.properties.full <- total.soil.data[,c(1:6,14:17,8,23,30:44)]
names(soil.properties.full)
tail(soil.properties.full)
soil.properties.full <- droplevels(soil.properties.full)
soil.properties.full$Region<- factor(soil.properties.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

soil.properties.full$Land_Use<- factor(soil.properties.full$Land_Use, levels = c("Pasture","Wild"))

# Properties of pH 
max(soil.properties$pH,na.rm = T)
min(soil.properties$pH,na.rm = T)
# pH range 5.68 - 8.4

#Making a table, by using the aggregata function - taking the mean values of texture by region 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

Soil.properties <- cbind((aggregate(MAP.mm_yr~Block+Region,soil.properties.full,mean, na.action = na.pass)),
                         (aggregate(Last_fire.yr~Block+Region,soil.properties.full,mean, na.action = na.pass))[3], 
                (aggregate(Fire_frequency.2000_2017~Block+Region,soil.properties.full,mean, na.action=na.pass))[3], 
                         (aggregate(Clay.per~Block+Region,soil.properties.full,mean))[3],
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

colnames(Soil.properties)<-c("Block","Region","MAP","Last.fire","Fire.freq","Clay","Clay.SE","Sand","Sand.SE","Silt","Silt.SE","CEC","CEC.SE","Al","Al.SE","Fe","Fe.SE","P","P.SE")
#pH<-aggregate(pH~Block+Region,soil.properties,mean)
#SE.pH<-aggregate(pH~Block+Region,soil.properties,SE)

write.csv(Soil.properties,file="Ecosystem carbon/Soil.data/Soil.Properties.csv")

# Making a table for soil C and N 
Soilfull <- read.csv(file="Ecosystem carbon/Soil.data/Total.Soil.Data.csv",header=T)
names(Soilfull)
Soilred <- Soilfull[,c(2:4,6,14,31:34,37)]
AHorizon <- Soilred[Soilred$Horizon=="A-hor",]
MinHorizon <- Soilred[Soilred$Horizon=="Min-hor",]
OrgHorizon <- Soilred[Soilred$Horizon=="O-hor",]
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Soil <- cbind((aggregate(Clay.per~Region+Land_Use,data=Soilred,mean)),
              (aggregate(Clay.per~Region+Land_Use,data=Soilred,SE))[3],
              (aggregate(Silt.per~Region+Land_Use,data=Soilred,mean))[3],
              (aggregate(Silt.per~Region+Land_Use,data=Soilred,SE))[3],
              (aggregate(Sand.per~Region+Land_Use,data=Soilred,mean))[3],
              (aggregate(Sand.per~Region+Land_Use,data=Soilred,SE))[3],
              #(aggregate(C.g_m2~Region+Land_Use,data=OrgHorizon,mean))[3],
              #(aggregate(C.g_m2~Region+Land_Use,data=OrgHorizon,SE))[3],
              (aggregate(C.g_m2~Region+Land_Use,data=AHorizon,mean))[3],
              (aggregate(C.g_m2~Region+Land_Use,data=AHorizon,SE))[3],
              (aggregate(C.g_m2~Region+Land_Use,data=MinHorizon,mean))[3],
              (aggregate(C.g_m2~Region+Land_Use,data=MinHorizon,SE))[3],
              (aggregate(N.g.m2~Region+Land_Use,data=Soilred,mean))[3],
              (aggregate(N.g.m2~Region+Land_Use,data=Soilred,SE))[3])

colnames(Soil) <- c("Region","Landuse","Clay","SE.Clay","Silt","SE.Silt","Sand","SE.Sand","CAHor","SE.CAHor","CMinHor","SE.CMinHor","N","SE.N")
write.csv(Soil,file="Ecosystem carbon/Soil.data/Soil.Carbon.csv")

#### Ploting soil C #### 
Soil.carbon <- read.csv(file="Ecosystem carbon/Soil.data/Soil.Carbon.csv",head=T)
soil.full <- read.csv(file="Ecosystem carbon/Soil.data/Total.Soil.Data.csv",head=T)

soil.full$Region<- factor(soil.properties.full$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera","Park Nyigoti","Ikorongo"))

soil.full$Land_Use<- factor(soil.properties.full$Land_Use, levels = c("Pasture","Wild"))

par(mfrow=c(1,2))
plot(Tot.C.per~Land_Use, data= soil.full)
plot(Tot.N.per~Land_Use, data= soil.full)

plot(Tot.C.per~Region,data=soil.full)
plot(Tot.N.per~Region,data=soil.full)

levels(soil.full)

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

#### Clay, correlation with MAP, fire, land-use? ####

total.soil.data<- read.csv("Ecosystem Carbon/Total.soil.data.csv", head = TRUE)
names(total.soil.data)

MAP.clay.total <- total.soil.data[,c(1:6,14:16,17,30)] #reducing the dataset
names(MAP.clay.total) 
tail(MAP.clay.total)
# Exclude all with NA 
MAP.clay.fire <- na.omit(MAP.clay.total) # removing NAs

# Making a dataset without fire 
MAP.clay <- total.soil.data[,c(1:6,14,17,30)]
MAP.clay <- na.omit(MAP.clay)

levels(MAP.clay$Region)
MAP.clay$Region <- factor(MAP.clay$Region,levels = c("Makao","Maswa","Mwantimba","Handajega", "Seronera","Park Nyigoti","Ikorongo"))

#write.csv(MAP.clay,file="Ecosystem carbon/MAP.clay.csv" )

# Making a simple plot of clay as a function of MAP 
par(mfrow=c(2,2))
plot(Clay.per~MAP.mm_yr, data=MAP.clay,col=MAP.clay$Region)
plot(Clay.per~factor(Land_Use), data=MAP.clay)
plot(Clay.per~Last_fire.yr, data=MAP.clay.fire)
plot(Clay.per~Fire_frequency.2000_2017, data=MAP.clay.fire)

summary(lm(Clay.per~MAP.mm_yr, data=MAP.clay))

# Boxplot illustrating difference in clay per site with colors indicating amount of MAP
par(mfrow=c(1,1))
plot(Clay.per~factor(Region), 
     xlab= "Region",
     ylab= "Clay (%)",
     data=MAP.clay,
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
  


#### Making models (Not doing yet!!) ####
# Making a lm to check what is affecting clay.
#summary(lm(Clay.per~MAP.mm_yr, data=MAP.clay)) # MAP not significant 
#total.model <- lm(Clay.per~MAP.mm_yr*factor(Land_Use)*Last_fire.yr*Fire_frequency.2000_2017, data=MAP.clay) # all the "possible" explainatory variables, use drop1? 
#summary(total.model)
#summary(lm(Clay.per~factor(Land_Use)+Last_fire.yr, data=MAP.clay))
#summary(lm(Clay.per~Last_fire.yr*MAP.mm_yr, data=MAP.clay)) # Year of last fire is significant, and good to use together with MAP?

# making unique block id (factor) by using the paste function - creating block.id column with region and block together seperated by "_" 
#MAP.clay$Block.id <- as.factor(with(MAP.clay,paste(Region,Block,sep="_")))
# Then transforming each unique combination into a number
#MAP.clay$Block.id <- as.factor(as.numeric(MAP.clay$Block.id))
#summary(levels(MAP.clay$Block.id))

####Packages####
#library(lattice)
#library(MASS)
#library(dplyr)
#library(plyr)
#library(lubridate)
#library(data.table)
#library(xlsx)


