####Making tables####

# Uploading the soil file
total.soil.data<- read.csv("Ecosystem Carbon/Total.soil.data.csv", head = TRUE)
names(total.soil.data)

# Want to have a table with SOIL TEXTURE (clay, silt and sand) and chemical traits
# First, reorganizing and removing collumns 
soil.chemistry <- total.soil.data[,c(1:5,30:32,36:41)]
names(soil.chemistry)

# Can do this to make sure R knows that Region, Site, Block and Circle are factors 
#soil.chemistry$fRegion <- as.factor(soil.chemistry$Region)
#soil.chemistry$fSite <- as.factor(soil.chemistry$Site)
#soil.chemistry$fBlock <- as.factor(soil.chemistry$Block)
#soil.chemistry$fCircle <- as.factor(soil.chemistry$Circle)

#Making a table, by using the aggregata function - taking the mean values of texture by region 

Clay<-aggregate(Clay.per~Region,soil.chemistry,mean)
Clay.sd<-aggregate(Clay.per~Region,soil.chemistry,sd)

Sand<-aggregate(Sand.per~Region,soil.chemistry,mean)
Sand.sd<-aggregate(Sand.per~Region,soil.chemistry,sd)

Silt<-aggregate(Silt.per~Region,soil.chemistry,mean)
Silt.sd<-aggregate(Silt.per~Region,soil.chemistry,sd)

pH<-aggregate(pH~Region,soil.chemistry,mean)
pH.sd<-aggregate(pH~Region,soil.chemistry,sd)

CEC<-aggregate(CEC.cmol_kg~Region,soil.chemistry,mean)
CEC.sd<-aggregate(CEC.cmol_kg~Region,soil.chemistry,sd)

Al<-aggregate(Al.mg_kg~Region,soil.chemistry,mean)
Al.sd<-aggregate(Al.mg_kg~Region,soil.chemistry,sd)

Fe<-aggregate(Fe.mg_kg~Region,soil.chemistry,mean)
Fe.sd<-aggregate(Fe.mg_kg~Region,soil.chemistry,sd)

P<-aggregate(P.mg_kg~Region,soil.chemistry,mean)
P.sd<-aggregate(P.mg_kg~Region,soil.chemistry,sd)

# using cbind to make one table, then colnames to change the table names 
SoilcheSummary<-cbind(Clay,Clay.sd[2],Silt[2],Silt.sd[2],Sand[2],Sand.sd[2],pH[2],pH.sd[2],CEC[2],CEC.sd[2],Al[2],Al.sd[2],Fe[2],Fe.sd[2],P[2],P.sd[2])
colnames(SoilcheSummary)<-c("Region","Clay","Clay.sd","Silt","Silt.sd","Sand","Sand.sd","pH","pH.sd","CEC","CEC.sd","Al","Al.sd","Fe","Fe.sd","P","P.sd")

SoilcheSummary

# Want to have a table with region, land use, year of last fire, fire frequency, MAP, altitude 

# First, reorganizing and removing collumns 
site.traits <- total.soil.data[,c(1:5,13:17)]
names(site.traits)

site.traits$fRegion <- as.factor(site.traits$Region)
site.traits$fLand_Use <- as.factor(site.traits$Land_Use)
#soil.chemistry$fSite <- as.factor(soil.chemistry$Site)
#soil.chemistry$fBlock <- as.factor(soil.chemistry$Block)
#soil.chemistry$fCircle <- as.factor(soil.chemistry$Circle)

# aggrigate 
MAP <- aggregate(MAP.mm_yr~fRegion,site.traits,mean)
MAP.sd <- aggregate(MAP.mm_yr~fRegion,site.traits,sd)

Altitude <- aggregate(Altitude~fRegion,site.traits,mean)
Altitude.sd <- aggregate(Altitude~fRegion,site.traits,sd)

Last.Fire <- aggregate(Last_fire.yr~fRegion,site.traits,mean)
Last.Fire.sd <- aggregate(Last_fire.yr~fRegion,site.traits,sd)
  
Fire.freq <- aggregate(Fire_frequency.2000_2017~fRegion,site.traits,mean)
Fire.freq.sd <- aggregate(Fire_frequency.2000_2017~fRegion,site.traits,sd)

# using cbind to make one table, then colnames to change the table names 
SoiltraitsSummary<-cbind(MAP,Altitude[2],Last.Fire[2],Fire.freq[2])
colnames(SoiltraitsSummary)<-c("Region","MAP (mm/yr)","Altitude","Year of last fire","Fire frequescy") 

SoiltraitsSummary

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
MAP.clay <- na.omit(MAP.clay.total) # removing NAs

levels(MAP.clay$Region)
MAP.clay$Region <- factor(MAP.clay$Region,levels = c("Makao","Maswa","Mwantimba","Handajega", "Seronera","Park Nyigoti","Ikorongo"))

#write.csv(MAP.clay,file="Ecosystem carbon/MAP.clay.csv" )

# Making a simple plot of clay as a function of MAP 
par(mfrow=c(2,2))
plot(Clay.per~MAP.mm_yr, data=MAP.clay)
plot(Clay.per~factor(Land_Use), data=MAP.clay)
plot(Clay.per~Last_fire.yr, data=MAP.clay)
plot(Clay.per~Fire_frequency.2000_2017, data=MAP.clay)

# Boxplot illustrating difference in clay per site with colors indicating amount of MAP
par(mfrow=c(1,1))
plot(Clay.per~factor(Region), 
     xlab= "Region",
     ylab= "Clay (%)",
     data=MAP.clay,
     border=c("darkgoldenrod2","darkgoldenrod2","blueviolet","blueviolet","palegreen3","darkcyan","darkcyan"))


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


