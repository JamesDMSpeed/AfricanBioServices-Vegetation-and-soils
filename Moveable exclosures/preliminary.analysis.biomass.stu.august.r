#### Preliminary analysis for report Sept 2018 ####
#Productivity and consumption #Hjelpefiler: ggplot2.training.R og Dataexploration.R
#setwd("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Data/R/Preliminary")
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Marit Arneberg")
getwd()

########################
#clear system & add package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(ggplot2)
library(dplyr)
library(gcookbook)
########################

# Import data with file name
#Data.Ex2<-read.delim("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Data/R/Preliminary/Exclosure.data.biomass.txt",header=TRUE)

Data.Ex2<-read.csv("Exclosure.data.biomass.csv", sep=",",header=TRUE)

dim(Data.Ex2) # 356  35
names(Data.Ex2)
head(Data.Ex2)
str(Data.Ex2)

#### Housekeeping ####
Data.Ex2$flanduse<-as.factor(Data.Ex2$landuse)
Data.Ex2$fregion<-as.factor(Data.Ex2$region)
Data.Ex2$fblock<-as.factor(Data.Ex2$block)
Data.Ex2$ftreatment<-as.factor(Data.Ex2$treatment)
Data.Ex2$fharvest<-as.factor(Data.Ex2$harvest)

# Removing EX2 from dataset - to be used later
DataEx<-Data.Ex2[Data.Ex2$treatment!="EX2",]
#DataEx<-DataEx[,-c(36:167)] #deleting extra columns from data frame
# Why you remove land-use etc...
DataEx<-DataEx[DataEx$treatment!="H0",]
# Remove onset H0

# Renaming productivity and consumption columnns
colnames(DataEx)[colnames(DataEx)=="productivity.target.g.m2.day"] <- "prodTarg"
colnames(DataEx)[colnames(DataEx)=="consumption.target.g.m2.day"] <- "consTarg"
colnames(DataEx)[colnames(DataEx)=="productivity.total.g.m2.day"] <- "prodTotal"
colnames(DataEx)[colnames(DataEx)=="consumption.total.g.m2.day"] <- "consTotal"


# Rdate create month column. default was (="%d.%m.%Y")
#MarRdate<-strptime(as.character(DataEx$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )# East African time
#class(MarRdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
#DataEx$Rdate<-MarRdate# Add to the dataframe
#DataEx$month<-DataEx$Rdate$mon+1 
#DataEx$fmonth<-as.factor(DataEx$month)
#DataEx$month<-month.abb[DataEx$month]

# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
MarDate<-strptime(as.character(DataEx$harvest.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
DataEx$YrMonth<-format(as.Date(MarDate), "%Y-%m")

# Remove first 20 rows where there is not a harvest date - i.e onset!
DataEx<-DataEx[!is.na(DataEx$YrMonth),]
DataEx$YrMonth # No NAs
dim(DataEx)
levels(DataEx$ftreatment)
DataEx<-droplevels(DataEx)
levels(DataEx$ftreatment) # Only Ex and OP

#### Data exploration ####
head(DataEx)
tail(DataEx)

# Exploration using new names!
hist(DataEx$prodTarg)
hist(DataEx$consTarg)
hist(DataEx$prodTotal) # Much wider variation here...
hist(DataEx$consTotal)

# Check for outliers
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2)) #mfrow several graphs on the same page
dotchart(DataEx$prodTarg,groups=as.factor(DataEx$plot.id)) # Outlier -4.29
dotchart(DataEx$prodTarg,groups=DataEx$flanduse,main = "flanduse") # Outlier -4.29

# Does this relate to consumption of target
dotchart(DataEx$consTarg,groups=DataEx$flanduse,main = "flanduse") # Outlier with high consumption

dotchart(DataEx$prodTotal,groups=as.factor(DataEx$plot.id)) # Outlier -4.29
dotchart(DataEx$prodTarg,groups=DataEx$flanduse,main = "flanduse") # 3.47 - is it same data point

plot(DataEx$prodTarg)
#identify(DataEx$prodTarg,groups=DataEx$plot.id,tolerance=5) #Identifisere outlier i figuren, viser n?r man trykker esc
DataEx[DataEx$prodTarg<=-2,"prodTarg"]
DataEx[273,]
# NAP Low value = -4.29. Low value!
# But cons target is 0.36 - same data point as extreme consumption - then why negative 

dotchart(DataEx$prodTotal,groups=as.factor(DataEx$plot.id)) # OK - no major issues
dotchart(DataEx$prodTotal,groups=DataEx$flanduse,main = "flanduse") # More variation in wildlife land-use

############################################################################################################
# BIOMASS THROUGH TIME MOVEABLE EXCLOSURES #
#### AVERAGES AND SE #### 

# Create a unique code for rainfall x land-use - follow through time
DataEx$landuseRain<-as.factor(with(DataEx, paste(region,flanduse, sep="_")))

# NET ABOVEGROUND PRODUCTIVITY
# Average of each site per harvest (8 productivity measures and 4 consumption measures)
# See aggregate for biomass done by Stu in Ggplot2.training.R
TotProd<-aggregate(prodTotal~region+flanduse+landuseRain+YrMonth+ftreatment,na.rm=T,DataEx,mean)
TotProd$prodTotal<-round(TotProd$prodTotal,digits=2)
colnames(TotProd)[6]<-"Productivity"
TotProd$pool<-"total"

TarProd<-aggregate(prodTarg~region+flanduse+landuseRain+YrMonth+ftreatment,na.rm=T,DataEx,mean)
TarProd$prodTarg<-round(TarProd$prodTarg,digits=2)  
colnames(TarProd)[6]<-"Productivity"
TarProd$pool<-"target"

# Rbind averages for totla and target species biommas
AvgProd<-rbind(TotProd,TarProd)

# Standard error
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TotProdSE<-aggregate(prodTotal~region+flanduse+landuseRain+YrMonth+ftreatment,DataEx,SE)
TotProdSE$prodTotal<-round(TotProdSE$prodTotal,digits=2)
colnames(TotProdSE)[6]<-"SE"
TotProdSE$pool<-"total"

TarProdSE<-aggregate(prodTarg~region+flanduse+landuseRain+YrMonth+ftreatment,DataEx,SE)
TarProdSE$prodTarg<-round(TarProdSE$prodTarg,digits=2)  
colnames(TarProdSE)[6]<-"SE"
TarProdSE$pool<-"target"

# Rbind averages for totla and target species biommas
SeProd<-rbind(TotProdSE,TarProdSE)
AvgProd$SE<-SeProd$SE
names(AvgProd)  

# Convert to date
AvgProd$YrMonth<-as.Date(paste(AvgProd$YrMonth,"-01",sep=""))

# Redo code to include differences in pool - linetype
AvgProd$landuseRain<-as.factor(with(AvgProd, paste(region,flanduse,ftreatment,pool, sep="_")))

# Remove SE from data
AvgProd2<-AvgProd[AvgProd$region!="SE",]
head(AvgProd2)
levels(AvgProd2$landuseRain)

# Biomass plot throught time
legend_title<-"land-use"
legend_title2<-"treatment"
dp<- ggplot(AvgProd2, aes(x=YrMonth, y=Productivity, colour=flanduse,shape=ftreatment,
                            linetype=pool,group=landuseRain))
dp<-dp+geom_line(size=1.2, alpha=.5, show.legend=F)
dp<-dp+geom_errorbar(aes(ymin=Productivity-SE, ymax=Productivity+SE),width=.2,lwd=1.1,show.legend=F)
dp<-dp+geom_point(size=4, fill="white", stroke=2)
dp<-dp+facet_wrap(~pool+region,ncol=2,scales='fixed')
dp<-dp+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(AvgProd$YrMonth),max=max(AvgProd$YrMonth))) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
dp<-dp+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
dp<-dp+ xlab("Time (month/year)") + ylab(expression(paste("Net Aboveground Productivity(g ",m^-2," ",day^-1,")")))
dp


######################################################################
# HERBIVORE CONSUMPTION
names(DataEx)
TotCons<-aggregate(consTotal~region+flanduse+landuseRain+YrMonth,na.rm=T,DataEx,mean)
TotCons$consTotal<-round(TotCons$consTotal,digits=2)
colnames(TotCons)[5]<-"Consumption"
TotCons$pool<-"total"

TarCons<-aggregate(consTarg~region+flanduse+landuseRain+YrMonth,na.rm=T,DataEx,mean)
TarCons$consTarg<-round(TarCons$consTarg,digits=2)  
colnames(TarCons)[5]<-"Consumption"
TarCons$pool<-"target"

# Rbind averages for totla and target species biommas
AvgCons<-rbind(TotCons,TarCons)

# Standard error
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

TotConsSE<-aggregate(consTotal~region+flanduse+landuseRain+YrMonth,DataEx,SE)
TotConsSE$consTotal<-round(TotConsSE$consTotal,digits=2)
colnames(TotConsSE)[5]<-"SE"
TotConsSE$pool<-"total"

TarConsSE<-aggregate(consTarg~region+flanduse+landuseRain+YrMonth,DataEx,SE)
TarConsSE$consTarg<-round(TarConsSE$consTarg,digits=2)  
colnames(TarConsSE)[5]<-"SE"
TarConsSE$pool<-"target"

# Rbind averages for totla and target species biommas
SeCons<-rbind(TotConsSE,TarConsSE)
AvgCons$SE<-SeCons$SE
names(AvgCons)  

# Convert to date
AvgCons$YrMonth<-as.Date(paste(AvgCons$YrMonth,"-01",sep=""))

# Redo code to include differences in pool - linetype
AvgCons$landuseRain<-as.factor(with(AvgCons, paste(region,flanduse,pool, sep="_")))

# Remove SE from data
AvgCons2<-AvgCons[AvgCons$region!="SE",]
head(AvgCons)
levels(AvgCons$landuseRain)

# NET ABOVEGROUND PRODUCTIVITY plot throught time
legend_title<-"land-use"
legend_title2<-"treatment"
cp<- ggplot(AvgCons2, aes(x=YrMonth, y=Consumption, colour=flanduse,shape=pool,
                          linetype=pool,group=landuseRain))
cp<-cp+geom_line(size=1.2, alpha=.5, show.legend=F)
cp<-cp+geom_errorbar(aes(ymin=Consumption-SE, ymax=Consumption+SE),width=.2,lwd=1.1,show.legend=F)
cp<-cp+geom_point(size=4, fill="white", stroke=2)
cp<-cp+facet_wrap(~pool+region,ncol=2,scales='fixed')
cp<-cp+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(AvgProd$YrMonth),max=max(AvgProd$YrMonth))) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
cp<-cp+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cp<-cp+xlab("Time (month/year)") + ylab(expression(paste("Net Aboveground Productivity(g ",m^-2," ",day^-1,")")))
cp

# RAINFALL SECOND Y AXIS
# Averaging rainfall data and getting SE by region # To be included in the Productivity aggregated dataframes per site
#per rainfall region (WET, SE , DRY)
RainRegion<-aggregate(rain.sum~region+YrMonth,DataEx,mean)
RainRegionSE<-aggregate(rain.sum~region+YrMonth,DataEx,SE)

RainRegionX<-cbind(RainRegion,RainRegionSE[,3])
colnames(RainRegionX)[4]<-"SE"

# Defining upper and lower limits
RainRegionX$SeUp<-RainRegionX$rain.sum+RainRegionX$SE
RainRegionX$SeLo<-RainRegionX$rain.sum-RainRegionX$SE

# Convert to date
RainRegionX$YrMonth<-as.Date(paste(RainRegionX$YrMonth,"-01",sep=""))

# Adding NAP AND rainfall to aggregated dataframes using left join. MUST MAKE THIS WORK!
AvgProd3<-left_join(AvgProd2,RainRegionX, by=c("region","YrMonth"),drop=F)
names(AvgProd3)

## Set values <0 to zero
#AvgProd3$Productivity[AvgProd3$Productivity<0.01]<-0

# Calculate error bars
#AvgProd3$SeUp<-AvgProd3$Productivity+AvgProd3$SE.x
#AvgProd3$SeLo<-AvgProd3$Productivity-AvgProd3$SE.x
#AvgProd3$SeUp[AvgProd3$SeUp<0.01]<-0
#AvgProd3$SeLo[AvgProd3$SeLo<0.01]<-0

# Remove target species
AvgProd4<-AvgProd3[AvgProd3$pool!="target",]

# Rename factors
AvgProd4<-droplevels(AvgProd4)
AvgProd4$region <- factor(AvgProd4$region, levels(AvgProd4$region)[c(2,1)])
levels(AvgProd4$region)<-c("Wet Region","Dry Region")
levels(AvgProd4$flanduse)<-c("pasture","wild")
levels(AvgProd4$ftreatment)<-c("exclosed","open")

# NAP AND RAINFALL
legend_title<-"land-use"
legend_title2<-"treatment"
dp<- ggplot(AvgProd4, aes(x=YrMonth, y=Productivity, colour=flanduse,shape=ftreatment,
                          group=landuseRain))
dp<-dp+ geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
dp<-dp+geom_line(aes(linetype=flanduse),size=1.2, alpha=.5, show.legend=F)
dp<-dp+geom_errorbar(aes(ymin=Productivity-SE.x, ymax=Productivity+SE.x),linetype="solid",width=.2,lwd=1.1,show.legend=F)
dp<-dp+geom_point(size=4, fill="white", stroke=2)
dp<-dp+facet_wrap(~region,ncol=2,scales='fixed', drop=F)
#dp<-dp+coord_capped_cart(left='both')
dp<-dp+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
dp<-dp+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
dp<-dp+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)
dp<-dp+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="white",size=.9,alpha=.1)
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
dp<-dp+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
dp<-dp+scale_shape_manual(legend_title2,values=c(22,21))
dp<-dp+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
dp<-dp+ xlab("") + ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
dp<-dp+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        #,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        ,axis.text.x=element_blank()
        ,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
dp<-dp+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
dp<-dp+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
dp

#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Marit Arneberg/Marit.NAP.jpeg",
#       width= 22, height = 14,units ="cm",
#       dpi = 600, limitsize = TRUE)

# Productivity averages + SE
aggregate(Productivity~flanduse+ftreatment+region,AvgProd4, mean)
aggregate(Productivity~flanduse+ftreatment+region,AvgProd4, SE)

#flanduse ftreatment     region Productivity
#1  pasture   exclosed Wet Region    1.2185714
#2     wild   exclosed Wet Region    1.4314286
#5  pasture   exclosed Dry Region    1.1157143
#6     wild   exclosed Dry Region    0.8057143

#flanduse ftreatment     region Productivity
#1  pasture   exclosed Wet Region    0.5157031
#2     wild   exclosed Wet Region    1.0008007
#5  pasture   exclosed Dry Region    0.7766683
#6     wild   exclosed Dry Region    0.3984366

############################################
# Adding NAP AND rainfall to aggregated dataframes using left join. MUST MAKE THIS WORK!
AvgCons3<-left_join(AvgCons2,RainRegionX, by=c("region","YrMonth"),drop=F)
names(AvgCons3)

# Remove target species
AvgCons4<-AvgCons3[AvgCons3$pool!="target",]

# Rename factors
AvgCons4<-droplevels(AvgCons4)
AvgCons4$region <- factor(AvgCons4$region, levels(AvgCons4$region)[c(2,1)])
levels(AvgCons4$region)<-c("Wet Region","Dry Region")
levels(AvgCons4$flanduse)<-c("pasture","wild")

# HERBIVORE CONSUMPTION plot throught time
legend_title<-"land-use"
cp<- ggplot(AvgCons4, aes(x=YrMonth, y=Consumption, colour=flanduse,
                          group=landuseRain))
cp<-cp+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
cp<-cp+geom_line(aes(linetype=flanduse),size=1.2, alpha=.5, show.legend=F)
cp<-cp+geom_errorbar(aes(ymin=Consumption-SE.x, ymax=Consumption+SE.x),width=.2,lwd=1.1,show.legend=F)
cp<-cp+geom_point(size=4, fill="white", stroke=2,show.legend=F)
cp<-cp+facet_wrap(~region,ncol=2,scales='fixed')
cp<-cp+scale_y_continuous(limits=c(-1,3.5),sec.axis = sec_axis(~ . *150, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
cp<-cp+geom_line(aes(y = rain.sum/150),colour="dark blue",linetype=1,size=1, alpha=.1)
cp<-cp+geom_point(aes(y = rain.sum/150),colour="dark blue",fill="white",size=.9,alpha=.1)
cp<-cp+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
cp<-cp+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cp<-cp+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
cp<-cp+xlab("Time (month|year)") + ylab(expression(paste("Consumption (g ",m^-2," ",day^-1,")")))
cp<-cp+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
cp<-cp+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
cp<-cp+ annotate(geom="text",x=as.Date("2017-02-28"), y=3.5, label=c("(b)",""),color="black",fontface="bold", size=6)

cp

#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Marit Arneberg/Marit.Herbivore.consumption.jpeg",
#       width= 22, height = 14,units ="cm",
#       dpi = 600, limitsize = TRUE)

# Consumption averages + SE
aggregate(Consumption~flanduse+region,AvgCons4, mean)
aggregate(Productivity~flanduse+region,AvgProd4, SE)

#flanduse     region Consumption
#1  pasture Wet Region   0.8057143
#2     wild Wet Region   0.5971429
#3  pasture Dry Region   0.7728571
#4     wild Dry Region   0.4742857

#flanduse     region Productivity
#1  pasture Wet Region    0.3434293
#2     wild Wet Region    0.6280695
#3  pasture Dry Region    0.4566952
#4     wild Dry Region    0.2735033

# Combine NAP and consumption plots
library(grid)
library(gridExtra)
library(ggpubr)

p1 <- ggarrange(dp, cp+ font("x.text", size = 12),
                ncol = 1, nrow = 2,
                legend = "right",
                common.legend = TRUE)
p1

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Marit Arneberg/NAP.consumption.jpeg",
       width= 22, height = 17,units ="cm",
       dpi = 600, limitsize = TRUE)

###################################################################################
# NAP and consumption on the same graph - 
AvgProd5<-AvgProd4[AvgProd4$ftreatment!="open",]
dim(AvgProd5) # 28 11
dim(AvgCons4) #28 12
AvgProd5b<-left_join(AvgProd5,AvgCons4, by=c("region","flanduse","YrMonth","pool","rain.sum"),drop=F)

AvgProd5b$col<-AvgProd5b$flanduse
levels(AvgProd5b$col)<-c("tan","turquoise3")
#levels(AvgProd5b$col)<-c("#D2B48C","#00C5CD")

AvgProd5b$Biomass_change<-c("Productivity","Consumption")

# Error bars
# Se.x.x = productivity
# Se.x.y = consumption
#SeLo and SeUp= rainfall?

# HERBIVORE CONSUMPTION plot throught time
legend_title<-"land-use"
hcp<- ggplot(AvgProd5b, aes(x=YrMonth, y=Productivity, colour=flanduse,fill=flanduse, shape=Biomass_change,
                          group=landuseRain.y))
hcp<-hcp+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
hcp<-hcp+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
hcp<-hcp+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
hcp<-hcp+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
hcp<-hcp+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
hcp<-hcp+geom_line(linetype=1,size=1.2, alpha=.5, show.legend=F)
hcp<-hcp+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
hcp<-hcp+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
hcp<-hcp+facet_wrap(~flanduse+region,ncol=2,scales='fixed')
hcp<-hcp+scale_y_continuous(limits=c(-2,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
hcp<-hcp+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.2)
hcp<-hcp+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
hcp<-hcp+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
hcp<-hcp+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#hcp<-hcp+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
hcp<-hcp+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption (g ",m^-2," ",day^-1,")")))
hcp<-hcp+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
hcp<-hcp+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
hcp<-hcp+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Wet Region","Dry Region","",""),color="black", size=5)
#hcp<-hcp+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
#                                                                         size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
hcp<-hcp+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                          size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))
hcp2<-hcp+geom_point(data =AvgProd5b, aes(size=flanduse, shape = NA), colour = "grey50")
hcp2<-hcp2+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                    nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
hcp2

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Marit Arneberg/NAP.consumption.LINE.jpeg",
       width= 26, height = 18,units ="cm",
       dpi = 600, limitsize = TRUE)

###################################################################################
# Consumption as a percentage of NAP
AvgProd5<-AvgProd4[AvgProd4$ftreatment!="open",]
dim(AvgProd5) # 28 11
dim(AvgCons4) #28 12

names(AvgProd5)
names(AvgCons4)

#AvgCons4$Consumption[AvgCons4$Consumption<0.01]<-0
#AvgProd5$Productivity[AvgProd5$Productivity<0.01]<-0

AvgProd5$Productivity[AvgProd5$Productivity==0.00]<-.01
AvgCons4$ConsumptionPer<-((AvgProd5$Productivity-AvgCons4$Consumption)/AvgProd5$Productivity)*100
# 14 ia inf! productivity is zero - change to 0.1!
AvgCons4

# Averages + SE
aggregate(ConsumptionPer~flanduse+region,AvgCons4, mean)
aggregate(ConsumptionPer~flanduse+region,AvgCons4, SE)

# Average consumption as percentage of NAP
#flanduse     region ConsumptionPer
#1  pasture Wet Region       135.3732
#2     wild Wet Region       103.9232
#3  pasture Dry Region       109.8672
#4     wild Dry Region      -306.0955

# Standard error - wet region pasture overlaps with zero
#flanduse     region ConsumptionPer
#1  pasture Wet Region      165.84222
#2     wild Wet Region       55.90518
#3  pasture Dry Region       25.24121
#4     wild Dry Region      250.67534


# PLOT mean conpsumption as a percentage of mean NAP
cP<- ggplot(AvgCons4, aes(x=YrMonth, y=ConsumptionPer, colour=flanduse,
                          group=landuseRain))
cP<-cP+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
cP<-cP+geom_line(aes(linetype=flanduse),size=1.2, alpha=.5, show.legend=F)
#cP<-cP+geom_errorbar(aes(ymin=Consumption-SE.x, ymax=Consumption+SE.x),width=.2,lwd=1.1,show.legend=F)
cP<-cP+geom_point(size=4, fill="white", stroke=2,show.legend=T)
cP<-cP+facet_wrap(~region,ncol=2,scales='fixed')
cP<-cP+scale_y_continuous(limits=c(-1500,1100),sec.axis = sec_axis(~ . *.55, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
cP<-cP+geom_line(aes(y = rain.sum/.55),colour="dark blue",linetype=1,size=1, alpha=.1,show.legend=F)
cP<-cP+geom_point(aes(y = rain.sum/.55),colour="dark blue",fill="white",size=.9,alpha=.1)
cP<-cP+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
cP<-cP+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cP<-cP+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
cP<-cP+xlab("Time (month|year)") + ylab("Consumption (% NAP)")
cP<-cP+theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(size=12)
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
cP<-cP+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
cP

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Marit Arneberg/Herbivore.Consumption.perc.jpeg",
       width= 22, height = 14,units ="cm",
       dpi = 600, limitsize = TRUE)

#####################################################
# Consumption as a percentage of MAXIMUM NAP

TotProdMax<-aggregate(prodTotal~region+flanduse+landuseRain+YrMonth+ftreatment,na.rm=T,DataEx,max)
TotProdMax$prodTotal<-round(TotProdMax$prodTotal,digits=2)
colnames(TotProdMax)[6]<-"Max.Productivity"
dim(TotProdMax)

# Remove SE and open plots
TotProdMax2<-TotProdMax[TotProdMax$region!="SE",]
TotProdMax3<-TotProdMax2[TotProdMax2$ftreatment!="OP",]
TotProdMax3<-droplevels(TotProdMax3)
dim(TotProdMax3)

# Rename factors
TotProdMax3$region <- factor(TotProdMax3$region, levels(TotProdMax3$region)[c(2,1)])
levels(TotProdMax3$region)<-c("Wet Region","Dry Region")
levels(TotProdMax3$flanduse)<-c("pasture","wild")
levels(TotProdMax3$ftreatment)<-c("exclosed")


AvgCons4$MaxConsumptionPer<-((TotProdMax3$Max.Productivity-AvgCons4$Consumption)/TotProdMax3$Max.Productivity)*100

cP<- ggplot(AvgCons4, aes(x=YrMonth, y=MaxConsumptionPer, colour=flanduse,
                          group=landuseRain))
cP<-cP+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
cP<-cP+geom_line(aes(linetype=flanduse),size=1.2, alpha=.5, show.legend=F)
#cP<-cP+geom_errorbar(aes(ymin=Consumption-SE.x, ymax=Consumption+SE.x),width=.2,lwd=1.1,show.legend=F)
cP<-cP+geom_point(size=4, fill="white", stroke=2,show.legend=T)
cP<-cP+facet_wrap(~region,ncol=2,scales='fixed')
cP<-cP+scale_y_continuous(limits=c(-180,140),sec.axis = sec_axis(~ . *3.5, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
cP<-cP+geom_line(aes(y = rain.sum/3.5),colour="dark blue",linetype=1,size=1, alpha=.1,show.legend=F)
cP<-cP+geom_point(aes(y = rain.sum/3.5),colour="dark blue",fill="white",size=.9,alpha=.1)
cP<-cP+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
cP<-cP+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cP<-cP+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
cP<-cP+xlab("Time (month|year)") + ylab("Consumption (% NAP)")
cP<-cP+theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        ,strip.text = element_text(size=12)
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
cP<-cP+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
cP

# Averages for using Maximum potential 
aggregate(MaxConsumptionPer~flanduse+region,AvgCons4, mean)
aggregate(MaxConsumptionPer~flanduse+region,AvgCons4, SE)

# Average consumption as percentage of NAP
#flanduse     region ConsumptionPer
#1  pasture Wet Region          48.73507
#2     wild Wet Region          78.39506
#3  pasture Dry Region          45.47229
#4     wild Dry Region          38.24763

# Standard error - wet region pasture overlaps with zero
#flanduse     region ConsumptionPer
#1  pasture Wet Region          10.41123
#2     wild Wet Region          12.07962
#3  pasture Dry Region          38.66930
#4     wild Dry Region          24.13157

#######################################################################################
# INLA
library(INLA)
library(lubridate)

# Unique plot ID through time
DataEx$plotID<-as.factor(with(DataEx, paste(block.id,ftreatment, sep="_")))

# Adjust time so factor for INLA
MarDate<-strptime(as.character(DataEx$harvest.date),format="%d/%m/%Y",tz="Africa/Nairobi" )
DataEx$YrMonth<-format(as.Date(MarDate), "%Y-%m")

# REMOVE NAS
colSums(is.na(DataEx))
DataEx3<-DataEx[!is.na(DataEx$rain.sum),]
DataEx4<-DataEx3[!is.na(DataEx3$prodTotal),]
colSums(is.na(DataEx4))

# INLA model for NAP - need to change to ar1
f1<- prodTotal ~  f(YrMonth, model="ar1",replicate=as.numeric(plotID)) +flanduse+
ftreatment+rain.sum+fregion+ftreatment:flanduse+flanduse:rain.sum+flanduse:fregion+
  ftreatment:rain.sum+
f(site.id, model = "iid")+f(fblock, model = "iid")

I1 <- inla(f1, family = "gaussian",  
           control.compute = list(waic = TRUE, cpo=TRUE, dic=TRUE),
           data = DataEx4)

cbind(I1$waic$waic[1], I1$dic$dic[1], -sum(log(I1$cpo$cpo)))

#pvalue histogram....
pval<-rep(NA, nrow=(DataEx4))
for(i in 1:nrow(DataEx4)){
  pval[i]<-inla.pmarginal(q=DataEx4$prodTotal[i],
                          marginal=I1$marginals.fitted.values[[i]])
}
hist(pval) # Bad...no central peak - issues with missing data in the series...
# Reduced slighty by removing NAs - need to resolve missing data

# Fixed factors
names(I1$marginals.fixed)
mEx<- I1$marginals.fixed$ftreatmentOP# Not important
mlanduse<- I1$marginals.fixed$flanduseW
mExlanduse<- I1$marginals.fixed$'flanduseW:ftreatmentOP' # Important
mrain<-I1$marginals.fixed$rain.sum # Not Important
mExrain<-I1$marginals.fixed$'ftreatmentOP:rain.sum'
mLandusrain<-I1$marginals.fixed$'flanduseW:rain.sum'

par(mfrow=c(1,1),mar = c(5,5,2,2))
plot(x = mEx[,1],
     y = mEx[,2], 
     xlab = "Beta for Ex vs Open",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Important-ish
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mlanduse[,1],
     y = mlanduse[,2], 
     xlab = "Beta for wild vs pasture",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mExlanduse[,1],
     y = mExlanduse[,2], 
     xlab = "Beta for Wild x Open",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important - treatment x land-use interaction
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mrain[,1],
     y = mrain[,2], 
     xlab = "Beta for Interval rain",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Important
abline(v = 0,lwd=2, lty = 2,col="red")
  
plot(x = mExrain[,1],
     y = mExrain[,2], 
     xlab = "Beta for Interval rain",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mLandusrain[,1],
     y = mLandusrain[,2], 
     xlab = "Beta for Interval rain",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Important
abline(v = 0,lwd=2, lty = 2,col="red")


#INLA model for consumption

# REMOVE NAS
colSums(is.na(DataEx))
DataEx3<-DataEx[!is.na(DataEx$rain.sum),]
DataEx5<-DataEx3[!is.na(DataEx3$consTotal),]

names(DataEx5)
# INLA model for consumption
f1<- consTotal ~  f(YrMonth, model="rw2",replicate=as.numeric(plotID)) +flanduse+
  rain.sum+fregion+flanduse:rain.sum+flanduse:fregion+
  f(site.id, model = "iid")+f(fblock, model = "iid")

I1 <- inla(f1, family = "gaussian",  
           control.compute = list(waic = TRUE, cpo=TRUE, dic=TRUE),
           data = DataEx5)

cbind(I1$waic$waic[1], I1$dic$dic[1], -sum(log(I1$cpo$cpo)))

#pvalue histogram....
pval<-rep(NA, nrow=(DataEx5))
for(i in 1:nrow(DataEx5)){
  pval[i]<-inla.pmarginal(q=DataEx5$consTotal[i],
                          marginal=I1$marginals.fitted.values[[i]])
}
hist(pval) # Bad...no central peak - issues with missing data in the series...
# Reduced slighty by removing NAs - need to resolve missing data

# Fixed factors
names(I1$marginals.fixed)
mlanduse<- I1$marginals.fixed$flanduseW
mRainlanduse<- I1$marginals.fixed$'flanduseW:rain.sum' # Important
mRainRegionW<- I1$marginals.fixed$'fregionWET'
mrain<-I1$marginals.fixed$rain.sum # Not Important


par(mfrow=c(1,1),mar = c(5,5,2,2))
plot(x = mlanduse[,1],
     y = mlanduse[,2], 
     xlab = "Beta for wild vs pasture",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important - no difference in consumption
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mRainlanduse[,1],
     y = mRainlanduse[,2], 
     xlab = "Beta for rain x landuse",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important - rain x land-use interaction
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mrain[,1],
     y = mrain[,2], 
     xlab = "Beta for Interval rain",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mRainRegionW[,1],
     y = mRainRegionW[,2], 
     xlab = "Beta for Rain x region Wet vs Dry",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

names(DataEx5)
plot((consTotal)~rain.sum, DataEx5)
abline(lm((consTotal)~rain.sum, DataEx5))
summary(lm((consTotal)~rain.sum, DataEx5))

TotCons<-aggregate(consTotal~site.name+ftreatment+fharvest,na.rm=T,DataEx,mean)
  TotCons$consTotal<-round(TotCons$consTotal,digits=2)
TarProd<-aggregate(prodTarg~site.name+ftreatment+fharvest,na.rm=T,DataEx,mean)
  TarProd$prodTarg<-round(TarProd$prodTarg,digits=2)
TarCons<-aggregate(consTarg~site.name+ftreatment+fharvest,na.rm=T,DataEx,mean)
  TarCons$consTarg<-round(TarCons$consTarg,digits=2)

# SE 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TotProdSE<-aggregate(prodTotal~region+site.name+ftreatment+fharvest,DataEx,SE)
  TotProdSE$prodTotal<-round(TotProdSE$prodTotal,digits=2)
TotConsSE<-aggregate(consTotal~site.name+ftreatment+fharvest,DataEx,SE)
  TotConsSE$consTotal<-round(TotConsSE$consTotal,digits=2)
TarProdSE<-aggregate(prodTarg~site.name+ftreatment+fharvest,DataEx,SE)
  TarProdSE$prodTarg<-round(TarProdSE$prodTarg,digits=2)
TarConsSE<-aggregate(consTarg~site.name+ftreatment+fharvest,DataEx,SE)
  TarConsSE$consTarg<-round(TarConsSE$consTarg,digits=2)

# Add SE to average datasets, with upper and lower limits
TotProd$SE<-TotProdSE$prodTotal
  TotProd$SeUp<-TotProd$prodTotal+TotProd$SE
  TotProd$SeLo<-TotProd$prodTotal-TotProd$SE
TotCons$SE<-TotConsSE$consTotal
  TotCons$SeUp<-TotCons$consTotal+TotCons$SE
  TotCons$SeLo<-TotCons$consTotal-TotCons$SE
TarProd$SE<-TarProdSE$prodTarg
  TarProd$SeUp<-TarProd$prodTarg+TarProd$SE
  TarProd$SeLo<-TarProd$prodTarg-TarProd$SE
TarCons$SE<-TarConsSE$consTarg
  TarCons$SeUp<-TarCons$consTarg+TarCons$SE
  TarCons$SeLo<-TarCons$consTarg-TarCons$SE
head(TarProd)

#### Rainfall data ####
# Averaging rainfall data and getting SE by region # To be included in the Productivity aggregated dataframes per site
  #per rainfall region (WET, SE , DRY)
RainRegion<-aggregate(rain.sum~region+fharvest,DataEx,mean)
  RainRegionSE<-aggregate(rain.sum~region+fharvest,DataEx,SE)

RainRegionX<-cbind(RainRegion,RainRegionSE[,3])
  colnames(RainRegionX)[4]<-"SE"
RainRegionX$fharvest<-as.factor(RainRegionX$fharvest)
RainRegionSE$fharvest<-as.factor(RainRegionSE$fharvest)

# Defining upper and lower limits
RainRegionX$SeUp<-RainRegionX$rain.sum+RainRegionX$SE
RainRegionX$SeLo<-RainRegionX$rain.sum-RainRegionX$SE

# Adding rainfall to aggregated dataframes using left join. MUST MAKE THIS WORK!
TotProd<-left_join(TotProd,RainRegionX, by=c("region","fharvest"),drop=F)
TotProd[1,]
names(TotProd)

#### Plotting ####

legend_title<-"Biomass"
  bp<- ggplot(DataEx, aes(x=fharvest, y=prodTotal)) #now skipping colour, fill and group 
  # What dataframe to use now? Can I use several? I've split all productivity and consumption into 4 diff. frames, by site.name
  bp<- bp + geom_ribbon(data= TotProd, aes(x=fharvest, colour=site.name, fill=site.name, ymax=))
  bp<-geom_line(aes
                (linetype=area,colour=site.name), size=1.2, show.legend=F)
  bp<-bp+facet_wrap() #Separating out the panels
  

#### Trial with Lena 13.8 #### # Makao
#1st aggregate the data to have the sd and the mean
prodTotal_mean<-aggregate(prodTotal~harvest+site.name,DataEx,mean)
colnames(prodTotal_mean)<-c("harvest","site.name","prodTotal_mean")
prodTotal_sd<-aggregate(prodTotal~harvest+site.name,DataEx,sd)
colnames(prodTotal_sd)<-c("harvest","site.name","prodTotal_sd")

prodTotal_meanperharvest<-cbind(prodTotal_mean,prodTotal_sd)

#subset for Makao
Makao<-subset(prodTotal_meanperharvest,prodTotal_meanperharvest$site.name=="Makao")
Makao<-Makao[Makao$harvest!="H0",]
unique(Makao$harvest)


#plot
dp<-ggplot(Makao, aes(x=harvest, y=prodTotal_mean))#Plotting area
dp<-dp+geom_line(size=0.5, show.legend=F)+geom_point(size=1, fill="white")
dp<-dp+ theme_bw()+theme(plot.background = element_blank())
dp<-dp+geom_errorbar(aes(ymin=prodTotal_mean-prodTotal_sd, ymax=prodTotal_mean-prodTotal_sd), colour="black", width=.1)
dp

