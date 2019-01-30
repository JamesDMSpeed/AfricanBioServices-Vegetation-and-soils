#### Biomass exploration ####
#-------------------------
#clear system & add package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(ggplot2)
library(dplyr)
library(gcookbook)
library(readr)
library(tidyr)
library(lemon)

library(grid)
library(gridExtra)
library(ggpubr)
library(nlme)
library(lme4)
library(MuMIn) #for mod.sel()
#-------------------------

Biomass <- read.csv("Biomass.csv")
tail(Biomass)
View(Biomass)
#-------------------------
#RUN FIRST
  #Housekeeping
  #Date variable
  #Aggregating rainfall per region
  #Average NAP
  #Average CONS 
#-------------------------
#### Housekeeping ####
# Removing Ex2 - separate analysis
Databiom <- Biomass[Biomass$treatment!="EX2",] #Removing Mesh exclosures
Databiom <- Databiom[Databiom$harvest!="H0",] #removing H0
Databiom <- Databiom[-281,]
Databiom <- droplevels(Databiom)

#attach(Databiom)
#detach(Databiom)
# Creating factor variables
Databiom$landuse<-as.factor(Databiom$landuse)
Databiom$region<-as.factor(Databiom$region)
Databiom$site.name <- as.factor(Databiom$site.name)
Databiom$block<-as.factor(Databiom$block)
Databiom$treatment<-as.factor(Databiom$treatment)
Databiom$harvest<-as.factor(Databiom$harvest)
Databiom$site.id <- as.factor(Databiom$site.id)
Databiom$block.id.harvest <- as.factor(Databiom$block.id.harvest)

#Renaming productivity and consumption columns
colnames(Databiom)[colnames(Databiom)=="productivity.target.g.m2.day"] <- "prodtarg"
colnames(Databiom)[colnames(Databiom)=="consumption.target.g.m2.day"] <- "constarg"
colnames(Databiom)[colnames(Databiom)=="productivity.total.g.m2.day"] <- "prodtot"
colnames(Databiom)[colnames(Databiom)=="consumption.total.g.m2.day"] <- "constot"

#Renaming levels in region, landuse and treatment columns
levels(Databiom$region)<-c("Dry Region","Intermediate Region","Wet Region")
levels(Databiom$landuse)<-c("pasture","wild")
levels(Databiom$treatment)<-c("exclosed","open")

#### Date variable ####
# Rdate create month column. default was (="%d.%m.%Y")
Rdate<-strptime(as.character(Databiom$harvest.date),format="%d.%m.%Y",tz="Africa/Nairobi" )# East African time #USE
class(Rdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
Databiom$Rdate<-Rdate# Add to the dataframe #
# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
Databiom$YrMonth<-format(as.Date(Rdate), "%Y-%m")

Databiom$month<-Databiom$Rdate$mon+1
Databiom$month<-as.factor(Databiom$month)
Databiom$month <- month.abb[Databiom$month] #Changing to month name abbrevitations

#### Plot.code to follow through time ####
Databiom$plot.code <- as.factor(with(Databiom,paste(region,landuse,block,treatment,sep="_")))
levels(Databiom$plot.code) #40 levels
#----------------------------------------------------
#### DATA EXPLORATION ####
# Zuur: 
# 1:outliers (Y&X) 2:heterogeneity (Y) 3:normality (Y) 4:zero's (Y) 5:collinearity(X)
# 6:relations between variables(X&Y) 7:interactions 8:independence(Y)

#### Spread of data ####
hist(prodtot) # right-skewed + some values up to 8!
hist(constot) # as much neg.values as pos! Mean cons. ~0... 
hist(prodtarg) # most around +/-1
hist(constarg)

#### Outliers ####
# Using boxplot to visualize mean and spread of data (lower and upper end of box is 25% and 75% quantile)

#From Stu's script
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2)) #mfrow several graphs on the same page
dotchart(Databiom$prodtarg,groups=as.factor(Databiom$plot.id)) # Outlier -4.29
dotchart(Databiom$prodtarg,groups=Databiom$landuse,main = "landuse") # Outlier -4.29 #Separating between landuses --> Both outliers in pasture
#To identify the outliers. plot(), then identify(). Click on outliers to define, then esc. 
plot(prodtot) # 25  37 243 247
plot(prodtarg) # 253 254
plot(constot) # 25 273
plot(constarg) #45
identify(constarg)
  # 25 is outlier in both prodtot and constot   #Large biomass! ~109g, due to underneath tree?

#Looking at specific rows with the outliers
Databiom[c(25,37,243,247),] 
  #All >7, exclosed, H1/H1/H7/H7, dry/int/wet/wet, Makao/Seronera/Hand/Hand
Databiom[c(253,254),"prodtarg"]
  #Both wet_P_3_H7  -4.29 -4.66
Databiom[c(25,273),]
  #Both ex, Dry_p_1 and se_1, H1/H7 4.59 and -2.79
Databiom[c(45),"constarg"]
  #Handajega Ex H2  3.35
---------------------------------------------------------------

#### GRAPHING ####
#### Trying to plot rain.sum with date on the x-axis ####
plot(as.Date(harvest.date, format ="%m/%d/%Y"),rain.sum)
plot(as.factor(harvest.date), rain.sum)

plot((rain.sum)~as.Date(Rdate), Databiom,na.rm=T)
#abline(lm((consTotal)~rain.sum, DataEx5))
#summary(lm((consTotal)~rain.sum, DataEx5))
plot(Rdate,rain.sum,x_breaks, xlab =month.abb)
?plot
#ggplot
rain <- ggplot(Databiom, aes(x=YrMonth, y=rain.sum, colour="dark blue"))
rain <- rain + geom_line(aes(y=rain.sum, colour="dark blue",linetype=1,size=1, alpha=.1))

rain <-rain + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) #NOT working

dp<-dp+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
rain <-rain + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(Databiom$YrMonth),max=max(Databiom$YrMonth))) 

#### Aggregating rainfall per region #### 
# Averaging rainfall data and getting SE by region # To be included in the NAP aggregated dataframes per site
#per rainfall region (WET, SE , DRY)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Rainregion<-aggregate(rain.sum~region+YrMonth,Databiom,mean)
RainregionSE<-aggregate(rain.sum~region+YrMonth,Databiom,SE)

RainregionX<-cbind(Rainregion,RainregionSE[,3])
colnames(RainregionX)[4]<-"SE"

# Defining upper and lower limits
RainregionX$SeUp<-RainregionX$rain.sum+RainregionX$SE
RainregionX$SeLo<-RainregionX$rain.sum-RainregionX$SE

# Convert to date
RainregionX$YrMonth<-as.Date(paste(RainregionX$YrMonth,"-01",sep=""))

#### Adding NAP AND rainfall to aggregated dataframes ####
# using left join.
Avgprod3<-left_join(Avgprod,RainregionX, by=c("region","YrMonth"),drop=F)
names(Avgprod3)

## Set values <0 to zero
#AvgProd3$Productivity[AvgProd3$Productivity<0.01]<-0

# Calculate error bars
#AvgProd3$SeUp<-AvgProd3$Productivity+AvgProd3$SE.x
#AvgProd3$SeLo<-AvgProd3$Productivity-AvgProd3$SE.x
#AvgProd3$SeUp[AvgProd3$SeUp<0.01]<-0
#AvgProd3$SeLo[AvgProd3$SeLo<0.01]<-0

#----------------------------------------------------------------------
#Stu averaged rainfall per region (aggregate(rain.sum~region+YrMonth,DataEx,mean))
  #avgRain <- aggregate(rain.sum~region+YrMonth,na.rm=T,Databiom,mean)
  #rain <- ggplot( data = avgRain, aes( x=YrMonth, y=rain.sum,colour="dark blue" )) + geom_line() 
  #rain

#### Precipitation line plot ####
plot(Databiom$rain.sum,Databiom$prodtot)
plot(Databiom$rain.sum,Databiom$constot)
summary(lm(prodtot~rain.sum,data=Databiom))


plot((constot)~rain.sum, Databiom)
abline(lm((constot)~rain.sum, Databiom))
summary(lm((constot)~rain.sum, Databiom))
#-----------------------------------------------------

#### Average NAP ####
# Average of each site per harvest
Totprod <- aggregate(prodtot~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
Totprod$prodtot<- round(Totprod$prodtot, digits=2)
colnames(Totprod)[6]<-"Productivity"
Totprod$pool<-"total" #Tagging these data with total productivity - combining later

Tarprod<-aggregate(prodtarg~region+landuse+site.id+YrMonth+treatment,na.rm=T,Databiom,mean)
Tarprod$prodtarg<-round(Tarprod$prodtarg,digits=2)  
colnames(Tarprod)[6]<-"Productivity"
Tarprod$pool<-"target"

# Average total and target productivity, in one dataframe
Avgprod<-rbind(Totprod,Tarprod)

#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgprod
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
TotprodSE <- aggregate(prodtot~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TotprodSE$prodtot<-round(TotprodSE$prodtot,digits=2)
colnames(TotprodSE)[6]<-"SE"
TotprodSE$pool<-"total"

TarprodSE <- aggregate(prodtarg~region+landuse+site.id+YrMonth+treatment,Databiom,SE)
TarprodSE$prodtarg<-round(TarprodSE$prodtarg,digits=2)
colnames(TarprodSE)[6]<-"SE"
TarprodSE$pool<-"total"

Seprod<-rbind(TotprodSE,TarprodSE)
Avgprod$SE<-Seprod$SE

# Convert to date
Avgprod$YrMonth<-as.Date(paste(Avgprod$YrMonth,"-01",sep=""))#Adding day (first of month)

# Redo code to include differences in pool - linetype
Avgprod$site.id<-as.factor(with(Avgprod, paste(region,landuse,treatment,pool, sep="_")))
#### Average CONS ####
# Average of each site per harvest
Totcons <- aggregate(constot~region+landuse+site.id+YrMonth,na.rm=T,Databiom,mean)
Totcons$constot<- round(Totcons$constot, digits=2)
colnames(Totcons)[5]<-"Consumption"
Totcons$pool<-"total" #Tagging these data with total productivity - combining later

Tarcons<-aggregate(constarg~region+landuse+site.id+YrMonth,na.rm=T,Databiom,mean)
Tarcons$constarg<-round(Tarcons$constarg,digits=2)  
colnames(Tarcons)[5]<-"Consumption"
Tarcons$pool<-"target"

# Average total and target CONS, in one dataframe
Avgcons<-rbind(Totcons,Tarcons)


#Then making dataframes for total and target SEs --> combining them in one frame, then adding them to the average dataframe Avgcons
TotconsSE <- aggregate(constot~region+landuse+site.id+YrMonth,Databiom,SE)
TotconsSE$constot<-round(TotconsSE$constot,digits=2)
colnames(TotconsSE)[5]<-"SE"
TotconsSE$pool<-"total"

TarconsSE <- aggregate(constarg~region+landuse+site.id+YrMonth,Databiom,SE)
TarconsSE$constarg<-round(TarconsSE$constarg,digits=2)
colnames(TarconsSE)[5]<-"SE"
TarconsSE$pool<-"target"

#Including SE in the averagecons frame
Secons<-rbind(TotconsSE,TarconsSE)
Avgcons$SE<-Secons$SE

# Convert to date
Avgcons$YrMonth<-as.Date(paste(Avgcons$YrMonth,"-01",sep=""))

# Redo code to include differences in pool - linetype
Avgcons$site.id<-as.factor(with(Avgcons, paste(region,landuse,pool, sep="_")))

####Dataset overview ####
  #Avgprod - 138 obs, 8 var
  #Avgprod2 - without Seronera
  #Avgprod3 - From Avgprod, joined with RainregionX
  #Avgprod4 - From Avgprod3, without target
  #Avgprod5 - From Avgprod4 without Seronera
  #Avgprod6 - From Avgprod5, without open
  #Avgprod6b - From Avgprod4, with Seronera, without open

  #Avgcons - 70 obs, 7 var
  #Avgcons2 - without Seronera
  #Avgcons3 - From Avgcons, joined with RainregionX
  #Avgcons4 - From Avgcons3, without target
  #Avgcons5 - From Avgcons4, without Seronera

#### NAP target+total ####
legend_title<-"land-use"
legend_title2<-"treatment"
nap <- ggplot(Avgprod, aes(x=YrMonth, y=Productivity, colour=landuse,shape=treatment,
                           group=site.id))
nap<-nap+geom_line(size=1.2, alpha=.5, show.legend=F)
nap<-nap+geom_errorbar(aes(ymin=Productivity-SE, ymax=Productivity+SE),width=.2,lwd=1.1,show.legend=F)
nap<-nap+geom_point(size=4, fill="white", stroke=2)
nap<-nap+facet_wrap(~region+pool,ncol=2,scales='fixed')
nap<-nap+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(Avgprod$YrMonth),max=max(Avgprod$YrMonth))) 
#nap<-nap+scale_fill_manual(values=c("white","white"),show.legend=F)
nap<-nap+scale_colour_manual(legend_title, values=c( "tan3","turquoise3")) #ABS colors
nap<-nap+ xlab("Time (month|year)") + ylab(expression(paste("Net Aboveground Productivity(g ",m^-2," ",day^-1,")")))
nap<-nap+scale_shape_manual(legend_title2,values=c(22,21))
nap<-nap+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
nap
ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPtarg.png",
       width= 26, height = 18,units ="cm",
       dpi = 600, limitsize = TRUE)

#### NAP with Seronera ####
# Remove target species
Avgprod4 <- Avgprod3[Avgprod3$pool!="target",]
Avgprod4<-droplevels(Avgprod4)
legend_title<-"land-use"
legend_title2<-"treatment"
nap2<- ggplot(Avgprod4, aes(x=YrMonth, y=Productivity, colour=landuse,shape=treatment,
                          group=site.id))
nap2<-nap2+ geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
nap2<-nap2+geom_line(aes(linetype=landuse),size=1.2, alpha=.5, show.legend=F)
nap2<-nap2+geom_errorbar(aes(ymin=Productivity-SE.x, ymax=Productivity+SE.x),linetype="solid",width=.2,lwd=1.1,show.legend=F)
nap2<-nap2+geom_point(size=4, fill="white", stroke=2)
nap2<-nap2+facet_wrap(~region,ncol=1,scales='fixed', drop=F)
#nap2<-nap2+coord_capped_cart(left='both')
nap2<-nap2+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
nap2<-nap2+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
nap2<-nap2+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)
nap2<-nap2+geom_point(aes(y = rain.sum/70),colour="dark blue",size=.9,alpha=.1)
#nap2<-nap2+scale_fill_manual(values=c("white","white"),show.legend=F)
nap2<-nap2+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
nap2<-nap2+scale_shape_manual(legend_title2,values=c(22,21))
nap2 <- nap2+scale_size_manual(legend_title2,values=1)
nap2<-nap2+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
nap2<-nap2+ xlab("Month|Year") + ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
nap2<-nap2+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#nap2<-nap2+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#nap2 <- nap2+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#nap2<-nap2+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
nap2

ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPwithSeronera.png",
       width= 26, height = 18,units ="cm",
       dpi = 600, limitsize = TRUE)

#### NAP without Seronera ####
Avgprod5 <- Avgprod4[Avgprod4$region!="Intermediate Region",]
Avgprod5 <- droplevels(Avgprod5)
legend_title<-"land-use"
legend_title2<-"treatment"
nap3<- ggplot(Avgprod5, aes(x=YrMonth, y=Productivity, colour=landuse,shape=treatment,
                            group=site.id))
nap3<-nap3+ geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
nap3<-nap3+geom_line(aes(linetype=landuse),size=1.2, alpha=.5, show.legend=F)
nap3<-nap3+geom_errorbar(aes(ymin=Productivity-SE.x, ymax=Productivity+SE.x),linetype="solid",width=.2,lwd=1.1,show.legend=F)
nap3<-nap3+geom_point(size=4, fill="white", stroke=2)
nap3<-nap3+facet_wrap(~region,ncol=1,scales='fixed', drop=F)
#nap3<-nap3+coord_capped_cart(left='both')
nap3<-nap3+ scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
nap3<-nap3+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
nap3<-nap3+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.1)
nap3<-nap3+geom_point(aes(y = rain.sum/70),colour="dark blue",size=.9,alpha=.1)
#nap3<-nap3+scale_fill_manual(values=c("white","white"),show.legend=F)
nap3<-nap3+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
nap3<-nap3+scale_shape_manual(legend_title2,values=c(22,21))
nap3 <- nap3+scale_size_manual(legend_title2,values=1)
nap3<-nap3+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
nap3<-nap3+ xlab("Month|Year") + ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))
nap3<-nap3+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text.y=element_text(size=12)
        ,axis.text.x=element_text(size=10,angle=35, hjust=1)
        ,axis.line=element_line( size=.5)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        #,legend.position = c(0.25, 0.82)
        ,plot.margin = unit(c(5,5,7,5), "mm")
        ,strip.background = element_blank()
        #,strip.text = element_text(size=12)
        ,strip.text = element_text(size=12)
        #,axis.text.x=element_blank()
        #,axis.ticks.x=element_blank()
        ,strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm"))) +
  theme(axis.line = element_line(color = 'black'))
#nap3<-nap3+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#nap3 <- nap3+ annotate(geom="text",x=as.Date("2017-02-28"),y=8)
#nap3<-nap3+  annotate(geom="text", x=as.Date("2017-02-28"), y=8, label=c("(a)",""),color="black",fontface="bold", size=6)
nap3

ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAP.png",
       width= 26, height = 18,units ="cm",
       dpi = 600, limitsize = TRUE)
#### CONS plot without Seronera ####
Avgcons2 <- Avgcons[Avgcons$region!="Intermediate Region",]
legend_title<-"land-use"
legend_title2<-"treatment"
cons<- ggplot(Avgcons2, aes(x=YrMonth, y=Consumption, colour=landuse,shape=pool,
                          linetype=pool,group=site.id))
cons<-cons+geom_line(size=1.2, alpha=.5, show.legend=F)
cons<-cons+geom_errorbar(aes(ymin=Consumption-SE, ymax=Consumption+SE),width=.2,lwd=1.1,show.legend=F)
cons<-cons+geom_point(size=4, fill="white", stroke=2)
cons<-cons+facet_wrap(~pool+region,ncol=2,scales='fixed')
cons<-cons+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(AvgProd$YrMonth),max=max(AvgProd$YrMonth))) 
#cons<-cons+scale_fill_manual(values=c("white","white"),show.legend=F)
cons<-cons+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cons<-cons+xlab("Time (month|year)") + ylab(expression(paste("Net Aboveground Productivity(g ",m^-2," ",day^-1,")")))
cons

### CONS plot ####
# Adding avg rainfall to the avgcons dataframe
Avgcons3 <- left_join(Avgcons,RainregionX, by=c("region","YrMonth"),drop=F)

# Removing target species
Avgcons4 <- Avgcons3[Avgcons3$pool!="target",]
Avgcons4 <- droplevels(Avgcons4)

legend_title<-"land-use"
cons2<- ggplot(Avgcons4, aes(x=YrMonth, y=Consumption, colour=landuse,
                          group=site.id))
cons2<-cons2+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey") #the dashed zero-line
cons2<-cons2+geom_line(aes(linetype=landuse),size=1.2, alpha=.5, show.legend=F) #2 lines per landuse
cons2<-cons2+geom_errorbar(aes(ymin=Consumption-SE.x, ymax=Consumption+SE.x),width=.2,lwd=1.1,show.legend=F)
cons2<-cons2+geom_point(size=4, fill="white", stroke=2,show.legend=F)
cons2<-cons2+facet_wrap(~region,ncol=1,scales='fixed')
cons2<-cons2+scale_y_continuous(limits=c(-1,3.5),sec.axis = sec_axis(~ . *150, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
cons2<-cons2+geom_line(aes(y = rain.sum/150),colour="dark blue",linetype=1,size=1, alpha=.1) #Why dividing by 150? 
cons2<-cons2+geom_point(aes(y = rain.sum/150),colour="dark blue",fill="white",size=.9,alpha=.1)
cons2<-cons2+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
#dp<-dp+scale_fill_manual(values=c("white","white"),show.legend=F)
cons2<-cons2+scale_colour_manual(legend_title, values=c( "tan3","turquoise3"))
cons2<-cons2+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
cons2<-cons2+xlab("Time (month|year)") + ylab(expression(paste("Consumption (g ",m^-2," ",day^-1,")")))
cons2<-cons2+ theme_bw() +
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
cons2<-cons2+  annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
#cons2<-cons2+ annotate(geom="text",x=as.Date("2017-02-28"), y=3.5, label=c("(b)",""),color="black",fontface="bold", size=6)

cons2

#### NAP+CONS in one dataframe ####
Avgcons5 <- Avgcons4[Avgcons4$region!="Intermediate Region",]
Avgprod6 <- Avgprod5[Avgprod5$treatment!="open",]
Avgprod6b <- Avgprod4[Avgprod4$treatment!="open",]
Avgprod6b <- droplevels(Avgprod6b)
#Redoing code for site.id - to remove treatment
Avgprod6b$site.id<-as.factor(with(Avgprod6b, paste(region,landuse,pool, sep="_")))

#Without Seronera
dim(Avgprod6) # 28 12
dim(Avgcons5) #28 11

Avgprodcons<-left_join(Avgprod6,Avgcons5, by=c("region","landuse","YrMonth","pool","rain.sum"),drop=F)

Avgprodcons$col<-Avgprodcons$landuse
levels(Avgprodcons$col)<-c("tan","turquoise3")
#levels(AvgProd5b$col)<-c("#D2B48C","#00C5CD")

Avgprodcons$Biomass_change<-c("Productivity","Consumption")

# Error bars
# Se.x.x = productivity
# Se.x.y = consumption
#SeLo and SeUp= rainfall

  #This one is not the same as the AvgProd5b dataframe! Here values of biomass change is in same column and not in each Productivity and Consumption
  #Avgprodcons <- gather(Avgprodcons, biomass_change,biomass, Productivity, Consumption, factor_key=TRUE )

#With Seronera
dim(Avgprod6b) #35 12
dim(Avgcons4) #35 11

Avgprodcons2<-left_join(Avgprod6b,Avgcons4, by=c("region","landuse","YrMonth","pool","rain.sum"),drop=F)

Avgprodcons2$col<-Avgprodcons2$landuse
levels(Avgprodcons2$col)<-c("tan","turquoise3")

Avgprodcons2$Biomass_change<-c("Productivity","Consumption","Productivity","Consumption","Productivity")

#### NAP+CONS plot without Seronera ####
legend_title<-"land-use"
napcons<- ggplot(Avgprodcons, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,shape=Biomass_change,
                            group=site.id.y))
napcons<-napcons+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons<-napcons+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons<-napcons+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons<-napcons+geom_line(linetype=1,size=1.2, alpha=.5, show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons<-napcons+facet_wrap(~landuse+region,ncol=2,scales='fixed')
napcons<-napcons+scale_y_continuous(limits=c(-2,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons<-napcons+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=.2)
napcons<-napcons+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
napcons<-napcons+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
napcons<-napcons+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#napcons<-napcons+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
napcons<-napcons+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption (g ",m^-2," ",day^-1,")")))
napcons<-napcons+ theme_bw() +
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
napcons<-napcons+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
napcons<-napcons+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","",""),color="black", size=5)
#napcons<-napcons+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
#                                                                         size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons<-napcons+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                            size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))
napconsb <-napcons+geom_point(data =Avgprodcons, aes(size=landuse, shape = NA), colour = "grey50")
napconsb<-napconsb+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                     nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napconsb


ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONS.png",
       width= 26, height = 18,units ="cm",
       dpi = 600, limitsize = TRUE)


#### NAP+CONS plot with Seronera ####
#prod6b cons4
Avgprodcons2$landuse <-factor (Avgprodcons2$landuse,levels=c("pasture","wild"))

legend_title<-"land-use"
napcons2<- ggplot(Avgprodcons2, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,shape=Biomass_change,
                                  group=site.id.y))
napcons2<-napcons2+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons2<-napcons2+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons2<-napcons2+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons2<-napcons2+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons2<-napcons2+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons2<-napcons2+geom_line(linetype=1,size=1.2, alpha=1, show.legend=F)
napcons2<-napcons2+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons2<-napcons2+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons2<-napcons2+facet_wrap(~region+landuse,ncol=2,scales='fixed')
napcons2<-napcons2+scale_y_continuous(limits=c(-1.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons2<-napcons2+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=0.5)
#napcons2<-napcons2+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
napcons2<-napcons2+scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) 
napcons2<-napcons2+scale_colour_manual(legend_title, values=c(pasture = "tan3", wild = "turquoise3"))
#napcons2<-napcons2+scale_linetype_manual(values = c(wild = "solid", pasture = "dashed"))
napcons2<-napcons2+xlab("Time (month|year)") + ylab(expression(paste("Productivity and consumption (g ",m^-2," ",day^-1,")")))
napcons2<-napcons2+ theme_bw() +
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
napcons2<-napcons2+ annotate(geom = "segment", x = as.Date("2017-02-10"), xend =as.Date("2017-02-10"), y = -Inf, yend = Inf, size = .6) 
napcons2<-napcons2+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Dry Region","Dry Region","Dry Region", "Wet Region"),color="black", size=5)
napcons2<-napcons2+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
                                                                       size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons2<-napcons2+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                                    size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))
napcons2b <-napcons2+geom_point(data =Avgprodcons2, aes(size=landuse, shape = NA), colour = "grey50")
napcons2b<-napcons2b+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                             nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napcons2b <- napcons2b+theme(panel.spacing.x=unit(2, "lines"),panel.spacing.y=unit(1, "lines"))
napcons2b
#could also use the lemon package with facet_rep_wrap(), but might need to reinstall R for this to work 


ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONSSeronera2d.png",
       width= 26, height = 18,units ="cm",
       dpi = 600, limitsize = TRUE)

####|####
#### ANALYSIS ####
#### Pearsons correlation ####
#If 100% correlation - points on a scatter plot lie on a straight line
#positive: slope=+1, negative: slope=-1
# From Stu: You can run the function below and use code – this only works with numerical or integers – you need to use boxplots for factors

# RUN THIS CODE FIRST
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

# Then select the variables to use in the pairs function with panel.cor
names(Databiom) #Showing column names

NumericVar <- c("rain.sum","pasture.disc.setup","height. setup", "total.veg.cover.setup", "pasture.disc.harvest","height.harvest") 
pairs(Databiom[,NumericVar],lower.panel = panel.cor)
#Mostly not any strong correlations
#pasture.disc.harvest and height.harvest 0.8 
#pasture.disc.setup and height.setup 0.5
#rain.sum and height.harvest 0.6

#-------------------
#Correlations between categorical variables
# aov (continuous vs categorical)
#otherwise a chisq.test? (categorical vs categorical)
pairs(~region+landuse+treatment+harvest+harvest.date+rain.sum+pasture.disc.harvest+height.harvest+total.veg.cover.harvest,data=Databiom) 
MyVar <- c("landuse", "treatment", "harvest", "rain.sum","region") 

#Test correlations between explanatory variables with boxplots
boxplot(landuse~region,data=Databiom)

#-------------------
  #From Impatiens script.. 
  ## Lager korrelasjonstester med p-verdier til alle variabler 
 # install.packages("Hmisc")
#library(Hmisc)
#mycor1 <- rcorr(as.matrix(Databiom["rain.sum","pasture.disc.setup","height. setup", "total.veg.cover.setup","pasture.disc.harvest","height.harvest"]), type="pearson")
#round(mycor1$P, digits=3)
#---------------------
#### Exploring variables with lm ####
# NAP and Landuse
napmodlanduse <- lm(prodtot~landuse,data=Databiom)
anova(napmodlanduse) #F is 1.04, non-significant
summary(napmodlanduse) #AdjRsquared: 0.00016
plot(napmodlanduse)

par(mfrow=c(1,1))
boxplot(prodtot~landuse,data=Databiom) #no difference
boxplot(prodtot~landuse+region,data=Databiom) #no difference
#-------------------------
plot(lengt, SM)
abline(model.cond1) # line for arithmetic model
range(lengt)
xx<-seq(35,52,0.1) #defining range of x-axis
yy<-exp(-11.87896)*xx^2.96885 #Equation, to make a curve. intercept * stigningstallet
mean(yy)
lines(xx,yy, lty=2) #Plotting a line based on points of two vectors. Here yy vector is based on the exponential function. lty= stipling av linje

#ANOVA
plot(factor(fertil[subset=-c(6,19,29)]), yield[subset=-c(6,19,29)])
hist(residuals(model.2))

plot(model.2)
library(sciplot)
par(mfrow=c(1,2))
lineplot.CI(fertil, yield)
lineplot.CI(factor(fertil[subset=-c(6,19,29)]), yield[subset=-c(6,19,29)], xlab = "Fertilization level", ylab = "Yield (tons)")
#--------------------------


#Rainfall 
napmodrainfall <- lm(prodtot~rain.sum,data=Databiom)
summary(napmodrainfall) #F:21.39, p<0.05, R-squared: 0.069
plot(prodtot~rain.sum,pch=landuse) #Trying but failing 
abline(napmodrainfall)

#Treatment
napmodtreatment <- lm(prodtot~treatment,data=Databiom)
summary(napmodtreatment) #F:7.985, p:0.005, R-squared: 0.025

#Interactions
napmod <- lm(prodtot~landuse*region*treatment*rain.sum,data=Databiom)
summary(napmod) #rain.sum regionSE and landuseW:regionWET #R-squared: 0.1528
napmod1 <- lm(prodtot~landuse+region+rain.sum+landuse:region)
summary(napmod1) #rainsum and regionIntermediate is significant
anova(napmod1) 
par(mfrow=c(2,2))
plot(napmod) #rows 25 37 outliers

#Removing these rows
Databiomtest <- Databiom[-c(25,37),]


#### NAP mixed model ####

#First testing random effects. NB: REML=T
napmodmixed <- lmer(prodtot~landuse*treatment +landuse*rain.sum + treatment*rain.sum + (landuse|site.name/harvest),REML=T,data=Databiom) # Random change in slope and intercept

napmodmixed1 <- lmer(prodtot~landuse*treatment +landuse*rain.sum + treatment*rain.sum + (1|site.name/harvest),REML=T,data=Databiom) #random variation in intercept only 

AIC(napmodmixed)
AIC(napmodmixed1)


#Testing fixed effects. NB: REML=F
napmodmixed <- lmer(prodtot~landuse*treatment +landuse*rain.sum + treatment*rain.sum + (1|site.name/harvest),REML=F,data=Databiom)
  summary(napmodmixed)
  hist(resid(napmodmixed))
napmodmixed1 <- lmer(prodtot~landuse+treatment +landuse*rain.sum + treatment*rain.sum + (1|site.name/harvest),REML=F,data=Databiom)
napmodmixed2 <- lmer(prodtot~landuse+treatment+rain.sum+landuse:treatment+landuse:rain.sum+ (1|site.name/harvest),REML=F,data=Databiom)
napmodmixed3 <- lmer(prodtot~landuse+treatment+rain.sum+landuse:treatment+ (1|site.name/harvest),REML=F,data=Databiom)
napmodmixed4 <- lmer(prodtot~landuse+treatment+rain.sum+ (1|site.name/harvest),REML=F,data=Databiom)

#Then selecting the best model
library(MuMIn)
model.sel(napmodmixed, napmodmixed1, napmodmixed2, napmodmixed3, napmodmixed4) #4 is best, then 3

aictab(cand.set=cm, modnames=Modenames)

#Parameter estimates for the best model
napmodmixed4<-lmer(prodtot~landuse+treatment+rain.sum+ (1|site.name/harvest),REML=T,data=Databiom) #Fit the model again with REML
summary(napmodmixed4)
#writing the model to obtain directly the estimates
napmodmixed4b<-lmer(prodtot~-1+landuse+treatment+rain.sum+ (1|site.name/harvest),REML=T,data=Databiom) #Fit the model again with REML
summary(napmodmixed4b)

#Fixed effects:
 # Estimate Std. Error t value
#landusepasture  0.434519   0.558163   0.778
#landusewild     0.655716   0.503728   1.302
#treatmentopen  -0.648243   0.123923  -5.231
#rain.sum        0.002991   0.001693   1.766

#Testing some of the assumptions
library(lattice)

plot(napmodmixed4,prodtot~resid(.),abline=0,data=Databiom)
#------------------------
#Testing some of the assumptions
library(lattice)
plot(model.daphnia, factor(pop)~resid(.), abline=0)
plot(model.daphnia, resid(.)~fitted(.)|factor(pop))
ranef(model.daphnia)                   

cm1<-lmer(spinlght~bodylght+pred+bodylght:pred +(bodylght|pop), REML=T)#Model selection should be done with ML
summary(cm1)
hist(ranef(cm1)$pop[,2])
cm2<-lmer(spinlght~bodylght+pred+bodylght:pred +(1|pop), REML=T)#Model selection should be done with ML
summary(cm2)
ranef(cm2)


#### CONS mixed model #####
consmodmixed <- lmer(constot~landuse*treatment +landuse*rain.sum + treatment*rain.sum + (1|site.name/harvest),REML=F,data=Databiom) # not working
summary(consmodmixed)
hist(resid(consmodmixed))

---------------------------------
#### NAP and CONS annual ####
# Need to have annual rainfall values per site (Feb-Feb?)
      # Need mean per site per harvest (mean.site.harvest)
      # Add all together (mean.site.annual)
---------------------------------

#### LME from QA ####
#Finding the right structure for the random effect----
#we test two different structures
model1<-lmer(spinlght~bodylght*pred +(bodylght|pop), REML=T) #random change in slope and intercept

model2<-lmer(spinlght~bodylght*pred +(1|pop), REML=T)#random variation in intercept only 

AIC(model1)
AIC(model2)

#finding the best model for the fixed effects----
#Model selection
library(MuMIn)


cm1<-lmer(spinlght~bodylght+pred+bodylght:pred +(bodylght|pop), REML=F)#Model selection should be done with ML
cm2<-lmer(spinlght~bodylght+pred+(bodylght|pop), REML=F)
cm3<-lmer(spinlght~bodylght+(bodylght|pop), REML=F)
cm4<-lmer(spinlght~pred+(bodylght|pop), REML=F)
cm5<-lmer(spinlght~1+(bodylght|pop), REML=F)

model.sel(cm1, cm2, cm3, cm4, cm5)

aictab(cand.set=cm, modnames=Modenames)

#Parameter estimates for the best model
cm1<-lmer(spinlght~bodylght+pred+bodylght:pred +(bodylght|pop), REML=T) #Fit the model again with REML
summary(cm1)
#writing the model to obtain directly the estimates
cm1b<-lmer(spinlght~-1+pred+bodylght:pred +(bodylght|pop), REML=T) #Fit the model again with REML
summary(cm1b)


#Fitting the model with nlme----

model.daphnia<-lme(spinlght~bodylght+pred+bodylght:pred, random=~bodylght|pop)
summary(model.daphnia)

#Testing some of the assumptions
library(lattice)
plot(model.daphnia, factor(pop)~resid(.), abline=0)
plot(model.daphnia, resid(.)~fitted(.)|factor(pop))
ranef(model.daphnia)                   

cm1<-lmer(spinlght~bodylght+pred+bodylght:pred +(bodylght|pop), REML=T)#Model selection should be done with ML
summary(cm1)
hist(ranef(cm1)$pop[,2])
cm2<-lmer(spinlght~bodylght+pred+bodylght:pred +(1|pop), REML=T)#Model selection should be done with ML
summary(cm2)
ranef(cm2)
#---------------------------

#### Temporal autocorrelation #####
# From Stu: 
#First you run a linear model, extract the residuals and look at these in acf – see if there is an temporal patterns in the residuals. You can see an example of what to look for in an ACF plot in the attached paper.
#Then the second is a model using lme with a temporal autocorrelation structure tern. corAR1. For this you need to have your plot (the unit of repeat measure) as the smallest thing in the mixed model structure i.e. site nested in block and then nested in plot.ID. Each plot needs a unique number that is then repeated through time. Then in the main model ~Time|random structure.

# Exploring
lmT<-lm(temp~  Disc_kg.m2  + HerbPRC +  total_dung + rain.sum.mm +NMDS2,SerEbio5climT )
summary(lmT)

# Extract residuals
E<-residuals(lmT,type="pearson")
I1<-!is.na(SerEbio5climT$temp)
I1
Efull<-vector(length=length(SerEbio5climT$tem))
Efull<-NA
Efull[I1]<-E
Efull

# Time auto-correlated
acf(Efull, na.action=na.pass,main="Auto-correlation plot for residuals")

names(SerEbio5NA)
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|farea/fblock/fpair/ fplot.ID) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = SerEbio5NA)
corMatrix(cs1AR1.)

B1<-lme(rain.sum.mm~moist,
        random= ~ 1|farea/fblock/fplot, na.action=na.pass, method="REML",
        correlation=corAR1(0.2, form=~YrMonthNumber|farea/fblock/fpair/fplot.ID),data=SerEbio5NA)
summary(B1)
anova(B1) # Highly signficant
AIC(B1) #7923.711
-----------------------------------
  # Trying out...
  napmod <- lm(prodtot~landuse*region*treatment*rain.sum,data=Databiom)

#Extracting residuals
E <- residuals(napmod,type="pearson")
I1 <- !is.na(Databiom$prodtot)
Efull <- vector(length=length(Databiom$prodtot))
Efull <- NA
Efull[I1]<- E
Efull

# Time auto-correlated
acf(Efull, na.action=na.pass,main="Auto-correlation plot for residuals")

names(Databiom)
cs1AR1 <- corAR1(0.2, form = ~YrMonth|region/landuse/site.name/block/plot.ID) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = Databiom)
corMatrix(cs1AR1.)

#Running a mixed model with the autocorrelation
B1<-lme(rain.sum.mm~moist,
        random= ~ 1|farea/fblock/fplot, na.action=na.pass, method="REML",
        correlation=corAR1(0.2, form=~YrMonth|region/landuse/site.name/block/plot.ID),data=Databiom)
summary(B1)
anova(B1)
AIC(B1)
-----------------------------------------
  
-----------------------------------------------------
#### USEFUL STUFF ####
#### Combined graphs ####
library(grid)
require(gridExtra)
grid.arrange(Nt, Nt2, ncol=2,nrow=1)
####Rounding decimals ####
round(column, digits=2)
####Replacing one value in a variable ####
my.df$V2[my.df$V2 == "-sh2"] <- -100
####From wide to long format ####
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(olddata_wide, condition, measurement, control:cond2, factor_key=TRUE)

