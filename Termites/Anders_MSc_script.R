####Loading data Anders Sundsdal MSc####
rm(list=ls())
library(lattice)
library(MASS)
library(ggplot2)
library(lme4)
library(glmmTMB)

setwd("C:/Users/ansun/Documents/Master data/GitHub__R-stat/AfricanBioServices-Vegetation-and-soils")
wsdata<- read.csv('Termites/Main & CG experiment/Wetseason.csv', sep=';',dec='.')#Wetseason data
dsdata <- read.csv("Termites/Main & CG experiment/Dryseason.csv", sep=";",dec=".")#Dryseason data
head(wsdata)
head(dsdata)
fulldata<-rbind(wsdata,dsdata)
fulldata$Massloss.per <- (1-fulldata$Ashed.final.corrected.weight..tea.only..g./fulldata$Ashed.initial.corrected.weight..tea.only..g.)*100


####Data exploration####
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?

#### Outliers####
#Ashed final weight

dotchart(fulldata$Ashed.final.corrected.weight..tea.only..g.)
plot(fulldata$Ashed.final.corrected.weight..tea.only..g.)
#identify(fulldata$Ashed.final.corrected.weight..tea.only..g)

#Outlier in ashed data % (should be between 0-100):

dotchart(fulldata$Ashed.final.subsample.percentage.....) # One very large outlier for the final weight subsample percantage
plot(fulldata$Ashed.final.subsample.percentage.....)
#identify(fulldata$Ashed.final.subsample.percentage.....) #[1] 516

#Checking outliers in initial and final weights:
#Inital weight:
dotchart(fulldata$Initial.weight.including.bag..cord.and.label..g.)
plot(fulldata$Initial.weight.including.bag..cord.and.label..g.)
#identify(fulldata$Initial.weight.tea.only..g.)
#106 and 1611 looks wierd
fulldata[106,] # 1,191 may be too low for initial weight! Have to check for speling mistake
fulldata[1611,] #0,229 is too low for initial weight! Have to check for speling mistake
#No need to remove these as long as the difference in mass loss make sense.

#Checking for massloss outlier
dotchart(fulldata$Massloss..g. ) #
plot(fulldata$Massloss..g.)
#identify(fulldata$Massloss..g.)
fulldata[289,] # Minus massloss check this code: R414
fulldata[1614,] # Minus mass loss check this code: R275
#Removing these for now: 289 and 1614 rows:
fulldata <- fulldata[-c(289, 1614), ]
#Checkig outliers in temperature and moisture:
dotchart(fulldata$Temperature..C.) #Looks ok
plot(fulldata$Temperature..C.)
#identify(fulldata$Temperature..C.)
dotchart(fulldata$Moisture..) #Looks ok
plot(fulldata$Moisture..)

#####B Collinearity X####
MyVar<-c("Massloss..g.","rain.sum..mm.","CLAY..","SILT..",
         "SAND..","OC..","Moisture..","Temperature..C.")
pairs(fulldata[,MyVar]) #Not sure whats up here...
plot(fulldata$Moisture..,fulldata$Temperature..C.)


####Housekeeping # Ensuring factors are factors####
names(fulldata)
fulldata$fsite<-as.factor(fulldata$Site)
fulldata$fregion<-as.factor(fulldata$Region)
fulldata$fseason<-as.factor(fulldata$Season)
fulldata$flanduse<-as.factor(fulldata$Landuse)
fulldata$ftreatment<-as.factor(fulldata$Treatment)
fulldata$flittertype<-as.factor(fulldata$Littertype) 
fulldata$fplot.id<-as.factor(fulldata$Plot) # 220 plots # correct
fulldata$fteabag_code<-as.factor(fulldata$Teabag.code) # 879 teabags - missing 1 teabag (Maybe one is a duplicate?)
fulldata$fsoil.class<-as.factor(fulldata$Soil.Class)
fulldata$fmound_type<-as.factor(fulldata$Mound.type)
fulldata$ftree_ants<-as.factor(fulldata$Tree.with.ants)
fulldata$fblock<-as.factor(fulldata$Block)
fulldata$fblockcode<-as.factor(fulldata$Blockcode)
fulldata$fholes<-as.factor(fulldata$Sign.of.hole.s.)
fulldata$fcheeting<-as.factor(fulldata$Sign.of.termite.cheeting)
fulldata$froots<-as.factor(fulldata$Sign.of.roots)

names(fulldata)

#### Seperate the experiments ####
FulldataCG<-fulldata[fulldata$Experiment=="CG",] # Only commongarden data
FulldataCG<-droplevels(FulldataCG) # Ensure factor level "Main" is removed.

FulldataMain<-fulldata[fulldata$Experiment!="CG",] #Only landuse experiement data
FulldataMain<-droplevels(FulldataMain)# Ensure factor level common garden dropped

levels(fulldata$Landuse) # All levels incl. common garden
levels(FulldataMain$Landuse)
levels(FulldataCG$Landuse)

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
# Main experiment means and standard error (exclude blocks) # From Stu: Need to seperate out Agricutlure in Makao and Mwantimba(WHY?)
names(FulldataMain)
#Creating means by landuse (excluding blocks)
FulldataMainmean1<-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, FulldataMain, mean)
FulldataMainse1 <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, FulldataMain, se)
#Creating new column with the SE in the Mainmean dataset.
FulldataMainmean1$SE <- FulldataMainse1$Massloss.per 


#### Graphing: Main experiment - decomposition across landuse #### 
# Fill by termite * landuse = empty = absence, filled = prescence 
FulldataMainmean1$tea.hole<-as.factor(with(FulldataMainmean1, paste(flittertype, ftreatment, sep=" ")))
levels(FulldataMainmean1$tea.hole)
#levels(FulldataMainmean$fregion)<-c("Dry region","Wet region")
FulldataMainmean1$fregion <- as.factor(FulldataMainmean1$fregion)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(FulldataMainmean1$fregion)
levels(FulldataMainmean1$fseason)
Mainexp <- FulldataMainmean1
#Now, ready for graphing: Main experiment massloss against landuse
#Want to reorder tea.hole to have a nicer legend:
Mainexp$tea.hole <- ordered(Mainexp$tea.hole, levels=c("Green Exclosed", "Rooibos Exclosed","Green Open","Rooibos Open"))
levels(Mainexp$tea.hole)

colnames(Mainexp)[1]<-"Season"
colnames(Mainexp)[2]<-"Region"
names(Mainexp)

####Point plot Main exp####
Mainp <- ggplot(data=Mainexp, aes(x=flanduse,y=Massloss.per,
                                   ymin=(Massloss.per-SE),
                                   ymax=(Massloss.per+SE),
                                   fill = tea.hole,
                                   color = flittertype,
                                   shape=flanduse)
                 )+
                   
   geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F) + 
   geom_point(size=5,stroke=1.2,position=position_dodge(width=.35),show.legend=T)+ 
   facet_grid(Region ~ Season, scale ="fixed", labeller=labeller(Region = c(`Dry`= "Dry Region", `Wet`="Wet Region"),
                                                                 Season = c(`Wet`= "Wet Season", `Dry`="Dry Season")))+
   scale_color_manual(values=c("green4", "orangered3"))+
   scale_fill_manual(values=c("green4","orangered3","white","white"))+
   scale_shape_manual(values=c(21,23,24))+
   guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
   scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))+
   xlab("Land-use")+
   ylab("Mass loss (%)"
        )+
   
   theme(rect = element_rect(fill ="transparent")
         ,panel.background=element_rect(fill="transparent")
         ,plot.background=element_rect(fill="transparent",colour=NA)
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank()
         ,panel.grid.major.x = element_blank()
         ,panel.grid.major.y = element_blank()
         ,axis.text=element_text(size=12,color="black")
         ,axis.title.y=element_text(size=14,color="black")
         ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
         ,axis.text.x = element_text(size=10,color="black",
                                     margin=margin(2.5,2.5,2.5,2.5,"mm"))
         ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
         ,axis.ticks.length=unit(-1.5, "mm")
         #,axis.line.y = element_blank()
         ,axis.line.x = element_blank()
         ,plot.margin = unit(c(8,50,5,5), "mm")
         ,strip.background = element_rect(fill="transparent",colour=NA)
         ,strip.text.x = element_text(size = 14,colour = "black")
         ,strip.text.y = element_text(size = 14,colour = "black")
        ,panel.spacing = unit(1, "lines")
         ,legend.background = element_rect(fill = "transparent")
         ,legend.title=element_blank()
         ,legend.position = c(1.3,0.5)
         ,legend.spacing.y = unit(-0.8, "mm")
         ,legend.key.height=unit(7.5,"mm")
         ,legend.key.width=unit(7.5,"mm")
         ,legend.key = element_rect(colour = NA, fill = NA)
         ,legend.key.size = unit(7,"mm")
         ,legend.text=element_text(size=12,color="black")
         
         )+
 
   annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
   annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
   annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
   annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15)
 
 Mainp
#ggsave("Termites//Main & CG experiment/Mainexp.png",
 #      width= 20, height = 15,units ="cm",bg ="transparent",
  #     dpi = 600, limitsize = TRUE)

#################################
####Stacked bar plot Main exp####
#################################
FulldataMain<-fulldata[fulldata$Experiment!="CG",] #Only landuse experiement data
 
#Creating 4 dataframes with each of the treatments and littertype in each.
Greenop<-FulldataMain[FulldataMain$Littertype=="Green" & FulldataMain$Treatment=="Open",] # Only Green Open data
Greenex<-FulldataMain[FulldataMain$Littertype=="Green" & FulldataMain$Treatment=="Exclosed",] # Only Green Open data
Redop<-FulldataMain[FulldataMain$Littertype=="Rooibos" & FulldataMain$Treatment=="Open",] # Only Green Open data
Redex<-FulldataMain[FulldataMain$Littertype=="Rooibos" & FulldataMain$Treatment=="Exclosed",] # Only Green Open data
 

#Creating dataframe with termite and microbe effect variable=Exlosed and Termite effect=Open minus exclosed):
GreendataT <- Greenop
GreendataM <- Greenop

GreendataM$Massloss.per <- Greenex$Massloss.per
GreendataT$Massloss.per <- abs(Greenop$Massloss.per-Greenex$Massloss.per)

ReddataT <- Redop
ReddataM <- Redop
ReddataM$Massloss.per<-Redex$Massloss.per
ReddataT$Massloss.per<- abs(Redop$Massloss.per-Redex$Massloss.per)

#Means and error
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
#Redtea
MeanReddataM<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataM,mean)
MeanReddataMSE<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataM,se)
MeanReddataM$SE<-MeanReddataMSE$Massloss.per
MeanReddataM$Decomposer <- "Microbe"

MeanReddataT<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataT,mean)
MeanReddataTSE<-aggregate(Massloss.per~fseason+fregion+flanduse,ReddataT,se)
MeanReddataT$SE<-MeanReddataTSE$Massloss.per
MeanReddataT$Decomposer <- "Termite"


#combine decomposition of red tea by termite and microbe:
ReddataTM <- rbind(MeanReddataM,MeanReddataT)
ReddataTM$Littertype <- "Recalcitrant"
#Green tea
MeanGreendataM<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataM,mean)
MeanGreendataMSE<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataM,se)
MeanGreendataM$SE<-MeanGreendataMSE$Massloss.per
MeanGreendataM$Decomposer <- "Microbe"

MeanGreendataT<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataT,mean)
MeanGreendataTSE<-aggregate(Massloss.per~fseason+fregion+flanduse,GreendataT,se)
MeanGreendataT$SE<-MeanGreendataTSE$Massloss.per
MeanGreendataT$Decomposer <- "Termite"

#Combine decomposition of red tea by termite and microbe:
GreendataTM <- rbind(MeanGreendataM,MeanGreendataT)
GreendataTM$Littertype <- "Labile"

#Combine both littertypes:
TMMasslossMain <- rbind(GreendataTM,ReddataTM) 
TMMasslossMain$Decomposer<-as.factor(TMMasslossMain$Decomposer)
levels(TMMasslossMain$Decomposer)

####BARPLOTTING#######################################
####Want to create three own grafs for each Landuse####
####showing termite and microbe contribution to decomposition in wet season and dry season.#### 

#Creating a fill factor:
TMMasslossMain$LD<-as.factor(with(TMMasslossMain, paste(Decomposer, Littertype, sep=" ")))
head(TMMasslossMain)
#Creating error bars for stacked barplot:
#Sort out each landuse into own dataframe
Agri <- droplevels(TMMasslossMain[TMMasslossMain$flanduse=="Agriculture",])
Pasture <- droplevels(TMMasslossMain[TMMasslossMain$flanduse=="Pasture",])
Wild <- droplevels(TMMasslossMain[TMMasslossMain$flanduse=="Wild",])
###################################
#Plotting a graph for each landuse#####
###################################
#AGRICULTURE#

#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- c("Litter Quality")

levels(Agri$LD)
levels(Agri$LD) <- c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite")
levels(Agri$LD)
levels(Agri$fseason)
levels(Agri$flanduse)
levels(Agri$Decomposer)
#Adjusting the upper SE for error bars for stacked barplot:
Agri$SE.up <- Agri$SE
Agri$SE.up[Agri$LD == "Labile Microbe"] <- with(Agri,SE[LD == "Labile Microbe"]+
                                                  #SE[LD == "Labile Termite"]+
                                               Massloss.per[LD=="Labile Termite"])

Agri$SE.up[Agri$LD == "Recalcitrant Microbe"] <- with(Agri,SE[LD == "Recalcitrant Microbe"]+
                                                  #SE[LD == "Recalcitrant Termite"]
                                                  +Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
Agri$Mass.stop <- Agri$Massloss.per
Agri$Mass.stop[Agri$LD == "Labile Microbe"] <- with(Agri,Massloss.per[LD == "Labile Microbe"]+
                                                  Massloss.per[LD=="Labile Termite"])

Agri$Mass.stop[Agri$LD == "Recalcitrant Microbe"] <- with(Agri,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                        +Massloss.per[LD=="Recalcitrant Termite"])


AgriP <- ggplot(data=Agri,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
          geom_errorbar(position="identity",width=NA,lwd=1)+
          geom_bar(stat="identity",position="stack",width=0.9)+
          facet_wrap(~fseason+fregion,nrow=1)+
          scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
          scale_alpha_discrete(TitleDecomp,range=c(0.3,1))+
          xlab("")+ylab("Massloss (%)")+
  theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.background = element_rect(fill="transparent")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=14,color="black")
    ,axis.title.y=element_text(size=16,color="black", margin = margin(t = 0, r = 10, b = 0, l = 0))
    )
AgriP <-AgriP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0))
AgriP <- AgriP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

AgriP

ggsave("Termites/Main & CG experiment/Agriculture_Main_experiment.png",
     width= 20, height = 15,units ="cm",bg ="transparent",
    dpi = 600, limitsize = TRUE)

#PASTURE#
#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- c("Litter Quality")

levels(Pasture$LD)
levels(Pasture$LD) <- c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite")
levels(Pasture$LD)
levels(Pasture$fseason)
levels(Pasture$flanduse)
levels(Pasture$Decomposer)
#Adjusting the upper SE for error bars for stacked barplot:
Pasture$SE.up <- Pasture$SE
Pasture$SE.up[Pasture$LD == "Labile Microbe"] <- with(Pasture,SE[LD == "Labile Microbe"]+
                                                  #SE[LD == "Labile Termite"]+
                                                  Massloss.per[LD=="Labile Termite"])

Pasture$SE.up[Pasture$LD == "Recalcitrant Microbe"] <- with(Pasture,SE[LD == "Recalcitrant Microbe"]+
                                                        #SE[LD == "Recalcitrant Termite"]
                                                        +Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
Pasture$Mass.stop <- Pasture$Massloss.per
Pasture$Mass.stop[Pasture$LD == "Labile Microbe"] <- with(Pasture,Massloss.per[LD == "Labile Microbe"]+
                                                      Massloss.per[LD=="Labile Termite"])

Pasture$Mass.stop[Pasture$LD == "Recalcitrant Microbe"] <- with(Pasture,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                            +Massloss.per[LD=="Recalcitrant Termite"])


PastureP <- ggplot(data=Pasture,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
  geom_errorbar(position="identity",width=NA,lwd=1)+
  geom_bar(stat="identity",position="stack",width=0.9)+
  facet_wrap(~fseason+fregion,nrow=1)+
  scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
  scale_alpha_discrete(TitleDecomp,range=c(0.3,1))+
  xlab("")+ylab("Massloss (%)")+
  theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.background = element_rect(fill="transparent")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=14,color="black")
    ,axis.title.y=element_text(size=16,color="black", margin = margin(t = 0, r = 10, b = 0, l = 0))
  )
PastureP <-PastureP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0))
PastureP <- PastureP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

PastureP

ggsave("Termites/Main & CG experiment/Pasture_Main_experiment.png",
       width= 20, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)


#WILD#
#Legend title:
TitleDecomp<-"Decomposer"
TitleLitter <- c("Litter Quality")

levels(Wild$LD)
levels(Wild$LD) <- c("Labile Microbe", "Recalcitrant Microbe","Labile Termite","Recalcitrant Termite")
levels(Wild$LD)
levels(Wild$fseason)
levels(Wild$flanduse)
levels(Wild$Decomposer)
#Adjusting the upper SE for error bars for stacked barplot:
Wild$SE.up <- Wild$SE
Wild$SE.up[Wild$LD == "Labile Microbe"] <- with(Wild,SE[LD == "Labile Microbe"]+
                                                        #SE[LD == "Labile Termite"]+
                                                        Massloss.per[LD=="Labile Termite"])

Wild$SE.up[Wild$LD == "Recalcitrant Microbe"] <- with(Wild,SE[LD == "Recalcitrant Microbe"]+
                                                              #SE[LD == "Recalcitrant Termite"]
                                                              +Massloss.per[LD=="Recalcitrant Termite"])
#Adjusting the lower SE for error bars (I want the lower to be same as the mean value)
Wild$Mass.stop <- Wild$Massloss.per
Wild$Mass.stop[Wild$LD == "Labile Microbe"] <- with(Wild,Massloss.per[LD == "Labile Microbe"]+
                                                            Massloss.per[LD=="Labile Termite"])

Wild$Mass.stop[Wild$LD == "Recalcitrant Microbe"] <- with(Wild,Massloss.per[LD == "Recalcitrant Microbe"]+
                                                                  +Massloss.per[LD=="Recalcitrant Termite"])


WildP <- ggplot(data=Wild,aes(x=Littertype,y=Massloss.per,fill=LD,alpha=Decomposer,ymax=Massloss.per+SE.up,ymin=Mass.stop))+
  geom_errorbar(position="identity",width=NA,lwd=1)+
  geom_bar(stat="identity",position="stack",width=0.9)+
  facet_wrap(~fseason+fregion,nrow=1)+
  scale_fill_manual(TitleLitter,values=c("Green4","Orangered3","Green4","Orangered3"))+
  scale_alpha_discrete(TitleDecomp,range=c(0.3,1))+
  xlab("")+ylab("Massloss (%)")+
  theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.background = element_rect(fill="transparent")
    ,legend.text = element_text(size=12,color="black")
    ,legend.title = element_text(size=14,color="black")
    ,axis.title.y=element_text(size=16,color="black", margin = margin(t = 0, r = 10, b = 0, l = 0))
  )
WildP <-WildP + scale_y_continuous(limits=c(0,100), breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), expand=c(0,0))
WildP <- WildP+guides(alpha=F, fill=guide_legend(override.aes =list(size=6,fill=c("Green4","Orangered3","Green4","Orangered3"), alpha=c(0.3,0.3,1,1))))

WildP

ggsave("Termites/Main & CG experiment/Wild_Main_experiment.png",
       width= 20, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

##########################
#### END OF BARPLOTTING ########################################################
##########################


#### Graphing: CGvsMain experiment ####
#But first sorting the commongarden data and main experiemnt for GGplot:

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
# Main experiment means and standard error (include blocks) # From Stu: Need to seperate out Agricutlure in Makao and Mwantimba(WHY?)
names(FulldataMain)
#Creating means by landuse (excluding blocks)
FulldataMainmean<-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, FulldataMain, mean)
FulldataMainse <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, FulldataMain, se)
#Creating new column with the SE in the Mainmean dataset.
FulldataMainmean$SE <- FulldataMainse$Massloss.per 
# Fill by termite * landuse = empty = absence, filled = prescence 
FulldataMainmean$tea.hole<-as.factor(with(FulldataMainmean, paste(flittertype, ftreatment, sep=" ")))
levels(FulldataMainmean$tea.hole)
#levels(FulldataMainmean$fregion)<-c("Dry region","Wet region")
FulldataMainmean$fregion <- factor(FulldataMainmean$fregion)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(FulldataMainmean$fregion)
levels(FulldataMainmean$fseason)

#Common garden means and standard error (Exclude blocks)
names(FulldataCG)
FulldataCGmean <- aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, FulldataCG, mean)
FulldataCGse <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse, FulldataCG, se)
#Creating new column with the SE in the CGmean dataset.
FulldataCGmean$SE <- FulldataCGse$Massloss.per
# Fill by termite * landuse = empty = absence, filled = prescence 
FulldataCGmean$tea.hole<-as.factor(with(FulldataCGmean, paste(flittertype, ftreatment, sep=" ")))
levels(FulldataCGmean$tea.hole)
levels(FulldataCGmean$fregion)
levels(FulldataCGmean$fseason)

#Need to add Intermediate into Maindata set. To have the local soil appear on the 1:1 line in the upcoming graph.
IntermediateCG<-FulldataCGmean[FulldataCGmean$fregion=="Intermediate",] #extracting rows with "intermediate" from CG dataset

FulldataMainmean<-rbind(FulldataMainmean,IntermediateCG) #Putting in the intermediate data into main dataset.
levels(FulldataMainmean$fregion)

#Putting CG means and SE with Main means and Se in the same dataset
names(FulldataMainmean)
names(FulldataCGmean)
#Renaming colums to restrict merging of the Massloss and SE from the two experiments, when using merge() later.
colnames(FulldataMainmean)[6]<-"massloss.perMain"
colnames(FulldataMainmean)[7]<-"SEMain"
colnames(FulldataCGmean)[6]<-"massloss.perCG"
colnames(FulldataCGmean)[7]<-"SECG"
#Now that we have same amount of observations in both CG and Main dataset, we combine the massloss columns to create on dataset:
MainCG <- merge(FulldataMainmean,FulldataCGmean)

#Want to reorder tea.hole to have a nicer legend:
MainCG$tea.hole <- ordered(MainCG$tea.hole, levels=c("Green Exclosed", "Rooibos Exclosed","Green Open","Rooibos Open"))
levels(MainCG$tea.hole)

#Now, ready for graphing: Main experiment vs CG
names(MainCG)

colnames(MainCG)[1]<-"Season"
MainCGplot<-ggplot(MainCG, aes(x=massloss.perMain, y=massloss.perCG, fill=tea.hole,color=flittertype,shape=flanduse)) +
  geom_abline(slope=1, intercept=0, size =.95) + 
  geom_errorbar(aes(ymin = massloss.perCG-SECG,ymax = massloss.perCG+SECG),show.legend=F) + 
  geom_errorbarh(aes(xmin = massloss.perMain-SEMain,xmax = massloss.perMain+SEMain),show.legend=F) +
  geom_point(size=4.5,stroke=1.5, show.legend=T) +
  facet_wrap(~Season, scale ="fixed", labeller=labeller(Season = c(`Wet`= "Wet Season", `Dry`="Dry Season"))) +
  scale_color_manual(values=c("green4","orangered3")) +
  scale_fill_manual(values=c("green4","orangered3","white","white"))+
  scale_shape_manual(values=c(21,23,24,22)) + 
  guides(fill = guide_legend(override.aes=list(shape=25, color=c("green4","orangered3","green4","orangered3"))),color=F)+
  scale_x_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))+
  scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))+
  xlab("Main experiment mass loss (%)") +  ylab("Common garden mass loss (%)") +
 
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
        ,axis.text.x = element_text(size=12,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.line.y = element_blank()
        ,axis.line.x = element_blank()
        ,plot.margin = unit(c(15,45,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1.5, "lines")
        ,legend.background = element_rect(fill = "transparent")
        ,legend.title=element_blank()
        ,legend.position = c(1.1, 0.50)
        ,legend.spacing.y = unit(-0.8, "mm")
        ,legend.key.height=unit(7.5,"mm")
        ,legend.key.width=unit(7.5,"mm")
        ,legend.key = element_rect(colour = NA, fill = NA)
        ,legend.key.size = unit(7,"mm")
        ,legend.text=element_text(size=11,color="black"))+
  
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  5, yend = 18,
           linetype = "dashed", color = "white",size = 1.15) +
  annotate(geom = "segment", x = 5, xend = 18, y =  -Inf, yend = -Inf,
           linetype = "dashed", color = "white",size = 1.15)



MainCGplot
#ggsave("Termites/Main & CG experiment/CommongardenvsMain.png",
 #     width= 30, height = 15,units ="cm",bg ="transparent",
  #   dpi = 600, limitsize = TRUE)


###Analysis####
###MODELLING####
####LANDUSE EXPERIMENT####
###Mixed linear effect model####
library(lme4)
library(nlme)

names(FulldataMain)
FulldataMain$Blockcode
firstmodel <- lmer(Massloss.per~Littertype+Landuse+Season+Region+Treatment+
                     Littertype:Landuse+Littertype:Season+Littertype:Region+Littertype:Treatment+
                     Landuse:Season+Landuse:Region+Landuse:Treatment+
                     Season:Region+Season:Treatment+
                     Littertype:Landuse:Treatment+
                     Littertype:Landuse:Season+
                     (1|Blockcode), data=FulldataMain, REML=T)

summary(firstmodel)
anova(firstmodel)
drop1(firstmodel,test="Chisq")

#Creating two data set for each littertype to be able to do a four-way interaction (UPDATE: NOT POSSIBLE WITH THIS DATA). We know already that Littertype is important so thats why we extract the data into littertype.
RecalDataMain <- droplevels(FulldataMain[FulldataMain$Littertype =="Rooibos",])
LabileDataMain <- droplevels(FulldataMain[FulldataMain$Littertype =="Green",])

####MIXED LINEAR MODEL FOR EACH LITTERTYPE####

Recalmodel <- lmer(Massloss.per~Landuse+Season+Region+Treatment+
                     Landuse:Season+Landuse:Region+Landuse:Treatment+
                     Season:Region+Season:Treatment+
                     Treatment:Landuse:Season+
                     Treatment:Landuse:Region+
                     Season:Landuse:Region+
                     (1|Blockcode), data=RecalDataMain, REML=T)

Labilemodel <- lmer(Massloss.per~Landuse+Season+Region+Treatment+
                      Landuse:Season+Landuse:Region+Landuse:Treatment+
                      Season:Region+Season:Treatment+
                      Treatment:Landuse:Season+
                      Treatment:Landuse:Region+
                      Season:Landuse:Region+
                      (1|Blockcode), data=LabileDataMain, REML=T)
summary(Recalmodel)
anova(Recalmodel)

summary(Labilemodel)
anova(Labilemodel)
drop1(Recalmodel,test="Chisq")
# Df    AIC     LRT   Pr(Chi)    
# <none>                      5667.4                      
# Landuse:Season:Treatment  2 5682.5 19.1388 6.983e-05 ***
#   Landuse:Region:Treatment  3 5673.6 12.2436  0.006594 ** 
#   Landuse:Season:Region     2 5667.7  4.2925  0.116923  

drop1(Labilemodel,test="Chisq")
# Df    AIC    LRT   Pr(Chi)    
# <none>                      4644.3                     
# Landuse:Season:Treatment  2 4641.2  0.898    0.6382    
# Landuse:Region:Treatment  3 4639.5  1.206    0.7516    
# Landuse:Season:Region     2 4681.8 41.553 9.481e-10 ***
#   ---
install.packages("lmerTest")
install.packages("emmeans")
library(lsmeans)
library(emmeans)
library(lmerTest)


#Working with model with Recalcitrant littertype####

emmeans(Recalmodel,"Landuse:Season:Treatment")
#class(Recalmodel)
#diffemmeans(Recalmodel,test.effs="Landuse:Season:Treatment")



summary(Recalmodel)
anova(Recalmodel)
AIC(Recalmodel) #11589.62

#CHecking assumptions for the linearity

E1 <- resid(Recalmodel, type ="pearson")  #THIS IS FOR lme4..NOT lme, in lme = "type = "n"
F1 <- fitted(Recalmodel)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
# Fitted values both above and below 0, and it looks OK spread around 0.

#Checking for autocorrelation
library(itsadug)
plot(acf_resid(Recalmodel), type="b",alpha=0.05)
abline(c(0,0), lty = 2, col = 1)





####Modelling Landuse main experiment####

















#### Provisional analysis####

# Rooibos Open only for mound effect
decomp1R<-decomp1[decomp1$littertype=="Rooibos",]
decomp1Ro<-decomp1R[decomp1R$treatment=="Open",]
droplevels(decomp1Ro)
decomp1Ro$treatment
plot(decomp1Ro$massloss.per~decomp1Ro$N.D.Mound_m, col=c(decomp1Ro$landuse))
abline(lm(decomp1Ro$massloss.per~decomp1Ro$N.D.Mound_m)) # Further away from mounds higher mass loss??

# Convert percentage to proportion 0 - 1
decomp1$Mass.loss.beta<-(decomp1$massloss.per/100)
decomp1Ro$Mass.loss.beta<-(decomp1Ro$massloss.per/100)
names(decomp1Ro)
plot(massloss.per~N.D.Mound_m, data=decomp1, col=c(flittertype), pch=c(decomp1$ftreatment))
plot(massloss.per~N.D.Mound_m, data=decomp1Ro, col=c(flittertype), pch=c(decomp1$ftreatment))

# Remove NAs, Common garden # Seronera
decomp1b<-decomp1[!is.na(decomp1$massloss.per),]
decomp1c<-decomp1b[decomp1b$fsite!="Seronera",]
decomp1c<-decomp1b[decomp1b$experiment!="common",]
decomp1d<-decomp1c[decomp1c$fregion!="Local",]
decomp1d<-droplevels(decomp1d)
dim(decomp1d) # 663 53

# Block needs to be a unique number - not repeated 
decomp1d$fblockdesign.num<-as.factor(with(decomp1d, paste(fregion,fsite,flanduse,fblockdesign, sep="")))
decomp1d$fblockdesign.num<-as.numeric(decomp1d$fblockdesign.num)
decomp1d$fblockdesign.num<-as.factor(decomp1d$fblockdesign.num)
table(decomp1d$fregion, decomp1d$flanduse) # OK balance


names(decomp1d)

# Mixed linear model
AndersTea<-lmer(massloss.per~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                  flanduse:fregion+fregion*flittertype*ftreatment+sand.per+flanduse:sand.per+tempC+
               (1|fblockdesign.num/fplot.id), na.action=na.omit, data = decomp1d)
summary(AndersTea)

# Check assumptions
par(mfrow=c(1,1))
plot(fitted(AndersTea),residuals(AndersTea)) # Not good rooibos huge variation and green small
abline(h = 0, lty = 2) 
abline(v = 0, lty = 2)
qqnorm(resid(AndersTea))
qqline(resid(AndersTea)) # Not good - higher loss - break from qqline

# Residuals to each covariate # Readjust with interactions..
plot(AndersTea,flittertype~resid(.),abline=0) # Rooibos larger resid error
plot(AndersTea,ftreatment~resid(.),abline=0) # Open larger residual error
plot(AndersTea,flanduse~resid(.),abline=0) # OK
plot(AndersTea,fregion~resid(.),abline=0) # OK

# Checking resids vs fit
E1 <- resid(AndersTea, type = "pearson")
F1 <- fitted(AndersTea)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)


# Mixed linear model with beta (0 -1)
# Beta model with glmmTMB
# Issues with beta distribution cannot be zero or 1 exactly...
decomp1d$Mass.loss.beta[decomp1d$Mass.loss.beta<0.01]<-0.01
min(decomp1d$Mass.loss.beta)
plot(decomp1d$Mass.loss.beta)

decomp1e<-decomp1d[!is.na(decomp1d$N.D.Mound_m),] # If included in model - NO NAS
dim(decomp1e)
#AndersTeaGMT<-glmmTMB(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
#                        flanduse:fregion+
#                        (1|fblockdesign.num/fplot.id),
#                family=list(family="beta",link="logit"),
#                data = decomp1d)


AndersTea<-glmmadmb(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                      flanduse:ftreatment+flanduse:fregion+sand.per+sand.per:flanduse+tempC+
                      fregion*flittertype*ftreatment+flanduse*flittertype*ftreatment+
                      (1|fblockdesign.num/fplot.id),
                family="beta",data = decomp1d)
                #admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))

# First tried: admb.opts=admbControl(shess=FALSE,noinit=FALSE)
# Second: admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5) # No improvement
# start=list(RE_sd=rep(1,2),RE_cor=rep(0,2))
# If everything fails = xtra.args="-ams 500000000 -simplex" 

summary(AndersTea)

#Deviance
# Opt 1
sum(residuals(AndersTea, type="pearson")^2) / df.residual(AndersTea) #0.7316665 # bad deviance

# Opt 2
#Option 2
#(100*(Null deviance-Residual deviance)/Null deviance)

#                   Null deviance - residual deviance
#Generalized R^2 = ------------------------------------#
#                           Null deviance
sum(resid(AndersTea))
-2*logLik(AndersTea) # 'log Lik.' -238.472 (df=9)
(-759.74--23.87073)/-759.74 # 0.9685804 # Good

# Remove factors - what is important
drop1(AndersTea,test="Chi")

#Mass.loss.beta ~ flittertype + ftreatment + flanduse + fregion + 
#Df     AIC    LRT Pr(>Chi)    
#<none>                                -673.25                    
#tempC                            1 -711.68 0.060   0.8065
#flanduse:fregion                 2 -709.88 3.860   0.1451
#flanduse:sand.per                2 -711.43 2.314   0.3144
#flittertype:ftreatment:fregion   1 -711.74 0.004   0.9496
#flittertype:ftreatment:flanduse  2 -713.07 0.668   0.7161 

# Drop tempC and littertype:treatment:rainfall region

AndersTea2<-glmmadmb(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                       flanduse:ftreatment+flanduse:fregion+sand.per+(1|fblockdesign.num/fplot.id),
                    family="beta",data = decomp1d)
summary(AndersTea2)

# Remove factors - what is important
drop1(AndersTea2,test="Chi")

#Df     AIC    LRT  Pr(>Chi) 
#sand.per                1 -666.53 19.084 1.251e-05 ***
#flittertype:ftreatment  1 -646.42 39.190 3.845e-10 ***
#flanduse:fregion        2 -676.79 10.824  0.004463 ** 

# Test significance of factors
AndersTea2a <- update(AndersTea2, .~. -flanduse:fregion)
AndersTea2b <- update(AndersTea2, .~. -flittertype:ftreatment)
AndersTea2c <- update(AndersTea2, .~. -sand.per)
AndersTea2d <- update(AndersTea2a, .~. -fregion)
AndersTea2e <- update(AndersTea2a, .~. -flanduse)
AndersTea2f <- update(AndersTea2b, .~. -flittertype)
AndersTea2g <- update(AndersTea2b, .~. -ftreatment)

# Interaction results - when removed from model
anova(AndersTea2,AndersTea2a) # flanduse:fregion signficiant
anova(AndersTea2,AndersTea2b) # flittertype:ftreatment significant
anova(AndersTea2,AndersTea2c) # Sand signficiant
anova(AndersTea2a,AndersTea2d) # Rainfall region NS
anova(AndersTea2a,AndersTea2e) # Landuse singificant
anova(AndersTea2b,AndersTea2f) # Litter type significant
anova(AndersTea2b,AndersTea2g) # Treatment significant

# Sand significant
plot(Mass.loss.beta~sand.per, decomp1d)
abline(lm(Mass.loss.beta~sand.per, decomp1d)) # Positive  - seems very marginal 
# Should check if colinear with landuse...


#### Predicted values from final model####
# Final model # Wthout sand for prediction
AndersTea2b<-glmmadmb(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+
                        flanduse:fregion+(1|fblockdesign.num/fplot.id),
                     family="beta",data = decomp1d)

# Plot predicted vs actual data
MyData5 <- expand.grid(flittertype = levels(decomp1d$flittertype),
                      ftreatment = levels(decomp1d$ftreatment),
                      flanduse = levels(decomp1d$flanduse),
                      fregion = levels(decomp1d$fregion))

#Create X matrix with expand.grid
X5 <- model.matrix(~flittertype+ftreatment+flanduse+fregion+flittertype:ftreatment+flanduse:fregion, data =MyData5)
head(X5)

#Calculate predicted values from beta distribution
#var[Y_i] = P_i * (1 - Pi) / (1 + phi) # Beta

#Extract parameters and parameter covariance matrix
betas2    <- fixef(AndersTea2b)
Covbetas2 <- vcov(AndersTea2b)

#Calculate the fitted values in the predictor scale
MyData5$eta <- X5 %*% betas2
MyData5$Pi  <- exp(MyData5$eta ) / (1 + exp(MyData5$eta))

#Calculate the SEs on the scale of the predictor function
MyData5$se    <- sqrt(diag(X5 %*% Covbetas2 %*% t(X5)))
MyData5$SeUp  <- exp(MyData5$eta + 1.96 *MyData5$se) / 
  (1 + exp(MyData5$eta  + 1.96 *MyData5$se))
MyData5$SeLo  <- exp(MyData5$eta - 1.96 *MyData5$se) / 
  (1 + exp(MyData5$eta  - 1.96 *MyData5$se))

#E. Combined predicted betas with actual beta data
# Remove predicted stems for all species = ONLY Zea mayas stem
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
CG.x<-aggregate(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion, data=decomp1d, mean)
CG.se<-aggregate(Mass.loss.beta~flittertype+ftreatment+flanduse+fregion, data=decomp1d, se)
MyData6<-cbind(MyData5,CG.x[5],CG.se[5])
colnames(MyData6)[7]<-"SE"
colnames(MyData6)[10]<-"Mass.loss"
colnames(MyData6)[11]<-"se"

# Convert Beta scaling back to percentage  *100
MyData6$Mass.loss<-MyData6$Mass.loss*100 
MyData6$se<-MyData6$se*100 
MyData6$SeUp<-MyData6$SeUp*100
MyData6$SeLo<-MyData6$SeLo*100

# Create second dataset for predictions
colnames(MyData5)[6]<-"Mass.loss"
MyData5$Mass.loss<-MyData5$Mass.loss*100 
MyData5$se<-MyData5$se*100 
MyData5$SeUp<-MyData5$SeUp*100
MyData5$SeLo<-MyData5$SeLo*100


# Fill by termite * landuse = empty = absence filled = prescence 
MyData6$tea.hole<-as.factor(with(MyData6, paste(flittertype, ftreatment, sep="")))
MyData5$tea.hole<-as.factor(with(MyData5, paste(flittertype, ftreatment, sep="")))


levels(MyData6$fregion)<-c("Dry region","Wet region")

# Mass loss by landuse - TBI WET SEASON
cp<- ggplot(data=MyData6, aes(y=Mass.loss,x=flanduse, shape=flanduse,
                              ymin=Mass.loss-se, ymax=Mass.loss+se, col=flittertype,fill=tea.hole))
#cp<- cp+ geom_errorbar(data=MyData5, aes(x=flanduse, ymin=SeLo , ymax=SeUp ),colour="dark grey", width=.1)
#cp<- cp+  geom_point(data = MyData5,aes(x =flanduse, y = Mass.loss),size = 3,
#                     color = "dark grey")
cp<- cp+ geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
cp<- cp+ geom_point(size=5,stroke=1.2,position=position_dodge(width=.35),show.legend=F) # Legend T on individual graph
cp<- cp+ facet_wrap(~fregion, scale ="fixed")
cp<- cp+ scale_color_manual(values=c("green4", "orangered3"))
cp<- cp+ scale_fill_manual(values=c("green4","white","orangered3","white"))
cp<- cp+scale_shape_manual(values=c(21,24,22))
cp<- cp+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
cp <- cp + xlab("Land-use") +  ylab("Mass loss (%)")  
cp <- cp + theme_bw() +
  theme(rect = element_rect(fill ="transparent")
        ,panel.background=element_rect(fill="transparent")
        ,plot.background=element_rect(fill="transparent",colour=NA)
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
        ,axis.text.x = element_text(size=12,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,legend.text=element_text(size=12,color="black")
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.ticks.x = element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="black", size = .75)
        ,plot.margin = unit(c(5,12,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1, "lines")
        ,legend.title=element_blank()
        #,legend.background = element_rect(fill = "transparent")
        ,legend.position = c(.99, .95)
        ,legend.spacing.y = unit(-0.5, "mm"))
cp <- cp + annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  5, yend = 18,
           linetype = "dashed", color = "white",size = 1.15) 
#cp <- cp + guides(colour=F, fill=F,shape = guide_legend(override.aes = list(shape=c(21,24,22),
#                                                                            size=3.2,fill="grey30",col="grey30", stroke=1.5)))

#cp2 <- cp +  geom_point(data = MyData6, aes(size=ftreatment, shape = NA), colour = "grey50")
#cp2 <- cp2 + guides(size=guide_legend("Source", override.aes=list(shape=c(21, 1), size=4.5,fill="grey30",col="grey30", stroke=1.5)))
#cp2 
cp

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/AndersMainLanduseWET.png",
       width= 25, height = 12,units ="cm", bg ="transparent",
       dpi = 600, limitsize = TRUE)