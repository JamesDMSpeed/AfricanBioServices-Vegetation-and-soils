########################################################################
#TBI Serengeti
#Anders Sundsdal MSc
#########################################################################
rm(list=ls())
library(lattice)
library(MASS)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(glmmADMB)
install.packages(glmmADMB)

#########################################################################

wsdata<- read.csv('Termites/Wetseason.csv', sep=';',dec='.')#Wetseason data
dsdata <- read.csv("Termites/Dryseason.csv", sep=";",dec=".")#Dryseason data
head(wsdata)
head(dsdata)
fulldata<-rbind(wsdata,dsdata)
fulldata$Massloss.per <- (1-fulldata$Ashed.final.corrected.weight..tea.only..g./fulldata$Ashed.initial.corrected.weight..tea.only..g.)*100


########################################################################
#Data exploration
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?
########################################################################
########################################################################
########################################################################
# Checking for outliers in ashed data
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

####################################################################################
###################################################################################
#####################################################################
#B Collinearity X
MyVar<-c("Massloss..g.","rain.sum..mm.","CLAY..","SILT..",
         "SAND..","OC..","Moisture..","Temperature..C.")
pairs(fulldata[,MyVar]) #Not sure whats up here...
plot(fulldata$Moisture..,fulldata$Temperature..C.)


#Housekeeping # Ensuring factors are factors
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
FulldataCG<-fulldata[fulldata$Landuse=="Common Garden",] # Only commongarden data
FulldataCG<-droplevels(FulldataCG) # Ensure factor level pasture,agriculture and wild dropped

FulldataMain<-fulldata[fulldata$Landuse!="Common Garden",] #Only landuse experiemnt data
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
FulldataMainmean1$tea.hole<-as.factor(with(FulldataMainmean1, paste(flittertype, ftreatment, sep="")))
levels(FulldataMainmean1$tea.hole)
#levels(FulldataMainmean$fregion)<-c("Dry region","Wet region")
FulldataMainmean1$fregion <- factor(FulldataMainmean1$fregion)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(FulldataMainmean1$fregion)
levels(FulldataMainmean1$fseason)

#Now, ready for graphing: Main experiment massloss against landuse
colnames(FulldataMainmean1)[1]<-"Season"
colnames(FulldataMainmean1)[2]<-"Region"
names(FulldataMainmean1)
Mainp <- ggplot(data=FulldataMainmean1, aes(x=flanduse,y=Massloss.per,
                                           ymin=Massloss.per-SE, ymax=Massloss.per+SE,
                                           fill = tea.hole,
                                           col = flittertype,
                                           shape=flanduse))
Mainp<- Mainp+ geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F) #Assign error bars on points.
Mainp<- Mainp+ geom_point(size=5,stroke=1,position=position_dodge(width=.35),show.legend=T) # Legend T on individual graph

Mainp<- Mainp+ facet_grid(Region ~ Season, scale ="fixed", labeller= label_both)
Mainp<-Mainp+scale_color_manual(values=c("green4", "orangered3")) #Gives what color the points shall have based on littertype
Mainp<- Mainp+ scale_fill_manual(values=c("green4","white","orangered3","white")) #Assign colour to the different categories within tea.hole column, shoulde be 4.
Mainp<- Mainp+scale_shape_manual(values=c(21,24,22))#Assgn the diferent shapes, here based on landuse
#Mainp<- Mainp+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
Mainp <- Mainp + xlab("Land-use") +  ylab("Mass loss (%)") 
Mainp
ggsave("Termites/Mainexp.png",
       width= 25, height = 15,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

#### Graphing: Main experiment and CG experiment ####

#But first sorting the commongarden data and main experiemnt for GGplot:

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
# Main experiment means and standard error (include blocks) # From Stu: Need to seperate out Agricutlure in Makao and Mwantimba(WHY?)
names(FulldataMain)
#Creating means by landuse (excluding blocks)
FulldataMainmean<-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse + fblock, FulldataMain, mean)
FulldataMainse <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse + fblock, FulldataMain, se)
#Creating new column with the SE in the Mainmean dataset.
FulldataMainmean$SE <- FulldataMainse$Massloss.per 
# Fill by termite * landuse = empty = absence, filled = prescence 
FulldataMainmean$tea.hole<-as.factor(with(FulldataMainmean, paste(flittertype, ftreatment, sep="")))
levels(FulldataMainmean$tea.hole)
#levels(FulldataMainmean$fregion)<-c("Dry region","Wet region")
FulldataMainmean$fregion <- factor(FulldataMainmean$fregion)#Need to "re-factor" the region as levels are changed fro 3 to 2 in landuse experiment (only wet and dry).
levels(FulldataMainmean$fregion)
levels(FulldataMainmean$fseason)

#Common garden means and standard error (include blocks)
names(FulldataCG)
FulldataCGmean <- aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse + fblock, FulldataCG, mean)
FulldataCGse <-aggregate(Massloss.per~fseason+fregion+ftreatment+flittertype+flanduse + fblock, FulldataCG, se)
#Creating new column with the SE in the CGmean dataset.
FulldataCGmean$SE <- FulldataCGse$Massloss.per
# Fill by termite * landuse = empty = absence, filled = prescence 
FulldataCGmean$tea.hole<-as.factor(with(FulldataCGmean, paste(flittertype, ftreatment, sep="")))
levels(FulldataCGmean$tea.hole)
levels(FulldataCGmean$fregion)
levels(FulldataCGmean$fseason)
#Putting CG means and SE with Main means and Se in the same dataset
names(FulldataMainmean)
names(FulldataCGmean)
#Renaming colums to restrict merging of the Massloss and SE from the two experiments, when using merge() later.
colnames(FulldataMainmean)[7]<-"massloss.perMain"
colnames(FulldataMainmean)[8]<-"SEMain"
colnames(FulldataCGmean)[7]<-"massloss.perCG"
colnames(FulldataCGmean)[8]<-"SECG"

write.csv(FulldataMainCGmean,"Termites/FulldataMainCGmean.csv")
#write.csv(FulldataCGmean,"Termites/FulldataCGmean.csv")
#write.csv(FulldataMainmean,"Termites/FulldataMainmean.csv")

#Rearrange the data into one csv file, then import it:
read.csv(FulldataMainCGmean )


#names(FulldataMainCGmean)
#levels(FulldataMainCGmean$fregion)
#levels(FulldataMainCGmean$fseason)

#Now, ready for graphing: Main experiment vs CG 

# facets for rain region # colour for littetype # holes for open
# Shape for land-use # Errors x and y # abline 1 = 1
names(FulldataMainCGmean)
CGmain<-ggplot(FulldataMainCGmean, aes(x=massloss.perMain, y=massloss.perCG, fill=tea.hole,colour=flittertype,shape=flanduse))
CGmain<-CGmain+geom_abline(slope=1, intercept=0, size =.95) # Reference line
CGmain<-CGmain+geom_errorbar(aes(ymin = massloss.perCG-SECG,ymax = massloss.perCG+SECG),show.legend=F) 
CGmain<-CGmain+geom_errorbarh(aes(xmin = massloss.perMain-SEMain,xmax = massloss.perMain+SEMain),show.legend=F)
CGmain<-CGmain+geom_point(size=4.5,stroke=1.5,show.legend=F)
CGmain<-CGmain+facet_wrap(~fregion, scale ="fixed")
CGmain<-CGmain+scale_color_manual(values=c("green4", "orangered3"))
CGmain<-CGmain+scale_fill_manual(values=c("green4","white","orangered3","white"))
CGmain<-CGmain+ scale_shape_manual(values=c(21,23,24,22))
CGmain<-CGmain+scale_x_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
CGmain<-CGmain+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
CGmain<-CGmain+ xlab("Main experiment mass loss (%)") +  ylab("Common garden mass loss (%)")  
CGmain<-CGmain+ #theme_bw() +
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
        ,axis.line.y = element_blank()
        ,axis.line.x = element_blank()
        ,plot.margin = unit(c(8,5,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1, "lines")
        #,legend.background = element_rect(fill = "transparent")
        ,legend.title=element_blank()
        ,legend.position = c(.1, .85)
        ,legend.spacing.y = unit(-0.5, "mm"))
CGmain<-CGmain+annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  5, yend = 18,
           linetype = "dashed", color = "white",size = 1.15) +
  annotate(geom = "segment", x = 5, xend = 18, y =  -Inf, yend = -Inf,
           linetype = "dashed", color = "white",size = 1.15)
#CGmain<-CGmain+guides(colour=F, fill=F,shape = guide_legend(override.aes = list(shape=c(21,23,24,22),
#                                                                         size=3.2,fill="grey30",col="grey30", stroke=1.5)))
CGmain
#CGmain2 <- CGmain + geom_point(data = FulldataMainmean, aes(size=ftreatment, shape = NA), colour = "grey50")
#CGmain2 <- CGmain2 + guides(size=guide_legend("Source", override.aes=list(shape=c(21, 1), size=3.2,fill="grey30",col="grey30", stroke=1.5)))
#CGmain2
ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/MainCGcorrelation2.png",
       width= 25, height = 12,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)










######################
cp<- ggplot(data=FulldataMainmean,
            aes(y=Massloss..g..Ash.uncorrected,x=flanduse, shape=flanduse,
                ymin=(Massloss..g..Ash.uncorrected-se),
                ymax=(Massloss..g..Ash.uncorrected+se),
                col=flittertype,fill=ftreatment)+
              )
cp
#cp<- cp+ geom_errorbar(data=MyData5, aes(x=flanduse, ymin=SeLo , ymax=SeUp ),colour="dark grey", width=.1)
#cp<- cp+  geom_point(data = MyData5,aes(x =flanduse, y = Mass.loss),size = 3,
#                     color = "dark grey")
cp<- cp+ geom_errorbar(width=.5,lwd=1,position=position_dodge(width=.35),show.legend=F)
cp<- cp+ geom_point(size=5,stroke=1.2,position=position_dodge(width=.35),show.legend=F) # Legend T on individual graph
cp<- cp+ facet_wrap(~fregion, scale ="fixed")
#cp<- cp+ facet_wrap(~fregion, scale ="fixed")
cp<- cp+ scale_color_manual(values=c("green4", "orangered3"))
cp<- cp+ scale_fill_manual(values=c("green4","white","orangered3","white"))
cp<- cp+scale_shape_manual(values=c(21,24,22))
cp<- cp+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
cp <- cp + xlab("Land-use") +  ylab("Mass loss (%)")  
cp <- cp + theme_bw() +
  theme(plot.background = element_blank()
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
        ,strip.background = element_blank()
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1, "lines")
        ,legend.title=element_blank()
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

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/AndersMainLanduseWET.jpeg",
       width= 25, height = 12,units ="cm",
       dpi = 600, limitsize = TRUE)




#################################################################################
# Common Garden versus Main experiment 
#################################################################################
names(fulldata)
# Seperate experiments
FulldataCG<-fulldata[fulldata$Landuse=="Common Garden",] # Only commongarden data
FulldataMain<-fulldata[fulldata$Landuse!="Common Garden",] #Only landuse experiemnt data


SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))# Function for Standard Error
# Main experiment means (include blocks) # Need to seperate out Agricutlure in Makao and Mwantimba
names(FulldataMain)
FulldataMainmean<-aggregate(Massloss..g..Ash.uncorrected~Season+fregion+ftreatment+flittertype+flanduse+fblock, FulldataMain, mean)

# Means by land-use without block
FulldataMainnmean2<-aggregate(Massloss..g..Ash.uncorrected~Season+fregion+ftreatment+flittertype+flanduse, FulldataMain, mean)
decompMainMean2SE<-aggregate(massloss.per~fregion+ftreatment+flittertype+flanduse,decompMain, SE)
decompMainMean2$se<-decompMainMean2SE$massloss.per
# 24 data points

# Common garden means including block # Need to seperate out Agricutlure in Makao and Mwantimba
decompCGMean<-aggregate(massloss.per~fregion+ftreatment+flittertype+flanduse+fblock,decompCG, mean)
#decompCGMean<-aggregate(massloss.per~fblockcode+ftreatment+flittertype,decompCG, mean)
# 110 # Dry_P3_CG1 and Dry_Ag2_CG3 missing

## Common garden means including block
decompCGMean2<-aggregate(massloss.per~fregion+ftreatment+flittertype+flanduse,decompCG, mean)
decompCG2SE<-aggregate(massloss.per~fregion+ftreatment+flittertype+flanduse,decompCG, SE)
decompCGMean2$se<-decompCG2SE$massloss.per
# 28 - includes Seronera + 4 litterbags
decompCGMean2b<-decompCGMean2[decompCGMean2$flanduse!="Local",]
plot(decompCGMean2b$massloss.per~decompMainMean2$massloss.per, col=c(decompCGMean2b$ftreatment), pch=c(decompCGMean2b$flanduse))
abline(1,1)

# Looks OK - adjust dataset in excel and re-import - issues with agriculture soil types - need to seperate out as pair

# Reimport main vs CG averages
decompCG<-read.csv("TBI.mainCGmassloss.csv", header=T)
names(decompCG)
levels(decompCG$ftreatment)
# Fill by termite * landuse = empty = absence filled = prescence 
decompCG$tea.hole<-as.factor(with(decompCG, paste(flittertype, ftreatment, sep="")))

# Graph main vs CG  # facets for rain region # colour for littetype # holes for open
# Shape for land-use # Errors x and y # abline 1 = 1
CGmain<-ggplot(decompCG, aes(x=massloss.perMain, y=massloss.perCG, fill=tea.hole,colour=flittertype,shape=flanduse))
CGmain<-CGmain+geom_abline(slope=1, intercept=0, size =.95) # Reference line
CGmain<-CGmain+geom_errorbar(aes(ymin = massloss.perCG-seCG,ymax = massloss.perCG+seCG),show.legend=F) 
CGmain<-CGmain+geom_errorbarh(aes(xmin = massloss.perMain-seMain,xmax = massloss.perMain+seMain),show.legend=F)
CGmain<-CGmain+geom_point(size=4.5,stroke=1.5,show.legend=F)
CGmain<-CGmain+facet_wrap(~fregion, scale ="fixed")
CGmain<-CGmain+scale_color_manual(values=c("green4", "orangered3"))
CGmain<-CGmain+scale_fill_manual(values=c("green4","white","orangered3","white"))
CGmain<-CGmain+ scale_shape_manual(values=c(21,23,24,22))
CGmain<-CGmain+scale_x_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
CGmain<-CGmain+scale_y_continuous(limits = c(5,95), expand = c(0,0),breaks = c(5,20,40,60,80), labels = c(0,20,40,60,80))
CGmain<-CGmain+ xlab("Main experiment mass loss (%)") +  ylab("Common garden mass loss (%)")  
CGmain<-CGmain+ #theme_bw() +
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
        ,axis.line.y = element_blank()
        ,axis.line.x = element_blank()
        ,plot.margin = unit(c(8,5,5,5), "mm")
        ,strip.background = element_rect(fill="transparent",colour=NA)
        ,strip.text.x = element_text(size = 16,colour = "black")
        ,panel.spacing = unit(1, "lines")
        #,legend.background = element_rect(fill = "transparent")
        ,legend.title=element_blank()
        ,legend.position = c(.1, .85)
        ,legend.spacing.y = unit(-0.5, "mm"))
CGmain<-CGmain+annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1.15) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y =  5, yend = 18,
         linetype = "dashed", color = "white",size = 1.15) +
annotate(geom = "segment", x = 5, xend = 18, y =  -Inf, yend = -Inf,
         linetype = "dashed", color = "white",size = 1.15)
#CGmain<-CGmain+guides(colour=F, fill=F,shape = guide_legend(override.aes = list(shape=c(21,23,24,22),
#                                                                         size=3.2,fill="grey30",col="grey30", stroke=1.5)))
CGmain
#CGmain2 <- CGmain + geom_point(data = decompCG, aes(size=ftreatment, shape = NA), colour = "grey50")
#CGmain2 <- CGmain2 + guides(size=guide_legend("Source", override.aes=list(shape=c(21, 1), size=3.2,fill="grey30",col="grey30", stroke=1.5)))
#CGmain2
ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Anders Sundsal/MainCGcorrelation2.png",
       width= 25, height = 12,units ="cm",bg ="transparent",
       dpi = 600, limitsize = TRUE)

# NOTE - transaprent does not work with jpeg


names(decomp1d)
aggregate(rain.sum.mm~region+site+flanduse+fblockdesign.num, decomp1d, mean) # CHECK rainfall data - does not make sense

###############################
# Provisional analysis

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
#Generalized R^2 = ------------------------------------
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


##################################################################
# Predicted values from final model
##################################################################
# Final model # Wthout sand for predictions
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


############################################################################################
# Soil texture triangle
library(lattice)
library(MASS)
library(soiltexture)
############################################################################################

#Insert data & observe data structure
Rootex<-decomp1<-read.csv("SoiltextAS.csv", header=T)

dim(Rootex) #28 14
str(Rootex)
head(Rootex)	
names(Rootex)

# Summary 
Soiltext.sum<-aggregate(cbind(Rootex$CLAY,Rootex$SILT,Rootex$SAND,Rootex$WETrain.sum.mm), by=list(Rootex$landuse,Rootex$block), mean)
colnames(Soiltext.sum)<-c("landuse","block","Clay","Silt","Sand","Rainfall (mm)")
names(Soiltext.sum)

my.text<-data.frame(Rootex$landuse,Rootex$block,Rootex$CLAY,Rootex$SILT,Rootex$SAND,Rootex$WETrain.sum.mm)
colnames(my.text)<-c("landuse","block","CLAY","SILT","SAND","Rainfall (mm)")

aggregate(cbind(CLAY,SILT,SAND)~site+landuse+region,Rootex,mean)


# Colour for rainfall
color_pallete_function2 <- colorRampPalette(
  colors = c("lightcyan", "deepskyblue","steelblue2", "blue4"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)

num_colors2 <- nlevels(as.factor(Rootex$WETrain.sum.mm))
num_colors2
diamond_color_colors2 <- color_pallete_function2(num_colors2)
my.text$pt.col<-diamond_color_colors2[as.factor(Rootex$WETrain.sum.mm)]
factCOLS<-unlist(levels(as.factor(my.text$pt.col)))


# Points for landuse
pt.landuse<- nlevels(Rootex$landuse) # 4 landuses
pt.to.landuse <- c(21,23,24,22)
my.text$pt.pch<-pt.to.landuse [Rootex$landuse]
my.text$pt.pch


# Load texture triangle space
geo     <- TT.plot(class.sys = "none", add=T, grid.show =F,frame.bg.col ="white", cex.axis = 1.2 , cex.lab=1.2, cex.main=1.2, lwd=1.2)

TT.plot( class.sys = "UK.SSEW.TT", main="",
         tri.data=my.text,frame.bg.col ="white", col=my.text$pt.col, bg="white",pch =my.text$pt.pch,# bg=my.text$pt.col,
         grid.show =F,cex.axis = 1.2 , cex.lab=1.2, cex.main=1.5, cex=1.75, lwd=9.9)
         #z.name="Rainfall (mm)", z.cex.range =c(0.5,2.5),
         #z.pch=my.text$pt.pch,
         #z.col.hue =0.99)

# Legend
max(Rootex$WETrain.sum.mm)
legend (x= 90,  y= 100, legend=levels(as.factor(Rootex$WETrain.sum.mm)),  pch=21, 
        pt.bg =rev(c(factCOLS)), cex =1.2, pt.cex=1.2,
        y.intersp =.8,x.intersp =.8, text.col=rev(c(factCOLS)), col=rev(c(factCOLS)), bty= "n")


# Seperate legend as gradient colour
#layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1)) # Can add to plot using layout
legend_image <- as.raster(matrix(rev(color_pallete_function2 (20)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Rainfall (mm)')
text(x=1.5, y = seq(0,1,l=5), labels = seq(170,230,l=5))
rasterImage(legend_image, 0, 0, 1,1)


#names(Rootex)
#Rootex[,"WETrain.sum.mm"] 
#z.cex.range <- TT.get("z.cex.range")
#def.pch <- par("pch")
#def.cex <- TT.get("cex")
#def.col <- par( "col")
#oc.str <- TT.str( Rootex[,"WETrain.sum.mm"],
#      z.cex.range[1],
#      z.cex.range[2])

#legend (x = 50,  y = 50,  title = "Rainfall (mm)",
#legend = formatC(c(max(Rootex[,"WETrain.sum.mm"] ), quantile(Rootex[,"WETrain.sum.mm"] ,probs=c(75,50,25)/100), min(Rootex[,"WETrain.sum.mm"] )), 
#                 format  = "f", digits  = 1, width   = 4, flag    = "0"), #
# pt.lwd = NA, col = c(factCOLS), pt.cex  = c(
#min( oc.str ),
# quantile(oc.str ,probs=c(25,50,75)/100), max( oc.str ) 
#pch = 19,
## ),  #,
#bty="o",
#y.intersp=1.4,x.intersp=1.2,
#bg=NA, 
#box.col = NA, # Uncomment this to remove the legend box
#text.col="black",
#cex = 1)




#### ANDERS script ######


# duplicates
decomp[duplicated(decomp$fTeabag.code), ]# G540 is a duplicate
#  Season Plot        Site Region..Wet.dry.       Landuse Block   Blockcode Treatment Teabag.code
# Wet  207 Mwantimba\n            Wet\n Common Garden   CG4 Wet_Ag3_CG4   Exclude        G540
#Initial.weight.including.bag..cord.and.label..g. Initial.weight.tea.only..g.
#                    2.083                       1.833
#Ashed.subsample.percentage.controll.initial.tea.only.... Ashed.initial.corrected.weight..tea.only..g.
#                              5.509                                        1.732
#Final.weight.including.bag.and.cord..no.label..g. Final.weight..tea.only..g.
#               0.733                      0.582
#Ashed.final.subsample.percentage..... Ashed.final.corrected.weight..tea.only..g. Burial.date Recovery.date
# 21.55                                      0.457  03.02.2017    19.03.2017
#incubation.time.red.and.green.tea..days. Massloss..g. fRegion      fLanduse       fSite fTreatment
#        44                                -1.275       Wet\n    Common Garden   Mwantimba\n    Exclude
#fTeabag.code fPlot.id
#      G540      207


########################################################################
#Data exploration
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?
########################################################################
# Alain Zuur - data exploration functions

#Missing values
colSums(is.na(decomp)) #Yes:
#incubation.time.red.and.green.tea..days.: 16 NAs 
# Massloss..g.: 106 NAs 
# Ashed.initial.corrected.weight..tea.only..g.: 2 NAs 
#Final.weight.including.bag.and.cord..no.label..g.: 67 NAs 
#Final.weight..tea.only..g.: 68 NAs
#Ashed.final.subsample.percentage..... : 83 NAs
#Ashed.final.corrected.weight..tea.only..g.: 104 NAs 
#Initial.weight.including.bag..cord.and.label..g.: 2
#|------>>>>NEED TO CHECK IF THESE ARE REAL NAs or if some should be "0".For example for final weights: A "0" is if the bag has been been retrieved, but was empty. For initial weights: check if the teacode is simply  not found due to slightly different naming.
#Number of real NA's should be atleast (according to the number of plots not found when rerieving the litterbags):


#Checking for outliers
  #Outlier in ashed data:
dotchart(decomp$Ashed.final.subsample.percentage.....) # One very large outlier for the final weight subsample percantage
plot(decomp$Ashed.final.subsample.percentage.....)
    #identify(decomp$Ashed.final.subsample.percentage.....) [1] 517, [1] 772
#decomp[517, ] #G509 has wrong percentage (-474.393).
#G509 needs re-ashing. 
decomp1<-decomp[decomp$Teabag.code!="G509",]#Removing this from the data for now...
decomp<-decomp1

#decomp[772, ] #Teabag R530, wrong percentage (-32.311).
#R530 needs re-ashing. Removed from the dataset for now...
decomp1<-decomp[decomp$Teabag.code!="R530",]#Removing this from the data for now...
decomp<-decomp1
#Checking again:
dotchart(decomp$Ashed.final.subsample.percentage.....) # One very large outlier for the final weight subsample percantage
plot(decomp$Ashed.final.subsample.percentage.....)

#Checking for massloss outlier
dotchart(decomp$Massloss..g. ) # Looks OK
plot(decomp$Massloss..g.)
  #identify(decomp$Massloss..g.)

#Dividing data between the main experiment (Pasture, Agri, Wild) and common garden (Seronera)
decompMain<-decomp[decomp$fLanduse!="Common Garden",] #Dataframe without common garden
decompMain<-droplevels(decompMain) # Ensure factor level common garden dropped
levels(decomp$Landuse) # All levels incl. common garden
levels(decompMain$fLanduse) #Common Garden is removed
decompCG<-decomp[decomp$fLanduse=="Common Garden",] #Dataframe with only common garden
decompCG<-droplevels(decompCG)
levels(decompMain$fLanduse)
levels(decompCG$fLanduse)

#################################################################################
# Working now with: decomMain dataframe (Agriculture, Pasture and Wild landuse) #
#################################################################################

head(decompMain)
#Average and SE of mass loss(%) within all sites for both regions
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
MasslossMeanMain<-aggregate(Massloss.per~ fRegion+fLanduse+fSite+fTreatment+fLittertype,na.rm=TRUE,data=decompMain,mean)
MasslossMeanMain #ERROR showing too many levels in fRegion
MasslossMeanSEMain<-aggregate(Massloss.per~fRegion+fLanduse+fSite+fTreatment+fLittertype,data=decompMain,SE)
#Creating a dataframe with Massloss mean and SE
MasslossMeanSEMain<-cbind(MasslossMeanMain,MasslossMeanSEMain[6])
colnames(MasslossMeanSEMain)[7]<-"SE" #Renaming the naming in the column
is.numeric(MasslossMeanSEMain$SE)#Checking if the values are numeric. True=correct.

# Number of 100 % mass loss
sum(decompMain$Massloss.per == 100, na.rm=T)  #0 # No zeroes for this dataset
decompMain$fLanduse

# Graph result in ggplot
MasslossPMain<-ggplot(data=MasslossMeanMain, aes(x=fLanduse, y=Massloss.per, colour=fLittertype,shape=fTreatment, group=fRegion))
MasslossPMain<-MasslossPMain+geom_errorbar(aes(ymin=Massloss.per-MasslossMeanSEMain$SE, ymax=Massloss.per+MasslossMeanSEMain$SE), width=.2,
                         position=position_dodge(.2))
#MasslossP<-MasslossP+geom_errorbar(data=MeanCG2,aes(ymin=Massloss.per-MeanTea2$SE, ymax=Massloss.per+MeanTea2$SE), width=.2,
 #                        position=position_dodge(.2), alpha=.5)
#MasslossP<-MasslossP+facet_grid()
MasslossPMain<-MasslossPMain+facet_wrap(~fRegion,ncol = 2, scales="fixed")
#MasslossP<-MasslossP+facet_wrap(~fTeabag.code+fRegion,ncol=2,scales='fixed')
MasslossPMain<-MasslossPMain+geom_point(stat="identity",size=4,position=position_dodge(.2))
#MasslossP<-TeaP+geom_point(data=MeanCG2,stat="identity",size=4,position=position_dodge(.2), alpha=.5)

MasslossPMain<-MasslossPMain+scale_color_manual(values=c("green4", "orangered3")) 
MasslossPMain <- MasslossPMain+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.background = element_rect(fill = NA, color = "black")
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
        ,axis.text.x = element_text(size=11,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,legend.text=element_text(size=12,color="black")
        ,axis.ticks.x = element_blank()
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(8,5,5,5), "mm")
        ,strip.background = element_blank()
        ,strip.text.x = element_text(size = 16, hjust=0.1,colour = "black")
        ,legend.title=element_blank()
        #,legend.position = c(1.1, 0.80)
        ,legend.spacing.y = unit(-0.5, "mm")
        ,legend.background = element_rect(fill="transparent")
        ,plot.title = element_text(hjust=0.5, size = 18)
        ,plot.subtitle = element_text(hjust = 0.5))
MasslossPMain <- MasslossPMain + labs(x="Landuse",y="Massloss %",title="Litter massloss across landuse for two regions", subtitle= "Serengeti, Tanzania")
MasslossPMain
ggsave("Massloss_Main_experiment.jpeg", width = 8, height = 5)

###UNFINISHED SCRIPT BELOW... SCROLL DOWN AT YOUR OWN RISK
#################################################################################
# Working now with: decompCG dataframe (Common garden)
#################################################################################

head(decompCG)
#Average and SE of mass loss(%) within all sites for both regions
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
MasslossMeanCG<-aggregate(Massloss.per~ fRegion+fLanduse+fBlock+fTreatment+fLittertype,na.rm=TRUE,data=decompCG,mean)
MasslossMeanCG
MasslossMeanSECG<-aggregate(Massloss.per~fRegion+fLanduse+fBlock+fTreatment+fLittertype,data=decompCG,SE)
#Creating a dataframe with Massloss mean and SE
MasslossMeanSECG<-cbind(MasslossMeanCG,MasslossMeanSECG[6])
colnames(MasslossMeanSECG)[7]<-"SE" #Renaming the SE column to SE
is.numeric(MasslossMeanSECG$SE)#Checking if the values are numeric. True=correct.

dev.off()
#Legend titles
legend_titleLAND <- "Land-use"
legend_titleTrt <- "Treatment"

# Graph result in ggplot
MasslossPCG<-ggplot(data=MasslossMeanCG, aes(x=fLanduse, y=Massloss.per, colour=fLittertype,shape=fsite, group=fRegion))
MasslossPCG<-MasslossPCG+geom_errorbar(aes(ymin=Massloss.per-MasslossMeanSECG$SE, ymax=Massloss.per+MasslossMeanSECG$SE), width=.2,
                                   position=position_dodge(.2))

#MasslossP<-MasslossP+geom_errorbar(data=MeanCG2,aes(ymin=Massloss.per-MeanTea2$SE, ymax=Massloss.per+MeanTea2$SE), width=.2,
#                        position=position_dodge(.2), alpha=.5)
#MasslossP<-MasslossP+facet_wrap(~Region,ncol = 2, scales="fixed")
#MasslossP<-MasslossP+facet_wrap(~fTeabag.code+fRegion,ncol=2,scales='fixed')
MasslossPCG<-MasslossPCG+geom_point(stat="identity",size=4,position=position_dodge(.2))
#MasslossP<-TeaP+geom_point(data=MeanCG2,stat="identity",size=4,position=position_dodge(.2), alpha=.5)

MasslossPCG<-MasslossPCG+scale_color_manual(values=c("green4", "orangered3")) 
MasslossPCG <- MasslossPCG+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.background = element_rect(fill = NA, color = "black")
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank()
        ,axis.text=element_text(size=12,color="black")
        ,axis.title.y=element_text(size=14,color="black")
        ,axis.title.x=element_text(size=14,vjust=-.4,color="black")
        ,axis.text.x = element_text(size=11,color="black",
                                    margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
        ,legend.text=element_text(size=12,color="black")
        ,axis.ticks.x = element_blank()
        ,axis.ticks.length=unit(-1.5, "mm")
        ,axis.line.y = element_line(color="black", size = .5)
        ,axis.line.x = element_line(color="black", size = .5)
        ,plot.margin = unit(c(8,5,5,5), "mm")
        ,strip.background = element_blank()
        ,strip.text.x = element_text(size = 18, hjust=0.1,colour = "black")
        ,legend.title=element_blank()
        #,legend.position = c(1.1, 0.80)
        ,legend.spacing.y = unit(-0.5, "mm")
        ,legend.background = element_rect(fill="transparent"))
MasslossPCG

############################################################################################
# Working now with: decompMain + decompCG dataframe (Pasture, Agri, Wild, Common Garden)   #
############################################################################################
head(MasslossMeanSEMain)
head(MasslossMeanSECG)

#Legend titles
legend_titleLAND <- "Land-use"
legend_titleTrt <- "Treatment"

# Graph result in ggplot
MasslossP <-ggplot()+
  geom_line(data=MasslossMeanSEMain, aes(x=fLanduse, y=Massloss.per, colour=fLittertype,shape=fBlock, group=fRegion))+
  geom_line(data=MasslossMeanSECG, aes(x=fLanduse, y=Massloss.per, colour=fLittertype,shape=fBlock, group=fRegion))
MasslossP
  
#MasslossP<-MasslossP+geom_errorbar(aes(ymin=MasslossMeanSECG$Massloss.per-MasslossMeanSECG$SE, ymax=MasslossMeanSECG$Massloss.per+MasslossMeanSECG$SE), width=.2,
#                                       position=position_dodge(.2))
#MasslossP<-MasslossP+geom_errorbar(aes(ymin=MasslossMeanSEMain$Massloss.per-MasslossMeanSEMain$SE,ymax= MasslossMeanSEMain$Massloss.per+MasslossMeanSEMain$SE), width=.2,
 #                                  position=position_dodge(.2))
MasslossP








