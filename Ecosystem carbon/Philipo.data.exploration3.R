########################################################################
#Exploring data - training
#Stuart Smith
#18/02/2018
#########################################################################
#clear system & add package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(dplyr)
library(plyr)
library(lubridate)
library(data.table)
library(xlsx)
library(ggplot2)
##########################################################################
# Import Philipo's tree density data

# Import data
# Set working directoy
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/")

# Importat data with file name
Philtrees<-read.csv(file="Tree.data.Seregenti.PhilipoBio.csv", sep=",",header=TRUE)

# Explore the basics
dim(Philtrees) # 2130 rows   34 columns
str(Philtrees) # Factors, integers, numbers # We can already see lots of NAs also
names(Philtrees) # Column names - good practice continous - no spaces..
head(Philtrees, n=20L) # First 6 rows - can be extended, n=20L

# Housekeeping - if it is really a factor - make sure R knows it..
Philtrees$farea<-as.factor(Philtrees$area)
Philtrees$flanduse<-as.factor(Philtrees$landuse)
Philtrees$fblock<-as.factor(Philtrees$block)
Philtrees$find.no<-as.factor(Philtrees$ind.no)

####################################################################################################################################################
# Add location, fire and precipitation to main dataset
#Philfire<-read.csv(file="Philipo.Fire.precips.csv", sep=",",header=TRUE)
#Philfire$farea<-as.factor(Philfire$area)
#Philfire$flanduse<-as.factor(Philfire$landuse)
#Philfire$fblock<-as.factor(Philfire$block)

#Philtrees$site_code<-as.factor(with(Philtrees, paste(farea,flanduse,fblock, sep="_")))
#Philfire$site_code<-as.factor(with(Philfire, paste(farea,flanduse,fblock, sep="_")))

#levels(Philtrees$site_code) # 39
#levels(Philfire$site_code) # 39

#Philtree_enviro<- left_join(Philtrees,Philfire, by=c("site_code"))

#library(xlsx)
#write.xlsx(Philtree_enviro, "/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philtree_enviro.xlsx")
####################################################################################################################################################

# What are the order of the factor levels
levels(Philtrees$farea)
#[1] "Ikorongo"        "Makao"           "MakaoWMA"        "Maswa"        
#[5] "Mwantimba"      "Ololosokwan"    "Park Nyigoti"   "Seronera"       
#[9] "SNP handejega"   "SNP Kleins gate"
# Areas are ordered alphabetically - do we want to reorder this in relation to
# the order we visted them experimentally # Important step for later
Philtrees[Philtrees$farea=="SNP kleins gate",]

Philtrees$farea<- factor(Philtrees$farea, levels = c("Makao","Maswa","MakaoWMA","Mwantimba","SNP handejega","Park Nyigoti","Ikorongo","Seronera","SNP kleins gate","Ololosokwan"))
levels(Philtrees$farea) # Releveled

# Can also fo this numerically
#Philtrees$farea <- factor(Philtrees$farea, levels(Philtrees$farea[c(1,2,3,4,5,6,7,8)]) # Need to alter numbers here...

# A unique code for each tree
# Philipo has assigned a number within each quadrat to a tree - we want this to be unique across all his surveyed trees

Philtrees$fTree.No<-as.factor(with(Philtrees, paste(farea, flanduse, fblock,find.no, sep="_")))
Philtrees$fTree.No<-as.factor(as.numeric(Philtrees$fTree.No))
summary(levels(Philtrees$fTree.No)) # 1498 levels = individual trees
head(Philtrees)

# Same again - unique block id
Philtrees$fblock.id<-as.factor(with(Philtrees, paste(farea, flanduse, fblock, sep="_")))
Philtrees$fblock.id<-as.factor(as.numeric(Philtrees$fblock.id))
summary(levels(Philtrees$fblock.id)) # 39 unique blocks
# 10 * 4 = 40? No 4th block for Seronera!

table(Philtrees$farea,Philtrees$fblock)
# Large differences in tree density - but remember double dates here!

# Rdate - assign Tanzanian time
# Create an R date
#PhilRdate<-strptime(as.character(Philtrees$date),format="%d.%m.%Y",tz="Africa/Nairobi" )# East African time
# If you have done / between day/month/year = format="%d/%m/%Y"

#class(PhilRdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
#Philtrees$Rdate<-PhilRdate# Add to the dataframe

# Can convert this to month/day of the year... day becomes an issue if over 2 years
#Philtrees$month<-Philtrees$Rdate$mon +1 # create a month # Add 1 to offset zero
# Two months May and December - makes sense

# Some entries are for tree turnks other branches - remove all rows with branches
Philtrees2<-Philtrees[Philtrees$tree.part!="branch",]
dim(Philtrees2) #2094   40 # Rows removed with branch
Philtrees2<-droplevels(Philtrees2) # Ensure factor level branch dropped
levels(Philtrees$tree.part)
levels(Philtrees2$tree.part) # dropped
# Subset data - into data based on date

# Species
levels(Philtrees2$species) # 38 species

########################################################################################################
# Test differences between May and December for trees with same ID
########################################################################################################

# Use December tree survey - except for SNP Klein's gate and Olosokowan
names(Philtrees2)
Philtrees2$date

# Rdate - month
RdatePhil<-strptime(as.character(Philtrees2$date),format="%d.%m.%Y",tz="Africa/Nairobi" )
class(RdatePhil)
Philtrees2$Rdate<-as.POSIXlt(RdatePhil) # Add to the R date to dataframe
Philtrees2$Rdate
Philtrees2$Month<- format(as.Date(Philtrees2$Rdate), "%m")

levels(Philtrees2$farea)#
Philtrees3<-Philtrees2[Philtrees2$Month=="12",]
dim(Philtrees3) # 1318   42

#Philtrees2Dec<-Philtrees2[Philtrees2$farea=="SNP kleins gate"| Philtrees2$farea=="Ololosokwan",]
Philtrees2Dec<-subset(Philtrees2, farea=="SNP kleins gate"|  farea=="Ololosokwan")
dim(Philtrees2Dec) #192  42 # WHY introducing rows with NAS
Philtrees2Dec<-droplevels(Philtrees2Dec)

Philtrees4<-rbind(Philtrees3,Philtrees2Dec)
dim(Philtrees4)
Philtrees4[1500,]
Philtrees4[Philtrees4$farea=="Ololosokwan",]
Philtrees4[Philtrees4$farea=="Seronera",]

##################################################################################################################################################
# Data exploration 
##################################################################################################################################################
#Data exploration
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?
########################################################################################################
# Alain Zuur - data exploration functions
source(file="/Users/anotherswsmith/Documents/AfricanBioServices/Training/HighstatLibV10.R")

#Missing values
colSums(is.na(Philtrees4)) 
# farea area NA - 58?!!
#levels(Philtrees4$area)
#Philtrees4$farea<-as.factor(Philtrees4$area)
colSums(is.na(Philtrees4)) 

# Outliers
names(Philtrees4)
MyVar<-c("Biomass.kg.per.tree","Biomass.kg.per.treePhilipo","Fire.freq","Year.of.last.fire",  
         "annual.precip.mm2015_2017","precip.mm2016","date")
Mydotplot(Philtrees4[ ,MyVar]) 
# Check date - OK
dotplot(Philtrees4$date) # OK

# Outliers by factors...
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
dotchart(Philtrees4$Biomass.kg.per.tree,groups=Philtrees4$N.non,main = "n.non.N")
dotchart(Philtrees4$Biomass.kg.per.tree,groups=Philtrees4$area,main = "area")
dotchart(Philtrees4$Biomass.kg.per.tree,groups=Philtrees4$landuse,main = "landuse")

#B Collinearity X
MyVar<-c("Fire.freq","Year.of.last.fire",  
         "annual.precip.mm2015_2017","precip.mm2016","date")
pairs(Philtrees4[,MyVar],lower.panel = panel.cor)
#Fire frequency and year since last fire 
# Average annual precip and 
# Precip and fire not correlated - date and precipitation correlated

# Boxplot for factors
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
boxplot(Fire.freq ~ flanduse, 
        xlab = "landuse",
        ylab = "Fire.freq",
        data = Philtrees4) # Big difference fire frequ qihth landuse - but overlap

boxplot(Year.of.last.fire ~ flanduse,  
        xlab = "landuse",
        ylab = "Year of last fire",
        data = Philtrees4) # Stronger than above - landuse effect with time since last fire

boxplot(annual.precip.mm2015_2017 ~ flanduse, 
        xlab = "landuse",
        ylab = "Annual rainfall",
        data = Philtrees4) # No correlation

# Collinearity issues - especially with temperature at plot-scale with canopy
# Landuse and herbivory cannot be used in same model

# Zero inflation?
sum(Philtrees4$Biomass.kg.per.tree == 0, na.rm=T)  # 73 zeroes - dead trees - May - December 
100 * sum(Philtrees4$Biomass.kg.per.tree == 0, na.rm=T) / nrow(Philtrees4)  # 4.8% can be removed...

# Spatial correlation
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
xyplot(Longitude ~ Latitude,
       data = Philtrees4,
       
       aspect = "iso",
       col = as.numeric(Philtrees4$area), # Repeat colour
       pch = 16) # Spatial correlation with XY and area - expected

par(pty = "s", mar = c(5,5,2,2), cex.lab = 1.5)       
plot(x = Philtrees4$Longitude,
     y = Philtrees4$Latitude,
     type = "p",
     pch = 16,
     xlab = "X-coordinates",
     ylab = "Y-coordinates")

ordihull(Philtrees4[, c("Longitude", "Latitude")],
         draw = "polygon",
         groups = Philtrees4[, "area"],
         label = F,
         col = "red")     
# Area is fine - summarizing long/lat grouping

##################################################################################################################################################
# Mean tree size analysis
library(nlme)
library(lme4)
library(glmmADMB) 
library(piecewiseSEM)
library(MuMIn)
##################################################################################################################################################

# Remove where no biomass NA - tree biomass
Philtrees5<-subset(Philtrees4, Biomass.kg.per.tree>.001)
dim(Philtrees5) ##1437   41

TreeFire<-lmer(Biomass.kg.per.tree~ flanduse+N.non+Fire.freq+ annual.precip.mm2015_2017+
                 flanduse:N.non+ N.non:Fire.freq+ N.non:annual.precip.mm2015_2017+
                (1|farea/fblock.id), data = Philtrees5, REML=T)
summary(TreeFire)
anova(TreeFire) # Landuse very strong

plot(TreeFire) # Bad - will need a better distribution 

TreeFire2<-glmmadmb(Biomass.kg.per.tree~ flanduse+N.non+Fire.freq+ annual.precip.mm2015_2017+
                      flanduse:N.non+ N.non:Fire.freq+ N.non:annual.precip.mm2015_2017+
                      (1|farea/fblock.id), 
                    #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                    family="gamma", data = Philtrees5)
#admb.opts=admbControl(shess=FALSE,noinit=FALSE)
#admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5)
#the above plus starting values:
#  start=list(RE_sd=rep(1,2),RE_cor=rep(0,2))
#You could also try with leaving sd out first. Essentially you just need to give one value for each random term in your model
#If everything else fails: xtra.args="-ams 500000000 -simplex"
#https://groups.google.com/a/admb-project.org/forum/#!topic/users/DXHfeLEHiAQ Useful link...

summary(TreeFire2)
AIC(TreeFire2) # 10195.68 - Awful - really high

#Checking assumptions
E1 <- resid(TreeFire2, type = "pearson") 
F1 <- fitted(TreeFire2)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 2) # Still bunched - lots of small numbers - few big
# Follows more gamma

# Drop factors from the model
drop1(TreeFire2, test="Chi")
#Df   AIC  LRT Pr(>Chi)  
#<none>                             10203            
#                                Df   AIC  LRT Pr(>Chi)  
#<none>                             10196                
#flanduse:N.non                   1 10199 5.34  0.02084 *
#N.non:Fire.freq                  1 10196 2.02  0.15524  
#N.non:annual.precip.mm2015_2017  1 10194 0.78  0.37714   


TreeFire3<-glmmadmb(Biomass.kg.per.tree~ flanduse+N.non+Fire.freq+ annual.precip.mm2015_2017+
                      flanduse:N.non+ (1|farea/fblock.id), 
                    #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                    family="gamma", data = Philtrees5)

summary(TreeFire3)
AIC(TreeFire3) # 10194.1 - Awful - really high

# Drop factors from the model
drop1(TreeFire3, test="Chi")

#Df   AIC   LRT  Pr(>Chi)    
#<none>                       10200                    
#Fire.freq                  1 10193  0.92 0.3374750    
#annual.precip.mm2015_2017  1 10199  6.48 0.0109095 *  # Annual precip significant
#flanduse:N.non             1 10206 13.96 0.0001867 *** # Landuse x tree fx group interaction

TreeFire4<-glmmadmb(Biomass.kg.per.tree~ flanduse+N.non+flanduse:N.non+ 
                      annual.precip.mm2015_2017 +(1|farea/fblock.id), 
                    #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                    family="gamma", data = Philtrees5)
levels(Philtrees5$fblock.id)
summary(TreeFire4) # Everything highly singificant
AIC(TreeFire4) # 10193.02

# Drop factors from the model
drop1(TreeFire4, test="Chi")
#               Df   AIC   LRT  Pr(>Chi)   
#annual.precip.mm2015_2017  1 10197  6.04 0.0139853 *  
#flanduse:N.non             1 10205 13.88 0.0001949 *** 

# Update - compare models with and without factors
TreeFire4a<- update(TreeFire4, .~. -flanduse:N.non) 
TreeFire4b<- update(TreeFire4a, .~. -flanduse) 
TreeFire4c<- update(TreeFire4a, .~. -N.non) 
TreeFire4d<- update(TreeFire4, .~. -annual.precip.mm2015_2017)
#                                                    NoPar  LogLik Df Deviance  Pr(>Chi)  
anova(TreeFire4,TreeFire4a) # flanduse:N.non # # 2     8 -5088.5  1    13.88 0.0001949 ***
anova(TreeFire4a,TreeFire4b) # -flanduse #  7 -5095.4  1     9.16 0.002474 ***
anova(TreeFire4a,TreeFire4c) # -fN.non #    7 -5095.4  1     10.1 0.001483 **
anova(TreeFire4,TreeFire4d) # annual.precip.mm2015_2017 # 2     8 -5088.5  1     6.04  0.01399 *

#Pairwise contrasts *ghlt* and *lsmeans*
library(multcomp)
library(multcompView)
library(lsmeans)
library(lmerTest)
library(Hmisc)
library(pbkrtest)

# lsmeans # Does not work for interaction...
#summary(glht(TreeFire4, mcp('flanduse:N.non'="Tukey")))
#difflsmeans(TreeFire4,test.effs= "flanduse:N.non" ) 

# Interaction plot
with(Philtrees5, {interaction.plot(flanduse,N.non,Biomass.kg.per.tree,
                                   xlab = "land-use",
                                   ylab = "Tree biomass (kg)",
                                   fun=mean)})
# size of legume and non-legume trees similar for wildlife land-uses across rainfall
# larger non-leguminous trees on pastureland than leguminous trees...

#Checking assumptions
E1 <- resid(TreeFire4, type = "pearson") 
F1 <- fitted(TreeFire4)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     xlim = c(min(F1), max(F1)))
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 2) # Still bunched - lots of small numbers - few big
# Follows more gamma distribution

# Tree biomass and annual precip
plot(Biomass.kg.per.tree~annual.precip.mm2015_2017,data=Philtrees5)
abline(lm(Biomass.kg.per.tree~annual.precip.mm2015_2017,data=Philtrees5))
summary(lm(Biomass.kg.per.tree~annual.precip.mm2015_2017,data=Philtrees5)) # Negative
# Larger trees with less rainfall 

##################################################################################################################################################
# Tree biomass graphics
##################################################################################################################################################
# Tree biomass

# Remove where no biomass NA - tree biomass
Philtrees5<-subset(Philtrees4, Biomass.kg.per.tree>.001)
dim(Philtrees5) ##1437   42
Philtrees5[Philtrees5$farea=="Seronera",]

# Sum Biomass  per block m2- summary across rainfall and fire 
names(Philtrees5)
meanTreeBio<-aggregate(Biomass.kg.per.tree~farea+flanduse+
                         fblock.id+N.non+area.m2+annual.precip.mm2015_2017+
                       Fire.freq+area.m2,Philtrees5, sum)
meanTreeBio$Biomass.kg.per.tree<-(meanTreeBio$Biomass.kg.per.tree/meanTreeBio$area.m2)
plot(Biomass.kg.per.tree~annual.precip.mm2015_2017, col=c(flanduse),meanTreeBio)
plot(Biomass.kg.per.tree~Fire.freq, col=c(flanduse),meanTreeBio)

meanTreeBio2<-aggregate(Biomass.kg.per.tree~farea+flanduse+
                         block+area.m2,Philtrees5, sum)
#meanTreeBio2$Biomass.kg.per.tree<-(meanTreeBio2$Biomass.kg.per.tree/meanTreeBio2$area.m2)

names(Philtrees5)
TreeSumArea2<-aggregate(total.basal.area.m2~farea+flanduse+
                          block+area.m2,Philtrees5, sum)
TreeSumArea2$total.basal.area.ha2<-(TreeSumArea2$total.basal.area.m2/(TreeSumArea2$area.m2*.0001))
TreeSumArea2$Biomass.kg.per.tree<-(meanTreeBio2$Biomass.kg.per.tree/meanTreeBio2$area.m2)

# Average per block by land-use
blockTreeBio<-aggregate(Biomass.kg.per.tree~farea+flanduse+
                        fblock.id+N.non+area.m2,Philtrees5, sum)
names(blockTreeBio)
blockTreeBio$Biomass.kg.per.tree<-(blockTreeBio$Biomass.kg.per.tree/blockTreeBio$area.m2)
dotchart(blockTreeBio$Biomass.kg.per.tree) # Dry wild - MaswaGR block 4 high - but big trees

blockTreeBiomean<-aggregate(Biomass.kg.per.tree~flanduse+N.non,blockTreeBio, mean)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
blockTreeBioSE<-aggregate(Biomass.kg.per.tree~flanduse+N.non,blockTreeBio, SE)
blockTreeBiomean$se<-blockTreeBioSE$Biomass.kg.per.tree*10
blockTreeBiomean$Biomass.kg.per.tree<-blockTreeBiomean$Biomass.kg.per.tree*10

# kg m2 - landuse only
#flanduse Biomass.kg.per.tree
#1  pasture           0.3463101 +  0.06595624
#2     wild           0.5410476 + 0.10084568

# kg m2 - tree functional type
#N.non Biomass.kg.per.tree
#1     N           0.5074532 + 0.11875854
#2   non           0.4088844 + 0.05699024

# Tree biomass graph by rainfall region and tree canopy
tp3<-ggplot(blockTreeBiomean, aes(x=flanduse, y=Biomass.kg.per.tree, fill=N.non,col=N.non, shape=N.non))
tp3<- tp3+geom_errorbar(aes(ymax = Biomass.kg.per.tree+se, ymin = Biomass.kg.per.tree-se), width=.2,
                        position=position_dodge(.2))
tp3<- tp3+ geom_point(size=5,width=.2,position=position_dodge(.2))
#tp3<-tp3+facet_wrap(~rain.region) 
tp3<-tp3+xlab("Land-use") + ylab(expression(paste("Tree biomass ( Mg ",ha^-1,")")))
tp3<-tp3+ scale_y_continuous(limits = c(0,10), expand = c(0,0),breaks = c(0,5,10), labels = c(0,5,10))
tp3<-tp3+theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank() 
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        ,legend.position = c(0.07, 0.94)
        ,strip.background = element_blank()
        ,strip.text.x =element_text(size=15)
        ,axis.text.x=element_text(angle=35, hjust=1)
        ,plot.margin = unit(c(5,5,5,5), "mm")) +
  theme(axis.line = element_line(color = 'black'))
tp3

#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philipo.treeBio.N.non.jpeg",
#       width= 24, height = 12,units ="cm",
#       dpi = 600, limitsize = TRUE)

# Density distribution tree size by land-use, N and non and rain.region

#########################################################################
# Tree biomass historgraph graph
#########################################################################
names(Philtrees5)
# Remove intermediate rain
Philtrees5I<-Philtrees5[Philtrees5$rain.region!="Intermediate",]
Philtrees5I<-droplevels(Philtrees5I)

# Reorder rain region
Philtrees5I$rain.region<-factor(Philtrees5I$rain.region,levels(Philtrees5I$rain.region)[c(2,1)])
grp.mean$rain.region

# Group means
grp.mean<-aggregate(log(Biomass.kg.per.tree+1)~N.non+landuse+rain.region,data=Philtrees5I,mean)
colnames(grp.mean)[4]<-"log.Biomass.kg.per.tree"
Philtrees5I$log.Biomass.kg.per.tree<-log(Philtrees5I$Biomass.kg.per.tree+1)
max(Philtrees5I$Biomass.kg.per.tree)
log(3400.136+1) # ~ 8 
log(2+1) # 1.098612 = threshold for small trees

# Main graph tree biomass density 
p<-ggplot()
p<-p + geom_density(data=Philtrees5I, aes(log.Biomass.kg.per.tree, fill = landuse,colour =landuse),
                    alpha=0.4) 
p<-p +facet_wrap(~N.non+rain.region)
p<-p +scale_fill_manual("Land-use",values=c("tan3","turquoise3"))
p<-p +scale_colour_manual("Land-use",values=c("tan3", "turquoise3"))
p<-p +scale_x_continuous(expand=c(0,0), limits = c(0, 8))
p<-p +scale_y_continuous(labels = c(0,5,10,15,20,25), breaks = c(0,.5,1.0,1.5,2.0,2.5), limits = c(0, 2.5), expand=c(0,0))
p<-p +geom_vline(data=grp.mean, aes(xintercept=log.Biomass.kg.per.tree,colour = landuse,linetype = landuse),size=.75)
p<-p +scale_linetype_manual("Land-use",values = c(wild = "solid", pasture = "dashed"))          
#p<-p + scale_fill_grey(start = 0.85, end = 0.30)
#p<-p + scale_colour_grey(start = 0.85, end = 0.30)
p <- p + xlab("log tree biomass (kg)") +  ylab("Density (%)")  
p <- p + theme_classic() + 
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank() 
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,strip.background = element_blank()
        #,strip.text.x =element_blank()
        ,panel.spacing = unit(1, "lines")
    ,axis.text=element_text(size=12)
    ,axis.title.y=element_text(size=14)
    ,axis.title.x=element_text(size=14)
    ,legend.text=element_text(size=12)
   ,strip.text= element_blank()
    ,legend.position = c(0.95, 0.95)
    ,plot.margin = unit(c(5,5,7,5), "mm"))
p<-p+ annotate(geom = "segment", x = 0, xend =0, y = -Inf, yend = Inf, size = .65) 
p<-p+ annotate(geom = "segment", x = -Inf, xend =Inf, y = 0, yend = 0, size = .65) 
p<-p+annotate(geom="text",x=4, y=2.38, label=c("Wet Region","Dry Region","",""),color="black", size=5)
p

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Tree.Biomass.hist.N.non.jpeg",
       width= 20, height = 12,units ="cm",
       dpi = 600, limitsize = TRUE)

############################################################################
# Biomass distribution - FULL DATASET
############################################################################
#Rename levels - legume and non legume
levels(Philtrees5$N.non)<-c("legume","non-legume")

# Group means
grp.mean<-aggregate(log(Biomass.kg.per.tree+1)~N.non+landuse,data=Philtrees5,mean)
colnames(grp.mean)[3]<-"log.Biomass.kg.per.tree"
Philtrees5$log.Biomass.kg.per.tree<-log(Philtrees5$Biomass.kg.per.tree+1)
max(Philtrees5$Biomass.kg.per.tree)
log(3400.136+1) # ~ 8.13
log(2+1) # 1.098612 = threshold for small trees

#grp.meanRAW<-aggregate(Biomass.kg.per.tree~N.non+landuse,data=Philtrees5,mean)
#grp.meanRAW$Biomass.kg.per.tree<-log(grp.meanRAW$Biomass.kg.per.tree+1)

# Main graph tree biomass density 
p<-ggplot()
p<-p + geom_density(data=Philtrees5, aes(log.Biomass.kg.per.tree, fill = landuse,colour =landuse),
                    alpha=0.4) 
p<-p +facet_wrap(~N.non)
p<-p +scale_fill_manual("Land-use",values=c("tan3","turquoise3"))
p<-p +scale_colour_manual("Land-use",values=c("tan3", "turquoise3"))
p<-p +scale_x_continuous(expand=c(0,0), limits = c(0, 8))
p<-p +scale_y_continuous(labels = c(0,5,10,15,20,25), breaks = c(0,.5,1.0,1.5,2.0,2.5), limits = c(0, 2.5), expand=c(0,0))
p<-p +geom_vline(data=grp.mean, aes(xintercept=log.Biomass.kg.per.tree,colour = landuse,linetype = landuse),size=.75)
p<-p +scale_linetype_manual("Land-use",values = c(wild = "solid", pasture = "dashed"))          
#p<-p + scale_fill_grey(start = 0.85, end = 0.30)
#p<-p + scale_colour_grey(start = 0.85, end = 0.30)
p <- p + xlab("log tree biomass (kg)") +  ylab("Density (%)")  
p <- p + theme_classic() + 
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank() 
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_blank() 
        ,strip.background = element_blank()
        ,panel.spacing = unit(1, "lines")
        ,axis.text=element_text(size=12)
        ,axis.title.y=element_text(size=14)
        ,axis.title.x=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,strip.text=element_text(size=14)
        ,legend.position = c(0.95, 0.95)
        ,plot.margin = unit(c(5,5,7,5), "mm"))
p<-p+ annotate(geom = "segment", x = 0, xend =0, y = -Inf, yend = Inf, size = .65) 
p<-p+ annotate(geom = "segment", x = -Inf, xend =Inf, y = 0, yend = 0, size = .65) 
#p<-p+annotate(geom="text",x=4, y=2.38, label=c("Wet Region","Dry Region","",""),color="black", size=5)
p

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Tree.Biomass.hist.N.nonPHIL.jpeg",
       width= 20, height = 12,units ="cm",
       dpi = 600, limitsize = TRUE)


# Small trees less than 2 kg...
dim(Philtrees5) #1437
Philtrees5small<-Philtrees5[Philtrees5$Biomass.kg.per.tree<2,]
dim(Philtrees5small)

Smalltree<-table(Philtrees5small$flanduse,Philtrees5small$N.non)
Alltree<-table(Philtrees5$flanduse,Philtrees5$N.non)
Smalltree/Alltree*100
#               N      non
#pasture 84.36658 78.34225 # Percent = small trees < 2 kg...
#wild    59.18367 65.73705

Smalltree<-table(Philtrees5small$flanduse)
Alltree<-table(Philtrees5$flanduse)
Smalltree/Alltree*100
#pasture     wild 
#81.34228 61.56069 
#########################################################################
# Summarising data 
# TREE DENSITY BY FUNCTIONAL TYPE
#########################################################################

# Count by tree ID in factor
Philtrees2 <- Philtrees2[order(Philtrees2$fblock.id),]
tapply(Philtrees2$fTree.No,Philtrees2$fblock.id,length) # This does not account for duplicate dates
tapply(Philtrees2$fTree.No, Philtrees2$fblock.id, function(x) length(unique(x))) # Unique accounts for TREE ID

names(Philtrees2)
Treecount<-aggregate(fTree.No~farea+flanduse+rain.region+fblock.id+N.non+area.m2, Philtrees2,function(x) length(unique(x))) # Unique accounts for TREE ID
Treecount<-as.data.frame(Treecount)
max(Treecount$fTree.No) # 82 trees
names(Treecount)
Treecount$fTree.No<-(as.numeric(Treecount$fTree.No)/Treecount$area.m2)*1000


TreecountMEAN<-aggregate(fTree.No~flanduse+rain.region+N.non, Treecount,mean) # Unique accounts for TREE ID
TreecountSE<-aggregate(fTree.No~flanduse+rain.region+N.non, Treecount,SE) 


TreecountMEAN$se<-TreecountSE$fTree.No

tp2<-ggplot(TreecountMEAN, aes(x=flanduse, y=fTree.No, fill=N.non,col=N.non, shape=N.non))
tp2<- tp2+geom_errorbar(aes(ymax = fTree.No+se, ymin = fTree.No-se), width=.2,
                        position=position_dodge(.2))
tp2<- tp2+ geom_point(size=5,width=.2,position=position_dodge(.2))
tp2<-tp2+facet_wrap(~rain.region) 
tp2<-tp2+xlab("Land-use") + ylab(expression(paste("Tree density (",ha^-1,")")))
#tp2<-tp2+ scale_y_continuous(limits = c(-0.1,1), expand = c(0,0),breaks = c(0,.25,.5,.75,1), labels = c(0,.25,.5,.75,1))
tp2<-tp2+theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank() 
        ,panel.grid.major.x = element_line()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        ,legend.position = c(0.07, 0.94)
        ,strip.background = element_blank()
        ,strip.text.x =element_text(size=15)
        ,axis.text.x=element_text(angle=35, hjust=1)
        ,plot.margin = unit(c(5,5,5,5), "mm")) +
  theme(axis.line = element_line(color = 'black'))
tp2

ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Tree.density.jpeg",
       width= 24, height = 12,units ="cm",
       dpi = 600, limitsize = TRUE)


# Density distribution number of trees by land-use

library(ggplot2)
#grp.mean<-aggregate(Fire.freq~Category,data=SppSNP,median)

names(Treecount)

p<-ggplot()
p<-p + geom_density(data=Treecount, aes(fTree.No, fill = N.non,colour = N.non),
                    alpha=0.4) 
p
p<-p +scale_y_continuous(labels = c(0,10,20,30), limits = c(0, .31), expand=c(0,0))
p<-p +scale_x_continuous(breaks=c(0,2,4,6,8,10,12),limits = c(0, 13),expand=c(0,0))
p<-p +geom_vline(data=grp.mean, aes(xintercept=Fire.freq, color=Category),
                 linetype="dashed", size=.75)
p<-p + scale_fill_grey(start = 0.85, end = 0.30)
p<-p + scale_colour_grey(start = 0.85, end = 0.30)
p <- p + xlab("Fire interval (years)") +  ylab("Density (%)")  
p <- p + theme_classic() + 
  theme(
    panel.grid.major.x = element_line( size=0.01, color="black" )
    ,panel.grid.major.y = element_line( size=0.01, color="black" )
    ,axis.text=element_text(size=12)
    ,axis.title.y=element_text(size=14)
    ,axis.title.x=element_text(size=14)
    ,legend.text=element_text(size=12)
    ,legend.position = c(0.85, 0.85))
p




#########################################################################
# Exploring  data 
#########################################################################
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

# A. Missing values
colSums(is.na(Philtrees2)) 
#bdh.cm 471
# base.di.cm 563 - some data has basal diamter other dbh 

# Let's just work with data with basal di
# Subset dataset
Philtrees2bas<-Philtrees2[!is.na(Philtrees2$base.di.cm),]
dim(Philtrees2bas) # 403  24
colSums(is.na(Philtrees2bas)) 
head(Philtrees2bas)

# B outliers
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
dotchart(Philtrees2bas$base.di.cm) # There is an outlier here >100
dotchart(Philtrees2bas$base.di.cm,groups=Philtrees2bas$farea,main = "farea")
# Outlier seems to be in Klein's gate

plot(Philtrees2bas$base.di.cm)
#identify(Philtrees2bas$base.di.cm) # this is interactive 
# MUST PRESS ESP to END
# [1] 18 # Our outlier is row 18
Philtrees2bas[18,] # Bingo base diameter # sounds like a mistake diamter > 1 m? 
                      #More likely 10.9 cm...?

# Remove outlier for timebeing
Philtrees3bas<-Philtrees2bas[-18,]

dotchart(Philtrees3bas$base.di.cm) # No outlier anymore...

# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)

# We will look c to F when we have more variables..for pre analysis stage

# G Zero inflation?
sum(Philtrees3bas$base.di.cm == 0, na.rm=T)  #0 # No zeroes for this dataset
100 * sum(Philtrees3bas$base.di.cm == 0, na.rm=T) / nrow(Philtrees3bas)  # 0 - no zeroes skewed low values

# H Are categorical covariates balanced?
# Best to do this on the oiginal dataset as make no sense with basal only

names(Philtrees2)
table(Philtrees2$farea,Philtrees2$fblock) # Useful to see if design is balanced
# Big diffrent in number of trees across areas - very very low in Klein's gate
# Some big block variation too - Mwanitmba block 3 stands out
# It is looking uneven

table(Philtrees2$farea,Philtrees2$flanduse) 









##########################################################################################
# OLD script
Treecount$fblock.id<-as.factor(dimnames(Treecount)[[1]]) # Convert dinmaes into block names

Treecount$fblock.id<-as.factor(Treecount$fblock.id)
levels(Treecount$fblock.id)
levels(Philtrees2$fblock.id)

# Order by block ID seems to help
Treecount<- Treecount[order(Treecount$fblock.id),] # This does not work...
Treecount$fblock.id <- factor(Treecount$fblock.id, levels(Treecount$fblock.id[1:39]))
head(Treecount)
Philtrees2 <- Philtrees2[order(Philtrees2$fblock.id),]

# Gain back some of the factor info # Merge datasets
Philtrees4<-left_join(Philtrees2,Treecount,by=c("fblock.id"))
dim(Philtrees4) # Added a tree count column for each block
head(Philtrees4)
Philtrees4$treesm2<-Philtrees4$Treecount/Philtrees4$area.m2

# Difference in a value between one row and next row...
names(Philtrees4)
Philtrees4$bdh.cm_diff <- ave(Philtrees4$bdh.cm, Philtrees4$fTree.No, FUN=function(x) c(0, diff(x)))
head(Philtrees4)

# creating an ordered data.frame
Philtrees4$date2<-as.Date(Philtrees4$date,format="%d.%m.%Y")
Philtrees4 <- Philtrees4[order(Philtrees4$fTree.No, Philtrees4$date2),]
# calculating the timedifference
Philtrees4$tdiff <- unlist(tapply(Philtrees4$date2, INDEX = Philtrees4$fTree.No,
                                  FUN = function(x) c(0, `units<-`(diff(x), "days"))))

head(Philtrees4) # tdiff has caluclated day difference bettween sampling points

# AVERAGES AND SE
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# Remember to exclude outlier
Philtrees4<-Philtrees4[!Philtrees4$bdh.cm>100,]

# dbh
meandbh<-aggregate(bdh.cm~flanduse+fblock.id, data=Philtrees4, mean) # mean
meandbh<-aggregate(bdh.cm~flanduse+fblock.id, data=Philtrees4, na.rm=T, mean) # mean - same when you na.action
SEdbh<-aggregate(bdh.cm~flanduse+fblock.id, data=Philtrees4, SE) # SE # Standard error 
# Can also use  sum or length

#trees m2
meantreedens<-aggregate(treesm2~flanduse, data=Philtrees4, mean) # mean
meantreedens<-aggregate(treesm2~flanduse, data=Philtrees4, na.rm=T, mean) # mean - same when you na.action
SEtreedens<-aggregate(treesm2~flanduse, data=Philtrees4, SE) # NO
# More trees in pastures

hist(Philtrees4$bdh.cm, breaks=20)

# Quick histogram by factor it is not all about averages...bimodal?
library(ggplot2)
p<-ggplot()
p<-p + geom_density(data=Philtrees4, aes(bdh.cm, fill = flanduse,colour = flanduse),
                    alpha=0.4) 
p<-p +scale_y_continuous(breaks=c(0,.005,.01,.015),labels = c(0,.5,1,1.5), limits = c(0, .02), expand=c(0,0))
p<-p +scale_x_continuous( expand=c(0,0))
p <- p + xlab("Basal diameter (cm)") +  ylab("Density (%)")  
p <- p + theme_classic() + 
theme(
    panel.grid.major.x = element_line( size=0.01, color="black" )
    ,panel.grid.major.y = element_line( size=0.01, color="black" )
    ,axis.text=element_text(size=12)
    ,axis.title.y=element_text(size=14)
    ,axis.title.x=element_text(size=14)
    ,legend.text=element_text(size=12))
p

# Export xslx of new data.frame
#library(xlsx)
#write.xlsx(Philtrees4, "/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philtrees4.xlsx")



####################################################################################
# TREE CARBON 
####################################################################################

meanTreeC<-aggregate(Carbon.kg.per.tree~farea+flanduse+
                       fblock+find.no+rain.region+N.non+area.m2,Philtrees4, mean)

# Sum Biomass and C per tree per block m2- summarize across sampling dates
names(meanTreeC)
blockTreeC<-aggregate(Carbon.kg.per.tree~farea+flanduse+
                        fblock+rain.region+N.non+area.m2,meanTreeC, sum)
names(blockTreeC)
blockTreeC$Carbon.kg.per.tree<-(blockTreeC$Carbon.kg.per.tree/blockTreeC$area.m2)
dotchart(blockTreeC$Carbon.kg.per.tree) # Dry wild - MaswaGR block 1 and 4 high?

blockTreeCmean<-aggregate(Carbon.kg.per.tree~flanduse+rain.region+N.non,blockTreeC, mean)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
blockTreeCSE<-aggregate(Carbon.kg.per.tree~flanduse+rain.region+N.non,blockTreeC, SE)
blockTreeCmean$se<-blockTreeCSE$Carbon.kg.per.tree*10
blockTreeCmean$Carbon.kg.per.tree<-blockTreeCmean$Carbon.kg.per.tree*10

# Graph tree carbon - rainfall - N vs non N
tp2<-ggplot(blockTreeCmean, aes(x=flanduse, y=Carbon.kg.per.tree, fill=N.non,col=N.non, shape=N.non))
tp2<- tp2+geom_errorbar(aes(ymax = Carbon.kg.per.tree+se, ymin = Carbon.kg.per.tree-se), width=.2,
                        position=position_dodge(.2))
tp2<- tp2+ geom_point(size=5,width=.2,position=position_dodge(.2))
tp2<-tp2+facet_wrap(~rain.region) 
tp2<-tp2+xlab("Land-use") + ylab(expression(paste("Tree carbon ( Mg ",ha^-1,")")))
tp2<-tp2+ scale_y_continuous(limits = c(-1,14), expand = c(0,0),breaks = c(0,3,6,9,12), labels = c(0,3,6,9,12))
tp2<-tp2+theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank() 
        ,panel.grid.major.x = element_line()
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)
        ,legend.title=element_text(size=14)
        ,legend.position = c(0.07, 0.94)
        ,strip.background = element_blank()
        ,strip.text.x =element_text(size=15)
        ,axis.text.x=element_text(angle=35, hjust=1)
        ,plot.margin = unit(c(5,5,5,5), "mm")) +
  theme(axis.line = element_line(color = 'black'))
tp2

#ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philipo.treeC.N.non.jpeg",
#       width= 24, height = 12,units ="cm",
#       dpi = 600, limitsize = TRUE)

#########################################################################
# Adding covariate from Stu's dataset to Philipo's
#########################################################################
# Data to add to main dataset
# max/min/mean - herbaceous biomass/temp/moisture X
# Mean HerbPRC score X
# Total dung - cattle guild? + absolute total X 
# cumulative rainfall
# size of tree - dbh? 
# soil BD
########################################################################
# Sum rainfall between surveying periods by date
########################################################################

setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Data/Exclosures Serengeti/Biomass excl/")
Excldata<-read.csv("Sero.seasonal.bio_enviroFULL.csv")
head(Excldata)
dim(Excldata) # 3174 39
names(Excldata)

# Create a unique code for each block
Excldata$block_code<-as.factor(with(Excldata, paste(area,landuse,block, sep="_")))
Excldata$plot_code<-as.factor(with(Excldata, paste(area,landuse,plot.ID, sep="_")))
levels(Excldata$block_code) # 24
levels(Excldata$plot_code) # 144

#### Tree sampling dates ######
# Import data seasonal biomass, dung, PRC and environmental data
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/")
PhilT0<-read.csv(file="Philipo.Excl.Enviro.T0.csv", sep=",",header=TRUE)

Philtrees<-read.csv(file="Exclosure.seedlings.Philipo.csv", sep=",",header=TRUE)

# Remove Maswa Illegal block 1 and 4 - lost midway through exp
Excldata2<-Excldata[Excldata$block_code!="Maswa_illegal_9",] # Remove Maswa Illegal block 1
Excldata3<-Excldata2[Excldata2$block_code!="Maswa_illegal_12",]
dim(Excldata) # 3174   43
dim(Excldata3) # 3102 43
Excldata3<-droplevels(Excldata3)
levels(Excldata3$block_code) # 22
levels(Excldata3$plot_code) # 132

# Philipo's survey times
PhilT0$block_code<-as.factor(with(PhilT0, paste(area,landuse,block, sep="_")))
PhilT0$plot_code<-as.factor(with(PhilT0, paste(area,landuse,plot.ID, sep="_")))
levels(PhilT0$block_code) # 22
levels(PhilT0$plot_code) # 132

Philtrees$block_code<-as.factor(with(Philtrees, paste(area,landuse,block, sep="_")))
Philtrees$plot_code<-as.factor(with(Philtrees, paste(area,landuse,plot.ID, sep="_")))
levels(Philtrees$block_code) # 22
levels(Philtrees$plot_code) # 132

# Convert disc to biomass
Excldata3$Disc_kg.m2<-((527.7*Excldata3$Disc_avg^0.67)/10000)

# Loop does not work with Rdate - just works with as.date!
PhilT0$date2 = as.Date(PhilT0$dateT1,"%d.%m.%Y")
PhilT0$date3 = as.Date(PhilT0$dateT0,"%d.%m.%Y")
Excldata3$date4 = as.Date(Excldata3$date,"%d/%m/%Y")

# Herbaceous biomass
# Max herbaceous biomass
for(i in 1:nrow(PhilT0)){
  PhilT0$MaxBio[i] <-max(Excldata3$Disc_kg.m2[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                      Excldata3$date4<= PhilT0$date2[i])]) 
}
# Min herbaceous biomass
for(i in 1:nrow(PhilT0)){
  PhilT0$MinBio[i] <-min(Excldata3$Disc_kg.m2[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                      Excldata3$date4<= PhilT0$date2[i])]) 
}
# Mean herbaceous biomass
for(i in 1:nrow(PhilT0)){
  PhilT0$MeanBio[i] <-mean(Excldata3$Disc_kg.m2[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                      Excldata3$date4<= PhilT0$date2[i])]) 
}
# Soil temperature
# Max soil temp
for(i in 1:nrow(PhilT0)){
  PhilT0$MaxTemp[i] <-max(Excldata3$temp[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                      Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# Min soil temp
for(i in 1:nrow(PhilT0)){
  PhilT0$MinTemp[i] <-min(Excldata3$temp[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                      Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}

# Mean soil temperature
for(i in 1:nrow(PhilT0)){
  PhilT0$MeanTemp[i] <-mean(Excldata3$temp[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                        Excldata3$date4>= PhilT0$date3[i] &
                                                        Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# Soil moisture
# Max soil moisture
for(i in 1:nrow(PhilT0)){
  PhilT0$MaxMoist[i] <-max(Excldata3$moist[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                 Excldata3$date4>= PhilT0$date3[i] &
                                                 Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# Min soil moisture
for(i in 1:nrow(PhilT0)){
  PhilT0$MinMoist[i] <-min(Excldata3$moist[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                 Excldata3$date4>= PhilT0$date3[i] &
                                                 Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# Mean soil moisture
for(i in 1:nrow(PhilT0)){
  PhilT0$MeanMoist[i] <-mean(Excldata3$moist[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                   Excldata3$date4>= PhilT0$date3[i] &
                                                   Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}

# Herbivory
# Mean Herbivore PRC score
for(i in 1:nrow(PhilT0)){
  PhilT0$HerbPRC[i] <-mean(Excldata3$HerbPRC[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                     Excldata3$date4>= PhilT0$date3[i] &
                                                     Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# Total herbivore dung - livestock and wildlife
for(i in 1:nrow(PhilT0)){
  PhilT0$rain.sum.mm[i] <-sum(Excldata3$rain.sum.mm[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                     Excldata3$date4>= PhilT0$date3[i] &
                                                     Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}


# Livestock grazer dung - cattle and sheep
for(i in 1:nrow(PhilT0)){
  PhilT0$livestock.grazer_dung[i] <-sum(Excldata3$live_grazer[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                          Excldata3$date4>= PhilT0$date3[i] &
                                                          Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}

# Cumulative rainfall
for(i in 1:nrow(PhilT0)){
  PhilT0$livestock.grazer_dung[i] <-sum(Excldata3$live_grazer[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                                      Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}

# Tree size 
# Tree
for(i in 1:nrow(PhilT0)){
  PhilT0$tree_height.m[i] <-mean(Excldata3$tree_height.m[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                                      Excldata3$date4>= PhilT0$date3[i] &
                                                                      Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# canopy height
for(i in 1:nrow(PhilT0)){
  PhilT0$canopy_height.m[i] <-mean(Excldata3$canopy_height.m[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                                 Excldata3$date4>= PhilT0$date3[i] &
                                                                 Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}
# tree dbh
for(i in 1:nrow(PhilT0)){
  PhilT0$dbh[i] <-mean(Excldata3$dbh[which(Excldata3$plot_code== PhilT0$plot_code[i] & 
                                                                     Excldata3$date4>= PhilT0$date3[i] &
                                                                     Excldata3$date4<= PhilT0$date2[i])],na.rm = T) 
}

# Soil BD data
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/")
PhilBD<-read.csv(file="Philipo.SoilBD.csv", sep=",",header=TRUE)

# Remove Maswa Illegal block 1 and 4 - lost midway through exp
PhilBD$block_code<-as.factor(with(PhilBD, paste(area,landuse,block, sep="_")))
PhilBD$plot_code<-as.factor(with(PhilBD, paste(area,landuse,plot.ID, sep="_")))
PhilBD2<-PhilBD[PhilBD$block_code!="Maswa_illegal_9",] # Remove Maswa Illegal block 1
PhilBD3<-PhilBD2[PhilBD2$block_code!="Maswa_illegal_12",]
dim(PhilBD) # 144   10
dim(PhilBD3) # 132 10
PhilBD3<-droplevels(PhilBD3)
levels(PhilBD3$block_code) # 22
levels(Excldata3$plot_code) # 132

# Left join dataset with soil bulk density
PhilT0BD<-left_join(PhilT0,PhilBD3,by=c("plot_code"))

#library(xlsx)
#write.xlsx(PhilT0BD, "/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Philtrees.Environmental.xlsx")

# Combine main data frame with environmental dataframe
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/")

Excltrees<-read.csv(file="Exclosure.seedlings.Philipo.csv", sep=",",header=TRUE)
EnviroPhil<-read.csv(file="Philtrees.Environmental.csv", sep=",",header=TRUE)

Excltrees$block_code<-as.factor(with(Excltrees, paste(area,landuse,block, sep="_")))
Excltrees$plot_code<-as.factor(with(Excltrees, paste(area,landuse,plot.ID, sep="_")))
levels(Excltrees$block_code) # 22
levels(Excltrees$plot_code) # 132

EnviroPhil$block_code<-as.factor(with(EnviroPhil, paste(area,landuse,block, sep="_")))
EnviroPhil$plot_code<-as.factor(with(EnviroPhil, paste(area,landuse,plot.ID, sep="_")))
levels(EnviroPhil$block_code) # 22
levels(EnviroPhil$plot_code) # 132

# Left join exclosure dataset with environmental covariables
Excltrees<-left_join(Excltrees,EnviroPhil,by=c("plot_code","date"))

#library(xlsx)
write.xlsx(Excltrees, "/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/Exclosure.seedlings.Philipo.environmental.xlsx")


##################################################################################################################################################################
# Exclosure tree seedlings probability of detection and survival and/or growth
##################################################################################################################################################################

# Import exclosure data with environmental covariables
setwd("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Philipo Jacob/")

Excltrees<-read.csv(file="Exclosure.seedlings.Philipo.enviro.csv", sep=",",header=TRUE)

names(Excltrees)
dim(Excltrees) # 570  37
str(Excltrees)

##################################################################################################################################################
# Data exploration 
##################################################################################################################################################
#Data exploration
# A Missing values?
# B Outliers in Y / Outliers in X
# C Collinearity X
# D Relationships Y vs X
# E Spatial/temporal aspects of sampling design (not relevant here)
# F Interactions (is the quality of the data good enough to include them?)
# G Zero inflation Y
# H Are categorical covariates balanced?
########################################################################################################
# Alain Zuur - data exploration functions
source(file="/Users/anotherswsmith/Documents/AfricanBioServices/Training/HighstatLibV10.R")

#Missing values
colSums(is.na(Excltrees)) 
# plot id/tree id nubers without tree seedligns and without mature trees (outside tree canopy)


# Outliers
names(Excltrees)
MyVar<-c("tree.biomass.g","MaxBio", "MinBio" , "MeanBio","MaxTemp",
  "MinTemp","MeanTemp", "MinMoist","MaxMoist", "MeanMoist", 
  "HerbPRC","total_dung", "livestock.grazer_dung","rain.sum.mm",
  "tree_height.m", "canopy_height.m","dbh","bulk_density_g.cm3")
Mydotplot(Excltrees[ ,MyVar]) 

# Outlier with BD potentiall


# Outliers by factors...
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
dotchart(Excltrees$tree.biomass.g,groups=Excltrees$n.non.N,main = "n.non.N")
dotchart(Excltrees$tree.biomass.g,groups=Excltrees$area,main = "area")
dotchart(Excltrees$tree.biomass.g,groups=Excltrees$landuse,main = "landuse")

#B Collinearity X
MyVar<-c("MaxBio", "MinBio" , "MeanBio","MaxTemp",
         "MinTemp","MeanTemp", "MinMoist","MaxMoist", "MeanMoist", 
         "HerbPRC","total_dung","rain.sum.mm")
         #"tree_height.m", "canopy_height.m","dbh","bulk_density_g.cm3")
pairs(Excltrees[,MyVar],lower.panel = panel.cor)


MyVar<-c("MaxBio", "MaxTemp", "MinMoist", 
         "HerbPRC","total_dung","rain.sum.mm",
         "tree_height.m", "canopy_height.m","dbh","bulk_density_g.cm3")
pairs(Excltrees[,MyVar],lower.panel = panel.cor)

# Boxplot for factors
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
boxplot(tree.biomass.g ~ landuse, 
        xlab = "landuse",
        ylab = "tree.biomass.g",
        data = Excltrees) # OK - but lots of zerso 

boxplot(Year.of.last.fire ~ flanduse,  
        xlab = "landuse",
        ylab = "Year of last fire",
        data = Philtrees4) # Stronger than above - landuse effect with time since last fire


# Collinearity of covariates with factors

boxplot(MaxBio ~ excl_open, 
        xlab = "excl_open",
        ylab = "MaxBio",
        data = Excltrees) # Cannot use in same model - exclosure more biomass

# Collinearity issues - especially with temperature at plot-scale with canopy
# Landuse and herbivory cannot be used in same model

# Zero inflation?
sum(Excltrees$tree.biomass.g == 0, na.rm=T)  # 294 no trees
100 * sum(Excltrees$tree.biomass.g== 0, na.rm=T) / nrow(Excltrees)  # 52% without trees

# Spatial correlation
par(mfrow = c(1, 1), mar = c(4, 3, 3, 2))
xyplot(Longitude ~ Latitude,
       data = Philtrees4,
       
       aspect = "iso",
       col = as.numeric(Philtrees4$area), # Repeat colour
       pch = 16) # Spatial correlation with XY and area - expected

par(pty = "s", mar = c(5,5,2,2), cex.lab = 1.5)       
plot(x = Philtrees4$Longitude,
     y = Philtrees4$Latitude,
     type = "p",
     pch = 16,
     xlab = "X-coordinates",
     ylab = "Y-coordinates")

ordihull(Philtrees4[, c("Longitude", "Latitude")],
         draw = "polygon",
         groups = Philtrees4[, "area"],
         label = F,
         col = "red")     
# Area is fine - summarizing long/lat grouping

########################################################################################################
# Probability of detecting a tree seedling
library(INLA)
library(nlme)
library(mgcv)
########################################################################################################

# Year Month
Rdate5May<-strptime(as.character(Excltrees$date),format="%d.%m.%Y",tz="Africa/Nairobi" )
Excltrees$Rdate<-as.Date(Excltrees$date,format="%d.%m.%Y",tz="Africa/Nairobi" )
Excltrees$YrMonth<-as.Date(Excltrees$Rdate, "%Y-%m")

Excltrees$YrMonth<-format(as.Date(Rdate5May), "%Y-%m")

# Unique block code
Excltrees$fblock.id<-as.factor(with(Excltrees, paste(area, landuse, block, sep="_")))
Excltrees$fblock.id<-as.factor(as.numeric(Excltrees$fblock.id))
summary(levels(Excltrees$fblock.id)) # 22

# Tree height - otside tree canopy with NA is zero
Excltrees$tree_height[is.na(Excltrees$tree_height)]<-0

# Housekeeping
Excltrees$flanduse<-as.factor(SerEbio5$landuse)
Excltrees$fYrMonth<-as.factor(Excltrees$YrMonth)
Excltrees$farea<-as.factor(Excltrees$area)
Excltrees$fblock.id<-as.factor(Excltrees$fblock.id)
Excltrees$fplot.ID<-as.factor(Excltrees$plot.ID)
Excltrees$fexcl_open<-as.factor(Excltrees$excl_open)
Excltrees$fn.non.N<-as.factor(Excltrees$n.non.N)


# Tree seedling - probabiliy model- INLA
ftreebin<- tree.bin ~  f(YrMonth, model="ar1", replicate=plot.ID)+
      + MaxBio+MaxTemp+MinMoist+HerbPRC + total_dung +fn.non.N+  
  tree_height +
  f(area, model = "iid")+f(block, model = "iid") +f(plot, model = "iid")

Tbinmds <- inla(ftreebin, family = "binomial",
             control.compute = list(waic=TRUE,dic=TRUE),
             data = Excltrees)  

# Waic
Tbinmds$waic$waic #  355.5483

names(Tbinmds)

# Guassian better # Get Betas
Tbinmds$summary.hyperpar
# sigma = sqrt(1 / precision)
1 /sqrt(Tbinmds$summary.hyperpar[,"mean"])

#Posterior mean values and 95% CI
Betas <-Tbinmds$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas, digits = 2) # Issues betas are all really small

#pvalue histogram....
pval<-rep(NA, nrow=(Excltrees))
for(i in 1:nrow(Excltrees)){
  pval[i]<-inla.pmarginal(q=Excltrees$tee.bin[i],
                          marginal=Tbinmds$marginals.fitted.values[[i]])
}
hist(pval) # Does not work for binomial 

# plot betas
Betas$mod<-"INLA"
Betas$names <- rownames(Betas)
pp<-ggplot(Betas, aes(x=names, y=mean,ymin=mean-sd, ymax=mean+sd, fill=mod,colour=mod))
pp<- pp+ geom_errorbar(width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
pp<- pp+ geom_point(size = 5, stroke=2,position=position_dodge(width=.65)) 
pp<-pp+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
pp

# Suggests negative effect of more biomass and positive effect of more wildlife herbivory

# Fixed factors
names(Tbinmds$marginals.fixed)
#mT<- Tbio4$marginals.fixed$temp
mM<- Tbinmds$marginals.fixed$MaxBio # Very important - negative effect
mHerb<- Tbinmds$marginals.fixed$HerbPRC 
mTemp<- Tbinmds$marginals.fixed$MaxTemp
mMoist<- Tbinmds$marginals.fixed$MinMoist
mTotDung<- Tbinmds$marginals.fixed$total_dung
mNfix<- Tbinmds$marginals.fixed$fn.non.NN
mNnon<- Tbinmds$marginals.fixed$fn.non.Nnon
mThi<- Tbinmds$marginals.fixed$tree_height # Postive effect - more wildlife = higher seedling rectruitment

par(mfrow=c(1,1), mgp=c(1.75,0.5,0),mar = c(3,3,3,3), xpd=F)

plot(x = mM[,1],
     y = mM[,2], 
     xlab = "Beta for Max herbaceous biomass",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Very important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mHerb[,1],
     y = mHerb[,2], 
     xlab = "Beta for Herbivory",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Very important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mTemp[,1],
     y = mTemp[,2], 
     xlab = "Beta for Max Temp",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mMoist[,1],
     y = mMoist[,2], 
     xlab = "Beta for Min Moist",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mTotDung[,1],
     y = mTotDung[,2], 
     xlab = "Beta for Total Dung",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mNfix[,1],
     y = mNfix[,2], 
     xlab = "Beta for N fix vs inter",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mNnon[,1],
     y = mNnon[,2], 
     xlab = "Beta for non tree fix vs inter",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")

plot(x = mThi[,1],
     y = mThi[,2], 
     xlab = "Beta for tree height",
     ylab = "Posterior distribution",
     type = "l",
     cex.lab = 1.5, lwd = 3) # Not important
abline(v = 0,lwd=2, lty = 2,col="red")


###########################################################
# Model validation - simulate the data 
###########################################################

# Tree seedling - probabiliy model- INLA
ftreebin<- tree.bin ~  f(YrMonth, model="ar1", replicate=plot.ID)+
  + HerbPRC+MaxBio+  
  f(area, model = "iid")+f(block, model = "iid") #+f(plot, model = "iid")

Tbinmds <- inla(ftreebin, family = "zeroinflated.binomial.1",
                control.compute = list(waic=TRUE,dic=TRUE),
                data = Excltrees) 

# Waic
Tbinmds$waic$waic #  377.7682...worse...?

#Posterior mean values and 95% CI
Betas <-Tbinmds$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas, digits = 2) # Issues betas are all really small


# plot betas
Betas$mod<-"INLA"
Betas$names <- rownames(Betas)
pp<-ggplot(Betas, aes(x=names, y=mean,ymin=mean-sd, ymax=mean+sd, fill=mod,colour=mod))
pp<- pp+ geom_errorbar(width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
pp<- pp+ geom_point(size = 5, stroke=2,position=position_dodge(width=.65)) 
pp<-pp+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
pp


# Predicted values from INLA model - presence absence
# Fixed factors
names(Tbinmds$marginals.fixed)

# Fitted
fitted4<-Tbinmds$summary.fitted.values$mean
fitted4

# Predictions 
# 1. Get the betas and gammas
#beta.mcmc <- outB$sims.list$beta 
#dim(beta.mcmc) #3000    6
names(Tbinmds)
Betas4 <- Tbinmds$summary.fixed[,("mean")] 
Betas4 # 10

#2. Define a grid of covariate values
#   without extrapolation
length(Excltrees)

MyData4 <- expand.grid( #fn.non.N=levels(Excltrees$fn.non.N),
                       MaxBio=seq(min(Excltrees$MaxBio),max(Excltrees$MaxBio), length=5000),
                      # MaxTemp=seq(min(Excltrees$MaxTemp),max(Excltrees$MaxTemp), length=25),          
                       HerbPRC=seq(min(Excltrees$HerbPRC),max(Excltrees$HerbPRC), length=10000))
                       #total_dung=seq(min(Excltrees$total_dung),max(Excltrees$total_dung), length=25),
                       #total_dung=seq(min(Excltrees$tree_height),max(Excltrees$tree_height), length=25))
head(MyData4)
max(MyData4$MaxBio)

Xp <- model.matrix(~MaxBio+HerbPRC, data = MyData4)
dim(Xp) # 30000 

#C. Calculate the predicted MCMC values
MyData4$eta <- Xp %*% as.matrix(Betas4)
MyData4$Pred   <- exp(MyData4$eta) / (1 + exp(MyData4$eta))# Back convert gamma 
dim(MyData4) # 15000     4
max(MyData4$Pred)

# Actually versus fitted probability of tree seedling
max(Excltrees$tree.bin)
Tbinmds$summary.fitted.values$mean
Z <- table(Tbinmds$summary.fitted.values$mean, Excltrees$tree.bin)
# Predicted = 1 = closer to absence, whereas prediced = 0 closer to present i.e 0.9 probabibility?!! Model is working

summary(Tbinmds$summary.fitted.values$mean>0.5)
#   Mode   FALSE    TRUE    NA's 
#logical     267     303       0  # 267 zeros and 303 present

summary(Excltrees$tree.bin>0.5)
#   Mode   FALSE    TRUE    NA's 
#logical     302     268       0  # Real data 
# Model is showing opposite 1 and 0 have been swamped when assigned

summary(MyData4$Pred<0.5)
# All predicted less than 0.5...why...not predicting above 0.5...

Z <- table(MyData4$Pred<0.5, Excltrees$tree.bin)
colnames(Z) <- c("Absent sampled", "Present sampled")
rownames(Z) <- c("Absent simulated", "Present simulated")
Z
#                    Absent sampled Present sampled
#Absent simulated             504             126
#Present simulated             97             149

N <- nrow(DistSNP2)
Correct <- sum(diag(Z)) / N 
CorrectAbsent <- Z[1,1] / N
CorrectPresent <- Z[2,2] / N
c(Correct, CorrectAbsent, CorrectPresent)


# Observed fitted verses actual - flip the <0.5 - as mode
Zmodel <- table(Tbinmds$summary.fitted.values$mean<0.5, Excltrees$tree.bin)
Zmodel

# THESE ARE PREDICTED VALUES AND NOT RANDOM SIMULATIONS...
par(mfrow = c(1,2))
hist(MyData4$Pred, 
     main = "Correctly classified absence and presence",
     xlim = c(0,1),
     xlab = "")
points(x = (Zmodel[1,1]+Zmodel[2,2])  / N, 
       y = 0, 
       col = 2, 
       cex = 3, 
       pch = 16) #

hist(MyData4$Pred, 
     main = "Correctly classified presence",
     xlim = c(0,1),
     xlab = "")
points(x = Zmodel[1,1] / N, 
       y = 0, 
       col = 2, 
       cex = 3, 
       pch = 16) # Under estimating zeros

hist(MyData4$Pred, 
     main = "Correctly classified absence",
     xlim = c(0,1),
     xlab = "")
points(x = Zmodel[2,2] / N, 
       y = 0, 
       col = 2, 
       cex = 3, 
       pch = 16) # It is OK for the presence  within range 

