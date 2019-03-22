setwd("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/R assistance/Analysis")
#### Biomass exploration ####
#clear system & add package libraries
rm(list=ls())
library(lattice) #model validation
library(MASS)
library(ggplot2)
library(dplyr)
library(gcookbook)
library(readr)
library(tidyr) #wide to long format
library(lemon)
library(Hmisc)

library(gridBase)
library(gridExtra)
library(ggpubr)
library(nlme)
library(lme4)
library(MuMIn) #for mod.sel()

Biomass <- read.csv("Biomass.csv", header=T,sep=",")
tail(Biomass)

Nuts <- read.csv("Nutrients.csv", header=T,sep=",")
Datanuts <- Nuts[Nuts$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Datanuts <- Datanuts[Datanuts$harvest!="H0",] #removing H0                #280 obs
Datanuts <- droplevels(Datanuts)

#### RUN FIRST ####
  #Housekeeping
  #Date variable
  #Dataframes for modelling
  #Functions
# For graphs
  #Average NAP
  #Average CONS 
  #Aggregating rainfall per region

#### Housekeeping ####
# Removing Ex2 - separate analysis
Databiom <- Biomass[Biomass$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Databiom <- Databiom[Databiom$harvest!="H0",] #removing H0                #280 obs
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
colnames(Databiom)[colnames(Databiom)=="productivity.other.g.m2.day"] <- "prodoth"
colnames(Databiom)[colnames(Databiom)=="consumption.other.g.m2.day"] <- "consoth"

colnames(Databiom)[colnames(Databiom)=="sand.per"] <- "sand"

#Renaming levels in region, landuse and treatment columns
levels(Databiom$region)<-c("Dry Region","Intermediate Region","Wet Region")
levels(Databiom$landuse)<-c("pasture","wild")
levels(Databiom$treatment)<-c("exclosed","open")

#### Date variable ####
# Rdate create month column. default was (="%d.%m.%Y")
Rdate<-strptime(as.character(Databiom$harvest.date),format="%m/%d/%Y",tz="Africa/Nairobi" )# East African time #USE
class(Rdate) # [1] "POSIXlt" "POSIXt" # This format needs a lot of memory - but good
Databiom$Rdate<-Rdate# Add to the dataframe #
# Create a Yr-Month time value as experiment runs over 15 months - > 2 years
# Rdate convert to Year-month
Databiom$YrMonth<-format(as.Date(Rdate), "%Y-%m")

Databiom$month<-Databiom$Rdate$mon+1
Databiom$month <- month.abb[Databiom$month] #Changing to month name abbrevitations
Databiom$month<-as.factor(Databiom$month)

# Running numeric value for months
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
Databiom$YrMonthNumber<-mondf(c(as.POSIXlt(as.Date(Databiom$harvest.date,format="%m/%d/%Y",tz="Africa/Nairobi" ))), "2017-02-01")*-1 # Need to remove lag - 1

# Plot.code to follow through time
Databiom$plot.code <- as.factor(with(Databiom,paste(region,landuse,block,treatment,sep="_")))
levels(Databiom$plot.code) #40 levels

#### New Y variables for hypotheses ####
# Adding new columns NAP-cons
Databiom$difftarget <- Databiom$prodtarg-Databiom$constarg
Databiom$difftotal <- Databiom$prodtot-Databiom$constot

# Target fraction in relation to community NAP target/total productivity
Databiom$prodtargfrac <- Databiom$prodtarg/Databiom$prodtot

# Adding new column with % NAP consumed 
Databiom$consper <- Databiom$constot/Databiom$prodtot*100
Databiom$constargper <- Databiom$constarg/Databiom$prodtarg*100

# Getting N total in Datanuts
Datanuts$N.target.biom <- Datanuts$N.target*Datanuts$biomass.target.sp./100
Datanuts$N.other.biom <- Datanuts$N.other*Datanuts$biomass.other.sp./100
Datanuts$N.total <- (Datanuts$N.target.biom+Datanuts$N.other.biom)/Datanuts$biomass.total.g*100

# Adding nitrogen data to Databiom
Databiom <- cbind(Databiom,Datanuts[,"N.target",drop=FALSE])
Databiom <- cbind(Databiom,Datanuts[,"N.other",drop=FALSE])
Databiom <- cbind(Databiom,Datanuts[,"N.total",drop=FALSE])

#Rain per day for each harvest period
Databiom$rain.day <- Databiom$rain.sum/Databiom$growth.period

#### Dataframes for modelling ####
# Dataframe total productivity
Dataprod <- Databiom[complete.cases(Databiom[c("prodtot")]),]   #271 obs

# Dataframe total consumption
Datacons <- Databiom[complete.cases(Databiom[c("constot")]),]   #135 obs

####Functions ####
# Loading function for determining marginal and conditional R square of mixed 
# models following Paul C.D. Johnson 2014. Extension of Nakagawa & Schielzeth's 
# R2Glmm to random slopes models, Methods in Ecology and Evolution

#' R-squared and pseudo-rsquared for a list of (generalized) linear (mixed) models
#'
#' This function calls the generic \code{\link{r.squared}} function for each of the
#' models in the list and rbinds the outputs into one data frame
#'
#' @param a list of fitted (generalized) linear (mixed) model objects
#' @return a dataframe with one row per model, and "Class",
#'         "Family", "Marginal", "Conditional" and "AIC" columns
rsquared.glmm <- function(modlist) {
  # Iterate over each model in the list
  do.call(rbind, lapply(modlist, r.squared))
}

#' R-squared and pseudo-rsquared for (generalized) linear (mixed) models
#'
#' This generic function calculates the r squared and pseudo r-squared for
#' a variety of(generalized) linear (mixed) model fits.
#' Currently implemented for \code{\link{lm}}, \code{\link{lmerTest::merMod}},
#' and \code{\link{nlme::lme}} objects.
#' Implementing methods usually call \code{\link{.rsquared.glmm}}
#'
#' @param mdl a fitted (generalized) linear (mixed) model object
#' @return Implementing methods usually return a dataframe with "Class",
#'         "Family", "Marginal", "Conditional", and "AIC" columns
r.squared <- function(mdl){
  UseMethod("r.squared")
}

#' Marginal r-squared for lm objects
#'
#' This method uses r.squared from \code{\link{summary}} as the marginal.
#' Contrary to other \code{\link{r.squared}} methods, 
#' this one doesn't call \code{\link{.rsquared.glmm}}
#'
#' @param mdl an lm object (usually fit using \code{\link{lm}},
#' @return a dataframe with with "Class" = "lm", "Family" = "gaussian",
#'        "Marginal" = unadjusted r-squared, "Conditional" = NA, and "AIC" columns
r.squared.lm <- function(mdl){
  data.frame(Class=class(mdl), Family="gaussian", Link="identity",
             Marginal=summary(mdl)$r.squared,
             Conditional=NA, AIC=AIC(mdl))
}

#' Marginal and conditional r-squared for merMod objects
#'
#' This method extracts the variance for fixed and random effects, residuals,
#' and the fixed effects for the null model (in the case of Poisson family),
#' and calls \code{\link{.rsquared.glmm}}
#'
#' @param mdl an merMod model (usually fit using \code{\link{lme4::lmer}},
#'        \code{\link{lme4::glmer}}, \code{\link{lmerTest::lmer}},
#'        \code{\link{blme::blmer}}, \code{\link{blme::bglmer}}, etc)
r.squared.merMod <- function(mdl){
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(lme4::fixef(mdl) %*% t(mdl@pp$X)))
  # Get variance of random effects by extracting variance components
  # Omit random effects at the observation level, variance is factored in later
  VarRand <- sum(
    sapply(
      VarCorr(mdl)[!sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))],
      function(Sigma) {
        X <- model.matrix(mdl)
        Z <- X[,rownames(Sigma)]
        sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X) } ) )
  # Get the dispersion variance
  VarDisp <- unlist(VarCorr(mdl)[sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))])
  if(is.null(VarDisp)) VarDisp = 0 else VarDisp = VarDisp
  if(inherits(mdl, "lmerMod")){
    # Get residual variance
    VarResid <- attr(lme4::VarCorr(mdl), "sc")^2
    # Get ML model AIC
    mdl.aic <- AIC(update(mdl, REML=F))
    # Model family for lmer is gaussian
    family <- "gaussian"
    # Model link for lmer is identity
    link <- "identity"
  }
  else if(inherits(mdl, "glmerMod")){
    # Get the model summary
    rmdl.summ <- summary(mdl)
    # Get the model's family, link and AIC
    family <- mdl.summ$family
    link <- mdl.summ$link
    mdl.aic <- AIC(mdl)
    # Pseudo-r-squared for poisson also requires the fixed effects of the null model
    if(family=="poisson") {
      # Get random effects names to generate null model
      rand.formula <- reformulate(sapply(findbars(formula(mdl)),
                                         function(x) paste0("(", deparse(x), ")")),
                                  response=".")
      # Generate null model (intercept and random effects only, no fixed effects)
      null.mdl <- update(mdl, rand.formula)
      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.mdl))
    }
  }
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, VarDisp, family = family, link = link,
                 mdl.aic = mdl.aic,
                 mdl.class = class(mdl),
                 null.fixef = null.fixef)
}

#' Marginal and conditional r-squared for lme objects
#'
#' This method extracts the variance for fixed and random effects,
#' as well as residuals, and calls \code{\link{.rsquared.glmm}}
#'
#' @param mdl an lme model (usually fit using \code{\link{nlme::lme}})
r.squared.lme <- function(mdl){
  # Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(mdl$call$fixed)[-2], mdl$data)
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(nlme::fixef(mdl) %*% t(Fmat)))
  # Get variance of random effects by extracting variance components
  VarRand <- sum(suppressWarnings(as.numeric(nlme::VarCorr(mdl)
                                             [rownames(nlme::VarCorr(mdl)) != "Residual",
                                               1])), na.rm=T)
  # Get residual variance
  VarResid <- as.numeric(nlme::VarCorr(mdl)[rownames(nlme::VarCorr(mdl))=="Residual", 1])
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, family = "gaussian", link = "identity",
                 mdl.aic = AIC(update(mdl, method="ML")),
                 mdl.class = class(mdl))
}

#' Marginal and conditional r-squared for glmm given fixed and random variances
#'
#' This function is based on Nakagawa and Schielzeth (2013). It returns the marginal
#' and conditional r-squared, as well as the AIC for each glmm.
#' Users should call the higher-level generic "r.squared", or implement a method for the
#' corresponding class to get varF, varRand and the family from the specific object
#'
#' @param varF Variance of fixed effects
#' @param varRand Variance of random effects
#' @param varResid Residual variance. Only necessary for "gaussian" family
#' @param family family of the glmm (currently works with gaussian, binomial and poisson)
#' @param link model link function. Working links are: gaussian: "identity" (default);
#'        binomial: "logit" (default), "probit"; poisson: "log" (default), "sqrt"
#' @param mdl.aic The model's AIC
#' @param mdl.class The name of the model's class
#' @param null.fixef Numeric vector containing the fixed effects of the null model.
#'        Only necessary for "poisson" family
#' @return A data frame with "Class", "Family", "Marginal", "Conditional", and "AIC" columns
.rsquared.glmm <- function(varF, varRand, varResid = NULL, varDisp = NULL, family, link,
                           mdl.aic, mdl.class, null.fixef = NULL){
  if(family == "gaussian"){
    # Only works with identity link
    if(link != "identity")
      family_link.stop(family, link)
    # Calculate marginal R-squared (fixed effects/total variance)
    Rm <- varF/(varF+varRand+varResid)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varResid)
  }
  else if(family == "binomial"){
    # Get the distribution-specific variance
    if(link == "logit")
      varDist <- (pi^2)/3
    else if(link == "probit")
      varDist <- 1
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else if(family == "poisson"){
    # Get the distribution-specific variance
    if(link == "log")
      varDist <- log(1+1/exp(null.fixef))
    else if(link == "sqrt")
      varDist <- 0.25
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else
    family_link.stop(family, link)
  # Bind R^2s into a matrix and return with AIC values
  data.frame(Class=mdl.class, Family = family, Link = link,
             Marginal=Rm, Conditional=Rc, AIC=mdl.aic)
}

#' stop execution if unable to calculate variance for a given family and link
family_link.stop <- function(family, link){
  stop(paste("Don't know how to calculate variance for",
             family, "family and", link, "link."))
}

### Setting LMM's
ctrl <- lmeControl(opt="optim", msMaxIter=500,msVerbose=TRUE)
varveg <- varIdent(form= ~ 1 | Veg_type)

####|####
#### DATA EXPLORATION ####
# Zuur 2010: 
# 1:outliers (Y&X) 2:heterogeneity (Y) 3:normality (Y) 4:zero's (Y) 5:collinearity(X)
# 6:relations between variables(X&Y) 7:interactions 8:independence(Y)

#### Spread of data ####
hist(Databiom$prodtot) # right-skewed + some values up to 8!
hist(Databiom$constot) # as much neg.values as pos! Mean cons. ~0... 
hist(Databiom$prodtarg) # most around +/-1, one outlier -4
hist(Databiom$constarg) #one value above 3

#### Outliers ####
# Using boxplot to visualize mean and spread of data (lower and upper end of box is 25% and 75% quantile)

par(mfrow = c(1, 1), mar = c(4, 3, 3, 2)) #mfrow several graphs on the same page
dotchart(Databiom$prodtarg,groups=as.factor(Databiom$plot.id)) # Outlier -4.29
dotchart(Databiom$prodtarg,groups=Databiom$landuse,main = "landuse") # Outlier -4.29 #Separating between landuses --> Both outliers in pasture
#To identify the outliers. plot(), then identify(). Click on outliers to define, then esc. 
plot(Databiom$prodtot) 
  #identify(Databiom$prodtot) # 25  37 243 247
plot(Databiom$prodtarg) 
  #identify(Databiom$prodtarg) # 253 254
plot(Databiom$constot) 
  #identify(Databiom$constot) # 25 273
plot(Databiom$constarg) 
  #identify(Databiom$constarg) #45 273

  # 25 is outlier in both prodtot and constot   #Large biomass! ~109g, due to underneath tree?

#Looking at specific rows with the outliers
Databiom[c(25,37,243,247),]  #49, 63, 311, 315
  #All >7, exclosed, H1/H1/H7/H7, dry/int/wet/wet, Makao/Seronera/Hand/Hand
Databiom[c(253,254),]
  #Both wet_P_3_H7  -4.29 -4.66 #There was a lot more target sp. in the setup than harvest plots
Databiom[c(25,273),]
  #Both ex, Dry_p_1 and se_1, H1/H7 4.59 and -2.79
Databiom[c(45),"constarg"]
  #Handajega Ex H2  3.35

#Plotting difftarget and difftotal
plot(Databiom$difftarget~Databiom$rain.sum)
  #identify(Databiom$difftarget~Databiom$rain.sum) #213 253

#### MAP of the study period #### 
# H3 to H7 (May 2017- May 2018) (Including H1 and H2 would give an overlap period, and want to include the large rainfall values from last harvest to show the unusual year)
AvgMAP <- aggregate(rain.sum~site.name+harvest,Databiom,mean)
AvgMAP <- AvgMAP[AvgMAP$harvest!="H1",]
AvgMAP <- AvgMAP[AvgMAP$harvest!="H2",]
AvgMAP1 <- aggregate(rain.sum~site.name,AvgMAP,sum)

####|####
#### GRAPHING ####
# #### Trying to plot rain.sum with date on the x-axis ####
# #plot(as.Date(harvest.date, format ="%m/%d/%Y"),rain.sum)
# #plot(as.factor(harvest.date), rain.sum)
# plot((rain.sum)~as.Date(Rdate), Databiom,na.rm=T)
# #abline(lm((consTotal)~rain.sum, DataEx5))
# #summary(lm((consTotal)~rain.sum, DataEx5))
# plot(Rdate,rain.sum,x_breaks, xlab =month.abb,Databiom)
# ?plot
# #ggplot
# rain <- ggplot(Databiom, aes(x=YrMonth, y=rain.sum, colour="dark blue"))
# rain <- rain + geom_line(aes(y=rain.sum, colour="dark blue",linetype=1,size=1, alpha=.1))
# 
# rain <-rain + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(as.Date("2017-02-10"),max=as.Date("2018-05-31")), expand=c(0,0)) #NOT working
# 
# dp<-dp+scale_y_continuous(limits=c(-2.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
# rain <-rain + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y", limits=c(min(Databiom$YrMonth),max=max(Databiom$YrMonth))) 

#### Average df site per harvest ####
DataEx <- Databiom[Databiom$treatment!="open",] #Removing Open
DataOp <- Databiom[Databiom$treatment!="exclosed",] #Removing Exclosure

Exmean <- aggregate.data.frame(DataEx[,c(18,19,21,26:33,40:46,51:59)], list(as.factor(DataEx$YrMonth), DataEx$site.id), mean, na.action="na.pass")
Exmean$treatment <- "exclosed"
Exmean$treatment <- factor(Exmean$treatment)
Exmean$Rdate <- as.character(Exmean$Rdate)

Opmean <- aggregate.data.frame(DataOp[,c(18,19,21,26:33,40:46,51:59)], list(as.factor(DataOp$YrMonth), DataOp$site.id), mean, na.action="na.pass")
Opmean$treatment <- "open"
Opmean$treatment <- factor(Opmean$treatment)
Opmean$Rdate <- as.character(Opmean$Rdate)

Datamean <- bind_rows(Exmean,Opmean) #NB! Using rbind resulted in aborting the whole session!!

names(Datamean)
View(Datamean)
?rbind
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
TarprodSE$pool<-"target"

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


#Stu averaged rainfall per region (aggregate(rain.sum~region+YrMonth,DataEx,mean))
#avgRain <- aggregate(rain.sum~region+YrMonth,na.rm=T,Databiom,mean)
#rain <- ggplot( data = avgRain, aes( x=YrMonth, y=rain.sum,colour="dark blue" )) + geom_line()

####OVERVIEW dataframes ####
  #Avgprod - 138 obs, 8 var
  #Avgprod2 - 
  #Avgprod3 - From Avgprod, joined with RainregionX
  #Avgprod4 - From Avgprod3, without target
  #Avgprod5 - From Avgprod4 without Seronera
  #Avgprod6 - From Avgprod5, without Seronera and open
  #Avgprod6b - From Avgprod4, with Seronera, without open

  #Avgcons - 70 obs, 7 var
  #Avgcons2 - without Seronera
  #Avgcons3 - From Avgcons, joined with RainregionX
  #Avgcons4 - From Avgcons3, without target
  #Avgcons5 - From Avgcons4, without Seronera

  #Avgprodcons  - From Avgprod6,Avgcons5    Without Seronera
  #Avgprodcons2 - Avgprod6b,Avgcons4        With Seronera 

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
#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPtarg.png",
     #  width= 26, height = 18,units ="cm",
    # dpi = 600, limitsize = TRUE)

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

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPwithSeronera.png",
    #   width= 26, height = 18,units ="cm",
    #  dpi = 600, limitsize = TRUE)

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

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAP.png",
    #   width= 26, height = 18,units ="cm",
    #   dpi = 600, limitsize = TRUE)
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

#### Avg NAP+CONS in one dataframe ####
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


#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONS.png",
    #   width= 26, height = 18,units ="cm",
    #   dpi = 600, limitsize = TRUE)

legend_title<-"land-use"
napcons<- ggplot(Avgprodcons, aes(x=YrMonth, y=Productivity, colour=landuse,fill=landuse,shape=Biomass_change,
                                    group=site.id.y))
napcons<-napcons+geom_hline(yintercept = 0, size =1, linetype="dotted", colour="grey")
napcons<-napcons+geom_line(aes(y = Consumption), linetype=2,size=1.2,show.legend=F)
napcons<-napcons+geom_point(aes(y = Consumption), shape =21,size=4,show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Consumption-SE.x.y, ymax=Consumption+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+scale_fill_manual(values=c(pasture = "tan3",wild = "turquoise3"))
napcons<-napcons+geom_line(linetype=1,size=1.2, alpha=1, show.legend=F)
napcons<-napcons+geom_errorbar(aes(ymin=Productivity-SE.x.x, ymax=Productivity+SE.x.x),width=.2,lwd=1.1,show.legend=F)
napcons<-napcons+geom_point(shape=22,size=4, fill="white", stroke=2,show.legend=F)
napcons<-napcons+facet_wrap(~landuse+region,ncol=2,scales='fixed')
napcons<-napcons+scale_y_continuous(limits=c(-1.5,8),sec.axis = sec_axis(~ . *70, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500), name = "Precipitation (mm)"))
napcons<-napcons+geom_line(aes(y = rain.sum/70),colour="dark blue",linetype=1,size=1, alpha=0.5)
#napcons<-napcons+geom_point(aes(y = rain.sum/70),colour="dark blue",fill="dark blue",size=.9,alpha=.2)
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
napcons<-napcons+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","Dry Region","Wet Region"),color="black", size=5)
napcons<-napcons+guides(shape=F, fill=F,colour = guide_legend(override.aes = list(shape=c(21, 21),
                                                                                    size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),nrow=2,byrow=TRUE))
napcons<-napcons+ guides(colour=F, fill=F,shape = guide_legend("Biomass change",override.aes = list(shape=c(21, 22),
                                                                                                      size=5,fill=c("gray50","white"),col="gray50", stroke=2),nrow=2,byrow=TRUE))

napconsb <-napcons+geom_point(data =Avgprodcons, aes(size=landuse, shape = NA), colour = "grey50")
napconsb<-napconsb+ guides(size=guide_legend("Land-use", override.aes=list(shape=c(21, 21), size=5,fill=c("tan3","turquoise3"),col=c("tan3","turquoise3"), stroke=2),
                                               nrow=2,byrow=TRUE),legend.margin=margin(0,0,0,0))
napconsb <- napconsb+theme(panel.spacing.x=unit(2, "lines"),panel.spacing.y=unit(1, "lines"))
napconsb

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONS2.png",
     #  width= 26, height = 18,units ="cm",
      # dpi = 600, limitsize = TRUE)

#### NAP+CONS plot with Seronera ####
#prod6b cons4

# Plot without intermediate and then add intermediate later
Avgprodcons2I<-droplevels(Avgprodcons2[Avgprodcons2$region=="Intermediate Region",])
Avgprodcons2excI<-droplevels(Avgprodcons2[Avgprodcons2$region!="Intermediate Region",])

Avgprodcons2$landuse <-factor (Avgprodcons2$landuse,levels=c("pasture","wild"))
Avgprodcons2$region <-factor (Avgprodcons2$region,levels=c("Dry Region","Wet Region","Intermediate Region"))


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
napcons2<-napcons2+facet_wrap(~landuse+region,ncol=2,scales='fixed')
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
napcons2<-napcons2+annotate(geom="text",x=as.Date("2017-10-01"), y=8, label=c("Dry Region","Wet Region","Dry Region","Wet Region", "Intermediate Region"),color="black", size=5)
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

#ggsave("C:/Users/Marit/Google Drive/0_Dokumenter/0_NTNU/0_Master/Presentations/Graphs/NAPCONSSeronera2BEST.png",
     #  width= 26, height = 18,units ="cm",
    # dpi = 600, limitsize = TRUE)


####|####
#### ANALYSIS ####
#### Precipitation line plot ####
#Producivity
par(mfrow=c(1,1))
plot(Databiom$rain.sum,Databiom$prodtot, ylab="productivity", xlab = "Rainfall", main="Rainfall and productivity",col=Databiom$landuse)
abline(lm((prodtot)~rain.sum, Databiom))
summary(lm(prodtot~rain.sum,data=Databiom)) #lm: y= 0.19+0.0032x, r^2=0.068

#Consumption
plot(Databiom$rain.sum,Databiom$constot, ylab="Consumption", xlab = "Rainfall", main="Rainfall and consumption",col=Databiom$landuse)
abline(lm((constot)~rain.sum, Databiom))
summary(lm((constot)~rain.sum, Databiom)) #lm: y=0.46+0.00081, r^2=0.004 

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

#Correlations between categorical variables
# aov (continuous vs categorical)
#otherwise a chisq.test? (categorical vs categorical)
pairs(~region+landuse+treatment+harvest+harvest.date+rain.sum+pasture.disc.harvest+height.harvest+total.veg.cover.harvest,data=Databiom) 
MyVar <- c("landuse", "treatment", "harvest", "rain.sum","region") 

#Test correlations between explanatory variables with boxplots

# Correlation between rainfall and productivity
rainprod <- Databiom %>%
  select("rain.sum", "prodtot")

mycor <- rcorr(rainprod$rain.sum, rainprod$prodtot,type="pearson")
mycor$r
mycor$P

# Different landuse  
rainprod_pasture <- Databiom %>%
  select("rain.sum", "prodtot")%>%
  subset(Databiom$landuse =="pasture")
mycor_pasture <- rcorr(rainprod_pasture$rain.sum, rainprod_pasture$prodtot,type="pearson")
mycor_pasture$r
mycor_pasture$P

rainprod_wild <- Databiom %>%
  select("rain.sum", "prodtot")%>%
  subset(Databiom$landuse =="wild")
mycor_wild <- rcorr(rainprod_wild$rain.sum, rainprod_wild$prodtot,type="pearson")
mycor_wild$r
mycor_wild$P

#Correlation between rainfall and consumption
raincons <- Databiom %>%
  select("rain.sum", "constot")
mycor2 <- rcorr(raincons$rain.sum, raincons$constot,type="pearson")
mycor2$r
mycor2$P

# Different landuse  
raincons_pasture <- Databiom %>%
  select("rain.sum", "constot")%>%
  subset(Databiom$landuse =="pasture")
mycor2_pasture <- rcorr(raincons_pasture$rain.sum, raincons_pasture$constot,type="pearson")
mycor2_pasture$r
mycor2_pasture$P

raincons_wild <- Databiom %>%
  select("rain.sum", "constot")%>%
  subset(Databiom$landuse =="wild")
mycor2_wild <- rcorr(raincons_wild$rain.sum, raincons_wild$constot,type="pearson")
mycor2_wild$r
mycor2_wild$P


#### Exploring variables with lm ####
# NAP and Landuse
napmodlanduse <- lm(prodtot~landuse,data=Databiom)
anova(napmodlanduse) #F is 1.04, non-significant
summary(napmodlanduse) #AdjRsquared: 0.00016
plot(napmodlanduse) #Outliers 49,311,315

par(mfrow=c(1,1))
boxplot(prodtot~landuse,data=Databiom) #no difference
boxplot(prodtot~landuse+region,data=Databiom) #no difference


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
napmod1 <- lm(prodtot~landuse+region+rain.sum+landuse:region, Databiom)
summary(napmod1) #rainsum and regionIntermediate is significant
anova(napmod1) 
par(mfrow=c(2,2))
plot(napmod) #rows 25 37 outliers

#Removing these rows
Databiomtest <- Databiom[-c(25,37),]

#### Example from QA ####
plot(lengt, SM)
abline(model.cond1) # line for arithmetic model
range(lengt)
xx<-seq(35,52,0.1) #defining range of x-axis
yy<-exp(-11.87896)*xx^2.96885 #Equation, to make a curve. intercept * slope
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

####MIXED models with auto-correlation ####
#AUTOCORRELATION - adding as a random component
#a.Extracting residuals from a linear model
#b.Look at residuals in acf - is there a pattern? 
#c.Making autocorrelation matrix
#d.Including autocorr in the mixed model

#### Total NAP periodic lme #### 
#Dataframe without the Handajega H7 values
Dataprod1 <- Dataprod[!(Dataprod$site.name=="Handajega" & Dataprod$harvest=="H7"),] #Removing Handajega H7

# Linear model
napmod <- lm(prodtot~landuse+treatment+log(rain.sum)+sand+
               landuse:log(rain.sum),data=Dataprod)
summary(napmod)
plot(resid(napmod)~Dataprod$landuse,xlab="landuse",ylab="residuals")
plot(resid(napmod)~Dataprod$rain.sum,xlab="rainfall",ylab="residuals")
par(mfrow=c(1,1))

#Plotting residuals against time (YrMonthNumber)
plot(resid(napmod)~Dataprod$YrMonthNumber,xlab="YrMonth",ylab="residuals") #not evenly distributed, so there is a pattern

#a.Extracting residuals from lm
E <- residuals(napmod,type="pearson")
I1 <- !is.na(Dataprod$prodtot)
Efull <- vector(length=length(Dataprod$prodtot))
Efull <- NA
Efull[I1]<- E
Efull

#b.time auto-correlated
acf(Efull, na.action=na.pass,main="Auto-correlation plot for residuals") #again, there is a pattern
xyplot(Efull~YrMonthNumber|site.name, col=1,ylab="Residuals",data=Dataprod)

#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|site.name/block.id/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = Dataprod)
corMatrix(cs1AR1.) #What does this give? 

#LME with temporal auto-correlation (using nlme package)
NAP.lme <- lme(prodtot~landuse+treatment+sand+rain.sum+
                  landuse:treatment+
                  landuse:rain.sum+
                  treatment:rain.sum+
                  landuse:sand+
                  rain.sum:sand+
                  landuse:treatment:rain.sum, 
              random=~1|site.name/block.id, method="REML",correlation=cs1AR1,data=Dataprod)
summary(NAP.lme)#for parameter estimates, don't use the p-values
anova(NAP.lme) #get F statistics and P-values
AIC(NAP.lme) #1185.861

# Checking the temporal autocorrelation
# Extracting residuals from mixed model
E2 <- resid(NAP.lme, type ="n")  # nlme: type = "n" , lme4: type= "pearson"
F2 <- fitted(NAP.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals NAP.lme")
abline(v = 0, lwd = 2, col = 2) # Some fitted values below zero. Bad? 
abline(h = 0, lty = 2, col = 1)  # Quite equally spread above/below zero

# Time auto-correlated
acf(E2, na.action=na.pass,main="Auto-correlation plot for residuals") # Temproal correlation

#Selecting fixed structure using ML. Simplifying with drop1
#Rain.sum non-transformed
NAPfull1 <- lme(prodtot~treatment+rain.sum,
                  #landuse:treatment+
                  #landuse:rain.sum,
                  #treatment:rain.sum,
                  #landuse:sand+
                  #rain.sum:sand+
                  #landuse:treatment:rain.sum, 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull1,test="Chisq") #dropping if not significant term
AIC(NAPfull1) #1094.439
P1 <- NAPfull1
anova(P1)

#Poly(rain.sum,2) 
  # As we expect effect size to level off at certain threshold
NAPfull2 <- lme(prodtot~landuse+treatment+sand+poly(rain.sum,2)+
                  #landuse:treatment+
                  landuse:poly(rain.sum,2)+
                  #treatment:poly(rain.sum,2)+
                  #landuse:sand+
                  sand:poly(rain.sum,2),
                  #landuse:treatment:poly(rain.sum,2), 
                 random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull2,test="Chisq") #dropping if not significant term
AIC(NAPfull2) #1032.561
P2 <- NAPfull2
anova(P2)

#Poly(rain.day,2)
NAPfull2.2 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
                  #landuse:treatment+
                  #landuse:sand+
                  landuse:poly(rain.day,2),
                  #treatment:poly(rain.day,2),
                  #sand:poly(rain.day,2)+
                #landuse:treatment:poly(rain.day,2), 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull2.2,test="Chisq") #dropping if not significant term
AIC(NAPfull2.2) #1026.974
P2.2 <- NAPfull2.2
anova(P2.2)

#Model averaging: All possible models from the global P2
P2dredge <- dredge(P2.2,trace=T,rank="AICc",REML=F) #No doubt in keeping all variables from this one. All other have deltaAIC >2

# Updating the model - generating p-values for each term (with ML)
# landuse + treatment + poly(rain.day,2) + land:rain
P2b <- update(P2.2,  .~. -landuse:poly(rain.day,2))
P2c <- update(P2b, .~. -landuse)
P2d <- update(P2b, .~. -treatment)
P2e <- update(P2b, .~. -poly(rain.day,2))

anova(P2,P2b) #landuse:poly(rain.day,2)   p-value: <.0001
anova(P2b,P2c) #landuse             p-value: NS (.32)
anova(P2b,P2d) #treatment           p-value: <.01
anova(P2b,P2e) #rain                p-value <.0001

#Log(rain.sum)
NAPfull3 <- lme(prodtot~landuse+treatment+sand+rain.sum+log(rain.sum)+
                  #landuse:treatment+
                  landuse:rain.sum+
                  landuse:log(rain.sum)+
                  #treatment:rain.sum+
                  #treatment:log(rain.sum)+
                  #landuse:sand+
                  sand:rain.sum+
                  sand:log(rain.sum),
                  #landuse:treatment:rain.sum+
                  #landuse:treatment:log(rain.sum), 
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod)
drop1(NAPfull3,test="Chisq") #dropping if not significant term
AIC(NAPfull3) #1075.48
P3 <- NAPfull3
anova(P3)
model.sel(P1,P2,P2.2,P3) #P2.2 seems best


#### Model without Handajega H7
#Poly(rain.day,2)
NAPfull2.3 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
                   #landuse:treatment+
                   #landuse:sand+
                   landuse:poly(rain.day,2),
                 #treatment:poly(rain.day,2),
                 #sand:poly(rain.day,2)+
                 #landuse:treatment:poly(rain.day,2), 
                 random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Dataprod1)
drop1(NAPfull2.3,test="Chisq") #dropping if not significant term
AIC(NAPfull2.3) #1026.974
P2.3 <- NAPfull2.3
anova(P2.3)
P2.3dredge <- dredge(P2.3,trace=T,rank="AICc",REML=F)

# Updating the model - generating p-values for each term (with ML)
# landuse + treatment + poly(rain.day,2) + land:rain
P2b <- update(P2.3,  .~. -landuse:poly(rain.day,2))
P2c <- update(P2b, .~. -landuse)
P2d <- update(P2b, .~. -treatment)
P2e <- update(P2b, .~. -poly(rain.day,2))

anova(P2.3,P2b) #landuse:poly(rain.day,2)   p-value: 0.04
anova(P2b,P2c) #landuse             p-value: NS (.41)
anova(P2b,P2d) #treatment           p-value: <.001
anova(P2b,P2e) #rain                p-value <.0001

####Validating models ####
#Step 9 and 10 - Zuur. The aftermath
# Refitting with REML and validating the model
P2.2 <- lme(prodtot~-1+landuse+treatment+poly(rain.day,2)+
      landuse:poly(rain.day,2),
    random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Dataprod)
summary(P2.2)

#Without WET_H7
P2.3 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
                   landuse:poly(rain.day,2),
                 random=~1|site.name/block.id,method="REML",correlation=cs1AR1, data=Dataprod1)
summary(P2.3)

#With weight: VarIdent, allowing unequal variance
P2.4 <- lme(prodtot~landuse+treatment+poly(rain.day,2)+
                  landuse:poly(rain.day,2),
                random=~1|site.name/block.id,method="REML",correlation=cs1AR1,
                weights = varIdent(form =~1 | treatment*landuse), data=Dataprod)

#Graphical model validation checking for homogeneity by plotting standardized residuals vs fitted values
par(mfrow=c(1,1))
E <- resid(P2.3,type="normalized")
Fit <- fitted(P2.3)
#plot(x=Fit,y=E,xlab="Fitted values",ylab="Residuals") 
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = Fit, 
     y = E,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals P2final")
abline(v = 0, lwd = 2, col = 2) #Fitted values: many below zero, and some large... bad spread? 
abline(h = 0, lty = 2, col = 1) 
#Alternatively: plot(P2cfinal)   Get the same, residuals Vs fitted
plot(E~landuse,data=Dataprod1,main="Landuse",ylab="Residuals") #a bit less var. for pasture
plot(E~treatment,data=Dataprod1,main="Treatment",ylab="Residuals") #quite equal
plot(x=Dataprod1$rain.sum,y=E,ylab="Residuals",xlab="Rainfall",main="Rainfall")
hist(E) #Residuals of the model
hist(Dataprod1$prodtot) #hist of Y-variable
plot(P2.3,prodtot~fitted(.)) #Y variable vs fitted values 
plot(P2.3,prodtot~resid(.))  # Y variable vs residuals

par(mfrow=c(2,2))
plot(predict(P2.3)~landuse+treatment+rain.day+
       landuse:rain.day,data=Dataprod)

#Another way of validating the model - line should be straight!
xyplot(E~rain.sum|treatment*landuse,
       data=Dataprod, ylab="Residuals", xlab="Rainfall (periodic)",
       panel=function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

#### Extracting estimates and F statistics ####
summary(P2cfinal) #Parameter estimates
anova(P2cfinal) #F statistics with p-values and df 
r.squared.lme(P2cfinal) #To get conditional and marginal R^2 for the model


# Bad fit - solution --> additive mixed model (gamm)  #See Zuur ch.5.10
library(mgcv)
P4 <- gamm(prodtot~landuse+treatment+sand+poly(rain.sum,2)+
             landuse:poly(rain.sum,2)+
             sand:poly(rain.sum,2),
           random = list(site.name=~ 1), data = Dataprod) #How to also add block to random structure? 

summary(P4)
plot(P4$lme) #Same bad structure!
plot(P4$gam)

#Exploring some raw data again. 
par(mfrow=c(1,2))
Prod1 <- boxplot(prodtot~landuse,data=Dataprod, main="Land-use",ylab="Productivity")
Prod2 <- boxplot(prodtot~landuse,data=Dataprod1, main="Land-use",ylab="Productivity1")

boxplot(prodtot~YrMonth,data=Dataprod,main="Time|harvest",ylab="Productivity")
plot(prodtot~rain.sum,data=Dataprod,main="Rainfall",ylab="Productivity")
plot(difftotal~landuse,data=Dataprod)

#### Sketch fitted values, following Stu's script ####
#A. Specify covariate values for predictions
#B. Create X matrix with expand.grid
#C. Calculate predicted values
#D. Calculate standard errors (SE) for predicted values
#E. Plot predicted values
#F. Plot predicted values +/- 	1.96 * SE
str(Dataprod)

#A:Specify covariate values for predictions
MyData <- expand.grid(landuse=levels(Dataprod$landuse),treatment=levels(Dataprod$treatment),
                      rain.sum=seq(min(Dataprod$rain.sum), max(Dataprod$rain.sum), length = 25)) #Length of rain.sum estimates 25 random numbers between the min and max for every other category (if just landuse in the model, then it would estimate 50 random points  - 25 for pasture/ 25 for wild)
#B. Create X matrix with expand.grid
X <- model.matrix(~landuse+treatment+poly(rain.sum,2)+
                    landuse:poly(rain.sum,2),data=MyData)
head(X)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X %*% fixef(P2.2)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X %*% vcov(P2.2) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[4]<-"prodtot"

library(tidybayes)
#### Plotting observed data versus prediction #####
# Scatter plot with community NAP and rainfall
NAPpred<-ggplot(data=Dataprod,aes(x=rain.sum, y=prodtot)) #observed
NAPpred<-NAPpred+geom_ribbon(data=MyData,aes(ymin=SeUp, ymax=SeLo),fill="springgreen4",colour="springgreen4",alpha=.65,lwd=NA,show.legend=F)
NAPpred<-NAPpred+geom_line(data=MyData,aes(ymin=SeUp, ymax=SeLo),colour="springgreen4",alpha=.9,lwd=2,show.legend=F)
NAPpred<-NAPpred+geom_point(stats="identity",aes(colour=region,fill=region),size=2.5) #observed values
NAPpred <- NAPpred+scale_colour_manual(values=c("goldenrod1","dodgerblue1","deepskyblue4"))
NAPpred<-NAPpred+facet_wrap(~landuse+treatment, scale="fixed")
NAPpred<-NAPpred+scale_x_continuous(limits=c(0,530), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0,0))
NAPpred<-NAPpred+scale_y_continuous(expand=c(0,0))
NAPpred<-NAPpred+ylab(expression(paste("Productivity (g ",m^-2," ",day^-1,")")))+xlab("Rainfall (mm)") # Adding x and ylabs to plot
NAPpred<-NAPpred+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
NAPpred<-NAPpred+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
NAPpred<-NAPpred+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 0.5) 
NAPpred

#### Total CONS, periodic lme ####
#Removing Nas
Datacons <- Datacons[complete.cases(Datacons$prodtot),]
Datacons <- Datacons[complete.cases(Datacons$N.total),]
#Dataframe without the Handajega H7 values
Datacons1 <- Datacons[!(Datacons$site.name=="Handajega" & Datacons$harvest=="H7"),]

#Linear model
consmod <- lm(constot~landuse+rain.sum+sand+prodtot+N.total+
               landuse:rain.sum,data=Datacons)
summary(consmod)
plot(resid(consmod)~Datacons$landuse,xlab="landuse",ylab="residuals")
plot(resid(consmod)~Datacons$YrMonthNumber,xlab="YrMonth",ylab="residuals") #pattern through time

#Implementing the AR-1 autocorrelation
cs1AR1 <- corAR1(0.2, form = ~YrMonthNumber|site.name/block.id/plot.code) # AR matrix needs to be unique
cs1AR1. <- Initialize(cs1AR1, data = Datacons)
corMatrix(cs1AR1.)

#LME with temporal auto-correlation (using nlme package)
CONS.lme <- lme(constot~landuse+prodtot+sand+poly(rain.sum,2)+N.total+
                  landuse:prodtot+
                  landuse:sand+
                  landuse:poly(rain.sum,2)+
                  landuse:N.total+
                  poly(rain.sum,2):sand+
                  poly(rain.sum,2):N.total+
                  prodtot:N.total+
                  sand:N.total+
                  prodtot:sand+
                  landuse:sand:poly(rain.sum,2), 
              random=~1|site.name/block.id, method="REML",correlation=cs1AR1,data=Datacons)
summary(CONS.lme)#don't use the p-values from here
anova(CONS.lme) #nothing significant
AIC(CONS.lme) #541.2802

# Checking the temporal autocorrelation on this model. Extracting residuals.
EC <- resid(CONS.lme, type ="n")  # nlme: type = "n" , lme4: type= "pearson"
FC <- fitted(CONS.lme)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = FC, 
     y = EC,
     xlab = "Fitted values",
     ylab = "Residuals",main="Residuals CONS.lme")
abline(v = 0, lwd = 2, col = 2) # No fitted values below zero!
abline(h = 0, lty = 2, col = 1)  # Quite equally spread above/below zero

# Time auto-correlated
acf(EC, na.action=na.pass,main="Auto-correlation plot for residuals") # Temproal correlation 

#Selecting fixed structure using ML. Simplifying with drop1
CONS.lme <- lme(constot~prodtot,
                  #landuse:prodtot+
                  #landuse:sand+
                  #landuse:poly(rain.sum,2)+
                  #landuse:N.total+
                  #poly(rain.sum,2):sand+
                  #poly(rain.sum,2):N.total+
                  #prodtot:N.total+
                  #sand:N.total,
                  #prodtot:sand+
                  #landuse:sand:poly(rain.sum,2),
                random=~1|site.name/block.id,method="ML",correlation=cs1AR1, data=Datacons)
drop1(CONS.lme,test="Chisq") #dropping if not significant term
C1 <- CONS.lme
AIC(C1)

# Refitting with REML and validating the model
C1final <- lme(constot~prodtot,
               random=~1|site.name/block.id,method="REML",na.action=na.pass, correlation=cs1AR1, data=Datacons)

#Checking for homogeneity by plotting standardized residuals vs fitted values, and resid. against each fixed component
par(mfrow=c(1,1))
EC <- resid(C1final,type="normalized")
FC <- fitted(C1final)
plot(x=FC,y=EC,xlab="Fitted values",ylab="Residuals") #looks like good spread
plot(C1final) #Gives same as above 

plot(EC~prodtot,data=Datacons,main="Productivity",ylab="Residuals")
hist(EC) #Residuals of the model
plot(C1,constot~fitted(.)) #Y variable vs fitted values. Should be straight line? 
plot(C1,constot~resid(.),ylab="Residuals",xlab="Consumption",main="Residuals model C1")  # Observed observations vs model residuals. Should be straight line? 
plot(predict(C1)~prodtot,data=Datacons) #Predicted(fitted) vs fixed effect

#### Extracting estimates and F statistics ####
summary(C1) #Parameter estimates
anova(C1) #F statistics with p-values and df 
r.squared.lme(C1) #To get conditional and marginal R^2 for the model

#### Sketch fitted values (from chosen model) ####
#A:Specify covariate values for predictions
MyData <- expand.grid(prodtot = seq(min(Datacons$prodtot), max(Datacons$prodtot), length = 25))

#B. Create X2 matrix with expand.grid
X2 <- model.matrix(~ prodtot, data = MyData)
head(X2)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$Pred <- X2 %*% fixef(C1)  # = X * beta

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X2 %*% vcov(C1) %*% t(X2))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)
colnames(MyData)[2]<-"constot"

#### CONS obs VS pred #####
# Scatter plot with community CONS and rainfall
CONSpred<-ggplot(data=Datacons,aes(x=rain.sum, y=constot)) #observed
CONSpred<-CONSpred+geom_ribbon(data=MyData2,aes(ymin=SeUp, ymax=SeLo),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)
CONSpred<-CONSpred+geom_line(data=MyData2,aes(ymin=SeUp, ymax=SeLo),colour="red",alpha=.9,lwd=2,show.legend=F)
CONSpred<-CONSpred+geom_point(stats="identity",colour="grey50",fill="grey50",size=2.5) #observed values
CONSpred<-CONSpred+facet_wrap(~landuse, scale="fixed")
CONSpred<-CONSpred+scale_x_continuous(limits=c(0,530), breaks = c(0,200,400), labels = c(0,200,400), expand=c(0,0))
CONSpred<-CONSpred+scale_y_continuous(expand=c(0,0))
CONSpred<-CONSpred+ylab(expression(paste("Consumption (g ",m^-2," ",day^-1,")")))+xlab("Rainfall (mm)") # Adding x and ylabs to plot
CONSpred<-CONSpred+theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
CONSpred<-CONSpred+annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
CONSpred<-CONSpred+annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 0.5) 
CONSpred

#### Dominant sp. NAP proportion lme ####
hist(Dataprod$prodtargfrac)




####|####
####VEGETATION COVER ####

##Harvest covers##
Speciesh <- read.csv("Species.cover.harvest.csv", header=T)
# Removing Ex2 and H0
Speciesh <- Speciesh[Speciesh$treatment!="EX2",] #Removing Mesh exclosures
Speciesh <- Speciesh[Speciesh$harvest!="H0",] #removing H0    
Speciesh <- droplevels(Speciesh)

# Creating factor variables
Speciesh$landuse<-as.factor(Speciesh$landuse)
Speciesh$region<-as.factor(Speciesh$region)
Speciesh$site.name <- as.factor(Speciesh$site.name)
Speciesh$block<-as.factor(Speciesh$block)
Speciesh$treatment<-as.factor(Speciesh$treatment)
Speciesh$harvest<-as.factor(Speciesh$harvest)
Speciesh$site.id <- as.factor(Speciesh$site.id)
Speciesh$block.id.harvest <- as.factor(Speciesh$block.id.harvest)

#Renaming several columns
colnames(Speciesh)[colnames(Speciesh)=="total.veg.cover.harvest"] <- "totalcover"
colnames(Speciesh)[colnames(Speciesh)=="pasture.disc.harvest"] <- "pasture.disc"
colnames(Speciesh)[colnames(Speciesh)=="height.harvest"] <- "height"
colnames(Speciesh)[colnames(Speciesh)=="sum.sp.harvest"] <- "sum.sp"

#Getting long format 
Speciesh_long <- gather(Speciesh, sp.abb, cover, 21:108, factor_key=TRUE)
Speciesh_long$setup.harvest <- "harvest"

#Removing NAs in cover column
Speciesh_long <- na.omit(Speciesh_long, cols="cover")

##SETUP covers ##
SpeciesS <- read.csv("Species.cover.setup.csv", header=T,sep=";")

# Removing Ex2 and H0
SpeciesS <- SpeciesS[SpeciesS$treatment!="EX2",] #Removing Mesh exclosures
SpeciesS <- SpeciesS[SpeciesS$harvest!="H0",] #removing H0    
SpeciesS <- droplevels(SpeciesS)

# Creating factor variables
SpeciesS$landuse<-as.factor(SpeciesS$landuse)
SpeciesS$region<-as.factor(SpeciesS$region)
SpeciesS$site.name <- as.factor(SpeciesS$site.name)
SpeciesS$block<-as.factor(SpeciesS$block)
SpeciesS$treatment<-as.factor(SpeciesS$treatment)
SpeciesS$harvest<-as.factor(SpeciesS$harvest)
SpeciesS$site.id <- as.factor(SpeciesS$site.id)
SpeciesS$block.id.harvest <- as.factor(SpeciesS$block.id.harvest)

#Renaming several columns
colnames(SpeciesS)[colnames(SpeciesS)=="total.veg.cover.setup"] <- "totalcover"
colnames(SpeciesS)[colnames(SpeciesS)=="target.sp.cover.setup"] <- "targetcover"
colnames(SpeciesS)[colnames(SpeciesS)=="pasture.disc.setup"] <- "pasture.disc"
colnames(SpeciesS)[colnames(SpeciesS)=="height.setup"] <- "height"
colnames(SpeciesS)[colnames(SpeciesS)=="sum.sp.setup"] <- "sum.sp"

#Getting long format 
SpeciesS_long <- gather(SpeciesS, sp.abb, sp.cover, 21:137, factor_key=TRUE)

#Need to make all NAs as zeros - to calculate means
SpeciesS_long$observations <- SpeciesS_long$sp.cover #keeping column with NA for #observations
SpeciesS_long$sp.cover[is.na(SpeciesS_long$sp.cover)]<-0
SpeciesS_long$sp.cover[SpeciesS_long$sp.cover<5]<-0

SpeciesS_long$setup.harvest <- "setup"

#### Aggregated sp.covers per site ####
AvgSpecies <- aggregate(sp.cover~sp.abb+site.name,SpeciesS_long, mean) #580
AvgSpecies$sp.cover <- round(AvgSpecies$sp.cover,digits=0)

#Adding SE to AvgSpecies
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
AvgSpeciesSE <- aggregate(sp.cover~sp.abb+site.name,SpeciesS_long, SE) #580
AvgSpeciesSE$sp.cover <- round(AvgSpeciesSE$sp.cover,digits=2)
colnames(AvgSpeciesSE)[3]<-"SE"

AvgSpecies$SE<-AvgSpeciesSE$SE

#Number of observations per species
AvgSpeciesObs <- aggregate(observations~sp.abb+site.name,SpeciesS_long, length) #151
AvgSpecies <- left_join(AvgSpeciesObs,AvgSpecies)

#Defining upper and lower limits
AvgSpecies$SeUp<-round(AvgSpecies$sp.cover+AvgSpecies$SE,digits=2)
AvgSpecies$SeLo<-round(AvgSpecies$sp.cover-AvgSpecies$SE,digits=2)

#### Average total vegetation cover per site ####
Avgtotal <- aggregate(totalcover~site.name,SpeciesS_long,mean)
AvgtotalSE <- aggregate(totalcover~site.name,SpeciesS_long,SE) #This SE must be bigger? 
AvgtotalSE$totalcover <- round(AvgtotalSE$totalcover,digits=2)
colnames(AvgtotalSE)[2] <- "SE"
Avgtotal$SE <- AvgtotalSE$SE

# Average veg. height per site 
Avgheight <- aggregate(height..setup~site.name,SpeciesS_long,mean)

#######################

#Getting average veg.covers per site per treatment for each harvest
AvgVegcover <- aggregate(total.veg.cover.harvest~region+landuse+site.name+harvest+treatment+setup.harvest,na.rm=T,Speciesh_long,mean)

#### HOW TO - aggregate correctly and adding SE ####
#Demo from previously
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


####|####
#### USEFUL STUFF ####
#### Vildes soil data ####
#Soil <- Total_soil_data #Total.soil.data.csv

#Getting N values per block
Nblock <- aggregate(Tot.N.per~Region+Block,Soil,mean)
Nsite <- aggregate(Tot.N.per~Region,Soil,mean)

#Getting SOC per block
SOCblock <- aggregate(Org.C.per~Region+Block,Soil,mean)
SOCsite <- aggregate(Org.C.per~Region,Soil,mean)

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
#### Citation from R #### 
#citate() 
#### Extracting random effects from a model ####
ranef(model) #using REML
#### Aggregate by several variables at once #### 
aggregate(cbind(var1,var2)~region+block,df,mean) #Anders brukte denne

#A:Specify covariate values for predictions
MyData <- expand.grid(landuse=levels(Dataprod$landuse),treatment=levels(Dataprod$treatment),
                      rain.sum=seq(min(Dataprod$rain.sum), max(Dataprod$rain.sum), length = 25),sand=seq(min(Dataprod$sand),max(Dataprod$sand),length.out = 25)) 

####Biomass stacked - target and other species connected in one column #### 
Stacked <- read.csv("Biomass.Stacked.csv", header=T,sep=",")

Stacked <- Stacked[Stacked$treatment!="EX2",] #Removing Mesh exclosures  #300 obs
Stacked <- Stacked[Stacked$harvest!="H0",] #removing H0                #280 obs
Stacked <- droplevels(Stacked)

Stacked$landuse<-as.factor(Stacked$landuse)
Stacked$region<-as.factor(Stacked$region)
Stacked$site.name <- as.factor(Stacked$site.name)
Stacked$block<-as.factor(Stacked$block)
Stacked$treatment<-as.factor(Stacked$treatment)
Stacked$harvest<-as.factor(Stacked$harvest)
Stacked$site.id <- as.factor(Stacked$site.id)
Stacked$block.id.harvest <- as.factor(Stacked$block.id.harvest)

colnames(Stacked)[colnames(Stacked)=="productivity.g.m2.day"] <- "prodsp"
colnames(Stacked)[colnames(Stacked)=="consumption.g.m2.day"] <- "conssp"
colnames(Stacked)[colnames(Stacked)=="productivity.total.g.m2.day"] <- "prodtot"
colnames(Stacked)[colnames(Stacked)=="consumption.total.g.m2.day"] <- "constot"

Datatarget <- subset(Stacked,target.sp.!="other")
Dataother <- subset(Stacked,target.sp.=="other")
