########################################################################
#### Serengeti turf trasnplant experiment ####
########################################################################
# Clean your R environment
rm(list=ls())
library(dplyr)
library(Matrix)
library(ggplot2)
library(glmmADMB)
library(glmmTMB)
library(multcomp)
library(lme4)
########################################################################
##### Aboveground plant cover ####

TP<-read.csv(file="Transplant experiment/Aboveground.survey.May2018.csv", sep=",",header=TRUE)

names(TP)
str(TP)
dim(TP) #152  22

# House keeping factors
TP$fSite<-as.factor(TP$Site)
TP$fBlock<-as.factor(TP$Block)
TP$fTagsp<-as.factor(TP$Tagsp)
TP$fLanduse<-as.factor(TP$Landuse)

#### Determine how well target species transplanted ####
# Remove plots not found with target species
TP2<-TP[!is.na(TP$FinalTagspCover),]
dim(TP2) #138  16
TP2$FinalTagspCover

# Convert to binary data - presence vs absence and probability of detection
TP2$FinalPresAb<-TP2$FinalTagspCover
TP2$FinalPresAb[TP2$FinalPresAb>0]<-1

TPprob<- glmmadmb(FinalPresAb~fTagsp+fLanduse+fTagsp:fLanduse+(1|fSite/fBlock), 
                        family="binomial",#zeroInflation = TRUE,
                        #admb.opts=admbControl(shess=FALSE,noinit=FALSE),
                        data=TP2)
summary(TPprob) # No difference in probabilty of species 
drop1(TPprob,test="Chi")
summary(glht(TPprob, mcp(fTagsp="Tukey"))) 

#Model matrix
MyData <- expand.grid(fTagsp = levels(TP2$fTagsp),
                      fLanduse = levels(TP2$fLanduse))
head(MyData)

#Convert the covariate values into an X matrix
Xp <- model.matrix(~ fTagsp+fLanduse+fTagsp:fLanduse, data = MyData)

#Extract parameters and parameter covariance matrix
betas    <- fixef(TPprob)
Covbetas <- vcov(TPprob)

#Calculate the fitted values in the predictor scale
MyData$eta <- Xp %*% betas
MyData$Pi  <- exp(MyData$eta) / (1 + exp(MyData$eta))


#Calculate the SEs on the scale of the predictor function
MyData$se    <- sqrt(diag(Xp %*% Covbetas %*% t(Xp)))
MyData$SeUp  <- exp(MyData$eta + 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  + 1.96 *MyData$se))
MyData$SeLo  <- exp(MyData$eta - 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  - 1.96 *MyData$se))

MyData
#fTagsp        eta        Pi        se      SeUp      SeLo
#1 Chl pyc  0.5108753 0.6250116 0.4216500 0.7920471 0.4217585
#2 Chr ori -0.4053894 0.4000182 0.7142230 0.7299734 0.1412112 # Cryo orien - low 
#3 Cyn dac  0.9985056 0.7307647 0.7657373 0.9240963 0.3769911 # Cynodon higher 
#4 Dig mac  0.4519652 0.6111064 0.6852178 0.8575382 0.2908908
#5 The tri  0.5306943 0.6296450 0.7177266 0.8740705 0.2939970

# No difference in probabilty of species being present after trasnplant

#### Determine species differences in cover ####
TPcover<- lmer(FinalTagspCover~fTagsp+fLanduse+fTagsp:fLanduse+(1|fSite/fBlock),REML=T,
                  data=TP2)
summary(TPcover) # No difference in cover - marginal
drop1(TPcover,test="Chi")

# Interaction plot
with(TP2, {interaction.plot(fLanduse,fTagsp,FinalTagspCover,
                                  xlab = "Landuse",
                                  ylab = "Target Cover",
                                  fun=mean)})

# Species differences
summary(glht(TPcover, mcp(fTagsp="Tukey"))) 

# No difference in probabilty of species 

#Model matrix
MyData <- expand.grid(fTagsp = levels(TP2$fTagsp))
head(MyData)

#Convert the covariate values into an X matrix
Xp <- model.matrix(~ fTagsp, data = MyData)

#Extract parameters and parameter covariance matrix
betas    <- fixef(TPcover)
Covbetas <- vcov(TPcover)

#Calculate the fitted values in the predictor scale
MyData$eta <- Xp %*% betas

#Calculate the SEs on the scale of the predictor function
MyData$se    <- sqrt(diag(Xp %*% Covbetas %*% t(Xp)))
MyData$SeUp  <- exp(MyData$eta + 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  + 1.96 *MyData$se))
MyData$SeLo  <- exp(MyData$eta - 1.96 *MyData$se) / 
  (1 + exp(MyData$eta  - 1.96 *MyData$se))

MyData

