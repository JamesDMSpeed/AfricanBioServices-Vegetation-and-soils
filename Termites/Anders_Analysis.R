rm(list=ls())
library(lme4)
library(glmmTMB)

wsdata<- read.csv('Termites/Main & CG experiment/Wetseason.csv', sep=';',dec='.')#Wetseason data
dsdata <- read.csv("Termites/Main & CG experiment/Dryseason.csv", sep=";",dec=".")#Dryseason data
head(wsdata)
head(dsdata)
fulldata<-rbind(wsdata,dsdata)
fulldata$Massloss.per <- (1-fulldata$Ashed.final.corrected.weight..tea.only..g./fulldata$Ashed.initial.corrected.weight..tea.only..g.)*100
names(fulldata)
fulldata <- fulldata[-c(27,28,29)]