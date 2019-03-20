#Reading in ; seperated csvs

TBIdata<-read.csv('Termites/Final data TBI.csv', sep=';',dec=',')
View(TBIdata)
head(TBIdata)
summary(TBIdata)

#Write as comma seperated csv (not neccessary but anyway...)
write.csv(TBIdata,'Termites/FinaldataTBI_comma.csv')
