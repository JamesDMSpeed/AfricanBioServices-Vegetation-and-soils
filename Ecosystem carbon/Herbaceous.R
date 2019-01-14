#### Sorting the herbaceous data ####
Herbaceous <- read.csv(file="Ecosystem carbon/12Herbaceous.csv", header=T)

# Remove NAs 
Herbaceous <- na.omit(Herbaceous)
Herbaceous <- droplevels(Herbaceous)
names(Herbaceous)
# Make a table for Herbaceous per region 
levels(Herbaceous$Region)
Herbaceous$Region<- factor(Herbaceous$Region, levels = c("Makao","Maswa","Mwantimba","Handajega","Seronera"))

SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
HerbC.kg_m2<-aggregate(C.kg_m2~Region, Herbaceous,sum)
HerbC.kg_m2.SE<-aggregate(C.kg_m2~Region,Herbaceous,SE)

Herb.Region <- cbind(HerbC.kg_m2,HerbC.kg_m2.SE[2])
colnames(Herb.Region) <- c("Region","HerbC.kg_m2","SE.HerbC.kg_m2")
Herb.Region$Landuse <- as.factor(c("Pasture","Wild","Pasture","Wild", "Wild"))

write.csv(Herb.Region,file="Ecosystem carbon/HerbC.Region.csv")

# Explore the data 
library(ggplot2)

HerbC.Region.plot <- ggplot(data = Herb.Region, aes(x = Region,y = HerbC_m2, ymin=HerbC_m2-SE.HerbC_m2,ymax=HerbC_m2+SE.HerbC_m2, group = Landuse, colour= Landuse))

Lines_gone <- theme(panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())

HerbC.Region.plot + xlab("Region") + ylab("Herbaceous carbon")  + geom_point(size = 3, shape=20,stroke=2)  + theme_bw() + Lines_gone + geom_errorbar(stat = "identity",width=.2,lwd=1.1,show.legend=F) +  scale_color_manual(breaks = c("Pasture", "Wild"),values=c("goldenrod3", "forestgreen"))
