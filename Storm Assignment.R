library(knitr)
library(ggplot2)
library(lattice)
library(reshape2)

StormData <- read.csv("/Users/sp/Downloads/rStorm/repdata_data_StormData.csv", header = FALSE, sep=",", stringsAsFactors=FALSE) 
head(StormData)
names(StormData)

#Fix dataframe
names(StormData) <- StormData[1,]
StormData <- StormData[-1,]

NewStormData <- StormData[c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
head(NewStormData)
     
MostFatal <- NewStormData[order(NewStormData$FATALITIES, decreasing = TRUE)[1:100],]
MostInjuries <- NewStormData[order(NewStormData$INJURIES, decreasing = TRUE)[1:100],]
head(MostFatal)

head(MostInjuries)

#Event Type: Fatalities
f <- ggplot(MostFatal, aes(EVTYPE, FATALITIES)) 
f + geom_bar(stat = "identity", fill = "red") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(x = "Event Type", y = "Fatalities") + labs(title = "The Top 100 Most Fatal Events")

#Event Type: Injuries
f <- ggplot(MostInjuries, aes(EVTYPE, INJURIES)) 
f + geom_bar(stat = "identity", fill = "lightblue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(x = "Event Type", y = "Injuries") + labs(title = "The Top 100 Most Injuries by Event")



#Property Damage
#H: Hundred
#K: Thousand
#M: Million
#B: Billion Crop Damage
NewStormData$crop_total = 0
NewStormData[NewStormData$CROPDMGEXP == "H",]$crop_total = NewStormData[NewStormData$CROPDMGEXP == "H",]$CROPDMG * 100
NewStormData[NewStormData$CROPDMGEXP == "K",]$crop_total = NewStormData[NewStormData$CROPDMGEXP == "K",]$CROPDMG * 1000
NewStormData[NewStormData$CROPDMGEXP == "M",]$crop_total = NewStormData[NewStormData$CROPDMGEXP == "M",]$CROPDMG * 1000000
NewStormData[NewStormData$CROPDMGEXP == "B",]$crop_total = NewStormData[NewStormData$CROPDMGEXP == "B",]$CROPDMG * 1000000000

NewStormData$prop_total = 0
NewStormData[NewStormData$PROPDMGEXP == "H",]$prop_total = NewStormData[NewStormData$PROPDMGEXP == "H",]$PROPDMG * 100
NewStormData[NewStormData$PROPDMGEXP == "K",]$prop_total = NewStormData[NewStormData$PROPDMGEXP == "K",]$PROPDMG * 1000
NewStormData[NewStormData$PROPDMGEXP == "M",]$prop_total = NewStormData[NewStormData$PROPDMGEXP == "M",]$PROPDMG * 1000000
NewStormData[NewStormData$PROPDMGEXP == "B",]$prop_total = NewStormData[NewStormData$PROPDMGEXP == "B",]$PROPDMG * 1000000000

#Plot showing crop and property damages by event type
crop <- NewStormData[order(NewStormData$crop_total, decreasing = TRUE)[1:100],]
prop <- NewStormData[order(NewStormData$prop_total, decreasing = TRUE)[1:100],]
crop_prop <- merge(crop, prop)
crop_prop <- crop_prop[c("EVTYPE", "crop_total", "prop_total")]

crop_prop.long<-melt(crop_prop)

ggplot(crop_prop.long,aes(EVTYPE ,(value/10^7),fill=variable))+
  geom_bar(stat="identity",position="dodge") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  labs(x = "Event Type", y = "Cost of Damages(in Billions)") + 
  labs(title = "The Cost of Property and Crop Damages Per Event")





