# Reproducible-Research---Course-Project-2
Exploring the U.S. National Oceanic and Atmospheric Administration’s (NOAA) storm database - Health and Economic Impacts
library(ggplot2)  
library(plyr) 
library(dplyr)
if(!exists("storm.data")) {
    storm.data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"),header = TRUE)
  }
dim(storm.data)
str(storm.data)
vars <- c( "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
mydata <- storm.data[, vars]
tail(mydata)
sum(is.na(mydata$FATALITIES))
sum(is.na(mydata$INJURIES))
sum(is.na(mydata$PROPDMG))
sum(is.na(mydata$CROPDMG))
sum(is.na(mydata$PROPDMGEXP))
sum(is.na(mydata$CROPDMGEXP))
sort(table(mydata$EVTYPE), decreasing = TRUE)[1:10]
mydata$EVENT <- "OTHER"
mydata$EVENT[grep("HAIL", mydata$EVTYPE, ignore.case = TRUE)] <- "HAIL"
mydata$EVENT[grep("HEAT", mydata$EVTYPE, ignore.case = TRUE)] <- "HEAT"
mydata$EVENT[grep("FLOOD", mydata$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
mydata$EVENT[grep("WIND", mydata$EVTYPE, ignore.case = TRUE)] <- "WIND"
mydata$EVENT[grep("STORM", mydata$EVTYPE, ignore.case = TRUE)] <- "STORM"
mydata$EVENT[grep("SNOW", mydata$EVTYPE, ignore.case = TRUE)] <- "SNOW"
mydata$EVENT[grep("TORNADO", mydata$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
mydata$EVENT[grep("WINTER", mydata$EVTYPE, ignore.case = TRUE)] <- "WINTER"
mydata$EVENT[grep("RAIN", mydata$EVTYPE, ignore.case = TRUE)] <- "RAIN"
sort(table(mydata$EVENT), decreasing = TRUE)
sort(table(mydata$PROPDMGEXP), decreasing = TRUE)[1:10]
sort(table(mydata$CROPDMGEXP), decreasing = TRUE)[1:10]
mydata$PROPDMGEXP <- as.character(mydata$PROPDMGEXP)
mydata$PROPDMGEXP[is.na(mydata$PROPDMGEXP)] <- 0 # NA's considered as dollars
mydata$PROPDMGEXP[!grepl("K|M|B", mydata$PROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
mydata$PROPDMGEXP[grep("K", mydata$PROPDMGEXP, ignore.case = TRUE)] <- "3"
mydata$PROPDMGEXP[grep("M", mydata$PROPDMGEXP, ignore.case = TRUE)] <- "6"
mydata$PROPDMGEXP[grep("B", mydata$PROPDMGEXP, ignore.case = TRUE)] <- "9"
mydata$PROPDMGEXP <- as.numeric(as.character(mydata$PROPDMGEXP))
mydata$property.damage <- mydata$PROPDMG * 10^mydata$PROPDMGEXP

mydata$CROPDMGEXP <- as.character(mydata$CROPDMGEXP)
mydata$CROPDMGEXP[is.na(mydata$CROPDMGEXP)] <- 0 # NA's considered as dollars
mydata$CROPDMGEXP[!grepl("K|M|B", mydata$CROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
mydata$CROPDMGEXP[grep("K", mydata$CROPDMGEXP, ignore.case = TRUE)] <- "3"
mydata$CROPDMGEXP[grep("M", mydata$CROPDMGEXP, ignore.case = TRUE)] <- "6"
mydata$CROPDMGEXP[grep("B", mydata$CROPDMGEXP, ignore.case = TRUE)] <- "9"
mydata$CROPDMGEXP <- as.numeric(as.character(mydata$CROPDMGEXP))
mydata$crop.damage <- mydata$CROPDMG * 10^mydata$CROPDMGEXP
sort(table(mydata$property.damage), decreasing = TRUE)[1:10]
sort(table(mydata$crop.damage), decreasing = TRUE)[1:10]
agg.fatalites.and.injuries <- ddply(mydata, .(EVENT), summarize, Total = sum(FATALITIES + INJURIES,  na.rm = TRUE))
agg.fatalites.and.injuries$type <- "fatalities and injuries"
agg.fatalities <- ddply(mydata, .(EVENT), summarize, Total = sum(FATALITIES, na.rm = TRUE))
agg.fatalities$type <- "fatalities"
agg.injuries <- ddply(mydata, .(EVENT), summarize, Total = sum(INJURIES, na.rm = TRUE))
agg.injuries$type <- "injuries"
agg.health <- rbind(agg.fatalities, agg.injuries)
health.by.event <- join (agg.fatalities, agg.injuries, by="EVENT", type="inner")
health.by.event
agg.propdmg.and.cropdmg <- ddply(mydata, .(EVENT), summarize, Total = sum(property.damage + crop.damage,  na.rm = TRUE))
agg.propdmg.and.cropdmg$type <- "property and crop damage"
agg.prop <- ddply(mydata, .(EVENT), summarize, Total = sum(property.damage, na.rm = TRUE))
agg.prop$type <- "property"
agg.crop <- ddply(mydata, .(EVENT), summarize, Total = sum(crop.damage, na.rm = TRUE))
agg.crop$type <- "crop"
agg.economic <- rbind(agg.prop, agg.crop)
economic.by.event <- join (agg.prop, agg.crop, by="EVENT", type="inner")
economic.by.event
agg.health$EVENT <- as.factor(agg.health$EVENT)
health.plot <- ggplot(agg.health, aes(x = EVENT, y = Total, fill = type)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event Type") + 
  ylab("Total number of health impact") +
  ggtitle("Weather event types impact on public health") +
  theme(plot.title = element_text(hjust = 0.5))
print(health.plot)  
agg.economic$EVENT <- as.factor(agg.economic$EVENT)
economic.plot <- ggplot(agg.economic, aes(x = EVENT, y = Total, fill = type)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event Type") + 
  ylab("Total damage in dollars") +
  ggtitle("Weather event types impact on property and crop damage") +
  theme(plot.title = element_text(hjust = 0.5))
print(economic.plot) 
