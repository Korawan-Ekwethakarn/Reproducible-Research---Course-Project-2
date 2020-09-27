Synopsis
The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

Data Processing
Download the raw data: Storm Data

setwd("d:/r/RStudio/coursera/5.Reproducible_research/ass2/")
if (!file.exists("./StormData.csv.bz2")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="StormData.csv.bz2")
}

if (!file.exists("./StormData.csv")) {
    zz <- read.csv(bzfile("StormData.csv.bz2"), as.is=TRUE)
    write.csv(zz, "./StormData.csv")
} else {
    zz <- read.csv("StormData.csv", as.is=TRUE)
}
## Warning in scan(file, what, nmax, sep, dec, quote, skip, nlines,
## na.strings, : EOF within quoted string
We can briefly check the data:

dim(zz)
## [1] 902298     38
head(zz)
##   X STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1 1    1.00  4/18/1950 0:00:00     0130       CST  97.00     MOBILE    AL
## 2 2    1.00  4/18/1950 0:00:00     0145       CST   3.00    BALDWIN    AL
## 3 3    1.00  2/20/1951 0:00:00     1600       CST  57.00    FAYETTE    AL
## 4 4    1.00   6/8/1951 0:00:00     0900       CST  89.00    MADISON    AL
## 5 5    1.00 11/15/1951 0:00:00     1500       CST  43.00    CULLMAN    AL
## 6 6    1.00 11/15/1951 0:00:00     2000       CST  77.00 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO      0.00                                               0
## 2 TORNADO      0.00                                               0
## 3 TORNADO      0.00                                               0
## 4 TORNADO      0.00                                               0
## 5 TORNADO      0.00                                               0
## 6 TORNADO      0.00                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA      0.00                      14.0   100 3   0          0
## 2         NA      0.00                       2.0   150 2   0          0
## 3         NA      0.00                       0.1   123 2   0          0
## 4         NA      0.00                       0.0   100 2   0          0
## 5         NA      0.00                       0.0   150 2   0          0
## 6         NA      0.00                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15   25.00          K    0.00                                    
## 2        0    2.50          K    0.00                                    
## 3        2   25.00          K    0.00                                    
## 4        2    2.50          K    0.00                                    
## 5        2    2.50          K    0.00                                    
## 6        6    2.50          K    0.00                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051    8806.00              1
## 2     3042      8755          0       0.00              2
## 3     3340      8742          0       0.00              3
## 4     3458      8626          0       0.00              4
## 5     3412      8642          0       0.00              5
## 6     3450      8748          0       0.00              6
summary(zz)
##       X               STATE__            BGN_DATE        
##  Length:902298      Length:902298      Length:902298     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    BGN_TIME          TIME_ZONE            COUNTY         
##  Length:902298      Length:902298      Length:902298     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##   COUNTYNAME           STATE              EVTYPE         
##  Length:902298      Length:902298      Length:902298     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##   BGN_RANGE           BGN_AZI           BGN_LOCATI       
##  Length:902298      Length:902298      Length:902298     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    END_DATE           END_TIME           COUNTY_END       COUNTYENDN    
##  Length:902298      Length:902298      Min.   :0        Min.   :  0     
##  Class :character   Class :character   1st Qu.:0        1st Qu.:  0     
##  Mode  :character   Mode  :character   Median :0        Median :  0     
##                                        Mean   :0        Mean   :  1     
##                                        3rd Qu.:0        3rd Qu.:  0     
##                                        Max.   :0        Max.   :176     
##                                        NA's   :354935   NA's   :547364  
##   END_RANGE           END_AZI           END_LOCATI       
##  Length:902298      Length:902298      Length:902298     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##      LENGTH             WIDTH              F               MAG          
##  Min.   :   0.000   Min.   :   0.0   Min.   :  0.0    Min.   :    0.00  
##  1st Qu.:   0.000   1st Qu.:   0.0   1st Qu.:  0.0    1st Qu.:    0.00  
##  Median :   0.000   Median :   0.0   Median : 34.0    Median :    0.00  
##  Mean   :   2.197   Mean   :   8.9   Mean   : 35.7    Mean   :   31.02  
##  3rd Qu.:   0.000   3rd Qu.:   0.0   3rd Qu.: 56.0    3rd Qu.:   55.00  
##  Max.   :3872.000   Max.   :4400.0   Max.   :750.0    Max.   :22000.00  
##  NA's   :1          NA's   :343572   NA's   :499993   NA's   :1         
##    FATALITIES          INJURIES          PROPDMG         
##  Min.   :0.00e+00   Min.   :   0.000   Length:902298     
##  1st Qu.:0.00e+00   1st Qu.:   0.000   Class :character  
##  Median :0.00e+00   Median :   0.000   Mode  :character  
##  Mean   :3.66e-02   Mean   :   4.754                     
##  3rd Qu.:0.00e+00   3rd Qu.:   0.000                     
##  Max.   :1.15e+03   Max.   :5000.000                     
##  NA's   :1          NA's   :1                            
##   PROPDMGEXP          CROPDMG           CROPDMGEXP       
##  Length:902298      Length:902298      Length:902298     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##      WFO             STATEOFFIC         ZONENAMES            LATITUDE     
##  Length:902298      Length:902298      Length:902298      Min.   :-14451  
##  Class :character   Class :character   Class :character   1st Qu.:  2929  
##  Mode  :character   Mode  :character   Mode  :character   Median :  3905  
##                                                           Mean   :  4541  
##                                                           3rd Qu.:  8143  
##                                                           Max.   : 17055  
##                                                           NA's   :1       
##    LONGITUDE       LATITUDE_E      LONGITUDE_          REMARKS         
##  Min.   :    0   Min.   :-14455   Length:902298      Length:902298     
##  1st Qu.:    0   1st Qu.:     0   Class :character   Class :character  
##  Median : 3908   Median :     0   Mode  :character   Mode  :character  
##  Mean   : 4586   Mean   :  2180                                        
##  3rd Qu.: 9100   3rd Qu.:  3831                                        
##  Max.   :17124   Max.   : 17052                                        
##  NA's   :41      NA's   :1                                             
##      REFNUM      
##  Min.   :     1  
##  1st Qu.:136842  
##  Median :273682  
##  Mean   :273682  
##  3rd Qu.:410523  
##  Max.   :548030  
##  NA's   :354935
Now for later analysis, we make several columns the correct data type:

zz$EVTYPE <- as.factor(zz$EVTYPE)
zz$FATALITIES <- as.numeric(zz$FATALITIES)
zz$INJURIES <- as.numeric(zz$INJURIES)
Results
Through below analysis, we found “TORNADO” is the most harmful event to population health, also it brings the greatest economic damage.

Find out which type of events are most harmful to population health? Here we evaluate the damage on FATALITIES and INJUREIS.
We only keep EVTYPE, FATALITIES and INJURIES column. Then remove any rows whose EVTYPE is actually digits (unknow type),

## calculate the fatalities for each env type
zz1 <- zz
zz1 <- zz1[which(!is.na(zz1$FATALITIES)),]
zz1 <- zz1[which(!is.na(zz1$INJURIES)),]
zz2 <- zz1[,c(9,24,25)]
zz3 <- zz2[(grep("^[a-zA-Z]",zz2$EVTYPE)),]
fatalities <- tapply(zz3$FATALITIES, zz3$EVTYPE, sum)
injuries <- tapply(zz3$INJURIES, zz3$EVTYPE, sum)
result <- as.data.frame(cbind(fatalities, injuries))
result <- result[order(fatalities, injuries,decreasing=TRUE), ]
result <- result[which(!is.na(result$fatalities)),]
head(result)
##                fatalities injuries
## TORNADO              4658    80084
## EXCESSIVE HEAT       1416     4354
## HEAT                  708      878
## LIGHTNING             562     3628
## FLASH FLOOD           559     1407
## TSTM WIND             471     6452
We can tell the worst event type (brings more fatalities/injuries) are “TORNADO”, then “EXCESSIVE HEAT”.

Remove unnecessary objects:

rm(zz1)
rm(zz2)
rm(zz3)
rm(fatalities)
rm(injuries)
rm(result)
Find out which event type brings greatest economic damage?
Here we try to list the events which has most damage:

library(dplyr)
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
zz1 <- zz[,c(9,26,27,28,29)]
zz2 <- zz1[(grep("^[a-zA-Z]",zz1$EVTYPE)),]
zz2$PROPDMG <- as.numeric(zz2$PROPDMG)
zz2$CROPDMG <- as.numeric(zz2$CROPDMG)
zz2$PROP_DMG_EXP <- zz2$PROPDMG
zz2$CROP_DMG_EXP <- zz2$CROPDMG
zz2[which(zz2$PROPDMGEXP=="K"),]$PROP_DMG_EXP <- 1000
zz2[which(zz2$PROPDMGEXP==""),]$PROP_DMG_EXP <- 1
zz2[which(zz2$PROPDMGEXP=="M"),]$PROP_DMG_EXP <- 1000000
zz2[which(zz2$CROPDMGEXP=="K"),]$CROP_DMG_EXP <- 1000
zz2[which(zz2$CROPDMGEXP==""),]$CROP_DMG_EXP <- 1
zz2[which(zz2$CROPDMGEXP=="M"),]$CROP_DMG_EXP <- 1000000

zz2$PDMG <- zz2$PROPDMG * zz2$PROP_DMG_EXP
zz2$CDMG <- zz2$CROPDMG * zz2$CROP_DMG_EXP
zz2 <- zz2[,c(1,8,9)]

pdmg <- tapply(zz2$PDMG, zz2$EVTYPE, sum)
cdmg <- tapply(zz2$CDMG, zz2$EVTYPE, sum)
result <- as.data.frame(cbind(pdmg, cdmg))
result$DAMAGE <- result$pdmg+result$cdmg


result <- result[which(!is.na(result$DAMAGE)),]
result <- result[order(result$DAMAGE, decreasing=TRUE),]
result <- result[1:10,]
result$EVTYPE <- rownames(result)
result <- result[,c(4,3)]
head(result)
##                  EVTYPE      DAMAGE
## TORNADO         TORNADO 41182246392
## FLOOD             FLOOD 11672056036
## DROUGHT         DROUGHT 10205543000
## HAIL               HAIL  9563068146
## FLASH FLOOD FLASH FLOOD  7743765708
## HURRICANE     HURRICANE  7262119022
It is also obvious to tell from plot:

library(ggplot2)
qplot(EVTYPE,DAMAGE, data=result,geom=c("point"),xlab="Event Type", ylab="Damage")


The event which has the greatest economic damage is “TORNADO”, then “FLOOD”.
