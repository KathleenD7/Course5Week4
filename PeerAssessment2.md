

```r
title: "Events That Had a Negative Impact"
```

```
## Warning: NAs introduced by coercion
```

```
## Error in title:"Events That Had a Negative Impact": NA/NaN argument
```

```r
output: 
  md_document
```

```
## Error in eval(expr, envir, enclos): object 'output' not found
```

```r
html_document:
    self_contained: no
```

```
## Error in eval(expr, envir, enclos): object 'html_document' not found
```

#Title: Events That Had a Negative Impact

Synopsis: I took the top 6 events for each category (harmfulto the population and economically harmful) and made 2 bar plots summarizing the top 6 harmful events. I came across these results by cleaning the data. I chose to clean the data not to the full extent, I felt this made the data harder to reproduce (and the whole point of the course is making your data easy to reproduce) so I chose to only combine events that could be easily combined. I also chose fatalities as the most harmful followed by injuries so if there was a tie in number of fatalities, injuries was the tiebreaker. I chose property damage as the most harmful economically because I live near a city and no one has crops here and felt they could be more easily replaced cost wise and the tiebreaker was crop damage.

#Data Processing

In this section, I read in the data by using read.csv I then filtered each file down for each question as described above.


```r
StormData <- read.csv("~/Documents/Course5Week4/repdata%2Fdata%2FStormData.csv")
##Question 1
ModStormData<-StormData[order(StormData$FATALITIES),c(8,23,24)]
AggFatalStorm<-aggregate(FATALITIES~EVTYPE,ModStormData,sum)
##Cleaning the Data
FatalRankStorm<-AggFatalStorm[order(AggFatalStorm$FATALITIES),c(1,2)]
FatalRankStorm[,1]<-(toupper(FatalRankStorm$EVTYPE))
FatalRankStorm<-FatalRankStorm[order(FatalRankStorm$FATALITIES),c(1,2)]
FatalRankStorm[FatalRankStorm=="AVALANCE"]="AVALANCHE"
FatalRankStorm[FatalRankStorm=="BEACH EROSIN"]="BEACH EROSION"
FatalRankStorm[FatalRankStorm=="BLOW-OUT TIDE"]="BLOW-OUT TIDES"
FatalRankStorm[FatalRankStorm=="BLOWING SNOW-EXTREME WIND CHI"]="BLOWING SNOW & EXTREME WIND CH"
FatalRankStorm[FatalRankStorm=="BLOWING SNOW/EXTREME WIND CHIL"]="BLOWING SNOW & EXTREME WIND CH"
FatalRankStorm[FatalRankStorm=="BRUSH FIRE"]="BRUSH FIRES"
FatalRankStorm<-aggregate(FATALITIES~EVTYPE,FatalRankStorm,sum)
##Note: I stopped cleaning the data here because going through each one manually makes it not reproducible easily. 
##Therefore, I am aware the data is not completely clean, but have chosen to leave it this way. This is one of the limitations of this analysis
AggInjStorm<-aggregate(INJURIES~EVTYPE,ModStormData,sum)
InjRankStorm<-AggInjStorm[order(AggInjStorm$INJURIES),c(1,2)]
InjRankStorm[,1]<-(toupper(InjRankStorm$EVTYPE))
InjRankStorm[InjRankStorm=="AVALANCE"]="AVALANCHE"
InjRankStorm[InjRankStorm=="BEACH EROSIN"]="BEACH EROSION"
InjRankStorm[InjRankStorm=="BLOW-OUT TIDE"]="BLOW-OUT TIDES"
InjRankStorm[InjRankStorm=="BLOWING SNOW-EXTREME WIND CHI"]="BLOWING SNOW & EXTREME WIND CH"
InjRankStorm[InjRankStorm=="BLOWING SNOW/EXTREME WIND CHIL"]="BLOWING SNOW & EXTREME WIND CH"
InjRankStorm[InjRankStorm=="BRUSH FIRE"]="BRUSH FIRES"
InjRankStorm<-aggregate(INJURIES~EVTYPE,InjRankStorm,sum)
InjRankStorm<-InjRankStorm[order(InjRankStorm$INJURIES),c(1,2)]
mergedStorm<-merge(FatalRankStorm,InjRankStorm, by="EVTYPE")
RankedStorm<-mergedStorm[order(mergedStorm$FATALITIES,mergedStorm$INJURIES,decreasing=TRUE),c(1,2,3)]
top6harmful<-RankedStorm[1:6,]
top6harmful[top6harmful=="TSTM WIND"]="WIND"
top6harmful[top6harmful=="EXCESSIVE HEAT"]="EXTRA HEAT"
##Question 2
library(dplyr)
StormDatacut<-select(StormData, "EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
StormDatacut[,1]<-(toupper(StormDatacut$EVTYPE))
StormDatacut[StormDatacut=="AVALANCE"]="AVALANCHE"
StormDatacut[StormDatacut=="BEACH EROSIN"]="BEACH EROSION"
StormDatacut[StormDatacut=="BLOW-OUT TIDE"]="BLOW-OUT TIDES"
StormDatacut[StormDatacut=="BLOWING SNOW-EXTREME WIND CHI"]="BLOWING SNOW & EXTREME WIND CH"
StormDatacut[StormDatacut=="BLOWING SNOW/EXTREME WIND CHIL"]="BLOWING SNOW & EXTREME WIND CH"
StormDatacut[StormDatacut=="BRUSH FIRE"]="BRUSH FIRES"
StormDatacut[,3]<-(toupper(StormDatacut$PROPDMGEXP))
StormDatacut[,5]<-(toupper(StormDatacut$CROPDMGEXP))
##I made H be hundreds, K be thousands, M be millions, b be billions, and 0-9 be 10^of whatever that number
##For example, if it said 9, it would be 10^9
StormDatacut[StormDatacut$PROPDMGEXP=="H",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="H",]$PROPDMG*10^2
StormDatacut[StormDatacut$PROPDMGEXP=="K",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="K",]$PROPDMG*10^3
StormDatacut[StormDatacut$PROPDMGEXP=="M",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="M",]$PROPDMG*10^6
StormDatacut[StormDatacut$PROPDMGEXP=="B",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="B",]$PROPDMG*10^9
StormDatacut[StormDatacut$PROPDMGEXP=="0",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="0",]$PROPDMG*1
StormDatacut[StormDatacut$PROPDMGEXP==" ",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP==" ",]$PROPDMG*1
StormDatacut[StormDatacut$PROPDMGEXP=="1",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="1",]$PROPDMG*10^1
StormDatacut[StormDatacut$PROPDMGEXP=="2",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="2",]$PROPDMG*10^2
StormDatacut[StormDatacut$PROPDMGEXP=="3",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="3",]$PROPDMG*10^3
StormDatacut[StormDatacut$PROPDMGEXP=="4",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="4",]$PROPDMG*10^4
StormDatacut[StormDatacut$PROPDMGEXP=="5",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="5",]$PROPDMG*10^5
StormDatacut[StormDatacut$PROPDMGEXP=="6",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="6",]$PROPDMG*10^6
StormDatacut[StormDatacut$PROPDMGEXP=="7",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="7",]$PROPDMG*10^7
StormDatacut[StormDatacut$PROPDMGEXP=="8",]$PROPDMG=StormDatacut[StormDatacut$PROPDMGEXP=="8",]$PROPDMG*10^8
StormDatacut[StormDatacut$CROPDMGEXP=="K",]$CROPDMG=StormDatacut[StormDatacut$CROPDMGEXP=="K",]$CROPDMG*10^3
StormDatacut[StormDatacut$CROPDMGEXP=="M",]$CROPDMG=StormDatacut[StormDatacut$CROPDMGEXP=="M",]$CROPDMG*10^6
StormDatacut[StormDatacut$CROPDMGEXP=="B",]$CROPDMG=StormDatacut[StormDatacut$CROPDMGEXP=="B",]$CROPDMG*10^9
StormDatacut[StormDatacut$CROPDMGEXP=="0",]$CROPDMG=StormDatacut[StormDatacut$CROPDMGEXP=="0",]$CROPDMG*1
StormDatacut[StormDatacut$CROPDMGEXP==" ",]$CROPDMG=StormDatacut[StormDatacut$CROPDMGEXP==" ",]$CROPDMG*1
StormDatacut[StormDatacut$CROPDMGEXP=="2",]$CROPDMG=StormDatacut[StormDatacut$CROPDMGEXP=="2",]$CROPDMG*10^2
StormDatacut<-StormDatacut[order(StormDatacut$PROPDMG),c(1,2,3,4,5)]
PropRankStorm<-aggregate(PROPDMG~EVTYPE,StormDatacut,sum)
CropRankStorm<-aggregate(CROPDMG~EVTYPE,StormDatacut,sum)
StormDataRanked<-merge(PropRankStorm,CropRankStorm, by="EVTYPE")
RankedStorm2<-StormDataRanked[order(StormDataRanked$PROPDMG,StormDataRanked$CROPDMG,decreasing=TRUE),c(1,2,3)]
top6economic<-RankedStorm2[1:6,]
top6economic[top6economic=="HURRICANE/TYPHOON"]="TYPHOON"
top6economic[top6economic=="STORM SURGE"]="STORM"
```
#Results


```r
##Question 1
library(ggplot2)
p <-ggplot(top6harmful, aes(EVTYPE, FATALITIES))
p +geom_bar(stat = "identity")+labs(title="Top 6 Events the Most Harmful to Population Health", x="Event Type", y="Fatalities")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


```r
##Question 2
p <-ggplot(top6economic, aes(EVTYPE, PROPDMG))
p +geom_bar(stat = "identity")+labs(title="Top 6 Events with Economic Consequences", x="Event Type", y="Property Damage")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

#Analysis

Therefore, the most harmful event to population health is tornados followed by Excessive Heat. The most economically harmful event is floods followed by typhoons
```

