# Reproducible Research: Peer Assessment 1
## Reproducible Research Peer Assessment - 1


## Loading and preprocessing the data


```r
## Unzipping the activity Data file and then reading the .csv file into a Data Frame


zipFileName <- "./repdata_data_activity.zip"
unzip(zipFileName)
activityDF <- read.csv("./activity.csv")

# converting the date variable into date format 

activityDF$date <-as.Date(activityDF$date)

# summary(activityDF)
# str(activityDF)

# Check the number of rows in the activity data file

nrow(activityDF)
```

```
## [1] 17568
```

```r
head(activityDF)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?


```r
# For this step we need to ignore NA cases

activityCompCases <- activityDF[complete.cases(activityDF),]

print ("Original Rows:")
```

```
## [1] "Original Rows:"
```

```r
print (nrow(activityDF))
```

```
## [1] 17568
```

```r
print ("After removing NAs:")
```

```
## [1] "After removing NAs:"
```

```r
print(nrow(activityCompCases))
```

```
## [1] 15264
```

```r
dateWiseStepsSum <- aggregate(activityCompCases$steps, list(activityCompCases$date), FUN = sum)
colnames(dateWiseStepsSum) <- c("Date", "TotSteps")

print("Date Wise Total Steps:")
```

```
## [1] "Date Wise Total Steps:"
```

```r
print(dateWiseStepsSum)
```

```
##          Date TotSteps
## 1  2012-10-02      126
## 2  2012-10-03    11352
## 3  2012-10-04    12116
## 4  2012-10-05    13294
## 5  2012-10-06    15420
## 6  2012-10-07    11015
## 7  2012-10-09    12811
## 8  2012-10-10     9900
## 9  2012-10-11    10304
## 10 2012-10-12    17382
## 11 2012-10-13    12426
## 12 2012-10-14    15098
## 13 2012-10-15    10139
## 14 2012-10-16    15084
## 15 2012-10-17    13452
## 16 2012-10-18    10056
## 17 2012-10-19    11829
## 18 2012-10-20    10395
## 19 2012-10-21     8821
## 20 2012-10-22    13460
## 21 2012-10-23     8918
## 22 2012-10-24     8355
## 23 2012-10-25     2492
## 24 2012-10-26     6778
## 25 2012-10-27    10119
## 26 2012-10-28    11458
## 27 2012-10-29     5018
## 28 2012-10-30     9819
## 29 2012-10-31    15414
## 30 2012-11-02    10600
## 31 2012-11-03    10571
## 32 2012-11-05    10439
## 33 2012-11-06     8334
## 34 2012-11-07    12883
## 35 2012-11-08     3219
## 36 2012-11-11    12608
## 37 2012-11-12    10765
## 38 2012-11-13     7336
## 39 2012-11-15       41
## 40 2012-11-16     5441
## 41 2012-11-17    14339
## 42 2012-11-18    15110
## 43 2012-11-19     8841
## 44 2012-11-20     4472
## 45 2012-11-21    12787
## 46 2012-11-22    20427
## 47 2012-11-23    21194
## 48 2012-11-24    14478
## 49 2012-11-25    11834
## 50 2012-11-26    11162
## 51 2012-11-27    13646
## 52 2012-11-28    10183
## 53 2012-11-29     7047
```

```r
## Plot of Total Number of Steps Taken Every Day

library(ggplot2)
stepsPlot <- ggplot(activityCompCases, aes(x = as.factor(date), y= steps)) 

stepsPlot + geom_bar (stat = "identity") + theme(axis.text.x = element_text(angle=-90)) + xlab ("Date") + ylab ("Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
## The histogram of total steps taken each day 

xMax <- max(dateWiseStepsSum$TotSteps) + 7000


hist(dateWiseStepsSum$TotSteps, breaks = 15, xlim = c(0, xMax), xlab = "Days::Total Steps", main = "Histogram of Days-Total Steps", col = "light blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png) 

```r
## The mean and median of the total number of steps taken every day

#dateWiseStepsMean <- aggregate(activityCompCases$steps, list(activityCompCases$date), FUN=mean)
#print("Mean of Steps every day:")
#print(dateWiseStepsMean)

#dateWiseStepsMedian <- aggregate(activityCompCases$steps, list(activityCompCases$date), FUN=median)
#print("Median of Steps every day:")
#print(dateWiseStepsMedian)


print("Mean of the total number of steps taken per day:")
```

```
## [1] "Mean of the total number of steps taken per day:"
```

```r
print(mean(dateWiseStepsSum$TotSteps))
```

```
## [1] 10766.19
```

```r
print("Median of the total number of steps taken per day:")
```

```
## [1] "Median of the total number of steps taken per day:"
```

```r
print(median(dateWiseStepsSum$TotSteps))
```

```
## [1] 10765
```

## What is the average daily activity pattern?
 

```r
## Interval Wise Mean of Steps across all days

IntervalWiseStepsMean <- aggregate(activityCompCases$steps, list(activityCompCases$interval), FUN=mean)

colnames(IntervalWiseStepsMean) <- c("Interval", "AvgSteps")

ggplot(data = IntervalWiseStepsMean, aes(x=Interval, y=AvgSteps)) + geom_line() + geom_point() + xlab ("Interval") + ylab ("Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
## The 5-minute interval, that contains the maximum number of steps

print("The 5-minute interval, that contains the maximum number of steps is:")
```

```
## [1] "The 5-minute interval, that contains the maximum number of steps is:"
```

```r
maximumIndex <- which.max(IntervalWiseStepsMean$AvgSteps)
print(IntervalWiseStepsMean[maximumIndex,"Interval"])
```

```
## [1] 835
```

## Imputing missing values


```r
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

print ("Number of missing values:")
```

```
## [1] "Number of missing values:"
```

```r
print(length(which(is.na(activityDF$steps))))
```

```
## [1] 2304
```

```r
## Lets use Average for a time interval to populate missing values in the original data

NewActivityDF <- activityDF

for (i in 1:nrow(NewActivityDF)) {
  if (is.na(NewActivityDF[i,"steps"])) {
    timeInt <- NewActivityDF[i, "interval"]
    AvgStepsOfInt <- IntervalWiseStepsMean[IntervalWiseStepsMean$Interval==timeInt, "AvgSteps"]
    NewActivityDF[[i,"steps"]] <- AvgStepsOfInt
  }
}

NewdateWiseStepsSum <- aggregate(NewActivityDF$steps, list(NewActivityDF$date), FUN = sum)
colnames(NewdateWiseStepsSum) <- c("Date", "TotSteps")

xMax <- max(NewdateWiseStepsSum$TotSteps) + 7000


hist(NewdateWiseStepsSum$TotSteps, breaks = 15, xlim = c(0, xMax), xlab = "Days::Total Steps", main = "New Histogram of Days-Total Steps", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
print("New Mean of the total number of steps taken per day:")
```

```
## [1] "New Mean of the total number of steps taken per day:"
```

```r
print(mean(NewdateWiseStepsSum$TotSteps))
```

```
## [1] 10766.19
```

```r
print("New Median of the total number of steps taken per day:")
```

```
## [1] "New Median of the total number of steps taken per day:"
```

```r
print(median(NewdateWiseStepsSum$TotSteps))
```

```
## [1] 10766.19
```

```r
## Marginal increase in the mean, median values
## Frequency counts for total steps increased in the histogram
```

### Marginal increase in the mean, median values
### Frequency counts for total steps increased in the histogram

## Are there differences in activity patterns between weekdays and weekends?


```r
WeekType <- data.frame()

for (i in 1:nrow(NewActivityDF)) {
  if ((weekdays(NewActivityDF [i, "date"]) == "Sunday") || (weekdays(NewActivityDF [i, "date"]) == "Saturday")) {
    WeekType[i,"WD"] <- "Weekend"
  } else {
    WeekType[i,"WD"] <- "WeekDay"
  }
}

cData <- cbind(NewActivityDF, WeekType)

sData <- aggregate(cData$steps, by = list(cData$WD, cData$interval), FUN = sum)

colnames(sData) <- c("WD", "Interval", "TotSteps")

ggplot(data=sData, aes(x=Interval, y=TotSteps)) + 
    geom_line() + xlab("Interval") +
    ylab("Average number of steps") + facet_grid(WD ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

### See higher counts during weekdays compared to weekends
