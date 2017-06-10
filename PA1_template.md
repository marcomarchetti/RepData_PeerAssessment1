# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


#### 1. Load the data 

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```

#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

#### 1 Calculate the total number of steps taken per day

```r
activityStepsByDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
head(activityStepsByDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

#### 2 Make a histogram of the total number of steps taken each day

```r
hist(activityStepsByDay$steps, main = "Total number of steps taken each day", 
     breaks = 16, 
     col="steelblue", 
     xlab="Steps",
     ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### 3 Calculate and report the mean and median of the total number of steps taken per day

```r
activityStepsByDayMean <- mean(activityStepsByDay$steps)
activityStepsByDayMedian <- median(activityStepsByDay$steps)
activityStepsByDayMean
```

```
## [1] 10766.19
```

```r
activityStepsByDayMedian
```

```
## [1] 10765
```

The mean is 1.0766189\times 10^{4} and the median is 10765.


## What is the average daily activity pattern?

#### 1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activityStepsByInterval <- aggregate(steps ~ interval, activityData, mean, na.rm=TRUE)
colnames(activityStepsByInterval)[2] <- "meanSteps"

plot(activityStepsByInterval$interval,activityStepsByInterval$meanSteps,
     type = "l",
     col="steelblue",
     main = "Average daily activity activity pattern",
     xlab = "Interval",
     ylab = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### 2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
maxStepsAvg <-which.max(activityStepsByInterval$meanSteps)
maxStepsAvg
```

```
## [1] 104
```

```r
maxStepAvgInterval <- activityStepsByInterval[maxStepsAvg,'interval']
maxStepAvgInterval
```

```
## [1] 835
```

```r
maxStepAvgValue <- activityStepsByInterval[maxStepsAvg,'meanSteps']
maxStepAvgValue
```

```
## [1] 206.1698
```

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

```r
numMissingValues <- sum(is.na(activityData$steps))
numMissingValues
```

```
## [1] 2304
```

#### 2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

New data set `activityDataImputed` filled using mean for that interval

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, mean)
```


##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
activityStepsByDayImputed <- aggregate(steps ~ date, activityDataImputed, sum, na.rm=TRUE)
hist(activityStepsByDayImputed$steps,
     breaks = 16,
     main="Total steps per day (Imputed)",
     col="steelblue",
     xlab="Steps",
     ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
activityStepsByDayImputedMean <- mean(activityStepsByDayImputed$steps)
activityStepsByDayImputedMean
```

```
## [1] 10766.19
```

```r
activityStepsByDayImputedMedian<- median(activityStepsByDayImputed$steps)
activityStepsByDayImputedMedian
```

```
## [1] 10766.19
```

#### Do these values differ from the estimates from the first part of the assignment? 
#### What is the impact of imputing missing data on the estimates of the total daily 
#### number of steps?

```r
#fare tabellina
# Mean
activityStepsByDayMean 
```

```
## [1] 10766.19
```

```r
activityStepsByDayImputedMean
```

```
## [1] 10766.19
```

```r
# Median
activityStepsByDayMedian 
```

```
## [1] 10765
```

```r
activityStepsByDayImputedMedian
```

```
## [1] 10766.19
```

```r
hist(activityStepsByDayImputed$steps,
     breaks = 16,
     main="Total number of steps taken each day (NA filled with mean)",
     col="steelblue",
     xlab="Steps",
     ylab="Days")

hist(activityStepsByDay$steps, main = "Total number of steps taken each day (NA vs Filled)", 
     breaks = 16, 
     col="red", 
     xlab="Steps",
     ylab="Days", add=T)

legend("topright", c("Imputed", "Non-Imputed"), col=c("steelblue", "red"), lwd=5, cex=.6)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

#### 1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
wt <- ifelse(weekdays(activityDataImputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDataImputed$wT <- as.factor(wt)
averagedActivityDataImputed <- aggregate(steps ~ interval + wT, data=activityDataImputed, mean)
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
xyplot(steps~interval|wT,
       data=averagedActivityDataImputed,
       type="l",
       layout=c(1,2),
       xlab = "Interval",
       ylab = "Steps",
       main = "Average Steps per Day - Weekend vs Weekday"
       )
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
