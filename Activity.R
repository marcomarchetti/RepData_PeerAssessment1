##-----------------------------------
## Marco Marchetti
## Reproducible Research: Peer Assessment 1
## Activity.R
## 10 June 2017
##-----------------------------------

##-----------------------------------
## Loading and preprocessing the data
##-----------------------------------

library(Hmisc)

Sys.setlocale(category = "LC_ALL", locale = "english")

# 1. load Data
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
activityData <- read.csv('activity.csv')

# 2. Process/transform the data (if necessary) into a format suitable for your analysis
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")


##---------------------------------------------------
## What is mean total number of steps taken per day?
##---------------------------------------------------
#For this part of the assignment, you can ignore the missing values in the dataset.

# 1 Calculate the total number of steps taken per day
activityStepsByDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
head(activityStepsByDay)

#2 Make a histogram of the total number of steps taken each day
hist(activityStepsByDay$steps, main = "Total number of steps taken each day", 
     breaks = 16, 
     col="steelblue", 
     xlab="Steps",
     ylab="Frequency")

#3 Calculate and report the mean and median of the total number of steps taken per day
activityStepsByDayMean <- mean(activityStepsByDay$steps)
activityStepsByDayMedian <- median(activityStepsByDay$steps)
activityStepsByDayMean
activityStepsByDayMedian

##---------------------------------------------
## What is the average daily activity pattern?
##---------------------------------------------

# 1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

activityStepsByInterval <- aggregate(steps ~ interval, activityData, mean, na.rm=TRUE)
colnames(activityStepsByInterval)[2] <- "meanSteps"
#temp <- sprintf("%04d", activityStepsByInterval$interval)
#activityStepsByInterval$intervalHM <- strptime(temp, format="%H%M")

plot(activityStepsByInterval$interval,activityStepsByInterval$meanSteps,
     type = "l",
     col="steelblue",
     main = "Average daily activity pattern",
     xlab = "5-minute interval",
     ylab = "Avg. Steps")


#2 Which 5-minute interval, on average across all the days in the dataset, 
#  contains the maximum number of steps
maxStepsAvg <-which.max(activityStepsByInterval$meanSteps)
maxStepsAvg

maxStepAvgInterval <- activityStepsByInterval[maxStepsAvg,'interval']
maxStepAvgInterval

maxStepAvgValue <- activityStepsByInterval[maxStepsAvg,'meanSteps']
maxStepAvgValue


##-----------------------------------
## Imputing missing values
##-----------------------------------

# 1. Calculate and report the total number of missing values in the dataset
numMissingValues <- sum(is.na(activityData$steps))
numMissingValues


# 2 Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, mean)


# 4 Make a histogram of the total number of steps taken each day 
# and Calculate and report the mean and median total number of steps taken per day. 

activityStepsByDayImputed <- aggregate(steps ~ date, activityDataImputed, sum, na.rm=TRUE)
hist(activityStepsByDayImputed$steps,
     breaks = 16,
     main="Total steps per day (Imputed)",
     col="steelblue",
     xlab="Steps",
     ylab="Frequency")


activityStepsByDayImputedMean <- mean(activityStepsByDayImputed$steps)
activityStepsByDayImputedMean
activityStepsByDayImputedMedian<- median(activityStepsByDayImputed$steps)
activityStepsByDayImputedMedian

# 4 Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

#fare tabellina
# Mean
activityStepsByDayMean 
activityStepsByDayImputedMean

# Median
activityStepsByDayMedian 
activityStepsByDayImputedMedian


hist(activityStepsByDayImputed$steps,
     breaks = 16,
     main="Total number of steps taken each day (NA filled with mean)",
     col="steelblue",
     xlab="Steps",
     ylab="Frequency")

hist(activityStepsByDay$steps, main = "Total number of steps taken each day (NA vs Filled)", 
     breaks = 16, 
     col="red", 
     xlab="Steps",
     ylab="Frequency", add=T)

legend("topright", c("Imputed", "Non-Imputed"), col=c("steelblue", "red"), lwd=5, cex=.6)

##-----------------------------------
## Are there differences in activity patterns between weekdays and weekends?
##-----------------------------------
#For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# 1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.

wt <- ifelse(weekdays(activityDataImputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDataImputed$wT <- as.factor(wt)


# 2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all weekday days or 
# weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot should 
# look like using simulated data.

averagedActivityDataImputed <- aggregate(steps ~ interval + wT, data=activityDataImputed, mean)

xyplot(steps~interval|wT,
       data=averagedActivityDataImputed,
       type="l",
       layout=c(1,2),
       xlab = "5-minute interval",
       ylab = "Steps",
       main = "Average Steps per Day - Weekend vs Weekday (imputed)"
       )
