# Reproducible Research: Peer Assessment 1
yxl1108  
Saturday, April 18, 2015  

## Loading and preprocessing the data


```r
# unzip and import
unzip("activity.zip")
activity <- read.csv("activity.csv", sep = ",", header = TRUE)

# convert date to POSIXct and add POSIXct data type for date and interval
activity$date<-strptime(activity$date, "%Y-%m-%d")
activity <- transform(activity, datetime = as.POSIXct(paste(date, sprintf("%04d", interval)), format = "%Y-%m-%d %H%M"))
```
## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
activitySum <- setNames(aggregate(activity$steps, by=list(as.character(activity$date)), FUN = sum, na.rm = TRUE), c("date","steps"))
hist(activitySum$steps, main="Histogram of Total Steps Per Day", xlab="Total Steps Per Day", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
setNames(aggregate(activity$steps, by=list(as.character(activity$date)), FUN = mean, na.rm = TRUE), c("date","meanSteps"))
```

```
##          date  meanSteps
## 1  2012-10-01        NaN
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08        NaN
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01        NaN
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04        NaN
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09        NaN
## 41 2012-11-10        NaN
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14        NaN
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30        NaN
```

```r
setNames(aggregate(activity$steps, by=list(as.character(activity$date)), FUN = median, na.rm = TRUE), c("date","medianSteps"))
```

```
##          date medianSteps
## 1  2012-10-01          NA
## 2  2012-10-02           0
## 3  2012-10-03           0
## 4  2012-10-04           0
## 5  2012-10-05           0
## 6  2012-10-06           0
## 7  2012-10-07           0
## 8  2012-10-08          NA
## 9  2012-10-09           0
## 10 2012-10-10           0
## 11 2012-10-11           0
## 12 2012-10-12           0
## 13 2012-10-13           0
## 14 2012-10-14           0
## 15 2012-10-15           0
## 16 2012-10-16           0
## 17 2012-10-17           0
## 18 2012-10-18           0
## 19 2012-10-19           0
## 20 2012-10-20           0
## 21 2012-10-21           0
## 22 2012-10-22           0
## 23 2012-10-23           0
## 24 2012-10-24           0
## 25 2012-10-25           0
## 26 2012-10-26           0
## 27 2012-10-27           0
## 28 2012-10-28           0
## 29 2012-10-29           0
## 30 2012-10-30           0
## 31 2012-10-31           0
## 32 2012-11-01          NA
## 33 2012-11-02           0
## 34 2012-11-03           0
## 35 2012-11-04          NA
## 36 2012-11-05           0
## 37 2012-11-06           0
## 38 2012-11-07           0
## 39 2012-11-08           0
## 40 2012-11-09          NA
## 41 2012-11-10          NA
## 42 2012-11-11           0
## 43 2012-11-12           0
## 44 2012-11-13           0
## 45 2012-11-14          NA
## 46 2012-11-15           0
## 47 2012-11-16           0
## 48 2012-11-17           0
## 49 2012-11-18           0
## 50 2012-11-19           0
## 51 2012-11-20           0
## 52 2012-11-21           0
## 53 2012-11-22           0
## 54 2012-11-23           0
## 55 2012-11-24           0
## 56 2012-11-25           0
## 57 2012-11-26           0
## 58 2012-11-27           0
## 59 2012-11-28           0
## 60 2012-11-29           0
## 61 2012-11-30          NA
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activityMeanInterval <- setNames(aggregate(activity$steps, by=list(activity$interval), FUN = mean, na.rm = TRUE), c("interval","steps"))
plot(activityMeanInterval$interval, activityMeanInterval$steps, type="l", xlab= "5-minute interval", ylab= "average steps", col="green" , lwd=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
groupbyInterval <- setNames(aggregate(activity$steps, by=list(activity$interval), FUN = sum, na.rm = TRUE), c("interval","steps"))

#interval 835 contains the maximum number of steps
head(groupbyInterval[order(-groupbyInterval$steps, groupbyInterval$interval),])
```

```
##     interval steps
## 104      835 10927
## 105      840 10384
## 107      850  9720
## 106      845  9517
## 103      830  9397
## 101      820  9071
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(which(is.na(activity)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. (The strategy is using the mean for that 5-minute interval to fill the missing values)

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# find average steps group by interval where steps is not NA
groupbyIntervalMean <- setNames(aggregate(activity$steps, by=list(activity$interval), FUN = mean, na.rm = TRUE), c("interval","meanSteps"))
#join the table to activity by interval
activityFill <- merge(activity, groupbyIntervalMean, by = "interval")
#update NA steps with the average steps for interval
activityFill <- within(activityFill, {steps <- ifelse(!is.na(steps), steps, meanSteps)})
```
summary of steps in data frame activity

```r
summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
summary of steps in data frame activityFill

```r
summary(activityFill$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   27.00  806.00
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



Make a histogram of the total number of steps taken each day (Filled)

```r
activityFillSum <- setNames(aggregate(activityFill$steps, by=list(as.character(activityFill$date)), FUN = sum), c("date","steps"))
hist(activityFillSum$steps, main="Histogram of Total Steps Per Day (Filled)", xlab="Total Steps Per Day", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

mean and median total number of steps taken per day (filled)

```r
setNames(aggregate(activityFill$steps, by=list(as.character(activityFill$date)), FUN = mean ), c("date","meanSteps"))
```

```
##          date  meanSteps
## 1  2012-10-01 37.3825996
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08 37.3825996
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01 37.3825996
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04 37.3825996
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09 37.3825996
## 41 2012-11-10 37.3825996
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14 37.3825996
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30 37.3825996
```

```r
setNames(aggregate(activityFill$steps, by=list(as.character(activityFill$date)), FUN = median), c("date","medianSteps"))
```

```
##          date medianSteps
## 1  2012-10-01    34.11321
## 2  2012-10-02     0.00000
## 3  2012-10-03     0.00000
## 4  2012-10-04     0.00000
## 5  2012-10-05     0.00000
## 6  2012-10-06     0.00000
## 7  2012-10-07     0.00000
## 8  2012-10-08    34.11321
## 9  2012-10-09     0.00000
## 10 2012-10-10     0.00000
## 11 2012-10-11     0.00000
## 12 2012-10-12     0.00000
## 13 2012-10-13     0.00000
## 14 2012-10-14     0.00000
## 15 2012-10-15     0.00000
## 16 2012-10-16     0.00000
## 17 2012-10-17     0.00000
## 18 2012-10-18     0.00000
## 19 2012-10-19     0.00000
## 20 2012-10-20     0.00000
## 21 2012-10-21     0.00000
## 22 2012-10-22     0.00000
## 23 2012-10-23     0.00000
## 24 2012-10-24     0.00000
## 25 2012-10-25     0.00000
## 26 2012-10-26     0.00000
## 27 2012-10-27     0.00000
## 28 2012-10-28     0.00000
## 29 2012-10-29     0.00000
## 30 2012-10-30     0.00000
## 31 2012-10-31     0.00000
## 32 2012-11-01    34.11321
## 33 2012-11-02     0.00000
## 34 2012-11-03     0.00000
## 35 2012-11-04    34.11321
## 36 2012-11-05     0.00000
## 37 2012-11-06     0.00000
## 38 2012-11-07     0.00000
## 39 2012-11-08     0.00000
## 40 2012-11-09    34.11321
## 41 2012-11-10    34.11321
## 42 2012-11-11     0.00000
## 43 2012-11-12     0.00000
## 44 2012-11-13     0.00000
## 45 2012-11-14    34.11321
## 46 2012-11-15     0.00000
## 47 2012-11-16     0.00000
## 48 2012-11-17     0.00000
## 49 2012-11-18     0.00000
## 50 2012-11-19     0.00000
## 51 2012-11-20     0.00000
## 52 2012-11-21     0.00000
## 53 2012-11-22     0.00000
## 54 2012-11-23     0.00000
## 55 2012-11-24     0.00000
## 56 2012-11-25     0.00000
## 57 2012-11-26     0.00000
## 58 2012-11-27     0.00000
## 59 2012-11-28     0.00000
## 60 2012-11-29     0.00000
## 61 2012-11-30    34.11321
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityFill <- cbind(activityFill, weekday = c(""))
activityFill <- within(activityFill, {weekday <- ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", "weekend", "weekday")})
table(activityFill$weekday)
```

```
## 
## weekday weekend 
##   12960    4608
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# install and load ggplot2, if not already there
if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
activityFillWeekday <- setNames(aggregate(activityFill$steps, by=list(activityFill$interval, activityFill$weekday), FUN = mean ), c("interval", "weekday", "steps"))

ggplot(activityFillWeekday, aes(x=interval, y=steps)) + geom_line() + facet_grid(weekday ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
