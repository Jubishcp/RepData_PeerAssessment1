
## ListsLoading and preprocessing the data
###Load the data (i.e. read.csv())

```r
setwd("C:/Users/divinity/Desktop/Coursera/Reproducible Research/Assignment")
unzip(zipfile="repdata-data-activity.zip")
activitydata <- read.csv("activity.csv")
```

##What is mean total number of steps taken per day?
###Calculate the total number of steps taken per day


```r
stepsperday<-aggregate(steps~date,activitydata,sum )
```
###Histogram of the total number of steps taken each day

```r
stepsperday<-aggregate(steps~date,activitydata,sum )
barplot(stepsperday$steps, names.arg = stepsperday$date, xlab = "Date", ylab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

###Mean and median of the total number of steps taken per day


```r
mean(stepsperday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsperday$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
###Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsinterval <- aggregate(steps ~ interval, data = activitydata, FUN = mean)
plot(stepsinterval, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsinterval$interval[which.max(stepsinterval$steps)]
```

```
## [1] 835
```

##Imputing missing values

### Total number of missing values in the dataset

```r
missingvalue <- is.na(activitydata$steps)
 table(missingvalue)
```

```
## missingvalue
## FALSE  TRUE 
## 15264  2304
```
###Devise a strategy for filling in all of the missing values in the dataset

```r
fillEmpty <- function(steps, interval) {
  retValue <- NA
  if (!is.na(steps))
    retValue <- c(steps)
  else
    retValue <- (stepsinterval[stepsinterval$interval==interval, "steps"])
  return(retValue)
}
```
###Creating a new dataset that is equal to the original dataset but with the missing data filled in & testing forempty value

```r
 filleddataset<-activitydata
filleddataset$steps <-mapply(fillEmpty,filleddataset$steps,filleddataset$interval)
sum(is.na(filleddataset))
```

```
## [1] 0
```
###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
TotalSteps <- steps.date <- aggregate(steps ~ date, data=filleddataset, FUN=sum)
barplot(TotalSteps$steps, names.arg=TotalSteps$date, xlab="Date", ylab="Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?
###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
finddaytype <- function(date) 
  {
    day <- weekdays(as.Date(date))
    if (day %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}


filleddataset$date <- as.Date(filleddataset$date)
filleddataset$day <- as.factor(sapply(filleddataset$date, FUN=finddaytype))
```
###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library("lattice")
averagesdata <- aggregate(steps ~ interval + day, data=filleddataset, mean)
names(averagesdata) <- c("interval", "daylevel", "steps")

xyplot(steps ~ interval | daylevel, averagesdata, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

