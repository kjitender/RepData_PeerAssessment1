# Title -Reproducible Research: Peer Assessment 1
## Author - kjitender

========================================================

Data: The data for this assignment can be downloaded from the course web site.
The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
    date: The date on which the measurement was taken in YYYY-MM-DD format
    interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


### 1. Code for reading in the dataset and/or processing the data

```r
data <- read.csv("activity.csv")
```

### 2. Histogram of the total number of steps taken each day

```r
steps_Day <- aggregate(data$steps, by=list(data$date), sum)
names(steps_Day) <- c("Date", "Total")
```


```r
suppressWarnings(hist(steps_Day$Total, 
     breaks=seq(from=0, to=25000, by=500),
     col="blue", 
     xlab="Total # of steps", 
     ylab="Count", 
     ylim=c(0, 10), 
     main="Histogram - Total number of steps each day"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

### 3. Mean and median number of steps taken each day

```r
meanSteps_Day <- mean(steps_Day$Total,na.rm=TRUE)
paste("mean =",round(meanSteps_Day, digits=2))
```

```
## [1] "mean = 10766.19"
```

```r
medianSteps_Day <- median(steps_Day$Total,na.rm=TRUE)
paste("median =",round(medianSteps_Day, digits=2))
```

```
## [1] "median = 10765"
```

### 4. Time series plot of the average number of steps taken

```r
steps_Interval <- aggregate(steps ~ interval, data = data, FUN = mean)
```


```r
suppressWarnings(with(steps_Interval, {
  plot(interval, steps, type="l", xlab="Intervals", ylab="Mean of steps", 
       main="Average daily activity", panel.first=grid(), 
       col.axis="white")
xMarks <- seq(from = 0, to = 2400, by = 100)
yMarks <- seq(from = 0, to = 200, by = 50)
axis(1, at=xMarks, col.axis="black", las=2, tck=0.02)
axis(2, at=yMarks, col.axis="black", las=2, tck=0.02)
}))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

### 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
maxSteps <- steps_Interval$interval[which.max(steps_Interval$steps)]
paste("Interval with max number of steps =",round(maxSteps))
```

```
## [1] "Interval with max number of steps = 835"
```
### 6. Code to describe and show a strategy for imputing missing data
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
Total_missing <- sum(is.na(data))
paste("Counting of missing values in this database =",Total_missing)
```

```
## [1] "Counting of missing values in this database = 2304"
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
I will use mean of that 5-minute interval.

```r
data_new <- data
for (i in 1:nrow(data_new)){
    if (is.na(data_new$steps[i])){
        data_new$steps[i] <- mean(data_new$steps[data_new$interval == data_new$interval[i]], na.rm=TRUE)    }   }
```

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
for (i in 1:nrow(data_new)){ data$steps_avg[i]=data_new$steps[i]}
```
#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
steps_Day1 <- aggregate(data_new$steps, by=list(date=data_new$date), sum)
names(steps_Day1) <- c("Date", "Total")
```


```r
suppressWarnings(hist(steps_Day1$Total, 
     breaks=seq(from=0, to=25000, by=500),
     col="blue", 
     xlab="Total # of steps", 
     ylab="Count", 
     ylim=c(0, 10), 
     main="Histogram - Total number of steps each day"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

Mean and Median of new data set

```r
meanSteps_Day1 <- mean(steps_Day1$Total,na.rm=TRUE)
paste("mean =",round(meanSteps_Day1, digits=2))
```

```
## [1] "mean = 10766.19"
```

```r
medianSteps_Day1 <- median(steps_Day1$Total,na.rm=TRUE)
paste("median =",round(medianSteps_Day1, digits=2))
```

```
## [1] "median = 10766.19"
```
#### 5. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?                       

These values do not 
differ from original values. 
Imapct is very low. This is because we have taken replaces NA values with mean and median of that interval.

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
day_week <- weekdays(strptime(data$date, format = "%Y-%m-%d"))   

for (i in 1:length(day_week)){
    if (day_week[i] == "Saturday" | day_week[i] == "Sunday"){
        day_week[i] <- "weekend"
    } 
    else {
        day_week[i] <- "weekday"
    }}
data$day_week<-day_week
weekday_mean <- aggregate(steps ~ interval + day_week, data = data, mean)
```
Plot panel for weekday and weekend

```r
library(lattice)
suppressWarnings(xyplot(steps ~ interval | day_week, data = weekday_mean, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of Steps"))
```

```
## Warning in par("page"): "page" is not a graphical parameter
```

```
## Warning in plot_snapshot(incomplete_plots): Please upgrade R to at least
## version 3.0.2
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

