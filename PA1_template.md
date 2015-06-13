---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



```r
library(dplyr)
library(ggplot2)
```


## Loading and preprocessing the data

```r
raw_data <- read.csv(file = 'activity.csv')
head(raw_data)
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

```r
tail(raw_data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
str(raw_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
Total number of steps taken per day:

```r
steps_each_day <- aggregate(x = raw_data$steps, by = list(raw_data$date), FUN = sum)
names(steps_each_day) <- c('date', 'steps')
head(steps_each_day)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
str(steps_each_day)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ steps: int  NA 126 11352 12116 13294 15420 11015 NA 12811 9900 ...
```

Histogram of the total number of steps taken each day:

```r
hist(steps_each_day$steps, main = 'Histogram of the total number of steps taken by day', xlab = 'Steps')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Mean of the total number of steps taken per day:

```r
mean(steps_each_day$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day:

```r
median(steps_each_day$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
by_interval <- aggregate(raw_data$steps, list(raw_data$interval), mean, na.rm = TRUE)
names(by_interval) <- c('interval', 'steps')
plot(by_interval$interval, 
     by_interval$steps, 
     type = 'l', 
     main = 'Average number of steps taken, averaged across all days', 
     xlab = '5-minute interval', ylab = 'Avg. number of steps taken')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
by_interval[by_interval$steps == max(by_interval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values
Total number of missing values in the dataset:

```r
sum(is.na(raw_data$steps))
```

```
## [1] 2304
```

Filling in missing values using the mean of the 5-minute interval:

```r
nas <- raw_data[is.na(raw_data$steps), ]
nas <- tbl_df(nas)
by_interval <- tbl_df(by_interval)
nas_filled <- left_join(nas, by_interval, by = c('interval' = 'interval'))
nas_filled <- select(nas_filled, date, interval, steps.y)
names(nas_filled) <- c('date', 'interval', 'steps')

raw_no_nas <- raw_data
raw_no_nas[is.na(raw_no_nas$steps), ]$steps <- nas_filled$steps
```

Total number of steps taken per day:

```r
steps_each_day_no_nas <- aggregate(x = raw_no_nas$steps, by = list(raw_no_nas$date), FUN = sum)
names(steps_each_day_no_nas) <- c('date', 'steps')
```

Comparing histograms of raw data versus NA's filled:

```r
par(mfrow = c(1, 2))
hist(steps_each_day$steps, main = 'Raw data', xlab = 'Steps')
hist(steps_each_day_no_nas$steps, main = 'Na\'s filled', xlab = 'Steps')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

```r
par(mfrow = c(1, 1))
```

Mean of the total number of steps taken per day (NA's filled):

```r
mean(steps_each_day_no_nas$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day (NA's filled):

```r
median(steps_each_day_no_nas$steps)
```

```
## [1] 10766.19
```

Filling in missing values seems to make no change in the mean and 
median summaries.

Watching the histograms, the more frequent steps count seems to have now 
a higher frequency.

## Are there differences in activity patterns between weekdays and weekends?

Calculating the mean for each 5-minute interval on weekdays and weekends:

```r
by_interval_no_nas <- tbl_df(raw_no_nas)
by_interval_no_nas <- mutate(by_interval_no_nas, 
                             date = as.Date(date),
                             type = ifelse(
                                     weekdays(date) == 'Domingo' | 
                                             weekdays(date) == 'Sábado', 
                                     'weekend', 'weekday')) %>% 
        group_by(type, interval) %>% 
        summarise(steps = mean(steps))
ggplot(by_interval_no_nas, aes(interval, steps)) + geom_line() + facet_wrap(~type, nrow = 2)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
