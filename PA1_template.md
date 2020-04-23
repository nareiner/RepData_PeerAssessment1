---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
library(ggplot2)
```

## Loading and preprocessing the data

#### 1. Load the data 

```r
activity <- read.csv("activity.csv")
```

#### 2. Process the data into a format suitable for your analysis

```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

```r
daysteps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
```

#### 2. Make a histogram of the total number of steps taken each day

```r
hist(daysteps, 50, xlab = 'Steps per Day', ylab = 'Frequency', main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(daysteps)
```

```
## [1] 9354.23
```

```r
median(daysteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

#### 1. Make a time series plot of the 5-minute interval (x) and the average number of steps taken, averaged across all days (y)

```r
avgsteps <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(names(avgsteps), avgsteps, xlab = "5-Minute Interval", ylab = "Average Number of Steps Taken", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_intavg <- avgsteps[avgsteps == max(avgsteps)]
max_intavg
```

```
##      835 
## 206.1698
```

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

```r
sum(is.na(activity))
```

```
## [1] 2304
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset

```r
activity2 <- activity
activity2[which(is.na(activity2$steps)), 1] <- avgsteps[as.character(activity2[which(is.na(activity2$steps)), 3])]
sum(is.na(activity2))
```

```
## [1] 0
```

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
daysteps2 <- tapply(activity2$steps, activity2$date, sum)
```

#### 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day

```r
par(mfrow = c(2, 1))
hist(daysteps, 50, xlab = 'Steps per Day', ylab = 'Frequency', main = "Total number of steps taken each day", sub = "(with missing values)", ylim = c(0, 20))
  abline(v = mean(daysteps), col = 1, lwd = 2)
hist(daysteps2, 50, xlab = 'Steps per Day', ylab = 'Frequency', main = "Total number of steps taken each day", sub = "(without missing values = replaced with interval mean)", ylim = c(0, 20))
  abline(v = mean(daysteps2), col = 1, lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
mean(daysteps2)
```

```
## [1] 10766.19
```

```r
mean(daysteps)
```

```
## [1] 9354.23
```

```r
mean(daysteps2) - mean(daysteps)
```

```
## [1] 1411.959
```

```r
median(daysteps2)
```

```
## [1] 10766.19
```

```r
median(daysteps)
```

```
## [1] 10395
```

```r
median(daysteps2) - median(daysteps)
```

```
## [1] 371.1887
```

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Creating a new factor variable in the dataset with two levels indicating whether a given date is a weekday day or weekend day

```r
activity2 <- mutate(activity2, week_ = ifelse(weekdays(activity2$date) == "Saturday" | weekdays(activity2$date) == "Sunday", "weekend", "weekday"))
activity2$week_ <- as.factor(activity2$week_)
head(activity2)
```

```
##       steps       date interval   week_
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

#### 2. Make a panel plot containing a time series plot of the 5-minute interval (x) and the average number of steps taken, averaged across all weekday days or weekend days (y)

```r
meanweek_ <- aggregate(steps ~ interval + week_, data = activity2, FUN = mean)
head(meanweek_)
```

```
##   interval   week_      steps
## 1        0 weekday 2.25115304
## 2        5 weekday 0.44528302
## 3       10 weekday 0.17316562
## 4       15 weekday 0.19790356
## 5       20 weekday 0.09895178
## 6       25 weekday 1.59035639
```

```r
ggplot(meanweek_, aes(interval, steps)) + geom_line() + facet_grid(week_ ~ .) + labs(x = "5-minute Interval", y = "Average Number of Steps Taken", title = "Activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
