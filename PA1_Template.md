---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
library(ggplot2)
data <- read.csv(file = "activity.csv")

data$date <- as.Date(data$date)

my_data <- data %>% filter(!is.na(steps))
```

# What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day?


```r
total_steps <- my_data %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))
```

## Histogram of the total number of steps taken each day


```r
total_steps %>%
  ggplot(aes(x = total_steps)) + geom_histogram() + xlab("Total number of steps taken each day") +
  ggtitle("Histogram showing total number of steps taken each day") + 
  theme(plot.title = element_text(hjust = 0.5))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is mean and median total number of steps taken per day?


```r
mean(total_steps$total_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$total_steps)
```

```
## [1] 10765
```

The mean total number of steps taken per day is 10766.19 and the median total number of steps taken per day is 10765.

# What is the average daily activity pattern?

## Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval <- my_data %>% group_by(interval) %>%
  summarize(average_steps = mean(steps))

interval %>% ggplot(aes(x = interval, y = average_steps)) + geom_line() + xlab("5-minute interval") +
  ylab("Average number of steps taken") + ggtitle("Average Daily Activity Pattern") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_Template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval[which.max(interval$average_steps), ]
```

```
## # A tibble: 1 x 2
##   interval average_steps
##      <int>         <dbl>
## 1      835          206.
```

835 is the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps

# Imputing missing values

## Calculate and report the total number of missing values in the dataset 

```r
sum(is.na(data))
```

```
## [1] 2304
```

There are 2304 total number of missing values in the dataset.  

## Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data <- data %>% group_by(interval) %>%
mutate(steps = ifelse(is.na(steps), round(mean(steps, na.rm = TRUE)), steps))
```

I have devised a strategy for filling the missing values in the dataset. I used the mean for that 5-minute interval for filling in all of the missing values in the dataset.

## Make a histogram of the total number of steps taken each day.


```r
new_data <- data %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps)) 

new_data %>%
  ggplot(aes(x = total_steps)) + geom_histogram() + xlab("Total number of steps taken each day") +
  ggtitle("Histogram showing total number of steps taken each day") + 
  theme(plot.title = element_text(hjust = 0.5))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_Template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## Calculate and report the mean and median total number of steps taken per day.


```r
mean(new_data$total_steps)
```

```
## [1] 10765.64
```

```r
median(new_data$total_steps)
```

```
## [1] 10762
```

The mean total number of steps taken per day is 10765.64 and the median total number of steps taken per day is 10762. Thus mean and median values  slightly differ from the estimates from the first part of the assignment.

# Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
data <- data %>% mutate(day = weekdays(date))

data <- data %>% mutate(weekday = ifelse((day == "Saturday") |(day == "Sunday"), "weekend", "weekday"))

data <- data %>% select(-day)

data$weekday <- as.factor(data$weekday)
```

## Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
data %>%
  group_by(interval, weekday) %>%
  mutate(average_steps = mean(steps)) %>%
  ggplot(aes(x = interval, y = average_steps)) + geom_line() + facet_wrap(weekday~., ncol = 1) +
  xlab("5-minute interval") + ylab("Average number of steps taken") + 
  ggtitle("Difference in the activity pattern between weekdays and weekends") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_Template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

 There is a difference in activity patterns between weekdays and weekends.
