---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
activity <- tbl_df(read.csv("activity.csv")) 

## Remove missing values:
filter(activity, steps != "NA") -> activity.f
group_by(activity.f, date) -> activity.fg
summarise(activity.fg, total = sum(steps)) -> act_sum
```
## What is mean total number of steps taken per day?

```r
summarise(activity.fg, total = sum(steps)) -> act_sum

summarise(act_sum, Mean = mean(total), Median = median(total))
```

```
## Source: local data frame [1 x 2]
## 
##       Mean Median
## 1 10766.19  10765
```

```r
##Histogram
p <- ggplot(data = act_sum, mapping = aes(x = as.Date(date), y = total))

p + layer(geom = "histogram", stat = "identity") -> p

p + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total Steps") -> p
p
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?


```r
group_by(activity, interval) -> g_act

select(g_act, steps) -> g_act_s

summarise(g_act_s, Mean = mean(steps, na.rm = T)) -> m_act

##time series plot
j <- ggplot(data = m_act, mapping = aes(x = interval, y = Mean))

j <- j + layer(geom = "line")

j <- j + labs(title = "Average Steps at Each Interval", x = "Interval", y = "Average Steps")
j
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
max(m_act$Mean) -> mmax

which(grepl(mmax, m_act$Mean)) -> m

m_act[m,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval     Mean
## 1      835 206.1698
```

```r
##The highest average number of steps, 206, occurs at 835 minutes.
```
## Imputing missing values

```r
## Find the number of NA's:

activity[is.na(activity$steps),] -> NAs

length(NAs$steps)
```

```
## [1] 2304
```

```r
## Function for creating NA value.
##

group_by(activity, date) -> gd_act

select(gd_act, steps) -> gd_act_s
summarise(gd_act_s, Mean = mean(steps, na.rm = T)) -> d_act
d_act$Mean[is.nan(d_act$Mean)] <- 0

NAmer <- tbl_df(merge(NAs, d_act, by.x = "date", by.y = "date"))
arrange(NAmer, date) -> NAmer
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- 
  NAmer$Mean[match(activity$steps, NAmer$steps)][is.na(activity$steps)]

group_by(activity2, date) -> activity2_g
summarise(activity2_g, total = sum(steps)) -> act2_sum
## Plot:

k <- ggplot(data = act2_sum, mapping = aes(x = as.Date(date), y = total))

k + layer(geom = "histogram", stat = "identity") -> k

k + labs(title = "Total Steps Each Day", x = "Date", y = "Total Steps") -> k
k
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
##Mean and Median:

summarise(act2_sum, Mean = mean(total), Median = median(total))
```

```
## Source: local data frame [1 x 2]
## 
##      Mean Median
## 1 9354.23  10395
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity.f$days <- weekdays(as.Date(activity.f$date))

sorter <- function(x){
  if (x == "Sunday"|x == "Saturday"){
    return("Weekend")
  }
  else{
    return("Weekday")
  }
}
  activity.f$daytype <- as.character(lapply(activity.f$days, sorter)[1:15264])
  q <- ggplot(activity.f,     aes(x = interval, y = steps)) + geom_line() + facet_grid(daytype ~ .) + labs(x = "Interval", y = "Number of Steps")
q
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
# Yes, there are differences in activity patterns.  Activity occurs later in the day on #weekends.
```