---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
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
act <- read.csv('activity/activity.csv')
steps_day <- act %>% group_by(date) %>% summarise(total_steps=sum(steps,na.rm=TRUE),
                                                  mean_steps=mean(steps,na.rm=TRUE),
                                                  median_steps=median(steps,na.rm=TRUE))
```

## What is mean total number of steps taken per day?
The mean and median total number of daily steps are:

```r
library(xtable)
day_step_table <- xtable(steps_day %>% summarise(mean_daily_steps=mean(total_steps,na.rm=TRUE),median_daily_steps=median(total_steps,na.rm=TRUE)))
print(day_step_table,type='html')
```

<!-- html table generated in R 4.3.1 by xtable 1.8-4 package -->
<!-- Mon Jul 10 19:36:31 2023 -->
<table border=1>
<tr> <th>  </th> <th> mean_daily_steps </th> <th> median_daily_steps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 9354.23 </td> <td align="right"> 10395 </td> </tr>
   </table>

The Following Histogram shows the total step count each day

```r
step_hist <- steps_day %>% 
        ggplot(aes(x=total_steps)) + geom_histogram(fill='#000099') + 
        scale_y_continuous(breaks=seq(0,10,by=2)) + 
        labs(title='Daily Steps Histogram',y='Num of Days',x='Step Count')
step_hist
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/daily_step_histogram-1.png)<!-- -->

## What is the average daily activity pattern?
This individual starts moving at around 500 and their activity increases until 835 where their mean number of steps is about 206. It then drops to between 25 to 100 throughout the day until about 2000 where it gradually decreases. You can see the daily activity pattern in the time series plot below.

```r
steps_interval <-act %>% group_by(interval) %>% summarise(mean_steps=mean(steps,na.rm=TRUE))
max_activity <- steps_interval[which.max(steps_interval$mean_steps),1:2]
steps_time <- steps_interval %>% ggplot(aes(x=interval,y=mean_steps)) + geom_line() + 
        geom_vline(xintercept=as.numeric(max_activity$interval),
                   linetype=4,color=2) +
        labs(title='Average Number of Steps per Time Interval',y='Avg Steps',x='Time of Day') + 
        annotate('text',label='Max Activity at\n Interval 835',y=210,x=975,size=3)
steps_time
```

![](PA1_template_files/figure-html/daily_activity_time_plot-1.png)<!-- -->

## Imputing missing values
The number of missing counts ("NAs"):

```r
total_missing <- act %>% filter(is.na(steps)) %>% count()
total_missing %>% pull()
```

```
## [1] 2304
```
To impute missing values we find the mean number of steps for each interval. After joining the dataset with steps_interval, we replace steps with the corresponding mean if the value is "NA". We then create a new dataframe with no missing values.

After extrapolating for missing data mean and median total number of daily steps are:

```r
no_missing_steps <- left_join(act,steps_interval,by=join_by(interval)) %>% 
        mutate(steps=ifelse(is.na(steps),mean_steps,steps)) %>%
        select(!mean_steps) 
filled_steps_day <- no_missing_steps %>% group_by(date) %>% summarise(total_steps=sum(steps,na.rm=TRUE),mean_steps=mean(steps,na.rm=TRUE),
                                                  median_steps=median(steps,na.rm=TRUE))

filled_day_step_table <- xtable(filled_steps_day %>% summarise(mean_daily_steps=mean(total_steps),median_daily_steps=median(total_steps)))
print(filled_day_step_table,type='html')
```

<!-- html table generated in R 4.3.1 by xtable 1.8-4 package -->
<!-- Mon Jul 10 19:36:32 2023 -->
<table border=1>
<tr> <th>  </th> <th> mean_daily_steps </th> <th> median_daily_steps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 10766.19 </td> <td align="right"> 10766.19 </td> </tr>
   </table>

The mean increases from around 9000 to 10766. The new median is also 10766 suprisingly. Filling in the values changes the mean and median for the days where steps weren't recorded. Since all missing values were found in the same days, there's no impact on the days with known steps. In the histogram the number of days with very little to no steps decreases significantly. More days are closely centered around the 10000 mark.

```r
filled_step_hist <- filled_steps_day %>% 
        ggplot(aes(x=total_steps)) + geom_histogram(fill='#000099') + 
        scale_y_continuous(breaks=seq(0,10,by=2)) + 
        labs(title='Daily Steps Histogram (imputing missing values with mean steps by interval)',y='Num of Days',x='Step Count')
filled_step_hist
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/no_na_daily_step_histogram-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

Activity starts later on weekends and increases more gradually. Weekdays have a steep increase in the morning before dropping off before 1000 and then remaining relatively low throughout the day. On Weekdays there is much more activity in the afternoon/evening.

```r
no_missing_steps$day_of_week <- weekdays(as.POSIXlt(no_missing_steps$date),abbreviate = TRUE)
no_missing_steps <- no_missing_steps %>% mutate(time_of_week=as.factor(if_else(
        day_of_week %in% c('Sat','Sun'),'weekend','weekday')))
no_missing_steps %>% group_by(interval,time_of_week) %>% 
        summarise(mean_steps=mean(steps,na.rm=TRUE)) %>%
        ggplot(aes(y=mean_steps,x=interval,color=time_of_week)) + 
        geom_line(show.legend = FALSE) + facet_wrap(~time_of_week,nrow=2) +
        labs(title='Number of Steps Throughout The Day: Weekday VS Weekend',y='avg steps',x='time of day')
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

![](PA1_template_files/figure-html/week_activity_time_plot-1.png)<!-- -->
