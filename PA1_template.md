# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip('activity.zip')
activity = read.csv('./activity.csv')
activity$date = as.Date(activity$date)
```



## What is mean total number of steps taken per day?

```r
daily.sums = aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = T)
hist(daily.sums$x, xlab = 'steps per day', main = 'steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean.daily = mean(daily.sums$x)
mean.daily
```

```
## [1] 9354.23
```

```r
median.daily = median(daily.sums$x)
median.daily
```

```
## [1] 10395
```

```r
## What is the average daily activity pattern?
activity.split2 = split(activity, activity$interval)
mean.interval = lapply(activity.split2, function(x) mean(x$steps, na.rm = T))
means = unlist(mean.interval)
plot(means ~ activity$interval[1:288], type = 'l', xlab= 'interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png) 

```r
max(means)
```

```
## [1] 206.1698
```

```r
index = which(means ==max(means))
max.interval = activity$interval[index]
max.interval
```

```
## [1] 835
```
  The mean number of steps per day was 9354.2295082.
  The median number of steps per day was 10395.
  The interval with the maximum number of steps was 835.
  
## Imputing missing values

```r
#Calculate total number of missing values
total.missing = sum(is.na(activity$steps))
total.missing
```

```
## [1] 2304
```

```r
#fill in missing values
#
activity2 = activity
for(i in 1:17568){
  if(is.na(activity2$steps[i])){
   activity2$steps[i] = as.numeric(means[as.character(activity2$interval[i])])
  }
}
daily2.sums = aggregate(activity2$steps, by = list(activity2$date), FUN = sum, na.rm = T)
hist(daily2.sums$x, xlab = 'steps per day', main = 'steps per day (imputed values)')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean.daily2 = mean(daily2.sums$x)
mean.daily2
```

```
## [1] 10766.19
```

```r
median.daily2 = median(daily2.sums$x)
median.daily2
```

```
## [1] 10766.19
```
The total number of missing values is 2304.
The mean daily activity inlcuding the imputed missing values is 1.0766189\times 10^{4}.
The median daily activity inlcuding the imputed missing values is 1.0766189\times 10^{4}.
The mean and median values for the data including the imputed values are both higher than with the data without the imputed values, as might be expected. 

## Are there differences in activity patterns between weekdays and weekends?
#create column with days of week

```r
activity2$day = weekdays(activity2$date)
weekends = c('Saturday', 'Sunday')
for(i in 1:17568){
if(activity2$day[i] %in% weekends){
  activity2$week[i] = 'weekend'
  }else{
    activity2$week[i] = 'weekday'
  } 
}
activity.wd = activity2[activity2$week == 'weekday',]
activity.we = activity2[activity2$week == 'weekend',]
activity.split.wd = split(activity.wd, activity.wd$interval)
mean.interval.wd = lapply(activity.split.wd, function(x) mean(x$steps, na.rm = T))
means.wd = unlist(mean.interval.wd)
activity.split.we = split(activity.we, activity.we$interval)
mean.interval.we = lapply(activity.split.we, function(x) mean(x$steps, na.rm = T))
means.we = unlist(mean.interval.we)
par(mfrow=c(1,2))
plot(means.wd ~ activity$interval[1:288], type = 'l', xlab= 'interval', main = 'weekday')
plot(means.we ~ activity$interval[1:288], type = 'l', xlab= 'interval', main = 'weekend')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 













There are a number of differences between the pattern of activity on weekdays and weekends. Activity starts earlier in the day on the weekdays, but there is more overall activity on the weekends.
