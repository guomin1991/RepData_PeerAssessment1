# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


```r
activity<-read.csv("./activity.csv")
activity$date<-as.Date(activity$date,"%Y-%m-%d")
```



## What is mean total number of steps taken per day?


```r
acsum<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(acsum)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
acmean<-mean(acsum);acmedian<-median(acsum)
print(acmean)
```

```
## [1] 9354.23
```

```r
print(acmedian)
```

```
## [1] 10395
```



## What is the average daily activity pattern?


```r
activity$avesteps<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
library(ggplot2)
qplot(interval,avesteps,data=activity,geom="line",xlab="interval",ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


```r
maxinterval<-activity$interval[which.max(activity$avesteps)]
maxinterval
```

```
## [1] 835
```




## Imputing missing values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


filling in all of the missing values by the mean for that 5-minute interval.

```r
activity2<-activity
activity2[is.na(activity2$steps)==TRUE,]$steps<-activity2[is.na(activity2$steps)==TRUE,]$avesteps
acsum2<-tapply(activity2$steps,activity2$date,sum)
hist(acsum2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
acmean2<-mean(acsum2);acmedian2<-median(acsum2)
print(acmean2)
```

```
## [1] 10766.19
```

```r
print(acmedian2)
```

```
## [1] 10766.19
```


It differs from the estimates from the first part of the assignment.
Filling the missing values makes the bigger mean and median total number of steps taken per day.



## Are there differences in activity patterns between weekdays and weekends?


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activity2$week<-weekdays(activity2$date)
activity2[activity2$week=="Saturday",]$week<-c("weekend")
activity2[activity2$week=="Sunday",]$week<-c("weekend")
activity2[activity2$week!="weekend",]$week<-c("weekday")

activity2[activity2$week=="weekday",]$avesteps<-tapply(activity2[activity2$week=="weekday",]$steps,activity2[activity2$week=="weekday",]$interval,mean)
activity2[activity2$week=="weekend",]$avesteps<-tapply(activity2[activity2$week=="weekend",]$steps,activity2[activity2$week=="weekend",]$interval,mean)





library(lattice)
activity2<-transform(activity2,week=factor(week))
xyplot(avesteps~interval|week,data=activity2,layout=c(1,2),type="l",xlab="interval",ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

weekend has more steps.


