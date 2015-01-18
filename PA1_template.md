# Reproducible Research: Peer Assessment 1


```r
deps = c("ggplot2","dplyr");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(dep);
  }
}

library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data


```r
if(!file.exists("activity.csv")){
  unzip("activity.zip", exdir=".")  
}
data <- read.csv("activity.csv")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?


```r
data_c <- data[complete.cases(data$steps),]
data_s <- aggregate(steps ~ date, data=data_c, sum)
hist(data_s$steps, main="Histogram of the total number of steps taken each day",xlab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


```r
total_steps_mean <- mean(data_s$steps)
total_steps_median <- median(data_s$steps)
```
For total number of steps taken per day:

- **mean=10766.19**
- **median=10765**


## What is the average daily activity pattern?


```r
data_t <- aggregate(steps ~ interval, data=data_c, mean)
g <- ggplot(data_t, aes(x = interval, y = steps))
g + geom_line() + labs(title = "Average number of steps taken per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 



```r
int_max <- which.max(data_t$steps)
int_max_avg_step <- data_t[which.max(data_t$steps),c("steps")]

subset(data_t, steps == int_max, select = interval)[1,1]
```

```
## [1] NA
```
**206.1698113th** interval on average across all the days in the dataset, contains the maximum number of steps equals **104.00**


## Imputing missing values

```r
missing_val_total <- nrow(data[!complete.cases(data),])
```
Total number of missing values is **2304**


```r
# Fill missing values with the mean for each 5-minute interval
m <- left_join(tbl_df(data), tbl_df(data_t), by="interval")
mf <- transform(m, steps = ifelse(is.na(steps.x), steps.y, steps.x))
data_f <- mf[,c("steps", "date", "interval")]
str(data_f)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
data_fs <- aggregate(steps ~ date, data=data_f, sum)
hist(data_fs$steps, main="Total number of steps taken each day over filled data",xlab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


```r
total_steps_mean_f <- mean(data_fs$steps)
total_steps_median_f <- median(data_fs$steps)
```

Comparision of mean and median total number of steps taken per day for raw base data set and the data set where missing values are filled with the interval mean.

Parameter     | Base data set           | Data set without missing values
------------- | ------------------------|----------------------------------
**mean**      | 1.0766189\times 10^{4}    | 1.0766189\times 10^{4}
**median**    | 10765  | 1.0766189\times 10^{4}

Despite filling missing values, the mean of of total number of steps is the same as in the original data set. The median has changed - it's higher now and equals the mean. It's caused by changing the total number of data samples.


## Are there differences in activity patterns between weekdays and weekends?

```r
weekend_days <- c("Saturday", "Sunday")
data_cf <- tbl_df(data_c)
data_cfr <- data_cf %>%
  mutate(weekday = ifelse(weekdays(as.Date(date)) %in% weekend_days, "weekend", "weekday")) %>%
  group_by(weekday, interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE)) 

g2 <- ggplot(data_cfr, aes(x = interval, y = avg_steps))
g2 + geom_line() + facet_grid(weekday~.) + labs(title = "Average number of steps taken per interval", y = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

On weekdays activity starts and finishes earlier than on weekends. Besides on weekends activity is more smooth.
