---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---



``` r
knitr::opts_chunk$set(echo = TRUE)
```


``` r
library(datasets)
activity <- read.csv("activity.csv")
```



``` r
activity$date <- as.Date(activity$date)
```


``` r
total_steps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```



``` r
hist(total_steps$steps,
     main = "Total Steps per Day",
     xlab = "Steps per day",
     col = "lightblue")
```

![](PA1_template_files/figure-html/hist1-1.png)<!-- -->


``` r
mean(total_steps$steps)
```

```
## [1] 10766.19
```

``` r
median(total_steps$steps)
```

```
## [1] 10765
```


``` r
interval_avg <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```


``` r
plot(interval_avg$interval,interval_avg$steps,type = "l",xlab = "5-minute interval",ylab = "Average steps", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/time_series-1.png)<!-- -->


``` r
interval_avg[which.max(interval_avg$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```


``` r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


``` r
activity_imputed <- activity
na_index <- is.na(activity_imputed$steps)

activity_imputed$steps[na_index] <-
  interval_avg$steps[match(activity_imputed$interval[na_index],
                            interval_avg$interval)]
```


``` r
total_steps_imp <- aggregate(steps ~ date,
                             data = activity_imputed,
                             sum)
```


``` r
hist(total_steps_imp$steps,
     main = "Total Steps per Day (Imputed)",
     xlab = "Steps per day",
     col = "lightgreen")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->


``` r
mean(total_steps_imp$steps)
```

```
## [1] 10766.19
```

``` r
median(total_steps_imp$steps)
```

```
## [1] 10766.19
```


``` r
activity_imputed$daytype <- ifelse(
  weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"),
  "Weekend",
  "Weekday"
)

activity_imputed$daytype <- factor(activity_imputed$daytype)
```


``` r
library(lattice)

avg_interval_daytype <- aggregate(steps ~ interval + daytype,
                                  data = activity_imputed,
                                  mean)

xyplot(steps ~ interval | daytype,
       data = avg_interval_daytype,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Average Steps")
```

![](PA1_template_files/figure-html/panel_plot-1.png)<!-- -->
