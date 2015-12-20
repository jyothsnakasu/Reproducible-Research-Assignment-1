# Reproducible-Research-Assignment-1
Loading and processing the data
> getwd ()
[1] "/Users/pkasu"
> rra <- read.csv("activity 2.csv",TRUE,",")
> if (!file.exists("activity 2.csv")) {
+     unzip("activity 2.zip")
+ }
> activity <- read.csv("activity.csv", colClass=c('integer', 'Date', 'integer'))
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'activity.csv': No such file or directory
> if (!file.exists("activity 2.csv")) {
+     unzip("activity 2.zip")
+ }
> activity <- read.csv("activity 2.csv", colClass=c('integer', 'Date', 'integer'))
> steps.date <- aggregate(steps ~ date, activity, sum)
> head(steps.date)
        date steps
1 2012-10-02   126
2 2012-10-03 11352
3 2012-10-04 12116
4 2012-10-05 13294
5 2012-10-06 15420
6 2012-10-07 11015
> barplot(steps.date$steps, names.arg=steps.date$date, ylim=c(0, 25000), 
+         xlab="date", ylab="sum(steps)",)
> mean(steps.date$steps)
[1] 10766.19
> median(steps.date$steps)
[1] 10765
> steps.interval <- aggregate(steps ~ interval, activity, mean)
> plot(steps.interval, type='l')
> steps.interval$interval[which.max(steps.interval$steps)]
[1] 835
> sum(is.na(activity$steps))
[1] 2304
> activity.clean <- merge(activity, steps.date, by="date", suffixes=c("", ".mean"))
> nas <- is.na(activity.clean$steps)
> activity.clean$steps[nas] <- activity.clean$steps.mean[nas]
> activity.clean <- activity.clean[, c(1:3)]
> head(activity.clean)
        date steps interval
1 2012-10-02     0     1740
2 2012-10-02     0     1715
3 2012-10-02     0     1725
4 2012-10-02     0     1710
5 2012-10-02     0     1735
6 2012-10-02     0     1855
> steps.date <- aggregate(steps ~ date, activity.clean, sum)
> barplot(steps.date$steps, names.arg=steps.date$date, ylim=c(0, 25000), 
+         xlab="date", ylab="sum(steps)",)
> mean(steps.date$steps)
[1] 10766.19
> median(steps.date$steps)
[1] 10765
> dayType <- function(dates) {
+     f <- function(date) {
+         if (weekdays(date) %in% c("Saturday", "Sunday")) {
+             "weekend"
+         }
+         else {
+             "weekday"
+         }
+     }
+     sapply(dates, f)
+ }
> 
> activity$dayType <- as.factor(dayType(activity$date))
> str(activity)
'data.frame':	17568 obs. of  4 variables:
 $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
 $ dayType : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
> library(lattice)
> 
> steps.interval <- aggregate(steps ~ interval + dayType, activity, mean)
> xyplot(steps ~ interval | dayType, data=steps.interval, layout=c(2,1), type='l')
