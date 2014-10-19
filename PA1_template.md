# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
When the repository is forked, the data is still zipped. So first of all we need to unzip the data and then load the data from the file into a variable for further processing

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?
To answer the first question we'll remove all NA rows from the dataset.
Then we sum up the number of steps taken per day and look at the frequency distribution by creating a histogram.

```r
a <- activity[!is.na(activity$steps),]
a.grouped <- aggregate(a$steps,list(a$date),sum)
hist(a.grouped$x, main="Histogram of total steps per day", xlab="Nuber of steps per day (total)",breaks=15)
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

Here we can see that most of the days the person wearing the device took more than 10000  steps.

Now we calculate the mean and median for the steps taken per day:

```r
a.mean <- mean(a.grouped$x)
a.median <- median(a.grouped$x)
```
The mean is 1.0766 &times; 10<sup>4</sup> and the median is 10765


## What is the average daily activity pattern?
In order to see the daily pattern we change the aggregation criterion from date to interval.
And this time we want to see the average number of steps so we choose mean as aggregation function.
The 

```r
a.day <- aggregate(a$steps,list(a$interval),mean)
plot(a.day$Group.1,a.day$x,type="l", xlab="Interval", ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

```r
a.maxInterval <- a.day[a.day$x==max(a.day$x),]
```
The interval with the most steps taken on average is 835 with 206.1698 steps on average.

## Imputing missing values

```r
a.missing <- nrow( activity[is.na(activity$steps),])
```
There are 2304 rows wit NA values

Fill the NA values with the mean of the given interval

```r
colnames(a.day)<- c("interval", "mean")
merged <- merge(activity, a.day, by.x= "interval")
merged["filled"]<- 0 
merged[!is.na(merged$steps),]$filled <- merged[!is.na(merged$steps),]$steps 
merged[is.na(merged$steps),]$filled <- merged[is.na(merged$steps),]$mean
```

Now we aggregate and plot the data

```r
merged.grouped <- aggregate(merged$filled,list(merged$date),sum)
hist(merged.grouped$x, main="Histogram of total steps per day", xlab="Nuber of steps per day (total)",breaks=15)
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 

The histogram seems pretty close to the first plot where we filtered the missing values.


```r
merged.mean <- mean(merged.grouped$x)
merged.median <- median(merged.grouped$x)
```

Also the mean (1.0766 &times; 10<sup>4</sup>) and median (1.0766 &times; 10<sup>4</sup>) are very close to those we calculated in the first task.


## Are there differences in activity patterns between weekdays and weekends?

```r
merged$date<-as.Date(merged$date)
merged["weekday"]<-weekdays(merged$date)
merged["typeOfDay"]<-NA
merged$typeOfDay <- ifelse(merged$weekday %in% c("Saturday","Sunday"), "weekend", "weekday")
merged$typeOfDay <- as.factor(merged$typeOfDay)

merged.day <- aggregate(merged$filled,list(merged$interval,merged$typeOfDay),mean)


merged.weekdays <- subset(merged.day,as.character(merged.day$Group.2)=="weekday")
merged.weekends <- subset(merged.day,as.character(merged.day$Group.2)=="weekend")
par(mfrow=c(1,2))

plot(merged.weekdays$Group.1,merged.weekdays$x,type="l", xlab="Interval", ylab="Average number of steps", main="Weekdays")
plot(merged.weekends$Group.1,merged.weekends$x,type="l", xlab="Interval", ylab="Average number of steps", main="Weekends")
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 
