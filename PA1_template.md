# Peer Assessment 1


```r
echo = TRUE
options(scipen = 1)
```

### 1. Loading the data:

```r
unzip("repdata-data-activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
```

### 2. Preprocessing the data:

```r
data$month <- as.numeric(format(data$date, "%m"))
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(noNA)
```

```
## [1] 15264     4
```

### 3. Choosing the plotting system:

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

### 4. What is mean total number of steps taken per day?
#### a. Total number of steps taken per day:

```r
stepsTotal <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
```
#### b. Histogram of the total number of steps taken each day

```r
ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "green", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of the total number of steps taken each day", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#### c. Mean of the total number of steps taken per day

```r
mean(stepsTotal)
```

```
## [1] 10766.19
```

#### d. Median of the total number of steps taken per day

```r
median(stepsTotal)
```

```
## [1] 10765
```

### 5. What is the average daily activity pattern?

#### a. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "green", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

#### b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```

### Imputing missing values

#### a. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

#### b. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy to fill in all the missing values is to use the mean for that 5-minute interval.

#### c. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newDataset <- data 
for (i in 1:nrow(newDataset)) {
    if (is.na(newDataset$steps[i])) {
        newDataset$steps[i] <- avgSteps[which(newDataset$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(newDataset)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(newDataset))
```

```
## [1] 0
```

#### d. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
ggplot(newDataset, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "green",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of total number of steps taken each day with no data missing", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

##### Mean and Median total number of steps taken per day

```r
newStepsTotal <- aggregate(newDataset$steps, 
                           list(Date = newDataset$date), 
                           FUN = "sum")$x
newMean <- mean(newStepsTotal)
newMean
```

```
## [1] 10766.19
```

```r
newMedian <- median(newStepsTotal)
newMedian
```

```
## [1] 10766.19
```

##### Comparing the Old and New mean/median values:

```r
oldMean <- mean(stepsTotal)
oldMedian <- median(stepsTotal)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```
### Are there differences in activity patterns between weekdays and weekends?

#### a. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
head(newDataset)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
newDataset$days <- factor(format(newDataset$date, "%A"))
levels(newDataset$days)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(newDataset$days) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newDataset$days)
```

```
## [1] "weekday" "weekend"
```

```r
table(newDataset$days)
```

```
## 
## weekday weekend 
##   12960    4608
```

#### b. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avgSteps <- aggregate(newDataset$steps, 
                      list(interval = as.numeric(as.character(newDataset$interval)), 
                           days = newDataset$days),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$days, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

