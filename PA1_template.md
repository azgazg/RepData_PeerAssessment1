---
title: "PA1_template"
author: "AZG"
date: "19.04.2015"
output: html_document
---

This is the R Markdown file created in purpose of peer assesment during Reproducible Research course from Coursera, e-learning platform.

# Peer Assesment 1

##Loading and preprocessing the data


```r
data <- read.table("activity.csv", header=T, quote="\"", sep=",")
```

```
## Warning in file(file, "rt"): nie można otworzyć pliku 'activity.csv': Nie
## ma takiego pliku ani katalogu
```

```
## Error in file(file, "rt"): nie można otworzyć połączenia
```

```r
# convert date to date data type
data$date <- as.Date(data$date, "%Y-%m-%d")
```

```
## Error in data$date: obiekt typu 'closure' nie jest ustawialny
```

##What is mean total number of steps taken per day?


```r
steps_taken_per_day <- aggregate(steps ~ date, data, sum, na.rm=TRUE)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

Plot the histogram:


```r
library(ggplot2)
ggplot(steps_taken_per_day, aes(x = steps)) + 
       geom_histogram(fill = "darkblue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", 
             y = "Number of times (Count)") + 
        theme_bw()    
```

```
## Error in ggplot(steps_taken_per_day, aes(x = steps)): nie znaleziono obiektu 'steps_taken_per_day'
```

Report mean and median of steps:


```r
mean(steps_taken_per_day$steps); 
```

```
## Error in mean(steps_taken_per_day$steps): nie znaleziono obiektu 'steps_taken_per_day'
```


```r
median(steps_taken_per_day$steps) 
```

```
## Error in median(steps_taken_per_day$steps): nie znaleziono obiektu 'steps_taken_per_day'
```

##What is the average daily activity pattern?


```r
time_series <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
```

```
## Error in data$interval: obiekt typu 'closure' nie jest ustawialny
```

Plot time series of the 5-minute interval and the average number of steps taken, averaged across all days


```r
plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "darkblue")
```

```
## Error in row.names(time_series): nie znaleziono obiektu 'time_series'
```

Report the 5-min interval, on average across all the days in the dataset, contains the maximum number of steps:


```r
which.max(time_series)
```

```
## Error in which.max(time_series): nie znaleziono obiektu 'time_series'
```

**Observations:**

Based on observed pattern, the highest activity of the person is around 8:35am. 

##Imputing missing values

Strategy:

The NA's are replaced by the mean value of the interval:


```r
StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
```

```
## Error in terms.formula(formula, data = data): argument 'data' posiada niepoprawny typ
```

```r
fillNA <- numeric()
for (i in 1:nrow(data)) {
    obs <- data[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

```
## Error in 1:nrow(data): argument ma długość 0
```

The new dataset is created but with NA's filled:


```r
new_activity <- data
new_activity$steps <- fillNA
```

```
## Error in new_activity$steps <- fillNA: obiekt typu 'closure' nie jest ustawialny
```

Histogram:


```r
full_steps_per_day <- aggregate(steps ~ date, new_activity, sum)
```

```
## Error in terms.formula(formula, data = data): argument 'data' posiada niepoprawny typ
```

```r
ggplot(full_steps_per_day, aes(x=steps)) + 
        geom_histogram(fill="darkblue", binwidth=1000) + 
        labs(title="Histogram of Full Steps Taken per Day", 
             x="Number of Steps after populate missing values", 
             y="Count") + 
        theme_bw()  
```

```
## Error in ggplot(full_steps_per_day, aes(x = steps)): nie znaleziono obiektu 'full_steps_per_day'
```

Mean and median:

```r
mean(full_steps_per_day$steps)
```

```
## Error in mean(full_steps_per_day$steps): nie znaleziono obiektu 'full_steps_per_day'
```


```r
median(full_steps_per_day$steps)
```

```
## Error in median(full_steps_per_day$steps): nie znaleziono obiektu 'full_steps_per_day'
```

##Are there differences in activity patterns between weekdays and weekends?

Create a factor variable daylevel with two levels (weekday, weekend). 


```r
day <- weekdays(data$date)
```

```
## Error in data$date: obiekt typu 'closure' nie jest ustawialny
```

```r
daylevel <- vector()
for (i in 1:nrow(data)) {
    if (day[i] == "sobota") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "niedziela") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
```

```
## Error in 1:nrow(data): argument ma długość 0
```

```r
data$daylevel <- daylevel
```

```
## Error in data$daylevel <- daylevel: obiekt typu 'closure' nie jest ustawialny
```

```r
data$daylevel <- factor(data$daylevel)
```

```
## Error in data$daylevel: obiekt typu 'closure' nie jest ustawialny
```

```r
stepsByDay <- aggregate(steps ~ interval + daylevel, data = data, mean)
```

```
## Error in terms.formula(formula, data = data): argument 'data' posiada niepoprawny typ
```

```r
names(stepsByDay) <- c("interval", "daylevel", "steps")
```

```
## Error in names(stepsByDay) <- c("interval", "daylevel", "steps"): nie znaleziono obiektu 'stepsByDay'
```

Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

```
## Error in eval(substitute(groups), data, environment(x)): nie znaleziono obiektu 'stepsByDay'
```
