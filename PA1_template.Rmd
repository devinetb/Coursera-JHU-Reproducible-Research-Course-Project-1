---
title: "devinetb_Course_Project_1"
author: "devinetb"
date: "6/29/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Reproducible Research Course Project 1

## Loading and preprocessing the data

### Show any code that is needed to
### 1. Load the data (i.e. read.csv())

```{r, echo=TRUE}
# load needed libraries
library("data.table")
library("ggplot2")
library("lattice")

# loading the .csv file as a data table
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
activity <- as.data.table(read.csv("activity.csv"))
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r, echo=TRUE}
# transforming 'date' data into a readable date
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
```

### What is mean total number of steps taken per day?
#### For this part of the assignment, you can ignore the missing values in the dataset.

### 1. Calculate the total number of steps taken per day

```{r, echo=TRUE}
# create new data table with two columns, the day and the sum of the steps taken that day 
steps <- activity[, c(lapply(.SD, sum, na.rm = FALSE)), 
	.SDcols = c("steps"), by = .(date)]
```

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. 
#### Make a histogram of the total number of steps taken each day

```{r, echo=TRUE}
# creating histogram
ggplot(steps, aes(x = steps)) +
	# using a magenta fill
	geom_histogram(fill = "magenta") +
	# adding labels
	labs(x = "Steps", y = "Frequency", 
		title = "Histogram of the Total Number of Steps Taken Each Day")
```

### 3. Calculate and report the mean and median of the total number of steps taken per day

```{r, echo=TRUE}
# calculating mean of 10766.19
steps[, .(Mean = mean(steps, na.rm = TRUE))]

# calculating median of 10765
steps[, .(Median = median(steps, na.rm = TRUE))]
```

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r, echo=TRUE}
# create new data table with two coluns, the 5-minute interval and the average number of steps taken
daily_pattern <- activity[, c(lapply(.SD, mean, na.rm = TRUE)), 
	.SDcols = c("steps"), by = .(interval)]

# creating line graph
ggplot(daily_pattern, aes(x = interval , y = steps)) + 
	# using a magenta line
	geom_line(color = "magenta") + 
	# adding labels
	labs(x = "Interval", y = "Average Steps per Day",
		title = "Average Daily Activity Pattern")
```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
# calculating the interval reveals it to be interval 835 with 206.1698 steps on average
daily_pattern[steps == max(steps)]
```

## Imputing missing values

### Note that there are a number of days/intervals where there are missing values (coded as NA).
### The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r, echo=TRUE}
# calculating amount of NA's reveals there to be 2304
activity[is.na(steps)]
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

#### For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r, echo=TRUE}
# using the mean of the dataset as a proxy to fill in the NAs
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps")]
```

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r, echo=TRUE}
# new 'activity' dataset has no NAs
activity[is.na(steps)]
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

#### Do these values differ from the estimates from the first part of the assignment? 

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, echo=TRUE}
# create new 'steps' data table with two columns, the day and the sum of the steps taken that day 
# no need to remove NAs as before
steps <- activity[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)]

# creating histogram
ggplot(steps, aes(x = steps)) +
	# using a magenta fill
	geom_histogram(fill = "magenta") +
	# adding labels
	labs(x = "Steps", y = "Frequency", 
		title = "Histogram of the Total Number of Steps Taken Each Day:
				NAs of Original Dataset Replaced with Mean")

# calculating mean of 10751.74 vice the 10766.19 as before with NAs in dataset
steps[, .(Mean = mean(steps, na.rm = TRUE))]

# calculating median of 10656 vice the 10765 as before with NAs in dataset
steps[, .(Median = median(steps, na.rm = TRUE))]
```

## Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r, echo=TRUE}
# computing the weekdays from the date attribute to be used in final analysis
activity <- data.frame(date=activity$date, 
	weekday=tolower(weekdays(activity$date)), 
	steps=activity$steps, 
	interval=activity$interval)

# computing the whether day type is weekday or weekend to be used in final analysis
activity <- cbind(activity, 
	datetype=ifelse(activity$weekday == "saturday" | 
	activity$weekday == "sunday", "Weekend", "Weekday"))

# creating the final data.frame for analysis
activity <- data.frame(date=activity$date, 
	weekday=activity$weekday, 
	datetype=activity$datetype, 
	interval=activity$interval,
	steps=activity$steps)

# sanity check
head(activity)
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
#### See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r, echo=TRUE}
# creating lattice plot of total steps instead of average number of steps
# kept getting object of type 'closure' is not subsettable error when using mean in place of steps below and could not find work around
xyplot(steps ~ interval | datetype, activity, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Total Number of Steps 
		(Note: Not Average Steps)", 
       layout=c(1,2))
```
