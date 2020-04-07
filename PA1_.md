---
title: "Reproducible Research: Peer Assessment 1"
author: "Maria"
date: "7 de abril de 2020"
output: html_document
---

## Loading and preprocessing the data


```{r}
setwd("D:/Coursera Esp/Reproducible Research/RepData_PeerAssessment1")
if (!file.exists('/repdata_data_activity.zip')){
        unzip("repdata_data_activity.zip", exdir = getwd())
        data <- read.csv(file="activity.csv", header=TRUE)
        data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

- mean = 10766.19
- median = 10765

```{r}
t_steps <- aggregate(steps ~ date, data, sum)
hist(t_steps$steps,col = c("red","blue","grey","green","black"),
     main = "Total Stes taken per day", xlab ="# Steps")
mn <- mean(t_steps$steps, na.rm = TRUE)
mdn <- median(t_steps$steps, na.rm= TRUE)

print(mn)
print(mdn)
```

## What is the average daily activity pattern?

 -    835 


```{r}
library(ggplot2)

mean_i_steps <- aggregate(steps ~ interval, data , FUN =  mean)
qplot(interval, steps ,data = mean_i_steps,col = interval,geom =("line"),
      main = "Average Daily Activity Pattern", xlab="5-min Interval", 
      ylab = "Steps taken Avrg across all days " )
maxin <- mean_i_steps[which.max(mean_i_steps$steps),]

print(maxin)
```

## Imputing missing values

```{r}
mval <- is.na(data)
sum(mval)
new_data <-  data
new_data$steps[which(is.na(new_data$steps))] <- mean(is.na(data$steps))
mval2 <- is.na(new_data)
sum(mval2)

nmv_steps <- aggregate(steps ~ date, new_data, sum)
hist(nmv_steps$steps,col = c("red","blue","grey","green","black"),
     main = "Total Stes taken per day", xlab ="# Steps")
mn1 <- mean(nmv_steps$steps, na.rm = TRUE)
mdn1 <- median(nmv_steps$steps, na.rm= TRUE)
print(mn1)
print(mdn1)

dif_mn <- mn1 - mn
dif_mnd <- mdn1 - mdn


print(dif_mn) 
print(dif_mnd)
```

## Are there differences in activity patterns between weekdays and weekends?



```{r}
Day <- function(date) {
        if (weekdays(date) %in% c("sabado", "domingo")) {
                "weekend"
        } else {
                "weekday"
        }
}
new_data$day <- as.factor(sapply(new_data$date, Day))

newmean_i_steps <- aggregate(steps ~ interval+day, new_data , FUN =  mean)
qplot(interval, steps ,data = newmean_i_steps,color = day, geom =("line"),
      main = "Average Daily Activity Pattern", xlab="5-min Interval", 
      ylab = "Steps taken Avrg across all days " )+ facet_grid(day~.)

```
