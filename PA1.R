## Set Working Directory

setwd("D:/Coursera Esp/Reproducible Research/RepData_PeerAssessment1")

## extracting and reading file

if (!file.exists('/repdata_data_activity.zip')){
        unzip("repdata_data_activity.zip", exdir = getwd())
}

data <- read.csv(file="activity.csv", header=TRUE)

data$date <- as.Date(data$date)


##What is mean total number of steps taken per day?

t_steps <- aggregate(steps ~ date, data, sum)
hist(t_steps$steps,col = c("red","blue","grey","green","black"),
     main = "Total Stes taken per day", xlab ="# Steps")
mn <- mean(t_steps$steps, na.rm = TRUE)
mdn <- median(t_steps$steps, na.rm= TRUE)

print(mn)
print(mdn)

## What is the average daily activity pattern?
library(ggplot2)

mean_i_steps <- aggregate(steps ~ interval, data , FUN =  mean)

png("plot1.png", 480,480,"px")
qplot(interval, steps ,data = mean_i_steps,col = interval,geom =("line"),
      main = "Average Daily Activity Pattern", xlab="5-min Interval", 
      ylab = "Steps taken Avrg across all days " )
dev.off()
maxin <- mean_i_steps[which.max(mean_i_steps$steps),]

print(maxin)

## Imputing missing values

mval <- is.na(data)

sum(mval)

## Code to describe and show a strategy for imputing missing data

new_data <-  data
new_data$steps[which(is.na(new_data$steps))] <- mean(is.na(data$steps))

##for (i in 1:nrow(data)) {
       ## new_data <-  data
        ##if (is.na(new_data$steps[i])) {
          ##      new_data$steps[i] <-  mean(is.na(data$steps))
        ##}
##}

mval2 <- is.na(new_data)
sum(mval2)

nmv_steps <- aggregate(steps ~ date, new_data, sum)
png("plot2.png", 480,480,"px")
hist(nmv_steps$steps,col = c("red","blue","grey","green","black"),
     main = "Total Stes taken per day", xlab ="# Steps")
dev.off()

mn1 <- mean(nmv_steps$steps, na.rm = TRUE)
mdn1 <- median(nmv_steps$steps, na.rm= TRUE)
print(mn1)
print(mdn1)

dif_mn <- mn1 - mn
dif_mnd <- mdn1 - mdn


print(dif_mn) 
print(dif_mnd)

## Are there differences in activity patterns between weekdays and weekends?

Day <- function(date) {
        if (weekdays(date) %in% c("sabado", "domingo")) {
                "weekend"
        } else {
                "weekday"
        }
}
new_data$day <- as.factor(sapply(new_data$date, Day))

newmean_i_steps <- aggregate(steps ~ interval+day, new_data , FUN =  mean)
png("plot1.png", 480,480,"px")
qplot(interval, steps ,data = newmean_i_steps,color = day, geom =("line"),
      main = "Average Daily Activity Pattern", xlab="5-min Interval", 
      ylab = "Steps taken Avrg across all days " )+ facet_grid(day~.)
dev.off()

