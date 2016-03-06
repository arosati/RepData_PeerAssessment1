# Get the data from my working directory
rep <- read.csv("activity.csv")

# make date column Dates instead of Factor
rep$date <- as.character(rep$date)
rep$dates <- as.Date(rep$date, "%Y-%m-%d")

# remove NAs
data<-na.omit(rep)

# Histogram of the total number of steps taken each day
library(ggplot2)
library(dplyr)
library(knitr)
stepsByDay <- tapply(rep$steps, rep$date, sum, na.rm=TRUE)
barplot(stepsByDay)


#Mean and Median number of steps taken each day
mean(stepsByDay)
# answer = 9354.23
median(stepsByDay)
# answer = 10395

#Time series plot of the average number of steps taken
meanplot <-ggplot(data = data, aes(dates, steps)) +
    stat_summary(fun.y = mean, geom = "bar")
meanplot+ggtitle("Mean Steps per Day")


#The 5-minute interval that, on average, contains the maximum number of steps
stepsByInterval <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
summary(stepsByInterval)
sort(stepsByInterval)[length(stepsByInterval)]
#answer is interval 835

#Code to describe and show a strategy for imputing missing data
library(zoo)
rep$steps<-na.aggregate(rep$steps)
summary(rep)

#Histogram of the total number of steps taken each day after missing values are imputed
stepsByDayNA <- tapply(rep$steps, rep$date, sum, na.rm=TRUE)
barplot(stepsByDayNA)


#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
rep$week <- weekdays(rep$dates)
rep <- mutate(rep, weekpart = ifelse(week == "Saturday" | week=="Sunday", "weekend", "weekday"))
str(rep)
rep$weekpart <- as.factor(rep$weekpart)

byDay<-aggregate(steps ~ interval+weekpart, data = rep, mean)
head(byDay)

ggplot(byDay, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(weekpart ~ .) +
    xlab("5-minute Interval") + 
    ylab("Mean Steps")
