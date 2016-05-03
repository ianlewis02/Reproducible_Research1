# Reproducible Research - Assignment One   #

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

  **steps:** Number of steps taking in a 5-minute interval (NA are missing values)
  
  **date:** The date on which the measurement was taken in YYYY-MM-DD format
  
  **interval:** Identifier for the 5-minute interval in which measurement was taken
  
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


##  Load the required R Libraries and Suppress Messages
    suppressMessages(library(dplyr))
    suppressMessages(library(ggplot2))
    suppressMessages(library(knitr))
    suppressMessages(library(lubridate))
    suppressMessages(library(scales))

##  Load Dataset and Transform Date
    setwd("C:/Users/ian/R/Rprogramming/assignment31")
    activity <- read.csv("activity.csv",header = TRUE, sep = ',')
    activity$date <- ymd(activity$date)
    	steps <- activity %>% 
    	filter(!is.na(steps)) %>%
    	group_by(date) %>% 
    	summarize(steps = sum(steps))

##  What is mean total number of steps taken per day?  ##
###   Plot Histogram of the Total Number of Steps Taken per Day
    ggplot(steps, aes(x = steps)) +
        geom_histogram(fill = "mistyrose3", col=I("black"), binwidth = 1000) +
        labs(title = "Total Number of Steps Taken Each Day",
             x = "Steps per Day", 
             y = "Occurances")

![](http://i.imgur.com/ByVLhPz.png)

###   Calculate and report the mean and median of the total number of steps taken per day
    mean.steps <- round(mean(steps$steps, na.rm=TRUE))
    median.steps <- round(median(steps$steps, na.rm=TRUE))
    paste("The Mean number of steps is",mean.steps,"and the Median is",median.steps)

**[1] "The Mean number of steps is 10766 and the Median is 10765"**

##  What is the average daily activity pattern?  ##
    activity$interval <- as.POSIXct(strptime(sprintf("%04d", activity$interval), "%H%M"))
    interval <- activity %>% 
        filter(!is.na(steps)) %>% 
            group_by(interval) %>% 
                summarize(steps = mean(steps))

###   Make a time-plot of the 5-minute interval and average number of steps taken, averaged across all days
    ggplot(interval, aes(x=interval, y=steps)) +
        geom_line(color = "mistyrose4") +
        scale_x_datetime(breaks = date_breaks("2 hour"), 
            labels = date_format("%H:%M"),
            limits = c(interval$Interval[1], interval$Interval[288])) +
        labs(title = "Average Number of Steps taken (Averaged Across All Days)", 
             x = "Time of Day", 
             y = "Average Steps")

![](http://i.imgur.com/RxCGvq0.png)

###   Which 5-minute interval, on average across all the days, contains the maximum number of steps?
    max.interval <- interval[which.max(interval$steps),]
    paste("The interval with the maximum number of steps is",
        round(max.interval$steps[1]), "at",
        strftime(max.interval$interval[1], format="%H:%M:%S"))

**[1] "The interval with the maximum number of steps is 206 at 08:35:00"**

There are a number of days/intervals where there are missing values (coded as NA). The presence of such days may introduce bias into some calculations or summaries of the data.
    
###   Calculate and report the total number of missing values in the dataset 
    missing.steps <- sum(is.na(activity$steps))
    paste("The number of missing values in the dataset is",missing.steps)

**[1] "The number of missing values in the dataset is 2304"**

###   Devise a strategy for filling in all of the missing values in the dataset. 
    tofill.activity <- activity
    missing.activity <- is.na(tofill.activity$steps)
    mean.interval <- tapply(tofill.activity$steps,
        tofill.activity$interval, 
        mean, na.rm=TRUE, simplify=TRUE)

###   Create a new dataset (equal to the original) but with the missing data filled in.
    tofill.activity$steps[missing.activity] <- mean.interval[as.character(tofill.activity$interval[missing.activity])]

###   Make a histogram of the total number of steps taken each day
    filled_steps <- tofill.activity %>%
        filter(!is.na(steps)) %>% 
            group_by(date) %>% 
                summarize(steps = sum(steps))
    ggplot(filled_steps, aes(x = steps)) +
        geom_histogram(fill = "mistyrose3",colour="Black",binwidth = 1000) +
        labs(title = "Total Number of Steps Each Day (missing values replaced)", 
        x = "Steps per day",
        y = "Occurances")

![](http://i.imgur.com/4IrqjXe.png)

###   Calculate and report the mean and median total number of steps taken per day   ###
    mean.steps <- round(mean(filled_steps$steps, na.rm=TRUE))
    median.steps <- round(median(filled_steps$steps, na.rm=TRUE))
    paste("The Mean number of steps is",mean.steps,"and the Median is",median.steps)

**[1] "The Mean number of steps is 10766 and the Median is 10766"**

###   Do these values differ from the estimates from the first part of the assignment?   ###
    imputed <- mutate(tofill.activity, 
        weektype = ifelse(weekdays(tofill.activity$date) == "Saturday" |
        weekdays(tofill.activity$date) == "Sunday", "Weekend", "Weekday"))
    imputed$weektype <- as.factor(imputed$weektype)
    imputed.full <- imputed %>%
        group_by(interval, weektype) %>%
            summarise(steps = mean(steps)) 
    ggplot(imputed.full, aes(x=interval, y=steps, color = weektype)) +
        geom_line(color = "mistyrose4") +
        facet_wrap(~weektype, ncol = 1, nrow=2)+
        scale_x_datetime(breaks = date_breaks("2 hour"),
            labels = date_format("%H:%M"),
            limits = c(imputed.full$Interval[1],
            imputed.full$Interval[288])) +
      labs(title = "Average Number of Steps taken (Weekdays vs. Weekends)",
            x = "Time of Day", 
            y = "Average Steps")

![](http://i.imgur.com/Q7ZzQ55.png)

#  To submit the assignment:
Commit your completed PA1_template.Rmd file to the master branch of your git repository (you should already be on the master branch unless you created new ones). Commit your PA1_template.md and PA1_template.html files produced by processing your R markdown file with knit2html() function in R (from the knitr package) by running the function from the console.
