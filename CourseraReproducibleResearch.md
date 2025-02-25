---
title: "CourseraProject1ReproducibleResearch"
author: "Yaksh Toyesh Ujoodha"
date: "2022-10-05"
output: html_document
---
### Loading and preprocessing the data

```r
library(dplyr)
library(tidyr)
library(reshape2)
library(lattice)
#Unzipping file
path_dataset <- "./repdata_data_activity.zip"
unzip(path_dataset)

#Reading file
activity_data <- read.csv("activity.csv", header = TRUE)
activity_data_imputed <- activity_data

#Converting date column to date class
activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")
```
###What is mean total number of steps taken per day?

Here is a histogram of the Total Numbe rof Steps Taken Per Day?

```r
# Calculating total number of steps per day
total_num_steps_per_day <-  with(activity_data, tapply(steps, date, sum, na.rm = TRUE))

##  Plotting a histogram
hist(total_num_steps_per_day, xlab = "Total Number of Steps Per Day", main = "Histogram of Total Number of Steps Per Day")
```

![plot of chunk histogram total](figure/histogram total-1.png)

Here is a summary of the mean and median of the total numbe rof steps taken per day

```r
##  Calculating mean and median of total number of steps per day
summary(total_num_steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
###What is the average daily activity pattern?

Here is a time series plot of the average daily pattern

```r
# Time series plot of average daily pattern 
average_number_of_steps_per_day <- with(activity_data, tapply(steps, date, mean, na.rm = TRUE))
plot(unique(activity_data$date), average_number_of_steps_per_day, type = "l", xlab = "", ylab ="")
```

![plot of chunk daily activity pattern](figure/daily activity pattern-1.png)

```r
## Finding 5-minute interval on average across all the days in the dataset, contains the maximum number of steps
average_number_of_steps_per_interval_across_all_days <- average_number_of_steps_per_day <- with(activity_data, tapply(steps, interval, mean, na.rm = TRUE))
max_steps <- max(average_number_of_steps_per_interval_across_all_days)
interval <- names(which(average_number_of_steps_per_interval_across_all_days == max_steps))
```

The 5-minute interval on average across all the days in the dataset, contains the maximum number of steps is 835.

###Imputing missing values

```r
# Imputing missing values

##  Calculate and report the total number of missing values in the dataset(count NAs)
Total_num_missing_values <- sum(is.na(activity_data))

##  Devise a strategy for filling up missing values such as using mean/median(basic)

#Obtaining list of missing values
activity_data_of_missing_values <- activity_data %>% subset(is.na(steps))
list_of_missing_values_by_date <- unique(as.character(activity_data_of_missing_values$date))

#Creating impute values
Mean_values_filled <- activity_data %>% 
  group_by(date) %>% 
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm =TRUE), steps)) %>% 
  summarise(steps = mean(steps)) %>% 
  fill(steps, .direction = "updown")

#Extracting only impute values
Impute_values <- Mean_values_filled[as.character(Mean_values_filled$date) %in% list_of_missing_values_by_date,]

#Assigning impute values to NAs only
for (index in 1:8){
  activity_data_of_missing_values[as.numeric(activity_data_of_missing_values$date) %in% as.numeric(Impute_values[index,1]),1] <- Impute_values[index, 2]
}

##  Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Replacing NAs by impute values in original dataframe
activity_data_imputed[is.na(activity_data$steps),1] <- activity_data_of_missing_values$steps
```
Total number of NAs in the dataset is 2304.

Below is the histogram for the total number of steps taken each day using the imputation dataset.

```r
##  Make a histogram of the total number of steps taken each day 

# Calculating total number of steps per day
total_num_steps_per_day_imputed <-  with(activity_data_imputed, tapply(steps, date, sum))

#  Plotting a histogram
hist(total_num_steps_per_day_imputed, xlab = "Total Number of Steps Per Day", main = "Histogram of Total Number of Steps Per Day After Imputation")
```

![plot of chunk histogram imputed](figure/histogram imputed-1.png)

Below is the mean and median of total number of steps taken per day.

```r
##  Calculate and report the mean and median total number of steps taken per day. 
summary(total_num_steps_per_day_imputed)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8821   10600   10441   12811   21194
```

Do these values differ from the estimates from the first part of the assignment?

Yes, median before was 10395, now after imputation it is 10600. For the mean, before it was 9354, after imputation it is 10441.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

Imputing missing data creates an overestimate in this case.

###Are there differences in activity patterns between weekdays and weekends?

Below is a panel plot showcasing differences in activity patterns between weekdays and weekends.

```r
##  Create factor variable
#Converting to date object
activity_data_imputed$date <- as.Date(activity_data_imputed$date)

#Creating factor variables for weekdays and weekend
weekends <- c("Saturday", "Sunday")
activity_data_imputed$week <- factor(weekdays(activity_data_imputed$date)%in% weekends, levels = c(FALSE, TRUE), labels = c("weekday","weekend"))

##  Create panel plot using lattice, base plot or ggplot2

#Creating new data frame containing average steps taken 5-minute intervals
average_5minute_interval_factored <- with(activity_data_imputed, tapply(steps, list(interval,week),mean))
average_5minute_interval_factored <- as.data.frame(average_5minute_interval_factored)

#melt data frame 
average_5minute_interval_factored$interval <- unique(activity_data_imputed$interval)
average_5minute_interval_factored_melted <- melt(average_5minute_interval_factored, id = c("interval"))
average_5minute_interval_factored_melted <- rename(average_5minute_interval_factored_melted, week = variable, Average_number_of_steps_per_day = value, Interval = interval)

#Plotting using lattice system
xyplot(Average_number_of_steps_per_day ~ Interval | week, data = average_5minute_interval_factored_melted, layout =c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)
