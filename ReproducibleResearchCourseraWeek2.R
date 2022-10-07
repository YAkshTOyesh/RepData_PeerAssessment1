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

# Calculating total number of steps per day
total_num_steps_per_day <-  with(activity_data, tapply(steps, date, sum, na.rm = TRUE))

##  Calculating mean and median of total number of steps per day
summary(total_num_steps_per_day)

##  Plotting a histogram
hist(total_num_steps_per_day, xlab = "Total Number of Steps Per Day", main = "Histogram of Total Number of Steps Per Day")

# Time series plot of average daily pattern 
average_number_of_steps_per_day <- with(activity_data, tapply(steps, date, mean, na.rm = TRUE))
plot(unique(activity_data$date), average_number_of_steps_per_day, type = "l", xlab = "", ylab = "")
title(ylab = "Average Number of Steps Per Day")
summary(average_number_of_steps_per_day)

## Finding 5-minute  on average across all the days in the dataset, contains the maximum number of steps
average_number_of_steps_per_interval_across_all_days <- average_number_of_steps_per_day <- with(activity_data, tapply(steps, interval, mean, na.rm = TRUE))
max_steps <- max(average_number_of_steps_per_interval_across_all_days)
interval <- names(which(average_number_of_steps_per_interval_across_all_days == max_steps))

# Imputing missing values

##  Calculate and report the total number of missing values in the dataset(count NAs)
Total_num_missing_values <- sum(is.na(activity_data))
print(paste("Total number of missing values in the dataset is:", Total_num_missing_values))

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


##  Make a histogram of the total number of steps taken each day 

# Calculating total number of steps per day
total_num_steps_per_day_imputed <-  with(activity_data_imputed, tapply(steps, date, sum))

#  Plotting a histogram
hist(total_num_steps_per_day_imputed, xlab = "Total Number of Steps Per Day", main = "Histogram of Total Number of Steps Per Day After Imputation")


##  Calculate and report the mean and median total number of steps taken per day. 
summary(total_num_steps_per_day_imputed)

##  Answer Questions

# Additional Question

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

#Saving Plot
png(file = "CourseProject2ReproducibleResearch.png")
xyplot(Average_number_of_steps_per_day ~ Interval | week, data = average_5minute_interval_factored_melted, layout =c(1,2), type = "l")
dev.off()

#Create link to GitHub repository
#Link to R
#Create R markdown and Push to repo
#Submit