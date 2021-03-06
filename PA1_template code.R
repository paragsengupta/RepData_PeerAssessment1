# Setting global options and working directory
library(knitr)
setwd("D:/DataScienceJohnHopkins/Reproducible Research/CourseProject1")
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning=FALSE)

#Loading and preprocessing the data
#(1) Load the data (i.e. read.csv())
#The first line of the data file "activity.csv" has all the variable names and na.strings is to be set as "NA".
if(!file.exists("repdata_data_activity.zip")) {
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
activity <- read.csv("activity.csv")
str(activity)
head(activity)
tail(activity)

# (2) Process/transform the data (if necessary) into a format suitable for your analysis
# Tranfer the date value to data format, which will be used later.

library(dplyr)
activity <- mutate(activity, date = as.Date(as.character(date), "%Y-%m-%d"))

# What is mean total number of steps taken per day?
# (1) Calculate the total number of steps taken per day.
# Two options: Either use group_by and summarise functions of dplyr package or use aggregate function to arrive our aim. Here, the latter is used.

steps_per_day <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

# (2) If you do not understand the difference between a histogram and a barplot, research the difference between them. 
# Difference: a histogram is useful to look at when one want to see more detail on the distribution of a data. The barplot is useful for summarizing categorical data. Following shows the difference between the two types of plots (just as an example)

example <- c(1,2,3,2)
par(mfrow = c(1,2))  # number of plots per row    
hist(example, breaks = 8, col = "green", main = "Plot with hist()")  # plot by hist()
barplot(example, col = "blue", main = "Plot with barplot()")  # plot by barplot

# Make a histogram of the total number of steps taken each day
# The histogram helps to investigate the steps distributions.

hist(steps_per_day$steps, main = "Histogram of the total steps taken per day", xlab = "Steps", breaks = 16, col = "wheat")

# (3) Calculate and report the mean and median of the total number of steps taken per day
# The group_by and summarise/summarize functions of package dplyr are more convinent here

library(dplyr)
mean_median_steps_per_day <- group_by(activity, date) %>%
        summarise(mean = mean(steps, na.rm = TRUE),
                  median = median(steps, na.rm = TRUE))
summary(mean_median_steps_per_day)
# head(mean_median_steps_per_day)
# tail(mean_median_steps_per_day)
dim(mean_median_steps_per_day)

# What is the average daily activity pattern?
# (1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-) and the average number of steps taken, averaged across all days (y-axis)
# (2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

library(dplyr)
interval_steps <- group_by(activity, interval) %>% summarise(mean = mean(steps, na.rm = TRUE))

# The time series plot
with(interval_steps, 
     plot(interval, mean, 
          type = "l", 
          col = "brown", 
          xlab = "5-minute interval",
          ylab = "Average number of steps taken per time interval"))

# The maximum number of steps
max_steps_interval <- interval_steps[which.max(interval_steps$mean), ]$interval
max_steps_interval

# Imputing missing values
# Note that there are a number of days/intervals where there are missing values, coded as NA. The presence of missing days may introduce bias into some calculations or summaries of the data.
# (1) Calculate and report the total number of missing values in the dataset, i.e. the total number of rows with NA.
# First check on every column of the data activity, aiming to detect the NA values.

any(is.na(activity$steps))
any(is.na(levels(activity$date)))
any(is.na(activity$interval))

NA_total <- sum(is.na(activity$steps))
NA_proportion <- sum(is.na(activity$steps)) / nrow(activity)
NA_total
NA_proportion

# (2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# The strategy: Given a NA step value, first try to fill it with the median of that day. If this median result is also NA, then fill it with the mean for that 5-minute interval. In the end, check the NA values of the new data.

# (3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity_copy <- activity  # for a protection of the original data
rm(activity)  # release the memory
l <- nrow(activity_copy)
for (i in 1:l) {
        if (is.na(activity_copy$steps[i])) {
                today = activity_copy$date[i]
                sub_value = mean_median_steps_per_day[mean_median_steps_per_day$date == today, "median"]
                if (!is.na(sub_value)) {
                        activity_copy$steps[i] = sub_value
                } else {
                        pattern = activity_copy$interval[i]
                        sub_value = interval_steps[interval_steps$interval == pattern, "mean"]
                        activity_copy$steps[i] = sub_value
                }
        }
}
sum(is.na(activity_copy$steps))

# summary(activity_copy)
head(activity_copy)

# convert steps column to numeric
activity_copy <- mutate(activity_copy, steps = as.numeric(steps))
write.csv(activity_copy, file = "activity_copy.csv") # save the new data

# (4) Make a histogram of the total number of steps taken each day.
# Calculate and report the mean and median total number of steps taken per day. 

# Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

library(dplyr)
sum_mean_median_day <- group_by(activity_copy, date) %>%
        summarise(sum_steps = sum(steps),
                  mean_steps = mean(steps),
                  median_steps = median(steps))
sum_mean_median_day

summary(sum_mean_median_day)

# Now we plot histograms of the total steps taken per day AFTER and BEFORE the filling of missing data.

hist(sum_mean_median_day$sum_steps, main = "Histogram of the total steps per day AFTER missing data filling", xlab = "Steps", breaks = 16, col = "brown")
hist(steps_per_day$steps, main = "Histogram of the total steps per day BEFORE missing data filling", xlab = "Steps", breaks = 16, col = "blue", ylim = c(0,15))

# Next we plot the mean steps taken per day AFTER and BEFORE the filling of missing data.

plot(sum_mean_median_day$date, sum_mean_median_day$mean_steps, type = "h", pch = 1, col = "red")
points(mean_median_steps_per_day$date, mean_median_steps_per_day$mean, pch = 4)
legend("topleft", pch = c(1,4), col = c("red", "black"), legend = c("before", "after"))

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays function may be of some help here. Use the dataset with the filled-in missing values for this part.
# (1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

class(activity_copy$date)
state <- ifelse(weekdays(activity_copy$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
state <- as.factor(state)
activity_copy$state <- state
str(activity_copy)

# (2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

library(dplyr)
average_steps_state <- group_by(activity_copy, state, interval) %>%
        summarise(average_steps = mean(steps))
str(average_steps_state)

library(ggplot2)
g <- ggplot(average_steps_state, aes(x = interval, y = average_steps)) +
        geom_line() + 
        facet_grid(state ~ .) + 
        labs(x = "Interval") + 
        labs(y = "Number of steps")
g

# plot above picture again using lattice system.
library(lattice)
xyplot(average_steps ~ interval | state, data = average_steps_state, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of steps")

# End of Project

