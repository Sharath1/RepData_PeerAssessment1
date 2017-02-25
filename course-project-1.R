library(sqldf)

download.file("Activity_Data.zip", 
              url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")

unzip(zipfile = "Activity_Data.zip")

activity <- read.csv("activity.csv")

class(activity$date)

# Converting Date from factor to POSIXct

activity$date <- as.POSIXct(activity$date, format = '%Y-%m-%d')

# Assigning relevant column names and adding a field which shows the day of the week
activity <- data.frame(date = activity$date, weekday = weekdays(activity$date), 
                       steps = activity$steps, interval = activity$interval)

#Aggregate the total number of steps per day
daily_activity <- aggregate(activity$steps, by = list(activity$date), FUN = sum, 
                            na.rm = TRUE)
names(daily_activity) <- c("Date", "Agg_activity")

hist(daily_activity$Agg_activity, xlab = "Total number of steps", 
     main = "Histogram of total number of steps taken each day", col = "red", 
     breaks = seq(from = 0, to = 25000, by = 2500))

mean(daily_activity$Agg_activity)
median(daily_activity$Agg_activity)



#Create a new subset of data to aggregate steps by intervals
activity_mean <- aggregate(activity$steps, by = list(activity$interval), 
                           FUN = mean, na.rm = TRUE)

names(activity_mean) <- c("interval", "mean_steps")

plot(activity_mean$interval, activity_mean$mean_steps, type = "l", col = "green", 
     lwd = 2, xlab = "Interval (minutes)", ylab = "Average number of steps", 
     main = "Average number of steps per intervals")

#Finding the 5-minute interval that contains the maximum average number of steps
activity_mean[which(activity_mean$mean_steps ==max(activity_mean$mean_steps)),1]

# Finding the interval with the maximum number of steps on average across all days, is 835
max_step_int <- activity_mean[which.max(activity_mean$mean_steps),]


# The next step is to impute missing values for missing steps with 'NA', this could work
# by replacing NA with 0, but that would sway the average so we replace the average of 
# each interval

activity_with_NA <- activity[(is.na(activity$steps)),]
activity_without_NA <- activity[!(is.na(activity$steps)),]

#impute the average steps in place of missing values
activity_with_NA_imp <- sqldf("SELECT date, weekday, mean_steps, interval 
                              FROM activity_with_NA
                              JOIN activity_mean USING(interval)")

names(activity_with_NA_imp)[names(activity_with_NA_imp) == "mean_steps"] <- "steps"

rbind(activity_with_NA_imp,activity_without_NA)

activity_imputed <- rbind(activity_with_NA_imp,activity_without_NA)

daily_act_imputed <- aggregate(activity_imputed$steps, 
                               by=list(activity_imputed$date), FUN = sum)
names(daily_act_imputed) <- c("date","Agg_activity")

hist(daily_act_imputed$Agg_activity, xlab = "Total number of steps", 
     main = "Histogram of total number of steps taken each day 
     (NA was replaced with mean)", col = "red", 
     breaks = seq(from = 0, to = 25000, by = 2500))

mean(daily_act_imputed$Agg_activity)
median(daily_act_imputed$Agg_activity)


# The days are split into weekdays and weekends

activity_imputed <- cbind(activity_imputed, 
                          daytype = ifelse(activity_imputed$weekday
                                           == "Saturday" |
                          activity_imputed$weekday == "Sunday"
                          , "weekend", "weekday"))

activity_mean <- aggregate(activity_imputed$steps,
                           by = list(activity_imputed$daytype, 
                                     activity_imputed$weekday, 
                                     activity_imputed$interval), mean)

names(activity_mean) <- c("daytype","weekday","interval", "mean_steps")

xyplot(mean_steps ~ interval | daytype, 
       activity_mean, type = "l", lwd = 1, 
       xlab = "Interval", ylab = "Number of steps", 
       layout = c(1,2))


