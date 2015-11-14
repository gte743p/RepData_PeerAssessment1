setwd("C:/Users/534300/Desktop/Work Files/Data Science/Coursera/reprodresearch/RepData_PeerAssessment1")

library("dplyr")
library("ggplot2")

## read and process data
data <- read.csv(file = "activity/activity.csv")

## total steps per day
stepsByDay <- data %>% 
      group_by(date) %>%
      summarize(totalSteps = sum(steps, na.rm = TRUE))

meanDailySteps <- mean(stepsByDay$totalSteps)
medianDailySteps <- median(stepsByDay$totalSteps)


## histogram plot of steps per day
h <- ggplot(stepsByDay, aes(x = totalSteps))
h <- h + geom_histogram(alpha = 0.25, bidnidth = 600)
h <- h + geom_vline(aes(xintercept = mean(totalSteps), color = "Mean"), 
                    size = 1, linetype = "dashed", show_guide = TRUE)
h <- h + geom_vline(aes(xintercept = median(totalSteps), color = "Median"), 
                    size = 1, linetype = "dashed", show_guide = TRUE)
h <- h + theme(legend.title = element_blank())
h + labs(title = "Histogram of Steps per Day",
              x = "Steps", y = "Density")

## average daily activity pattern
stepsByInterval <- data %>%
      group_by(interval) %>%
      summarize(averageSteps = mean(steps, na.rm = TRUE))

## finding interval with max average steps
maxInterval <- stepsByInterval %>%
      filter(averageSteps == max(averageSteps)) %>%
      select(interval, averageSteps)

## plotting steps by Interval
l <- ggplot(stepsByInterval, aes(x = interval, y = averageSteps))
l <- l + geom_line()
l + labs(title = "Average Steps by Time Interval",
         x = "Time Interval", y = "Average Steps")

## handling NA values

## 2304 entries where "steps" is NA
NAdata <- data[is.na(data$steps), ]
numNAs <- nrow(NAdata)

## replace NA values with interval average
NArowNums <- which(is.na(data$steps))                 ## row indices of NA values
NArowIntervals <- NArowNums %% nrow(stepsByInterval)  ## corresponding interval index
NArowIntervals[NArowIntervals == 0] <- 288            ## fix last element
replacementValues <- stepsByInterval[NArowIntervals, 2]

## create new dataset that fills in NA values with replacement values
data_filled <- data
for (i in seq_along(NArowNums)) {
      data_filled[i, 1] <- replacementValues[i, 1]
}

## recalculate mean and median daily step totals
stepsByDayNoNAs <- data_filled %>% 
      group_by(date) %>%
      summarize(totalSteps = sum(steps, na.rm = TRUE))

meanDailyStepsNoNAs <- mean(stepsByDayNoNAs$totalSteps)
medianDailyStepsNoNAs <- median(stepsByDayNoNAs$totalSteps)

## histogram plot of steps per day after romoval of NAs
h <- ggplot(stepsByDayNoNAs, aes(x = totalSteps))
h <- h + geom_histogram(alpha = 0.25, binwidth = 600)
h <- h + geom_vline(aes(xintercept = mean(totalSteps), color = "Mean"), 
                    size = 1, linetype = "dashed", show_guide = TRUE)
h <- h + geom_vline(aes(xintercept = median(totalSteps), color = "Median"), 
                    size = 1, linetype = "dashed", show_guide = TRUE)
h <- h + theme(legend.title = element_blank())
h + labs(title = "Histogram of Steps per Day After Replacing NA Values",
         x = "Steps", y = "Density")


## weekdays vs. weekend
data_filled$weekday <- weekdays(as.Date(data_filled$date), abbreviate = FALSE)

stepsByIntervalWkDay <- data_filled %>% 
      group_by(weekday, interval) %>%
      summarize(averageSteps = mean(steps, na.rm = TRUE)) %>%
      ungroup()

stepsByIntervalWkDay$dayType <- "weekday"
stepsByIntervalWkDay[stepsByIntervalWkDay$weekday %in% c("Saturday", "Sunday"), 4] <- "weekend"

#table(stepsByIntervalWkDay$weekday, stepsByIntervalWkDay$dayType)

l2 <- ggplot(stepsByIntervalWkDay, aes(x = interval, y = averageSteps))
l2 <- l2 + geom_line()
l2 <- l2 + facet_grid(dayType ~ .)
l2 + labs(title = "Average Steps by Time Interval",
         x = "Time Interval", y = "Average Steps")
