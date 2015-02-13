Sys.setlocale("LC_TIME", "C")

library(data.table)
library(ggplot2)
# library(lubridate)

unzip(zipfile = 'activity.zip')
activityData <- fread('activity.csv')
activityData[, date := as.POSIXct(activityData[, date], format="%Y-%m-%d")]

stepsPerDay <- activityData[, list(totalSteps = sum(steps)), by = date]
hist(stepsPerDay$totalSteps, 
     col  = 'Red',
     main = 'Histogram of total steps per day',
     xlab = 'Number of steps',
     ylab = 'Days'
)
meanStepsPerDay   <- stepsPerDay[, mean(totalSteps, na.rm = TRUE)]
medianStepsPerDay <- stepsPerDay[, median(totalSteps, na.rm = TRUE)]

dailyActivity <- activityData[, 
                              list(avgSteps = mean(steps, na.rm = TRUE)), 
                              by = interval
                 ]

dailyActivity[, 
              time := as.POSIXct(sprintf("%05.2f", 
                                         dailyActivity$interval/100
                                 ), 
                                 format = "%H.%M"
                      )
]

with(dailyActivity, 
     plot(avgSteps ~ time, 
          type = 'l',
          col = 'Black',
          main = 'Average Steps per 5 Minute Interval',
          xlab = 'Time',
          ylab = 'Average Steps'
     )
)
intervalOfMaxSteps <- dailyActivity[which.max(dailyActivity$avgSteps), interval]

avgSteps <- function(x) { return(dailyActivity[interval == x]$avgSteps) }

missingRows <- which(is.na(activityData$steps))
totalMissingRows <- length(missingRows)

cleanActivityData <- fread('activity.csv')
cleanActivityData[, date := as.POSIXct(cleanActivityData[, date], format="%Y-%m-%d")]
for (i in missingRows) {
    cleanActivityData[i, steps := as.integer(avgSteps(cleanActivityData[i, interval]))]
}

cleanStepsPerDay <- cleanActivityData[, list(totalSteps = sum(steps)), by = date]
hist(cleanStepsPerDay$totalSteps, 
     col  = 'Red',
     main = 'Histogram of total steps per day - NAs removed',
     xlab = 'Number of steps',
     ylab = 'Days'
)

meanCleanStepsPerDay   <- cleanStepsPerDay[, mean(totalSteps, na.rm = TRUE)]
medianCleanStepsPerDay <- cleanStepsPerDay[, median(totalSteps, na.rm = TRUE)]

cleanActivityData[, 
                  dayType := ifelse(is.element(weekdays(cleanActivityData[, date]), 
                                               c('Saturday', 'Sunday')
                                              ), 
                                    'Weekend', 
                                    'Weekday'
                             )
                 ]
cleanActivityData[, dayType := as.factor(dayType)]

cleanDailyActivity <- cleanActivityData[, 
                              list(avgSteps = mean(steps, na.rm = TRUE)), 
                              by = list(interval, dayType)
                              ]
with(cleanDailyActivity[dayType == "Weekday"], 
     plot(avgSteps ~ interval, 
          type = 'l', 
          col = 'Blue',
          main = 'Average Steps per 5 Minute Interval - NAs Removed',
          xlab = 'Time',
          ylab = 'Average Steps'
     )
)
with(cleanDailyActivity[dayType == "Weekend"], 
     lines(avgSteps ~ interval, 
           col = 'Red'
     )
)

g <- qplot(interval, 
           avgSteps, 
           data   = cleanDailyActivity, 
           geom   = 'line',
           xlab   = 'Time',
           ylab   = 'Average Steps',
           title  = 'Average Steps per 5 Minute Interval - NAs Removed',
           facets = dayType ~ .
)

print(g)
