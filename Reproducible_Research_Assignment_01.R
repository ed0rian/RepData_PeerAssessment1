Sys.setlocale("LC_TIME", "C")

library(data.table)
library(ggplot2)

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
meanStepsPerDay   <- as.integer(stepsPerDay[, mean(totalSteps, na.rm = TRUE)])
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
g <- qplot(time, 
           avgSteps, 
           data   = dailyActivity, 
           geom   = 'line',
           xlab   = 'Time',
           ylab   = 'Average number of steps'
)
gTitle <- 'Average number of steps per 5 minute interval'
print(g + ggtitle(gTitle))
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

meanCleanStepsPerDay   <- as.integer(cleanStepsPerDay[, mean(totalSteps, na.rm = TRUE)])
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
cleanDailyActivity[, 
                   time := as.POSIXct(sprintf("%05.2f", 
                                              cleanDailyActivity$interval/100
                                             ), 
                                      format = "%H.%M"
                           )
              ]

g <- qplot(time, 
           avgSteps, 
           data   = cleanDailyActivity, 
           geom   = 'line',
           xlab   = 'Time',
           ylab   = 'Average number of steps',
           facets = dayType ~ .
)
gTitle <- 'Average number of steps per 5 minute interval - NAs removed'
print(g + ggtitle(gTitle))
