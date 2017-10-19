#Read and load Data
activityData <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
#Show Data summary
summary(activityData)
#     steps                date          interval     
#Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
#1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
#Median :  0.00   2012-10-03:  288   Median :1177.5  
#Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
#3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
#Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
#NA's   :2304     (Other)   :15840  
#Reformat
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")
activityData$interval <- factor(activityData$interval)
#Ignore missing numbers
NA_index <- is.na(as.character(activityData$steps))
activityData_no_NA <- activityData[!NA_index,]
head(activityData_no_NA)
#    steps       date interval
#289     0 2012-10-02        0
#290     0 2012-10-02        5
#291     0 2012-10-02       10
#292     0 2012-10-02       15
#293     0 2012-10-02       20
#294     0 2012-10-02       25
#Daily steps
DailySteps <- aggregate(steps ~ date, data = activityData_no_NA, sum)
colnames(DailySteps) <- c("date", "steps")
#Generate histogram of daily steps
hist(as.numeric(DailySteps$steps), breaks = 20, col = "purple", xlab = "Number of Steps", main= "Histogram of Daily Step Total")
#Mean of daily steps
mean(DailySteps$steps)
#[1] 10766.19
#Median daily steps
median(DailySteps$steps)
#[1] 10765
#Find average steps for all days
averageSteps <- aggregate(activityData_no_NA$steps, by=list(interval=activityData_no_NA$interval), FUN=mean)
colnames(averageSteps) <- c("interval", "average_steps")
#Plot average steps
plot(as.integer(levels(averageSteps$interval)), averageSteps$average_steps, type="l",
     xlab = "Interval", ylab = "Step Average", main = "Daily Step Pattern",  col ="deeppink3")
#Max activity interval
MaxActivityAverage <- max(averageSteps$average_steps)
MaxActivityAverage
#[1] 206.1698
#Interval with most steps
MaxStepInterval<-averageSteps[which.max(averageSteps$average_steps),]$interval
MaxStepInterval
#[1] 835
#288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 125 130 135 ... 2355
#Fill in missing values
NA_index <- which(is.na(as.character(activityData$steps)))
DataWhole <- activityData
#Use interval mean to fill in data
DataWhole[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
  averageSteps[activityData[NA_index,]$interval==averageSteps$interval,]$average_steps
}))
#Check data
summary(DataWhole)
#steps             date               interval    
#Min.   :  0.00   Min.   :2012-10-01   0      :   61  
#1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
#Median :  0.00   Median :2012-10-31   10     :   61  
#Mean   : 37.38   Mean   :2012-10-31   15     :   61  
#3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
#Max.   :806.00   Max.   :2012-11-30   25     :   61  
#(Other):17202 
#Make histogram of complete data set
DailyStepsWhole <- aggregate(steps ~ date, data = DataWhole, sum)
colnames(DailyStepsWhole) <- c("date", "steps")
hist(as.numeric(DailyStepsWhole$steps), breaks = 20, col = "seagreen", xlab = "Number of Steps", main= "Complete data daily steps histogram")
#Mean of daily steps with complete data
mean(DailyStepsWhole$steps)
#[1] 10766.19
#Median of daily steps with complete data
median(DailyStepsWhole$steps)
#[1] 10766.19
#Determine differences between weekdays and weekends
DataWhole$day <- as.factor(weekdays(DataWhole$date))
DataWhole$is_weekday <- ifelse(!(DataWhole$day %in% c("Saturday","Sunday")), TRUE, FALSE)
#Average weekday steps
WeekdayData <- DataWhole[DataWhole$is_weekday,]
WeekdayIntervalSteps <- aggregate(WeekdayData$steps, by=list(interval=WeekdayData$interval), FUN=mean)
#Average weekend steps
WeekendData <- DataWhole[!DataWhole$is_weekday,]
WeekendIntervalSteps <- aggregate(WeekendData$steps, by=list(interval=WeekendData$interval), FUN=mean)
#Add column labels
colnames(WeekdayIntervalSteps) <- c("interval", "average_steps")
colnames(WeekendIntervalSteps) <- c("interval", "average_steps")
#Label Column
WeekdayIntervalSteps$day <- "Weekday"
WeekendIntervalSteps$day <- "Weekend"
#Bind data
FullWeekData<- rbind(WeekendIntervalSteps, WeekdayIntervalSteps)
FullWeekData$day <- as.factor(FullWeekData$day)
#Plot Weekday and Weekend Data
library(lattice)
xyplot(average_steps ~  interval | day, data = FullWeekData, layout = c(1,2), type ="l", ylab="Number of Steps")

