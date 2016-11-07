#######################################################################################
# Basic R Code For First Peer Reviewed Programming Assignment for Reproducible Research
#######################################################################################

library(ggplot2)
library(timeDate)

#Read Data
activity <- read.csv('activity.csv')



#ANALYZE STEPS TAKEN IN A DAY

#Sum Steps By Date
daydata <- aggregate(activity$steps, by=list(date=activity$date), FUN=sum)
colnames(daydata)[2] <- "steps"

#Histogram of total number of steps in a day
hist(daydata$steps, 
     main = 'Total Steps', 
     xlab = 'Total Steps Taken (per day)',
     col = "red"  )

mean_s <- mean(daydata$steps, na.rm=TRUE)
med_s  <- median(daydata$steps, na.rm=TRUE)


#ANALYZE STEPS TAKEN IN EACH INTERVAL

#Mean Steps In Each 5 Minute Interval
intdata <- aggregate(activity$steps, by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
colnames(intdata)[2] <- "mean_steps"


ggplot(intdata, aes(interval, mean_steps)) + geom_line() +
  xlab("5 minute intervals") + ylab("Avg Steps Across Days")

#5 minute interval with max average number of steps (across days)
maxsteps <- round( max(intdata$mean_steps),0)
maxint <- intdata[intdata$mean_steps > maxsteps, ]
maxsteps <- round(maxint$mean_steps, 0)





#IDENTIFY MISSING VALUES AND IMPUT

miss_int <- sum(is.na(activity$steps))

#Combine original activity data with intdata (has mean number of steps in each interval)
imputdata <- merge(activity, intdata, by="interval", all=TRUE)
imputdata <- imputdata[order(imputdata$date, imputdata$interval),]

#Replace NA with mean_steps
imputdata$steps <- ifelse( is.na(imputdata$steps), imputdata$mean_steps, imputdata$steps)


#Recreate Initial Analysis of Steps Taken In A Day (imputed data)
#Sum Steps By Date
imp_daydata <- aggregate(imputdata$steps, by=list(date=imputdata$date), FUN=sum)
colnames(imp_daydata)[2] <- "steps"

#Histogram of total number of steps in a day
hist(imp_daydata$steps, 
     main = 'Total Steps - Imputed', 
     xlab = 'Total Steps Taken (per day)',
     col = "red"  )

mean_s <- mean(imp_daydata$steps, na.rm=TRUE)
med_s  <- median(imp_daydata$steps, na.rm=TRUE)



#COMPARE WEEKENDS TO WEEKDAYS

#Define Weekends and Weekdays
library(timeDate)
imputdata$daytype <- isWeekday(imputdata$date)
imputdata$daytype <- ifelse( imputdata$daytype, "weekday", "weekend")


#Mean Steps In Each 5 Minute Interval - by daytype
intdata2 <- aggregate(imputdata$steps, by=list(interval=activity$interval, daytype=imputdata$daytype),
                      FUN=mean)
colnames(intdata2)[3] <- "mean_steps"

#Graph The Two Daytypes
ggplot(intdata2, aes(interval, mean_steps)) + geom_line() + facet_grid(daytype ~ .) +
  xlab("5 minute intervals") + ylab("Avg Steps Across Days")








