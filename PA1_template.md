Reproducible Research Course Project 1"
Prepared 2017-07-28 by Justin Goldstein using RStudio v.1.0.143
=====================================================================================

## We load the necessary files and libraries from a directory on the analyst's laptop into R version 3.4.1.  
Data are obtained from the page accessble at: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
Our main dataframe will be called *activity*, which contains three (3) columns: *steps* (total number of steps during 5-minute interval), 
*date*, and *interval* (number of five-minute interval).


```r
setwd("C:\\Users\\gold9629\\Documents")
activity<-read.csv("activity.csv", header=TRUE, sep=",")
library(knitr)
library(dplyr)
par(mfrow=c(1,1))
```

---

## We compute a **histogram** of the total number of steps taken each day.  
We perform the analysis by using the "tapply" command to sum the
steps taken each day.  *Steps* having "NA" values are ignored during this analysis. 


```r
#Aggregates steps by day.
totalstepsperday<-tapply(activity$steps, activity$date, sum, na.rm=TRUE) 

#Plots histogram
hist(totalstepsperday, xlab="Steps Per Day", main="Histogram of\n Total Steps per Day")
```

![plot of chunk totalstepshistogram](figure/totalstepshistogram-1.png)

---

## We then compute the **median** and **mean** values,
respectively, for steps per day, 
using the *totalstepsperday* value mentioned in the chunk above.


```r
mediantotalstepsday<-median(totalstepsperday)
meantotalstepsday<-mean(totalstepsperday)
```

The median steps per day is 10395.
The mean steps per day is 9354.2295082.

---

## We compute a time series plot of the average number of steps taken daily.  
Again, we employ the *tapply* command but this time compute the mean instead of the sum.



```r
#Computes mean and total number of steps per day
meanstepsperday<-tapply(activity$steps, activity$date, mean) %>%
  as.data.frame()
totalstepsperday<-tapply(activity$steps, activity$date, sum) %>%
  as.data.frame()

#Plot
activity$date<-as.Date(as.character(activity$date), "%Y-m-%d")
names(meanstepsperday) = "Steps"
plot(meanstepsperday$Steps, xlab="Steps", ylab="Date", main="Mean Number of Steps per Day")
lines(meanstepsperday[,1])
```

![plot of chunk avg_steps_day](figure/avg_steps_day-1.png)

---

## We now calculate the 5-minute interval that, on average, contains the maximum number of steps.  
We do this by using the "tapply" command to aggregate the mean number of steps by interval
and locating the number of steps mentioned above.



```r
#Aggregates mean number of steps by interval
stepsmean<-tapply(activity$interval,  activity$steps, mean) %>%
as.data.frame()
names(stepsmean) = "Steps"
maxstepsmean<-max(stepsmean)                    # Produces max steps:  2050
# but need"interval" value for index
answer<-which(stepsmean$Steps == maxstepsmean) # Just interested in x-val: Produces 591.
answer<-names(answer)

#Plot
plot(stepsmean, type="l", xlab="Interval",
ylab="Steps Taken", main="Number of steps\n taken on average, averaged across all days", cex = 0.7)
```

![plot of chunk fivemininterval](figure/fivemininterval-1.png)

The interval which has the highest average number of steps is 591, 
producing an average value of 2050 steps.


---

## We now display a code to describe and show a strategy for **imputing** missing data.
Here, due to the fact that there are entire days without measurements of steps, we impute 
these missing values using the median value of all steps (i.e., not those with "NA") values.  

A histogram is subsequently created.  New median and mean steps per day values are created 
identically to the fashion mentioned above, except using these imputed values.



```r
numberNAs<-sum(is.na(activity$steps))  # Quantifies number of "NA"s in "steps" column
numberNAsavg<-100*numberNAs/length(activity[,1])

activitynona<-activity           # Creates an identical dataset

# Perform the imputation
for (i in 1:length(activitynona$steps)) {
if (is.na(activitynona$steps[i])) {
activitynona$steps[i] <- median(activity$steps,na.rm =TRUE)
}
}

# Create the histogram
hist(totalstepsperday, xlab = "Steps per Day", 
main = "Histogram of data when NAs are imputed")
```

```
## Error in hist.default(totalstepsperday, xlab = "Steps per Day", main = "Histogram of data when NAs are imputed"): 'x' must be numeric
```

```r
#Computes new median and mean steps per day values.
totalstepsperday<-tapply(activitynona$steps, activitynona$date,
sum, na.rm=TRUE)
meantotalstepsday<-mean(totalstepsperday)
mediantotalstepsday<-median(totalstepsperday)
```

The "steps" column in the "activity" dataframe contains 2304 missing values which is 
equivalent to 2304 of all those values.

As the median value for all steps is zero, all the "NA" values in the "steps" 
column of this new otherwise identical dataset were replaced with zeroes.

This new dataset (which doesn't contain "NA" values) is called *activitynona*.  
A histogram of the steps is practically identical to that shown above.

The mean total of steps taken per day was derived by summing up the number of steps taken per day and 
then averaging across all days.  The median was computed by taking the median of the daily measurements.  
These values: NaN (mean) and NA (median) are practically identical to 
those displayed above (i.e., before imputation of "NA" values).

---
  
## We now produce a panel plot comparing the average number of steps taken per 5-minute interval
across **weekdays** and **weekends**.  We do this by (1) copying the "activity" dataset to one called 
*activitynona* and then using the "weekdays" command on the "date" field.


```r
#Prepare file formats
activitynona$date <- as.Date(activitynona$date)
activitynona$daytype<-weekdays(activitynona$date) %>%
  as.vector()

# Check to see to which of two categories: "weekend" or "weekday," an obs belongs
for (i in 1:length(activitynona$daytype)) {
  if (activitynona$daytype[i] %in% c("Saturday", "Sunday")) { 
    activitynona$daytype[i] = "weekend"
  } else {
    activitynona$daytype[i] = "weekday"
  }
}

#Subset by *activitynona* value for "weekday" or "weekend"
weekendinfo<-subset(activitynona, activitynona$daytype == "weekend")
weekdayinfo<-subset(activitynona, activitynona$daytype == "weekday")

#Plot
par(mfrow=c(1,2))
stepsweekendmean <- tapply(weekendinfo$steps, weekendinfo$interval, mean)
  plot(stepsweekendmean, main="Mean Interval of Steps\n Taken on Weekends", cex = 0.8, xlab = "Mean Number of Steps", ylab = "Interval")
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Error in plot.window(...): need finite 'xlim' values
```

```r
  points(stepsweekendmean, pch=1)
  lines(stepsweekendmean, col=1)


stepsweekdaymean <- tapply(weekdayinfo$steps, weekdayinfo$interval, mean) 
  plot(stepsweekdaymean, main="Mean Interval of Steps\n Taken on Weekdays",
       xlab = "Mean Number of Steps", ylab = "Interval", cex = 0.8)
```

![plot of chunk weekends_weekdays](figure/weekends_weekdays-1.png)

```r
  points(stepsweekdaymean, pch=1)
  lines(stepsweekdaymean, col=1)
```

The nearly identical nature of these plots indicates that one's step exercise routine doesn't seem to vary 
according to weekend or weekdays.  This may be a function of the aforementioned imputation routine, in which 
the "NA" step values (which comprised of approxiately 13.1147541 % of the total "steps" values) 
were substituted with the median of all step values, which turned out to be zero.     



