**Load libraries**

    library(data.table)

    ## Warning: package 'data.table' was built under R version 3.1.3

    library("lubridate")

    ## Warning: package 'lubridate' was built under R version 3.1.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, mday, month, quarter, wday, week, yday, year

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(grid)
    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.1.3

    library(grid)
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.1.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lattice)
    library(knitr) 

    ## Warning: package 'knitr' was built under R version 3.1.3

    #setwd("/Volumes/KINGSTON/moocs/coursera/datascience/represearch/week1")
    setwd('f:/moocs/coursera/datascience/represearch/week1')

**Read in activity file**

    f1 <- file.path(getwd(), "activity.csv")
    activeData <- read.table(f1,sep=',',stringsAsFactors=FALSE,header=TRUE)
    #head(activeData)

*1. Transform data so that we have total steps taken each day*

    totalStepsPerDay <- tapply(activeData$steps, activeData$date, sum, na.rm=TRUE)
    #head(totalStepsPerDay)

*2. Histogram of total steps taken per day*

    hist(totalStepsPerDay, main= "histogram of the total number of steps taken each day", xlab="Total steps taken per day", ylab="Count", las=1,breaks=30, col="green", xlim=c(0,30000), ylim=c(0,10))

![](pa1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)<!-- -->

*3. mean and median of the total number of steps taken per day*

    mean(totalStepsPerDay)

    ## [1] 9354.23

    median(totalStepsPerDay)

    ## [1] 10395

**Mean of totalSteps Per Day: 9354.23**

**Median of total Steps Per Day: 10395**

### What is the average daily activity pattern?

*1. Make a time series plot of the 5-minute interval (x-axis) and the
average number of steps taken, averaged across all days (y-axis)*

    aveSteps <- aggregate(x=list(steps=activeData$steps), by=list(interval=activeData$interval),FUN=mean, na.rm=TRUE)
    #head(aveSteps)                     

    plot(aveSteps,type='l', xlab("5-minute interval"), ylab("average number of steps taken"),main="time series plot of the average number of steps taken")

![](pa1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)<!-- -->

*2. Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?*

    aveSteps[which.max(aveSteps$steps),]

    ##     interval    steps
    ## 104      835 206.1698

            interval    steps

    104      835      206.1698

### Imputing missing values

*1. Calculate and report the total number of missing values in the
dataset (i.e. the total number of rows with ð½ð°s)*

    missStepsData <- is.na(activeData)
    sum(missStepsData)

    ## [1] 2304

**Total number of missing values: 2304**

*2. Devise a strategy for filling in all of the missing values in the
dataset.*

*3. Create a new dataset that is equal to the original dataset but with
the missing data filled in*

    activeData2<-activeData #Create a new data set
    avgStepsPerInterval <- tapply(activeData2$steps, activeData2$interval, mean, na.rm=TRUE, simplify=TRUE)
    #head(avgStepsPerInterval)

    #is.na(activeData2$steps) #creates a vector of the logical value of NA's as TRUE
    #which(is.na(activeData2$steps)) #creates a vector of the indices of the TRUE values

    #use the indices to change the NA values to mean of steps per day
    activeData2$steps[which(is.na(activeData2$steps))] <- avgStepsPerInterval
    #head(activeData2)

*4. Histogram, Mean and Median*

    totalStepsPerDay2 <- tapply(activeData2$steps, activeData2$date, sum, na.rm=TRUE)
    #head(totalStepsPerDay2)
    hist(totalStepsPerDay2 , main= "histogram of the total number of steps taken each day", xlab="Total steps taken per day", ylab="Count", las=1,breaks=30, col="green", xlim=c(0,25000), ylim=c(0,20))

![](pa1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)<!-- -->

    mean(totalStepsPerDay2)

    ## [1] 10766.19

    median(totalStepsPerDay2)

    ## [1] 10766.19

**Mean of totalSteps Per Day: 10766.19**

**Median of total Steps Per Day: 10766.19**

### Are there differences in activity patterns between weekdays and weekends?

*1. Create a new factor variable in the dataset with two levels â
âweekdayâ and âweekendâ.*

    activeData3<-activeData2

    weekend <- weekdays(as.Date(activeData3$date)) %in% c("Saturday", "Sunday") #logical vector of weekend (TRUE) and weekdays(FALSE)

    activeData3['daytype'] <- "weekday" # Create a new column for type of day and fill with default value
    #head(activeData3)

    activeData3$daytype[weekend =='TRUE'] <- "weekend" #Change default value for weekend days

    activeData3$daytype <- as.factor(activeData3$daytype) #convert to factor

    #str(activeData3)

    aveSteps3 <- aggregate(steps ~ interval + daytype,  activeData3, mean)
    #head(aveSteps3)                     

### plot time series

    xyplot(steps ~ interval | daytype, aveSteps3, type = "l", layout = c(1,2), main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days", xlab = "5-Minute Interval", ylab = "Average Number of Steps Taken")

![](pa1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)<!-- -->
