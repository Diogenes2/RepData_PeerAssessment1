Reading the Data
----------------

    unzip("rrproj1data.zip")
    df <- read.csv("activity.csv")

Manipulating the Data
---------------------

    datelist <- levels(factor(df$date))
    rdatelist <- lapply(datelist, strptime, format = "%Y-%m-%d")
    rdlist <- vector()
    for (i in 1:length(rdatelist)){
            ww <- rdatelist[[i]]
            rdlistlist <- append(rdlist, ww)
    }
    datesums <- vector()
    for (i in datelist) { 
        qq <- sum(df$steps[df$date == i], na.rm = TRUE)
        datesums <- append(datesums, qq)}

Histogram of Step Frequency
---------------------------

    hist(datesums, breaks = 10, main = "Histogram of Step Frequency", xlab = "Steps")

![](project1_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Median and Mean Steps in a Day
------------------------------

For my calculation here I remove the days where there were only NA's.
The days that only had NA's were recorded as NA in my making of
datesums.

    meanspd <- mean(datesums, na.rm = TRUE)
    print(meanspd)

    ## [1] 9354.23

    medianspd <- median(datesums, na.rm = TRUE)
    print(medianspd)

    ## [1] 10395

Looking at Number of Steps By Time of Day
-----------------------------------------

    intervalmeans <- vector()
    intervallist <- levels(factor(df$interval))
    for (i in intervallist) { 
        pp <- mean(df$steps[df$interval == i], na.rm = TRUE)
        intervalmeans <- append(intervalmeans, pp)}

    plot(intervallist, intervalmeans, type = "l", xlab = "Time Interval", ylab = "Mean Steps")

![](project1_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    highstepinterval <- which.max(intervalmeans)
    highstepinterval <- highstepinterval*5 ## Each spot on the list represents five minutes, so if we multiply the number we got by five, we know the number of minutes after midnight.
    highstepinterval

    ## [1] 520

    ## We may want to know what time this is.
    hours <- highstepinterval/60
    hours

    ## [1] 8.666667

    ## 8.666667 means 8 and 2/3 hours, or 8:40am.

    ## We know which interval has the highest mean, now we want to know what the maximum value is for mean steps during the 5 minutes.
    max(intervalmeans)

    ## [1] 206.1698

Strategy for Replacing Missing Data
===================================

How Many Times is the Number of Steps Recorded as "na"?
-------------------------------------------------------

    totalnas <- sum(is.na(df$steps))
    print(totalnas)

    ## [1] 2304

What to do to Replace the Missing Data
--------------------------------------

I think a good replacement would be to replace each na with the mean
from the interval. I have a list of interval means from before. So to
index the list I have to take the interval, say 25, and divide it by
five then add one to get the correct spot from my list of interval means
that was calculated earlier.

    dfnona <- df
    m <- c(1:288)
    mm <- 5*m
    newiv <- rep(mm, 61)
    dfnona<- cbind(dfnona, newiv)
    head(dfnona)

    ##   steps       date interval newiv
    ## 1    NA 2012-10-01        0     5
    ## 2    NA 2012-10-01        5    10
    ## 3    NA 2012-10-01       10    15
    ## 4    NA 2012-10-01       15    20
    ## 5    NA 2012-10-01       20    25
    ## 6    NA 2012-10-01       25    30

    for (i in 1:nrow(dfnona)){
            im <- dfnona[i,4]
            iint <- im/5
            imeanmarker <- iint 
            dfnona[i,1][is.na(dfnona[i,1])] <- intervalmeans[imeanmarker]
            
    }
    datelist <- levels(factor(dfnona$date))
    rdatelist <- lapply(datelist, strptime, format = "%Y-%m-%d")
    rdlist <- vector()
    for (i in 1:length(rdatelist)){
            ww <- rdatelist[[i]]
            rdlistlist <- append(rdlist, ww)
    }
    datesums <- vector()
    for (i in datelist) { 
        qq <- sum(dfnona$steps[dfnona$date == i], na.rm = TRUE)
        datesums <- append(datesums, qq)}
    print(datesums)

    ##  [1] 10766.19   126.00 11352.00 12116.00 13294.00 15420.00 11015.00
    ##  [8] 10766.19 12811.00  9900.00 10304.00 17382.00 12426.00 15098.00
    ## [15] 10139.00 15084.00 13452.00 10056.00 11829.00 10395.00  8821.00
    ## [22] 13460.00  8918.00  8355.00  2492.00  6778.00 10119.00 11458.00
    ## [29]  5018.00  9819.00 15414.00 10766.19 10600.00 10571.00 10766.19
    ## [36] 10439.00  8334.00 12883.00  3219.00 10766.19 10766.19 12608.00
    ## [43] 10765.00  7336.00 10766.19    41.00  5441.00 14339.00 15110.00
    ## [50]  8841.00  4472.00 12787.00 20427.00 21194.00 14478.00 11834.00
    ## [57] 11162.00 13646.00 10183.00  7047.00 10766.19

    hist(datesums, breaks = 10, main = "Histogram of Step Frequency After Impution", xlab = "Steps")

![](project1_files/figure-markdown_strict/unnamed-chunk-7-1.png)

Comparing weekends to weekdays to see if steps per day changes
--------------------------------------------------------------

    ## Warning: package 'timeDate' was built under R version 3.3.2

    weekenddf <- df
    for (i in 1:nrow(weekenddf)){
            weekenddf[i,3]<- isWeekday(weekenddf[i,2])
            
            
    }
    weekenddf<- cbind(weekenddf, newiv)
    library(lattice)
    xyplot(steps~newiv | factor(interval), data = weekenddf, main = "Steps Per Interval Throughout the Day, Comparing Weekend(0) to Weekdays (1)", xlab = "Time Interval", ylab = "Number of Steps", alpha = 1/4 )

![](project1_files/figure-markdown_strict/unnamed-chunk-9-1.png)
