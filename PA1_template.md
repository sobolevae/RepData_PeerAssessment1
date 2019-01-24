---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
The project data may currentry exist in the working directory (in a -zip.file or already unzipped). The following code checks if the *activity.csv* file already exist, if it doesn't, checks the existence of *activity.zip* file and upzips it. If there is neither *activity.csv* nor *activity.zip* the archive *activity.zip* will be downloded from the project resource and unzipped.  
As soon as the data file existence is confirmed, the data is loaded into **act_data** variable. The class of the varaible will be "data.frame" class.

```r
dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("activity.csv")){
    if(!file.exists("activity.zip")){
        download.file(dataURL, destfile = "activity.zip", method="curl")
    }
    unzip("activity.zip", exdir = ".")
act_data <- read.csv("activity.csv")
}  
```
## What is mean total number of steps taken per day?
For analyse the total number of steps per date we should group and summarize the data first. In the following code the functions of "dplyr" library are used for this purpose.

```r
library(dplyr)
act_data_sum <- act_data %>% 
    group_by(date) %>% 
    summarize(sum=sum(steps, na.rm = TRUE))
```
As soon as we have the data prepared for analyse, we can build a historgam that shows the distribution of the total spets per day. There is a code that build a histogram by means of Basic plotting system and saves the histogram in a "figure" folder as .png file.

```r
library(grDevices)
hist(act_data_sum$sum, 
     main = "Total steps per day", 
     xlab="steps per day", 
     col="steelblue", 
     border="blue4", 
     breaks = 10, 
     xlim=c(0, 25000),
     ylim =c(0, 25))
```

![](PA1_template_files/figure-html/totapstepshist-1.png)<!-- -->

```r
invisible(dev.copy(png, filename="./figure/plot1.png"))
invisible(dev.off ())
```
Having the agregated values in variable **act_data_sum** we can calculate the mean and median of total number of steps per day:  
The **mean** can be get by the following expression:

```r
  mean(act_data_sum$sum, na.rm = TRUE)
```

```
## [1] 9354.23
```
And the **median** can be get by this way:

```r
  median(act_data_sum$sum, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
We can get the data for analyse the average daily activity pattern by grouping the data saved in variable **act_data** by **interval** and summarizing them getting the mean of spets number.

```r
act_data_time <- act_data %>% 
    group_by(interval) %>% 
    summarize(avg=mean(steps, na.rm = TRUE))
```
As soon as we have the data prepared for analyse we can make a plot that shows the average pattern of daily activity:

```r
plot(act_data_time$interval, act_data_time$avg, 
     type = "l",
     col = "blue",
     main = "Average daily activity pattern", 
     xlab = "number of 5-min interval", 
     ylab = "avg number of steps per 5-min interval")
```

![](PA1_template_files/figure-html/patternplot-1.png)<!-- -->

```r
invisible(dev.copy(png, filename="./figure/plot2.png"))
invisible(dev.off ())
```
The number of 5-min interval that contains, in average, the maximum number of steps can be found by the following way:

```r
maxst <- act_data_time[which.max(act_data_time$avg), 1]
as.numeric(maxst)
```

```
## [1] 835
```
## Imputing missing values
The total number of missing values in the **steps** column of the dataset can be found by the following code:

```r
sum(is.na(act_data$steps))
```

```
## [1] 2304
```
I choose this strategy for imputate the missing values in given dataset: *replace every missing value with a rounded mean of steps for the same time interval calculated throughout the whole dataset*. We already have these avarage values for each interval in **act_data_time** variable.  
The following code imputates the missing values of then **act_data** dataset using the choosen stratagy and stores the result in **act_data_imp** variable:

```r
act_data_imp <- act_data %>% 
    left_join(act_data_time, by="interval") %>% 
    mutate(steps = ifelse(is.na(steps), round(avg), steps)) %>%
    select (-avg)
```
Now we can analyse the imputated data and compare them with the initial data. This code groups and summarizes the imputated data and builds a historgam that shows the distribution of total number of steps per day:

```r
act_data_imp_sum <- act_data_imp %>% 
    group_by(date) %>% 
    summarize(sum=sum(steps))
hist(act_data_imp_sum$sum, 
     main = "Total steps per day \n(imputated data)", 
     xlab="steps per day", 
     col="steelblue", 
     border="blue4", 
     breaks = 10, 
     xlim=c(0, 25000),
     ylim =c(0, 25))
```

![](PA1_template_files/figure-html/impdataanalysis-1.png)<!-- -->

```r
invisible(dev.copy(png, filename="./figure/plot3.png"))
invisible(dev.off ())
```

We can also calculate the mean and median of total number of steps per day in imputated data:  
The **mean** can be get by the following expression:

```r
  mean(act_data_imp_sum$sum)
```

```
## [1] 10765.64
```
And the **median** can be get by this way:

```r
  median(act_data_imp_sum$sum)
```

```
## [1] 10762
```
The histogram and the mean and median values show some effects of the data imputation. Some of them are:  
- the distribution of imputated values is smoother, it is closer to a normal distribution, the standart deviation seems to be smaller;  
- the average number of steps per day (mean value)  appeared to be greater;  
- the median value of steps per day haven't changed significantly.  
All these effects could be explanied by the choosen strategy of imputation - using the mean values for each interval. This method makes the distribution closer to normal.

## Are there differences in activity patterns between weekdays and weekends?
Here the functions of **lubridate** library have been used to add a new variable **is.weekday** to the data set with imputated values:

```r
library(lubridate)
act_data_imp <- act_data_imp %>% 
    mutate(is.weekday = 
      ifelse((wday(as_date(date)) == 1) | (wday(as_date(date)) == 7), "weekend", "weekday"))
```
This code gets two datasets, one for weekdays and the other for weekends, groups and summarizes their data and makes the plots. 

```r
act_data_imp_wd <- act_data_imp %>%
    filter(is.weekday == "weekday") %>%
    group_by(interval) %>% 
    summarize(avg = mean(steps))
act_data_imp_we <- act_data_imp %>%
    filter(is.weekday == "weekend") %>%
    group_by(interval) %>% 
    summarize(avg = mean(steps))

par(mfrow = c(2, 1), mar=c(2, 4, 2, 2))
with(act_data_imp_wd, 
     plot(interval, avg, 
         type = "l",
         col = "blue",
         main = "weekdays", 
         xlab = "number of 5-min interval", 
         ylab = "avg number of steps",
         ylim = c(0, 250)))
with(act_data_imp_we, 
     plot(interval, avg, 
         type = "l",
         col = "blue",
         main = "weekends", 
         xlab = "number of 5-min interval", 
         ylab = "avg number of steps",
         ylim = c(0, 250)))
```

![](PA1_template_files/figure-html/comparepatterns-1.png)<!-- -->

```r
par(mfrow = c(1, 1))
invisible(dev.copy(png, filename="./figure/plot4.png"))
invisible(dev.off ())
```
We can see by these plots that the weekend activity in more regular than the weekday activity (it doesn't have one high extremum that weekdays have). The average activity of daytime (intervals 1000 - 1500) is higher in the weekends that in the weekdays.
