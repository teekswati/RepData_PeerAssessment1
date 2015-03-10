---
output: word_document
---
#Assignment-1 for "Reproducible Research""

##Loading and preprocessing the data

1. Load the data 

```{r}
data=read.csv("activity.csv", header=T)
head(data)
```
##What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day and  Process/transform the data  into a format suitable for your analysis



```{r,sum}
sum_data=tapply(data$steps, data$date, sum)
data_transform=transform(sum_data)
data_transform$date= row.names(data_transform)
 names(data_transform)= c("counts", "date")
dim(data_transform)
head(data_transform)
```

2.Make a histogram of the total number of steps taken each day
 
```{r}
hist(data_transform$counts, xlab= "counts", ylab="frequency of counts", col="blue", main="histogram showing counts")
```

3.Calculate and report the mean and median of the total number of steps taken per day

```{r}
avg=mean(data_transform$counts, na.rm=TRUE)
avg
med=median(data_transform$counts, na.rm=TRUE)
med
```

## What is the average daily activity pattern:-

1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
pattern = tapply(data$steps,data$interval,mean , na.rm=TRUE )
dim(pattern)
patt=transform(pattern)
patt$interval= row.names(patt)
 names(patt)= c("avg_steps", "interval")
head(patt)
plot(patt$interval, patt$avg_steps, type="l", col="blue",xlab="interval", ylab="average_steps", main="time seies_plot")
```
2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
avgstep=which.max(patt$avg_steps)
names(avgstep)
```

##Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(data))
```

2.Devise a strategy for filling in all of the missing values in the dataset.  use the mean for that day, or the mean for that 5-minute interval, etc.

```{r}
avg=aggregate(steps ~ interval,data=data, mean)
Imp_data=as.numeric()
for(i in 1: nrow(data)){
  val=data[i,]
  if(is.na(val$steps)){
    steps = subset(avg,interval==val$interval)$steps
  } else {
    steps = val$steps
  }
    Imp_data <- c(Imp_data, steps)
  }
Impute_data=data
Impute_data$steps=Imp_data
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
head(Impute_data)
dim(Impute_data)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
new_data=aggregate(steps ~ date, data=Impute_data, sum)
head(new_data)
hist(new_data$steps, col="red")
mean(new_data$steps)
median(new_data$steps)
```

##Are there differences in activity patterns between weekdays and weekends?


1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```{r}
Impute_data$date=as.Date(strptime(Impute_data$date, format="%Y-%m-%d"))
Impute_data$day= weekdays(Impute_data$date)
for(i in 1:nrow(Impute_data))
  { if(Impute_data[i,]$day %in% c("Saturday","Sunday"))
    {   
    Impute_data[i,]$day ="weekend"
    }else{
      Impute_data[i,]$day ="weekday"
      }
    }
 head(Impute_data)
dim(Impute_data)

ser=aggregate(Impute_data$steps ~ Impute_data$interval +as.factor(Impute_data$day),data= Impute_data,mean)
names(ser)=c("interval","days", "steps")
head(ser)
```

2.Make a panel plot containing a time series plot f the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r}
library(lattice)
p <- xyplot(steps ~ interval | factor(days), data=ser, type = 'l',col=c("red", "blue"))
print (p)
```


