library(plyr)
library(timeDate)
library(chron)
library(ggplot2)
library(gridExtra)
library(lattice)
#options(echo=TRUE)
setwd("~/selfdev/Coursera/Course 5 - Reproducible Research/Week 1")
#Download the file and unzip it if it does not exist(Check for the constituent file)
if(!file.exists("activity.csv")) {
  file <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",file)
  unzip(file) # unzip the file
  unlink(file) # remove the temporary file
}

#Read data and ignore nulls(NA values)
fitnessData = read.csv(file="activity.csv",colClasses =  c("integer", "character", "character"))
fitnessData$date<-as.Date(fitnessData$date,format="%Y-%m-%d" ) #Convert dates from string to date
weekEnd = (is.weekend(fitnessData$date))  #create vector with weekend indicator
fitnessData[,"weekend"]<-as.factor(weekEnd) # add weekend as a factor to original data
fitnessData$interval=as.numeric(fitnessData$interval)
nonNullfdata = na.omit(fitnessData) #Create a separate set of non-NA values

##What is mean total number of steps taken per day?
aggFitnessData = aggregate(nonNullfdata$steps,list(nonNullfdata$date), sum)
rename(aggFitnessData,c("Group.1" = "Date", "x"="Steps"))
names(aggFitnessData)<-c("Date","Steps")
meanSteps = mean(aggFitnessData$Steps)
medianSteps = median(aggFitnessData$Steps)

meanstr = sprintf("The mean number of steps per day = %f",meanSteps)
medianstr = sprintf("The median number of steps per day = %f",medianSteps)

print(meanstr)
print(medianstr)

grid.arrange(qplot(aggFitnessData$Steps, geom="histogram", xlab = "Steps by Day", ylab = "Frequency",col=I("black"),fill=I("blue"),bins=50))


meanByInterval <- aggregate(nonNullfdata$steps,list(nonNullfdata$interval), mean) 
names(meanByInterval)<-c("Interval","MeanSteps")
grid.arrange(xyplot(MeanSteps ~ Interval, data=meanByInterval, type="l", grid=TRUE, ylab="Number of steps", xlab="5-minute interval", main="Average steps by 5-minutes intervals"))

maxSteps = max(meanByInterval$MeanSteps)
maxIndex=which(meanByInterval$MeanSteps==maxSteps)
maxinterval = meanByInterval$Interval[maxIndex]
maxstr= sprintf("The interval in which max number of steps occured is %g", maxinterval)
print(maxstr)

totalNA = nrow(fitnessData) - nrow(nonNullfdata)
NAstr= sprintf("The number of rows with null values is %g", totalNA)
print(NAstr)

normalisedData<-fitnessData

for (i in 1:nrow(normalisedData)){
  if (is.na(normalisedData$steps[i])){
      value <- meanByInterval$MeanSteps[which(normalisedData$interval[i] == meanByInterval$Interval)]
      normalisedData$steps[i] = value
    }
}

##What is mean total number of steps taken per day?
aggnFitnessData = aggregate(normalisedData$steps,list(normalisedData$date), sum)
rename(aggnFitnessData,c("Group.1" = "Date", "x"="Steps"))
names(aggnFitnessData)<-c("Date","Steps")
meannSteps = mean(aggnFitnessData$Steps)
mediannSteps = median(aggnFitnessData$Steps)

meannstr = sprintf("The mean number of steps per day = %f",meannSteps)
mediannstr = sprintf("The median number of steps per day = %f",mediannSteps)

print(meannstr)
print(mediannstr)

grid.arrange(xyplot(steps~interval|weekend, data=normalisedData, type='l', layout=c(1, 2)))





