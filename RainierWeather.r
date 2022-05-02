#load relevant libraries
library(tidyverse)
library(reshape2)
library(ggplot2)
library(dbplyr)
library(readr)
#install.packages("caret")
library(caret)
#install.packages("repr")
library(repr)
library(data.table)
#install.packages(TTR)
#install.packages("forecast")
library(forecast)
library(lubridate)

#Pull Rainier weather file
weather <- read.csv('Rainier_Weather.csv')

#look at headers to ensure everything looks ok
head(weather)
glimpse(weather)

#date is a character so that needs to be changed 
weather$Date = as.Date(weather$Date, format = '%m/%d/%Y')
glimpse(weather)

#look at summary stats for weather
summary(weather)


#visualizing the data to see patterns
ggplot(data = melt(weather), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
#several of the variables seem to be skewed, temperature avg is the 
#only one that really seems to resemble a normal distribution

###########################
#CLEANING THE DATA

#are there missing values in any of the variables?
which(is.na(weather$Battery.Voltage.AVG))
which(is.na(weather$Temperature.AVG))
which(is.na(weather$Relative.Humidity.AVG))
which(is.na(weather$Wind.Speed.Daily.AVG))
which(is.na(weather$Wind.Direction.AVG))
which(is.na(weather$Solare.Radiation.AVG))
#There's no missing data in any of the variables so no need to impute anything


#Center all the variables for using in regression later
weather$BatVScale <-scale(weather$Battery.Voltage.AVG, center = TRUE, scale = TRUE)
weather$TempScale <- scale(weather$Temperature.AVG, center = TRUE, scale = TRUE)
weather$HumidityScale <- scale(weather$Relative.Humidity.AVG, center = TRUE, scale = TRUE)
weather$WindSpeedScale <- scale(weather$Wind.Speed.Daily.AVG, center = TRUE, scale = TRUE)
weather$WindDirectScale <- scale(weather$Wind.Direction.AVG, center = TRUE, scale = TRUE)
weather$SolarRadScale <- scale(weather$Solare.Radiation.AVG, center = TRUE, scale = TRUE)


head(weather)

#######################################
#Dataset 2
#Need other data set to get success rate
climbingstats <- read.csv("climbing_statistics.csv", fileEncoding = 'UTF-8-BOM')
head(climbingstats)
glimpse(climbingstats)

#data is a character so that needs to be converted
climbingstats$Date = as.Date(climbingstats$Date, format = '%m/%d/%Y')
glimpse(climbingstats)

nrow(climbingstats)
#Need to condense data from the same date
#aggregate attempts by date
aggclimba <- aggregate(climbingstats$Attempted, by = 
            list(as.Date(climbingstats$Date, "%Y-%m-%d")),
            FUN = sum)
#aggregate successes by date
aggclimbs <- aggregate(climbingstats$Succeeded,
                       by= list(as.Date(climbingstats$Date,
                                        "%Y-%m-%d")), FUN =sum)
#create a new data frame with these variables
aggclimbstats = data.frame(aggclimba, aggclimbs)
#make sure it looks correct
head(aggclimbstats)

#make a new success percent variable using the new aggregate attempts and success columns
aggclimbstats$SuccessPercent <- (aggclimbstats$x.1/aggclimbstats$x)

#check out the dataframe to make sure this was added correctly (also checked in the CSV file if these new
#number made sense)
head(aggclimbstats)
tail(aggclimbstats)

#change name of date column
aggclimbstats <- rename(aggclimbstats, c("Date" = Group.1))
drops = c('x', 'Group.1.1','x.1')
aggclimbstats = aggclimbstats[ , !(names(aggclimbstats) %in% drops)]

#need to get rid of errors- success percentage greater than 1
aggclimbstats <- subset(aggclimbstats, SuccessPercent <= 1)
head(aggclimbstats)
glimpse(aggclimbstats)

summary(aggclimbstats)

######################################
#COMBINE DATASETS
weathersuccess <- inner_join(weather, aggclimbstats, by = "Date")
head(weathersuccess)

#Add a month column in case I want to look at anything by month later
weathersuccess$month <- strftime(weathersuccess$Date, "%m")
head(weathersuccess)
tail(weathersuccess)
glimpse(weathersuccess)
summary(weathersuccess)


#Create variables that are easier to call up to use in other analyses
BatteryVol <- weathersuccess$Battery.Voltage.AVG
Temp <- weathersuccess$Temperature.AVG
RelHumidity <- weathersuccess$Relative.Humidity.AVG
WindSpeed <- weathersuccess$Wind.Speed.Daily.AVG
WindDirection <- weathersuccess$Wind.Direction.AVG
SolarRad <- weathersuccess$Solare.Radiation.AVG
SuccessPercent <-weathersuccess$SuccessPercent


#Save the combined data file as a CSV file for use in power BI to make charts
write.csv(weathersuccess, "C:\\Users\\amand\\OneDrive\\Desktop\\AK Live Project\\DigiTechResearch\\Amanda K\\weathersuccess.csv", row.names = FALSE)

###############################
#Look at correlations betwen variables prior to running regression, look at relationships and check for multicollinearity
#correlation table w histogram
library(Hmisc)
library(corrplot)

#dot plot correlation table
res <- cor(weathersuccess_num)
res2 <- rcorr(as.matrix(weathersuccess_num))
res2


#cleaner correlation table

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2<- rcorr(as.matrix(weathersuccess_num[,1:7]))
flattenCorrMatrix(res2$r, res2$P)

#######################################
#Stats
######################################

#get an idea of relationship between the date/time of year and success rate
plot(weathersuccess$month, SuccessPercent, xlab = "Month", ylab = "Success Rate")

#means and standard deviations by month for weather success 
aggregate(SuccessPercent ~ month, weathersuccess, mean)
aggregate(SuccessPercent ~ month, weathersuccess, sd)


model <- lm(SuccessPercent ~ BatVScale + TempScale + HumidityScale + WindSpeedScale + WindDirectScale + SolarRadScale, data= weathersuccess)
summary(model)

model1 <-lm(SuccessPercent ~ weathersuccess$month)
summary(model1)
summary(model)$coefficient

#wind speed and solar radiation were significant- get means and SD for these by month 
aggregate(WindSpeed ~ month, weathersuccess, mean)
aggregate(WindSpeed ~ month, weathersuccess, sd)

aggregate(SolarRad ~ month, weathersuccess, mean)
aggregate(SolarRad ~ month, weathersuccess, sd)

#test between success percent and wind speed and solar radiation
t.test(SuccessPercent, WindSpeed, var.equal = TRUE)
t.test(SuccessPercent, SolarRad, var.equal = TRUE)

######
#MACHINE LEARNING 
#################
#I didn't use this in the report but left this in since I had already done it when I was working on making the rshiny app
library(randomForest)

input.variables <- c("BatteryVol", "Temp", "RelHumidity", "WindSpeed", "WindDirection", "SolarRad")
input.numbers <- c(BatteryVol, Temp, RelHumidity, WindSpeed, WindDirection, SolarRad)
target.variable <- "SuccessPercent"

input.variables

sample = sample.int(n = nrow(weathersuccess), size = floor(.8*nrow(weathersuccess)), replace = F)
train = weathersuccess[sample, ] #just the samples
test  = weathersuccess[-sample, ] #everything but the samples

head(train)

nrow(train) + nrow(test) == nrow(weathersuccess)

names(train)

set.seed(76418)

train_y = train[,'SuccessPercent']
train_x = train[, names(train) !='SuccessPercent']

head(train_y)
head(train_x)

rf_model = randomForest(train_x, y = train_y , ntree = 500, 
                        importance = TRUE)

rf_model

