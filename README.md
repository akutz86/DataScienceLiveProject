# DataScienceLiveProject
 
 ## Introduction
 The final 2 weeks of the Tech Academy Data Science Boot Camp consisted of a live project. During this live project I was able to implement the skills I learned throughout the course using Jupyter Notebook/Python, R, and Power BI. I completed 3 stories which required me to clean and analyze data and create data visualizations using different data sets. Each story asked for different information which allowed me to test my data analysis and programming skills in various ways. 
 
 Descriptions of each data set used and the corresponding stories I worked on are listed below. 
 
 ## Mount Rainier
 There were two csv dataset files that were utilized to do analyses.
 * Climbing statistics- this data set included information about the date, the different routes to climb Mount Rainier, climbing attempts, climbing successes and the percentage of successful climbs. 
 * Mountain weather- this data set included information about the date and different weather variables (battery voltage, temperature, humidity, wind speed, wind direction and solar radiation). Weather data was an average of each variable for each date data was collected. 
 
 The first story utilized data from the climbing statistics dataset. The request was to display success percentage by route. Initially, I worked on cleaning the data through looking for null variables and errors in the dataset. There was a relatively restricted range for climbing attempts and climbing successes so there were no outliers to transform or remove. There were also no null values. There were errors to remove. Following data cleaning, I sorted the data by route, dropped extraneous values (climbing attempts and climbing successes) and created a table displaying success percentage by route.
 
 The second story asked if there was a relationship between weather and climbing success. This story required me to combine the climbing statistics and mountain weather datasets. Prior to combining datasets, I cleaned both datasets separately - again looking for null values, outliers and errors and removed, replaced or transformed data accordingly. I combined datasets on the date as this was the only shared variable between datasets. Before combining datasets, I had to do some aggregation in the climbing statistics dataset as there were multiple data points per day (whereas the mountain weather dataset only had one data point per variable per day). To determine if there was a relationship between weather and climbing success, I decided to run a multiple linear regression using Percent Successful as the dependent variable and weather variables (battery volatage, temperature etc.) as the independent variables. I initially checked for multicollinearity. Given the large sample size (n = 203) I did not normalize the data. I did center variables to make output easier to read and to ensure that multicollinearity issues would be minimized. Here is a code snippet of the statistical analyses I ran. 
 
plot(weathersuccess$month, SuccessPercent, xlab = "Month", ylab = "Success Rate")
aggregate(SuccessPercent ~ month, weathersuccess, mean)
aggregate(SuccessPercent ~ month, weathersuccess, sd)

model <- lm(SuccessPercent ~ BatVScale + TempScale + HumidityScale + WindSpeedScale + WindDirectScale + SolarRadScale, data= weathersuccess)
summary(model)

model1 <-lm(SuccessPercent ~ weathersuccess$month)
summary(model1)
summary(model)$coefficient

aggregate(WindSpeed ~ month, weathersuccess, mean)
aggregate(WindSpeed ~ month, weathersuccess, sd)
aggregate(SolarRad ~ month, weathersuccess, mean)
aggregate(SolarRad ~ month, weathersuccess, sd)

t.test(SuccessPercent, WindSpeed, var.equal = TRUE)
t.test(SuccessPercent, SolarRad, var.equal = TRUE)

## COVID-19
This dataset contained a large amount of data (n > 100,000) on different aspects of COVID including location (continent and country), new cases, deaths, 
 
