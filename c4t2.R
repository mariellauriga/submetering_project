library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)

#Reading the data
data <- read.csv("final_data.csv")

str(data)
summary(data)
head(data)
tail(data)

##Some inspecting plots

## Subset the second week of 2008 - All Observations
houseWeek08 <- filter(data, year == 2008 & week == 2)
houseWeek09 <- filter(data, year == 2009 & week == 2)
houseWeek07 <- filter(data, year == 2007 & week == 2)
plot(houseWeek08$Sub_metering_1)

houseDay <- filter(data, year == 2008 & month == 1 & day == 9)
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$Time, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


plot_ly(houseWeek08, x = ~houseWeek08$Time, y = ~houseWeek08$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek08$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek08$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption the second week of 2008 - All Observations",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseWeek09, x = ~houseWeek09$Time, y = ~houseWeek09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption the second week of 2009 - All Observations",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseWeek07, x = ~houseWeek07$Time, y = ~houseWeek07$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek07$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek07$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption the second week of 2007 - All Observations",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

####################One type of sub-seting#######################
## Sub-setting to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(data, day == 2 & hour == 20 & minute == 1)
head(house070809weekly)
tail(house070809weekly)
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=13,
                         start=c(2007,1))
## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=12,
                         start=c(2007,1))
## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=8,
                         start=c(2007,1))

## Plotting with plot.ts
plot.ts(tsSM3_070809weekly)
plot.ts(tsSM2_070809weekly)
plot.ts(tsSM1_070809weekly)

####################Another type of sub-seting#######################
## Sub-setting to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house07weekly <- filter(data, year==2007 & day == 2 & hour == 20 & minute == 1)
head(house07weekly)
tail(house07weekly)

## Create TS object with SubMeter3
tsSM3_07weekly <- ts(house07weekly$Sub_metering_3, frequency=13,
                         start=c(2007,1))
## Create TS object with SubMeter2
tsSM2_07weekly <- ts(house07weekly$Sub_metering_2, frequency=12,
                         start=c(2007,1))
## Create TS object with SubMeter1
tsSM1_07weekly <- ts(house07weekly$Sub_metering_1, frequency=8,
                         start=c(2007,1))

## Plotting with plot.ts
plot.ts(tsSM3_07weekly, ylab =  "Power (watt-hours)")
plot.ts(tsSM2_07weekly, ylab =  "Power (watt-hours)")
plot.ts(tsSM1_07weekly, ylab =  "Power (watt-hours)")
###################3###################################

summary(tsSM3_070809weekly)
summary(tsSM2_070809weekly)
summary(tsSM1_070809weekly)

#Fitting
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season)
summary(fitSM2)

fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season)
summary(fitSM1)

## Forecasting for ahead 20 time periods
forecastfitSM3 <- forecast(fitSM3, h=20, level=c(80,90))
forecastfitSM2 <- forecast(fitSM2, h=20, level=c(80,90))
forecastfitSM1 <- forecast(fitSM1, h=20, level=c(80,90))

## Plotting the forecast
plot(forecastfitSM3, ylim = c(-10, 30), ylab= "Watt-Hours", xlab="Time")
plot(forecastfitSM2, ylim = c(-10, 40), ylab= "Watt-Hours", xlab="Time")
plot(forecastfitSM1, ylim = c(-10, 40), ylab= "Watt-Hours", xlab="Time")

#Decomposing to remove the season
components070809SM3weekly <- decompose(tsSM3_070809weekly)
components070809SM2weekly <- decompose(tsSM2_070809weekly)
components070809SM1weekly <- decompose(tsSM1_070809weekly)

plot(components070809SM3weekly)
plot(components070809SM2weekly)
plot(components070809SM1weekly)

summary(components070809SM3weekly)
summary(components070809SM2weekly)
summary(components070809SM1weekly)

#Removing seasonal component
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal

autoplot(tsSM3_070809Adjusted)
autoplot(tsSM2_070809Adjusted)
autoplot(tsSM1_070809Adjusted)

#Veryfying
plot(decompose(tsSM3_070809Adjusted))
plot(decompose(tsSM2_070809Adjusted))
plot(decompose(tsSM1_070809Adjusted))

#HoltWinters Simple Exponential Smoothing
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)

tsSM3_HW070809
tsSM3_HW070809$SSE

tsSM2_HW070809
tsSM2_HW070809$SSE

tsSM1_HW070809
tsSM1_HW070809$SSE

plot(tsSM3_HW070809, ylim = c(0, 25))
plot(tsSM2_HW070809, ylim = c(0, 25))
plot(tsSM1_HW070809, ylim = c(0, 25))

#HoltWinters Forecast with diminished confidence levels
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25, level=c(10,25))

plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

## Plot only the forecasted area
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
