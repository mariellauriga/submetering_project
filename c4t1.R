library(RMySQL)
library(dplyr)
library(lubridate)
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListTables(con)
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Seeing what is in the tables
dbListFields(con,'yr_2006')
############### Getting tables 2006 through 2010 ##########
#df1 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
#                 Global_reactive_power,
#                 Global_intensity, Voltage,
#                 Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
df1 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                  Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")

df2 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                  Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")

df3 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                  Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")

df4 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                  Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

df5 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                  Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

############### Inspecting tables 2006 through 2010 ##########
summary(df1)
summary(df2)
summary(df3)
summary(df4)
summary(df5)

str(df1)
str(df2)
str(df3)
str(df4)
str(df5)

head(df1) # 1     2006-12-16 17:24:00
tail(df1) # 21992 2006-12-31 23:59:00

head(df2) # 1      2007-01-01 00:00:00
tail(df2) # 521669 2007-12-31 23:59:00

head(df3) # 1      2008-01-01 00:00:00
tail(df3) # 526905 2008-12-31 23:59:00

head(df4) # 1      2009-01-01 00:00:00
tail(df4) # 521320 2009-12-31 23:59:00

head(df5) # 1      2010-01-01 00:00:00
tail(df5) # 457394 2010-11-26 21:02:00

##Does each data frame cover an entire year?
#Not, it doesn't.

## Combine tables into one dataframe using dply
newDF <- bind_rows(df2, df3, df4)
str(newDF)
summary(newDF)
head(newDF)  # 1        2007-01-01 00:00:00
tail(newDF)  #  1569894 2009-12-31 23:59:00

# Are the dates in the correct order? If so, you are ready to move on to the next step.

#Yes.

## Combine Date and Time attribute values in a new attribute column

newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(newDF)[7] <-"DateTime"

## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)

## Convert DateTime from character to POSIXct
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(newDF)

#What is the data type for DateTime? What do the values look like?
#The format data type is POSIXct. It looks like a character

## Create "year" attribute with lubridate

newDF$year <- year(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)

newDF$no_meassured <- newDF$Global_active_power*1000/60-newDF$Sub_metering_1 -newDF$Sub_metering_2 -newDF$Sub_metering_3

#Review your primary data frame. Was the year attribute created? What values
#are found in the year attribute?
#Yes, the year attribute was  created. The values are 2007, 2008, 2009, 2010
str(newDF)
summary(newDF)

write.csv(newDF, file="final_data.csv", row.names = TRUE)


#Which sub-meter is using the most power? The least? Is there anything to
#learn from the max and min?

#Propose three high-level recommendations you can suggest based on your
#initial exploration of the power consumption data

#* Thought Starter: If you could add more information to the data set, what
#kinds of attributes would you add? What would be important to understanding
#the power usage in this home?
#  * Thought Starter: Should the appliances on the sub-meters be grouped
#the way they are currently grouped? Could more information be gained if
#some were separated?

plot1 <- function() {
  hist(newDF$Global_active_power, main = paste("Global Active Power"), col="yellow", xlab="Global Active Power (kilowatts-min)")
  dev.copy(png, file="1_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
}
plot1()

plot2 <- function() {
  hist(newDF$Global_reactive_power, main = paste("Global Reactive Power"), col="Green", xlab="Global Reactive Power (kilowatts-min)")
  dev.copy(png, file="2_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot2.png has been saved in", getwd())
}
plot2()

plot3 <- function() {
  hist(newDF$Voltage, main = paste("Voltage (minute averaged)"), col="Red", xlab="Voltage(Volt)")
  dev.copy(png, file="3_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot3.png has been saved in", getwd())
}
plot3()

plot4 <- function() {
  hist(newDF$Sub_metering_1, main = paste("Active energy (hour averaged)"), col="Red", xlab="(watt-hour)")
  dev.copy(png, file="3_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot3.png has been saved in", getwd())
}
plot4()

plot5 <- function() {
  hist(newDF$Sub_metering_2, main = paste("Active energy (hour averaged)"), col="Red", xlab="(watt-hour)")
  dev.copy(png, file="3_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot3.png has been saved in", getwd())
}
plot5()

plot6 <- function() {
  hist(newDF$Sub_metering_3, main = paste("Active energy (hour averaged)"), col="Red", xlab="(watt-hour)")
  dev.copy(png, file="3_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot3.png has been saved in", getwd())
}
plot6()

plot7 <- function() {
  hist(newDF$Global_active_power*1000/60, main = paste("Global Active Power"), col="yellow", xlab="Global Active Power (watts-hour)")
  dev.copy(png, file="1_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
}
plot7()

plot8 <- function() {
  hist(newDF$no_meassured, main = paste("Global Active Power No-measured"), col="yellow", xlab="Global Active Power (watts-hour)")
  dev.copy(png, file="1_Hist.png", width=480, height=480)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
}
plot8()

