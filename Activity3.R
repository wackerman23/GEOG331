#Will Ackerman
#Activity 3
#2/21/2022

#Assert Function ----------------
##create a function. The names of the arguments for your function will be in 
#parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#Initializing Data ----------------------------
#set the working directory
setwd("/Users/willackerman/GEOG331/Bewkes Weather")

# Import Bewkes Data
datW <- read.csv("/Users/willackerman/GEOG331/Bewkes Weather/bewkes_weather.csv", 
                 na.strings=c("#N/A"), skip=3, header=FALSE)

#create sensor info data
sensorInfo <- read.csv("/Users/willackerman/GEOG331/Bewkes Weather/bewkes_weather.csv",
                       na.strings=c("#N/A"), nrows=2)


#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)


#Lubridate Package -----
#use install.packages to install lubridate
#install.packages(c("lubridate"))
#library("lubridate")

#Convert date to standardized format -----
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#Question 4 -----
#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)
#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,] 
#QA/QC 1 ----
#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularly confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)


#measurements outside of sensor capability -----
#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked

#Question 5 ----
assert(length(lightscale) == length(datW$lightning.acvitivy), "error: unequal lengths")

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))


#Question 6 -----
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm. 
# filter windspeeds below .5mph and gust speeds over 4mph
#create a new air temp column
datW$air.tempQ3 <- ifelse(datW$wind.speed  >= .5 & datW$gust.speed > 4, NA, datW$air.tempQ2)

length(datW$air.tempQ3[!is.na(datW$air.tempQ3)])

#plot wind speed
plot(datW$DD, datW$air.tempQ3, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#Question 7 ----
datW$soil.moisture2 <- ifelse(datW$soil.moisture > datW$precipitation + .25, NA, datW$soil.moisture)
datW$soil.temp2 <- ifelse(datW$soil.temp > datW$air.temperature + sd(datW$soil.temp, na.rm=TRUE), NA, datW$soil.temp)

length(datW$soil.moisture2[!is.na(datW$soil.moisture2)])
length(datW$soil.temp2[!is.na(datW$soil.temp2)])

#Question 8 ----
Q8 = data.frame(datW$DD)
Q8$air.temp <- datW$air.tempQ3
Q8$precipitation <- datW$precipitation
Q8$moisture <- datW$soil.moisture2
Q8$soil.temp <- datW$soil.temp2

#sum of precipitation
sum(Q8$precipitation, na.rm=TRUE)

#number of observations
length(Q8$precipitation[!is.na(Q8$precipitation)])

#time frame 
time = Q8[length(Q8$precipitation[!is.na(Q8$precipitation)]), "datW.DD"] - Q8[1, "datW.DD"]
days = time%/%1
hours = ((time%%1)*24)%/%1
minutes = ((((time%%1)*24)%%1)*60)%/%1
paste(days, " days ", hours, " hours ", minutes, " minutes")



#Question 9 ----
#create plot squares
par(mfrow = c(2,2))

plot(Q8$datW.DD, Q8$air.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
plot(Q8$datW.DD[Q8$precipitation > 0], Q8$precipitation[Q8$precipitation > 0], pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (CM)")
plot(Q8$datW.DD[!is.na(Q8$soil.temp)], Q8$soil.temp[!is.na(Q8$soil.temp)], pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temperature (degrees C)")
plot(Q8$datW.DD[!is.na(Q8$moisture)], Q8$moisture[!is.na(Q8$moisture)], pch=19, type="b", xlab = "Day of Year",
     ylab="soil moisture")

