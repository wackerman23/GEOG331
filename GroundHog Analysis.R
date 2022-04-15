#Will Ackerman
#GroundHog Analysis

#Initializing Data ----------------------------
#set the working directory
setwd("Z:\\students\\wackerman\\Final Project")

#load in tidyverse
library(tidyverse)
library(lubridate)

#read in data
dataStart <- read.csv("Z:\\students\\wackerman\\Final Project\\2927109.csv")

#read in groundhog data
dataGroundHog <- read.csv("Z:\\students\\wackerman\\Final Project\\2927109.csv")


#### define time for data #####
#convert date and time
datesD <- as.Date(dataStart$DATE, "%Y-%m-%d")
#get day of year
dataStart$doy <- yday(datesD)
#calculate year
dataStart$year <- year(datesD)

#### get decimal data format #####

#calculate a decimal year, but account for leap year
dataStart$decYear <- ifelse(leap_year(dataStart$year), dataStart$year + (dataStart$doy/366),
                       dataStart$year + (dataStart$doy/365))


####Check the data for overlap####
dataSorted <- distinct(dataStart, dataStart$decYear, .keep_all = TRUE)

####Take applicable data
flags <- vector()

for(i in dataSorted$doy)
{
  
  if(i >= 34.0 & i <= 76.0){flags <- append(flags, 'after')}
  else if(i >= 357.0 || i < 33.0){flags <- append(flags, 'before')}
  else if(i == 33.0){flags <- append(flags, 'day')}
  else{flags <- append(flags, 'useless')}
  
}
        
dataSorted$flag <- flags

beforeWinter <- mean(weather$TMAX[weather$NAME == "ABERDEEN, WA US"], na.rm=TRUE)



