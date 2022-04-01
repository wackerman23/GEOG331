#Will Ackerman
#GroundHog Analysis

#Initializing Data ----------------------------
#set the working directory
setwd("Z:\\students\\wackerman\\Final Project")

#load in lubridate
library(lubridate)

#read in streamflow data
dataStart <- read.csv("Z:\\students\\wackerman\\Final Project\\2927109.csv")

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


####Check the data for consistency####
dataAnalysis <- dataStart[dat]
                 