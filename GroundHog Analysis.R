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
Setflags <- vector()
flagCounter = 0


for(i in dataSorted$doy)
{
  if(i == 357.0){flagCounter <- flagCounter + 1}
  if(i >= 34.0 & i <= 76.0){flags <- append(flags, 'after')
  Setflags <- append(Setflags, flagCounter)}
  else if(i >= 357.0 || i < 33.0){flags <- append(flags, 'before')
  Setflags <- append(Setflags, flagCounter)}
  else if(i == 33.0){flags <- append(flags, 'day')
  Setflags <- append(Setflags, flagCounter)}
  else{flags <- append(flags, 'useless')
  Setflags <- append(Setflags, flagCounter)}
  
}
        
dataSorted$flag <- flags
dataSorted$setFlag <- Setflags

#### Calculate temperature Average ####

for(i in dataSorted$TMAX)
{
  dataSorted$TAVE <- i - 
  
}


#### Analyze Temperature Data ####
finalAverages <- vector()

for(i in dataSorted$setFlag)
{
  finalAverages <- append(mean(dataSorted$TAVE[dataSorted$flag[dataSorted$setFlag == i] == 'before']))
  finalAverages <- append(mean(dataSorted$TAVE[dataSorted$flag[dataSorted$setFlag == i] == 'after']))
  
}



