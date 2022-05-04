#Will Ackerman
#GroundHog Analysis

#Initializing Data ----------------------------
#set the working directory
setwd("Z:\\students\\wackerman\\Final Project")

#load in tidyverse
library(tidyverse)
library(lubridate)

#read in data
dataStart <- read.csv("Z:\\students\\wackerman\\Final Project\\2952812.csv")

#read in groundhog data
dataGroundHog <- read.csv("Z:\\students\\wackerman\\Final Project\\2952812.csv")


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
  if(i > 48.0 & i <= 76.0){flags <- append(flags, 'after2')
  Setflags <- append(Setflags, flagCounter)}
  else if(i >= 34.0 & i <= 48.0){flags <- append(flags, 'after1')
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

for(i in 1:length(dataSorted$decYear))
{
  dataSorted$TAVE[i] <- (dataSorted$TMAX[i] + dataSorted$TMIN[i])/2
 
}

mean(dataSorted$TAVE[dataSorted$setFlag == 1][dataSorted$flag == 'before'], na.rm=TRUE)

#### Analyze Temperature Data ####
firstAverages <- vector()
shortAverages <- vector()
longAverages <- vector()
years <- vector()
uselessVect <- vector()
count <- 0

for(l in dataSorted$setFlag)
{
  count = l
  firstAverages <- append(mean(dataSorted$TAVE[dataSorted$setFlag == count][dataSorted$flag == 'before'], na.rm=TRUE))
  shortAverages <- append(mean(dataSorted$TAVE[dataSorted$setFlag == count][dataSorted$flag == 'after1'], na.rm=TRUE))
  longAverages <- append(mean(dataSorted$TAVE[dataSorted$setFlag == count][dataSorted$flag == 'after2'], na.rm=TRUE))
  years <- append(dataSorted$year[dataSorted$setFlag == l][dataSorted$flag == 'day'], na.rm=TRUE)
  #uselessVect <- append(mean(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'useless'], na.rm=TRUE))
}


finalDifferenceShort <- vector()
finalDifferenceLong <- vector()

for(i in 1:length(firstAverages))
{
  finalDifferenceShort <- append(firstAverages[i]/shortAverages[i])
  finalDifferenceLong <- append(firstAverages[i]/(shortAverages[i] + longAverages[i] + longAverages[i])/3)
}


finalData <- data.frame(years, finalDifferenceShort, finalDifferenceLong)
predictionAccuracyShort <- vector()
predictionAccuracyLong <- vector()

for(i in finalData$years)
{
  if (finalData$finalDifferenceShort[i] <= 1.05 & finalData$finalDifferenceShort[i] >= .95)
  {predictionAccuracyShort <- append(TRUE)}
  else {predictionAccuracyShort <- append(FALSE)}
  if (finalData$finalDifferenceLong[i] <= 1.05 & finalData$finalDifferenceLong[i] >= .95)
  {predictionAccuracyLong <- append(TRUE)}
  else {predictionAccuracyLong <- append(FALSE)}
}

finalData$predictionAccuracyShort <- predictionAccuracyShort
finalData$predictionAccuracyLong <- predictionAccuracyLong


