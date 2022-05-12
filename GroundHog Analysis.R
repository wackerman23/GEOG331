#Will Ackerman
#GroundHog Analysis


#Change File Locations
#Change Plot Limits
#Eliminate Snow Variables for Buckeye Chuck

#Initializing Data ----------------------------
#set the working directory
setwd("/Users/willackerman/GEOG331/Project Data")


#load in tidyverse
library(tidyverse)
library(lubridate)

#read in data
dataStart <- read.csv("/Users/willackerman/GEOG331/Project Data/2890512.csv")

#read in groundhog data
dataGroundHog <- read.csv("/Users/willackerman/GEOG331/Project Data/StatenIslandChuck.csv")


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
firstAveragesSD <- vector()
shortAverages <- vector()
shortAveragesSD <- vector()
longAverages <- vector()
longAveragesSD <- vector()

years <- vector()


for(l in 1:21)
{
  firstAverages <- append(firstAverages, mean(na.omit(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'before'])))
  firstAveragesSD <- append(firstAveragesSD, sd(na.omit(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'before'])))
  
  shortAverages <- append(shortAverages, mean(na.omit(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'after1'])))
  shortAveragesSD <- append(shortAveragesSD, sd(na.omit(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'after1'])))
  
  longAverages <- append(longAverages, mean(na.omit(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'after2'])))
  longAveragesSD <- append(longAveragesSD, sd(na.omit(dataSorted$TAVE[dataSorted$setFlag == l][dataSorted$flag == 'after2'])))
  
  years <- append(years, na.omit(dataSorted$year[dataSorted$setFlag == l][dataSorted$flag == 'day']))
}

####add to final data frame ####
finalData <- data.frame(years, firstAverages, firstAveragesSD, shortAverages, shortAveragesSD, longAverages, longAveragesSD)



#### Accuracy testing ####

predictionAccuracyShort <- vector()
predictionAccuracyLong <- vector()

for(i in finalData$years)
{
  if (finalData$shortAverages[finalData$years == i] <= finalData$firstAverages[finalData$years == i] + finalData$firstAveragesSD[finalData$years == i] 
      & finalData$shortAverages[finalData$years == i] >= finalData$firstAverages[finalData$years == i] - finalData$firstAveragesSD[finalData$years == i])
  {predictionAccuracyShort <- append(predictionAccuracyShort, TRUE)}
  else {predictionAccuracyShort <- append(predictionAccuracyShort, FALSE)}
  
  temp <- (finalData$shortAverages[finalData$years == i] + finalData$longAverages[finalData$years == i] + finalData$longAverages[finalData$years == i])/3
  
  if (temp <= finalData$firstAverages[finalData$years == i] + finalData$firstAveragesSD[finalData$years == i] 
      & temp >= finalData$firstAverages[finalData$years == i] - finalData$firstAveragesSD[finalData$years == i])
  {predictionAccuracyLong <- append(predictionAccuracyLong, TRUE)}
  else {predictionAccuracyLong <- append(predictionAccuracyLong, FALSE)}
}

finalData$predictionAccuracyShort <- predictionAccuracyShort
finalData$predictionAccuracyLong <- predictionAccuracyLong



#### Compare against  groundhog #####

correctPrediction <- vector()
predict <- vector()
Nullpredict <- vector()


for (r in 1:length(finalData$predictionAccuracyLong))
{
  if(dataGroundHog$Shadow[r] == "yes")
  {
    predict <- append(predict, TRUE)
    if(finalData$predictionAccuracyLong[r])
    {
      correctPrediction <- append(correctPrediction, TRUE)
    }
    else
    {
      correctPrediction <- append(correctPrediction, FALSE)
    }
    Nullpredict <- append(Nullpredict, TRUE)
    
  }
  else if(dataGroundHog$Shadow[r] == "no")
  {
    predict <- append(predict, FALSE)
    if(finalData$predictionAccuracyShort[r] & !finalData$predictionAccuracyLong[r])
    {
      correctPrediction <- append(correctPrediction, TRUE)
    }
    else
    {
      correctPrediction <- append(correctPrediction, FALSE)
    }
    Nullpredict <- append(Nullpredict, TRUE)
  }
  else
  {
    predict <- append(predict, FALSE)
    correctPrediction <- append(correctPrediction, FALSE)
    Nullpredict <- append(Nullpredict, FALSE)
    
  }
  
}


finalData$correctPrediction <- correctPrediction
finalData$predict <- predict
finalData$Nullpredict <- Nullpredict




####make plot####
plot(finalData$years, finalData$firstAverages,
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Temperature C")),
     lwd=2,
     ylim=c(20,65),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes

polygon(c(finalData$years, rev(finalData$years)),#x coordinates
        c(finalData$firstAverages - finalData$firstAveragesSD, rev(finalData$firstAveragesSD + finalData$firstAverages)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

lines(finalData$years, finalData$shortAverages, col = "dodgerblue3")
lines(finalData$years, (finalData$shortAverages + finalData$longAverages + finalData$longAverages)/3, col = "darkorange1")


axis(1, seq(2000,2021, by=2), #tick intervals
     lab=seq(2000,2021, by=2)) #tick labels
axis(2, seq(20, 65, by=5),
     seq(20, 65, by=5),
     las = 2)#show ticks at 90 degree angle

legend("top", c("Standard Year Mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

# Add points for long data
points(x = finalData$years[finalData$correctPrediction&finalData$predict&finalData$Nullpredict],
       y = finalData$longAverages[finalData$correctPrediction&finalData$predict&finalData$Nullpredict],
       pch = 16,
       col = "darkgreen")

# Add points for short data
points(x = finalData$years[finalData$predict == FALSE&finalData$correctPrediction&finalData$Nullpredict],
       y = finalData$shortAverages[finalData$predict == FALSE&finalData$correctPrediction&finalData$Nullpredict],
       pch = 16,
       col = "darkgreen")


# Add points for long data
points(x = na.omit(finalData$years[finalData$correctPrediction==FALSE&finalData$predict&finalData$Nullpredict]),
       y = na.omit(finalData$longAverages[finalData$correctPrediction == FALSE&finalData$predict&finalData$Nullpredict]),
       pch = 16,
       col = "firebrick1")

# Add points for short data
points(x = na.omit(finalData$years[finalData$predict == FALSE&finalData$correctPrediction == FALSE&finalData$Nullpredict]),
       y = na.omit(finalData$shortAverages[finalData$predict == FALSE&finalData$correctPrediction == FALSE&finalData$Nullpredict]),
       pch = 16,
       col = "firebrick1")


#### Confirm against snow data ####

rainDay <- vector()
snowDay <- vector()


for (h in 1:21)
{
  if (length(dataSorted$PRCP[dataSorted$setFlag == h&dataSorted$flag == 'day'])!= 0 && !is.na(dataSorted$PRCP[dataSorted$setFlag == h&dataSorted$flag == 'day']))
  {
    if (dataSorted$PRCP[dataSorted$setFlag == h&dataSorted$flag == 'day'] > .2)
    {
      rainDay <- append(rainDay, TRUE)
    }
    else
    {
      rainDay <- append(rainDay, FALSE)
    }
  }
  else 
  {rainDay <- append(rainDay, FALSE)}
  
  if (length(dataSorted$SNOW[dataSorted$setFlag == h&dataSorted$flag == 'day'])!= 0 && !is.na(dataSorted$SNOW[dataSorted$setFlag == h&dataSorted$flag == 'day']))
  {
    if (dataSorted$SNOW[dataSorted$setFlag == h&dataSorted$flag == 'day'] > .2)
    {
      snowDay <- append(snowDay, TRUE)
    }
    else
    {
      snowDay <- append(snowDay, FALSE)
    }
  }
  else {snowDay <- append(snowDay, FALSE)}
  
}

finalData$snow <- snowDay
finalData$rain <- rainDay


#### PRECIP accuracy ####

rainy = 0

for (k in 1 : length(finalData$rain))
{
  if (finalData$rain[k]) {rainy <- rainy+1}
}

rainy/21

snowy = 0

for (k in 1 : length(finalData$rain))
{
  if (finalData$snow[k]) {snowy <- snowy+1}
}

snowy/21


rainy2 = 0

for (k in 1 : length(finalData$rain))
{
  if (finalData$rain[k] & !finalData$correctPrediction[k]) {rainy2 <- rainy2+1}
}

RainIncorrect <- rainy2/rainy

snowy2 = 0

for (k in 1 : length(finalData$rain))
{
  if (finalData$snow[k] & !finalData$correctPrediction[k]) {snowy2 <- snowy2+1}
}

SnowIncorrect <- snowy2/snowy


rainy3 = 0

for (k in 1 : length(finalData$rain))
{
  if ((finalData$rain[k] & !finalData$predict[k]) &finalData$Nullpredict) {rainy3 <- rainy3+1}
}

RainPredict <- rainy3/rainy

snowy3 = 0

for (k in 1 : length(finalData$rain))
{
  if ((finalData$snow[k] & !finalData$predict[k])&finalData$Nullpredict) {snowy3 <- snowy3+1}
}

SnowPredict <- snowy3/snowy


#### precip total #####


firstRain <- aggregate(dataSorted$PRCP[dataSorted$flag == 'before'], list(dataSorted$setFlag[dataSorted$flag == 'before']), sum, drop = TRUE, na.rm = TRUE)
shortRain <- aggregate(dataSorted$PRCP[dataSorted$flag == 'after1'], list(dataSorted$setFlag[dataSorted$flag == 'after1']), sum, drop = TRUE, na.rm = TRUE) 
longRain <- aggregate(dataSorted$PRCP[dataSorted$flag == 'after2'], list(dataSorted$setFlag[dataSorted$flag == 'after2']), sum, drop = TRUE, na.rm = TRUE) 

finalData$firstRain <- firstRain$x
finalData$shortRain <- shortRain$x
finalData$longRain <- longRain$x


#### bar precip long winter ####

barData <-NULL
barData2 <-NULL
barData3 <-NULL


barData <- data.frame(years = c(finalData$years[finalData$predict&finalData$Nullpredict]), precipitation = c(finalData$firstRain[finalData$predict&finalData$Nullpredict]),
                      site = c("Current Winter"))

barData2 <- data.frame(years = c(finalData$years[finalData$predict&finalData$correctPrediction&finalData$Nullpredict]), precipitation = c(finalData$longRain[finalData$predict&finalData$correctPrediction&finalData$Nullpredict]),
                      site = c("Correct Long Winter"))

if (length(finalData$years[finalData$predict &!finalData$correctPrediction & finalData$Nullpredict]) != 0)
  {barData3 <- data.frame(years = c(finalData$years[finalData$predict&!finalData$correctPrediction & finalData$Nullpredict]), precipitation = c(finalData$longRain[finalData$predict&!finalData$correctPrediction&finalData$Nullpredict]),
                       site = c("Incorrect Long Winter"))}
  
finalData$longRain[finalData$predict&!finalData$correctPrediction&finalData$Nullpredict]
barFinal <- rbind(barData, barData2)
barFinal <- rbind(barFinal, barData3)

ggplot(data=barFinal, aes(x= years, y = precipitation, fill = site)) +
          geom_bar(stat="identity", position=position_dodge())+
    ggtitle("Precipitation") +
  xlab("Years") + ylab("Precipitation (in)") + labs(fill = "Time")



#### bar precip short winter ####

barData <- data.frame(years = c(finalData$years[!finalData$predict&finalData$Nullpredict]), precipitation = c(finalData$firstRain[!finalData$predict&finalData$Nullpredict])/3,
                      site = c("Current Winter"))

barData2 <- data.frame(years = c(na.omit(finalData$years[!finalData$predict&finalData$correctPrediction&finalData$Nullpredict])), precipitation = c(na.omit(finalData$longRain[!finalData$predict&finalData$correctPrediction&finalData$Nullpredict])),
                       site = c("Correct Short Winter"))

barData3 <- data.frame(years = c(na.omit(finalData$years[!finalData$predict&!finalData$correctPrediction&finalData$Nullpredict])), precipitation = c(na.omit(finalData$longRain[!finalData$predict&!finalData$correctPrediction&finalData$Nullpredict])),
                       site = c("Incorrect Short Winter"))

barFinal <- rbind(barData, barData2)
barFinal <- rbind(barFinal, barData3)

ggplot(data=barFinal, aes(x= factor(years), y = precipitation, fill = site)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Precipitation") +
  xlab("Years") + ylab("Precipitation (in)") + labs(fill = "Time")

