#Will Ackerman
#Activity 5

#Initializing Data ----------------------------
#set the working directory
setwd("/Users/willackerman/GEOG331/streamflow")

#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("/Users/willackerman/GEOG331/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)    

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Users/willackerman/GEOG331/streamflow/2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))          


#### Question 3 ####
#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

tabulate(datD$doy)
length(datD$discharge)
length(datD$decYear)
length(datD$decYear)/(365-270 + (365*11))

#### Question 4 + Question 5 + Question 6 ####


#basic formatting
aveF <- aggregate(datD$discharge[datD$doy < 366], by=list(datD$doy[datD$doy < 366]), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge[datD$doy < 366], by=list(datD$doy[datD$doy < 366]), FUN="sd")
colnames(sdF) <- c("doy","dailySD")
D17 <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$doy[datD$year == 2017]), FUN="mean")
colnames(D17) <- c("doy","Ave2017")
aveF <- merge(aveF, D17, by="doy")


#start new plot
dev.new(width=8,height=8)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy, aveF$dailyAve,
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
lines(aveF$doy, aveF$Ave2017, col = "green")

axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


  
#### Question 7 ####



finalPRCP <- vector()
days <- vector()

  for(i in 2007:2014)
  {
    if(leap_year(i)) {index <-366}
    else {index <-365}
    for(j in 1:index)
    {
      vectorCheck1 <- c(datP$year[datP$doy == j])
      vectorCheck2 <- c(datP$HPCP[datP$doy == j])
      vectorCheck5 <- c(datP$decYear[datP$doy == j])
      vectorCheck3 <- c(vectorCheck1[vectorCheck1 == i])
      vectorCheck4 <- c(vectorCheck2[vectorCheck1 == i])
      vectorCheck6 <- c(vectorCheck5[vectorCheck1 == i])
      
      if(length(vectorCheck3) == 24)
      {
        finalPRCP <- append(finalPRCP, vectorCheck4)
        days <- append(days, vectorCheck6)
      }
      
      
    }
  }

PRCP <- data.frame(finalPRCP, days)

plot(PRCP$days, PRCP$finalPRCP, pch=12, type="l", xlab = "Decimal Day of Year",
     ylab="Precipitation (mm)")


#### Question 8 ####

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 352 & datD$doy < 354 & datD$year == 2012,]
hydroP <- datP[datP$doy >= 352 & datP$doy < 354 & datP$year == 2012,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#### Question 9 ####
library(ggplot2)

#specify year as a factor
datD$yearPlot <- as.factor(datD$year)

datPlot <- data.frame(datD$decDay[datD$yearPlot == 2016], datD$discharge[datD$yearPlot == 2016])
colnames(datPlot) <- c("doy", "discharge")

flags <- vector()

for(i in datPlot$doy)
{
 
  if(i >= 79.0 & i <= 170.25){
    flags <- append(flags, 'Spring')}
  else if(i >= 170.25 & i <= 261.5){flags <- append(flags, 'Summer')}
  else if(i >= 261.5 & i <= 352.75){flags <- append(flags, 'Fall')}
  else if(i >= 352.75 || i <= 79.0){flags <- append(flags, 'Winter')}
}

datPlot$season <- flags

#make a violin plot
ggplot(data= datPlot, aes(flags, discharge)) + 
  geom_violin()


datPlot <- data.frame(datD$decDay[datD$yearPlot == 2017], datD$discharge[datD$yearPlot == 2017])
colnames(datPlot) <- c("doy", "discharge")

flags <- vector()
for(i in datPlot$doy)
{
  if(i >= 79.0 & i <= 170.25){
    flags <- append(flags, 'Spring')}
  else if(i >= 170.25 & i <= 261.5){flags <- append(flags, 'Summer')}
  else if(i >= 261.5 & i <= 352.75){flags <- append(flags, 'Fall')}
  else if(i >= 352.75 || i <= 79.0){
    flags <- append(flags, 'Winter')}
}

datPlot$season <- flags

#make a violin plot
ggplot(data= datPlot, aes(flags, discharge)) + 
  geom_violin()

