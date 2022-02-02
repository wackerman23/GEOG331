#Will Ackerman
#01/31/21
#Activity 2

#set the working directory
setwd("Z:/students/wackerman/NOAA Weather/")

# Import NOAA Data
weather <- read.csv("Z:\\students\\wackerman\\NOAA Weather\\2011124.csv", stringsAsFactors = T)

#print weather data information
str(weather)


#calculate number of rows
nrow(weather)

#calculate number of columns
ncol(weather)

#change the date format
weather$dateF <- as.Date(weather$DATE, "%Y-%m-%d")

#change year to numeric
weather$year <- as.numeric(format(weather$dateF,"%Y"))

#find the data names
unique(weather$NAME)

#maximum mean temp of Aberdeen
mean(weather$TMAX[weather$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#average daily temp for Aberdeen
weather$TAVE <- weather$TMIN + ((weather$TMAX-weather$TMIN)/2)

#mean temp for all site across data set
averageTemp <- aggregate(weather$TAVE, by=list(weather$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the column names
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#change the name of the sites to numeric coding
weather$siteN <- as.numeric(weather$NAME)

#Histograms
#_________________________________________________________

#make a histogram for the first site in our levels
#main= is the title name argument.
hist(weather$TAVE[weather$siteN == 1],
     freq=FALSE, 
     main = paste(levels(weather$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")


#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#lty changes dash pattern
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE) - sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE) + sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

