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

#Question 2
#character vector
words = c("apple", "peach", "pear", "orange", "strawberry")

#Integer
ints = c(3L, 35L, 7L, 90L, 23L)

#Numeric
nums =c(23.5,78, 55.9,90.0, 23.3)

#Factors
appleTypes = c('green','green','yellow','red','red')
appleFactor = factor(appleTypes)

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

#Question 3
help(hist)
help(paste)
#Question 4 
#create the histogram squares
par(mfrow = c(2,2))

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

#Histogram 2
#--------------------------------------------------------
#make a histogram for the first site in our levels
#main= is the title name argument.
hist(weather$TAVE[weather$siteN == 2],
     freq=FALSE, 
     main = paste(levels(weather$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="coral1",
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



#Histogram 3
#--------------------------------------------------------
#make a histogram for the first site in our levels
#main= is the title name argument.
hist(weather$TAVE[weather$siteN == 3],
     freq=FALSE, 
     main = paste(levels(weather$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue3",
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


#Histogram 4
#--------------------------------------------------------
#make a histogram for the first site in our levels
#main= is the title name argument.
hist(weather$TAVE[weather$siteN == 4],
     freq=FALSE, 
     main = paste(levels(weather$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="deepskyblue3",
     border="white")
weather$NAME(3)


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


#add probability curve
#--------------------------------------------------------------
#make a histogram for the first site in our levels
#using the weather index for Aberdeen 
#note I've named the histogram so I can reference it later
h1 <- hist(weather$TAVE[weather$siteN == 1],
           freq=FALSE,
           main = paste(levels(weather$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the 
#normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
                 sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a 
#different range from the data density.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguments are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "tomato3",
       lwd = 4, 
       lty = 2)


#Weather averages
#----------------------------------------------------------------------
#pnorm(value to evaluate at mean, standard deviation)
pnorm(0,
      mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
      sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))


#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
      sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
      sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))- 
  pnorm(0,
        mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
        sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))





#pnorm of 20 gives the probability (area of the curve) below 20 
#subtracting from one leaves the area above 20
1 - pnorm(20,
          mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
          sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))

#qnorm of .95 - .05 gives the probability negating the outer 10% of data 
qnorm(0.95,
      mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
      sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))-
  qnorm(0.05,
        mean(weather$TAVE[weather$siteN == 1],na.rm=TRUE),
        sd(weather$TAVE[weather$siteN == 1],na.rm=TRUE))
