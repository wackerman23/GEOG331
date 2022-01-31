#Will Ackerman
#01/31/21
#Activity 2

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


