#use built in iris dataset
#take a look at it 
head(iris)
#versicolor
flower <- iris[iris$Species == "versicolor",]

#load in some tidyverse packages
install.packages(c("dplyr"))
library("dplyr")
install.packages(c("ggplot2"))
library("ggplot2")

#####################################
##### Part 1: for loops         #####
#####################################

#Initialization of vectors
regressions <- vector()

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
for(i in 1:length(flower$Species)){
   
  #1. iris  sepal length x width
  sepal <- lm(flower$Sepal.Length ~ flower$Sepal.Width)
  regressions[1] <- sepal
  #2. iris  petal length x width
  petal <- flower$Petal.Length[i] * flower$Petal.Width[i]
  regressions[2] <- petal
  
  #3. iris sepal length x petal length
  sepalxpetal[i] <- flower$Sepal.Length[i] * flower$Petal.Length[i] 
  regressions[3] <- sepalxpetal
  
 
}
flower$Sepal.Length[1]
sepal[1]
length(sepal)

# hint: consider using a list, and also new vectors for regression variables



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

irisD <- data.frame()



#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot


#3b. make a scatter plot with ggplot and get rid of  busy grid lines


#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

