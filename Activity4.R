#use built in iris dataset
#take a look at it 
head(iris)
#whole table
Full <- iris
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
for(i in 1:length(flower$Sepal.Length)){
   
  #1. iris  sepal length x width
  sepal <- vector()
  sepal <- lm(flower$Sepal.Length ~ flower$Sepal.Width)
  regressions1 <- append(1, sepal)
  
  #2. iris  petal length x width
  petal <- lm(flower$Petal.Length ~ flower$Petal.Width)
  regressions2 <- append(2, petal)
  
  #3. iris sepal length x petal length
  sepalxpetal <- lm(flower$Sepal.Length ~ flower$Petal.Length) 
  regressions3 <- append(3, sepalxpetal)
  
 regressions <-append(1, regressions1)
}


flower$Sepal.Length[1]
sepal[1]
length(regressions)

# hint: consider using a list, and also new vectors for regression variables



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

NewT <- data.frame()

shark <- full_join(
  iris,
  height,
  by = "Species",
  copy = FALSE,
  keep = TRUE
)


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

