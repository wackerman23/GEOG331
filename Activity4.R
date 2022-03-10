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
regressions <- list()

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
for(i in 1:1){

  #1. iris  sepal length x width
  regressions[[i]] <- summary(lm(flower$Sepal.Length ~ flower$Sepal.Width))

  #2. iris  petal length x width
  regressions[[i +1]] <- summary(lm(flower$Petal.Length ~ flower$Petal.Width))

  #3. iris sepal length x petal length
  regressions[[i+2]] <- summary(lm(flower$Sepal.Length ~ flower$Petal.Length))
 
 
}



# hint: consider using a list, and also new vectors for regression variables



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
as_tibble(height)
as_tibble(iris)
shark <- left_join(
  iris,
  height,
  by = "Species",
  copy = FALSE,
  keep = FALSE
)


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#plot with no gridlines
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  #Titles
  ggtitle("Plot of Sepal Length vs Width") +
  xlab("Sepal Length") + ylab("Sepal Width")+
  #color and size
  geom_point(aes(colour = Species), size = iris$Petal.Length/2)
  


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

