#Will Ackerman
#Activity 4

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
index = 1
count = 1

all[[1]] <- flower$Sepal.Length 
all[[2]] <- flower$Sepal.Width
all[[3]] <- flower$Petal.Length
all[[4]] <- flower$Petal.Width
all[[5]] <- flower$Sepal.Length
all[[6]] <- flower$Petal.Length

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
for(i in 1:(length(all)/2)){

  #1. iris  sepal length x width
  regressions[[index]] <- summary(lm(all[[count]] ~ all[[count+1]]))
  count = count+2
  index = index+1

  
 
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
Iris2 <- left_join(
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
#The ggplot had a lot more variables. This was helpful as it allowed
#more customization within the scatter plot. For example, using different
#colors, the three different species of iris were able to be differentiated
#on the scatter plot. 

