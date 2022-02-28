#import the Iris Data Set
flower <- iris[iris$Species == "virginica",]

#plot the scatter plot
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19,
     xlab = "Sepal Length", ylab = "petal length",
     main="Iris Virignica")

#look at a linear model fit
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

#information about the repgression 
summary(fit)

#plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 16,
     xlab = "Sepal Length", ylab = "residuals",
     main="Regression Residuals", col = "purple")

#add horizontal line for reference
abline(h=0, lty = "dashed")

#create histogram
hist(summary(fit)$residuals, pch = 16,
     xlab = "Sepal Length", ylab = "petal length",
     main="Regression Residuals", col = "purple")

#shapiro test
shapiro.test(summary(fit)$residuals)

#add distribution line
qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm, 
               probs = c(0.25, .75), qtype = 7, pch = 16)
